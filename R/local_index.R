#' Paths of the location index
#' @keywords internal
#' @noRd
gvs_index_path <- function(what, dir = gvs_cache_dir()) {
  switch(what,
    units = file.path(dir, "gadmindex-units.gz.parquet"),
    raster = file.path(dir, "gadmindex-units-30s.tif"),
    edges = file.path(dir, "gadmindex-edges-30s.tif"),
    stop("unknown index file: ", what)
  )
}

#' Build the raster location index for the current GADM release
#'
#' Internal.  Locating a point in its country, state/province and county/parish
#' by exact point-in-polygon against GADM is too slow for large inputs.  This
#' builds, once, a 30 arc-second raster of GADM level-2 divisions (level 1 or 0
#' where a country has no level 2) and a mask of cells near a division boundary.
#' A point in a cell well inside one division is located by raster lookup; a point
#' in a boundary cell, or in a cell no division covers (coasts), is resolved
#' exactly against the GeoPackage when one is available and flagged otherwise.
#'
#' Method, chosen to bound memory: GDAL burns each feature's unit number straight
#' from the GeoPackage, the unit being numbered in SQL (\code{DENSE_RANK} over the
#' unit key), so no features are loaded into R and no reclassification is
#' needed.  Boundary cells are those whose (2 \code{edge_radius} + 1)-cell square
#' neighbourhood holds more than one value (a unit, or no unit), computed in
#' blocks of rows.  Because a cell takes the unit at its centre, a sliver of
#' another division narrower than a cell can be missed; \code{edge_radius} widens
#' the margin.
#'
#' Outputs: \code{gadmindex-units.gz.parquet} (unit -> gid_0, gid_1, gid_2 and
#' names), \code{gadmindex-units-30s.tif} (INT32, 0 = no division) and
#' \code{gadmindex-edges-30s.tif} (INT8, 1 = boundary cell).
#' @keywords internal
#' @noRd
gvs_build_index <- function(gpkg, gadm_layer = "gadm_410", dir = gvs_cache_dir(create = TRUE),
                            resolution = 1 / 120, edge_radius = 1, block_rows = 500, quiet = FALSE) {
  for (pkg in c("sf", "terra", "nanoparquet")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Building the index needs the '", pkg, "' package.", call. = FALSE)
  }
  t0 <- Sys.time()
  el <- function() round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  blank <- function(col) sprintf("(%s IS NULL OR %s IN ('', 'NA'))", col, col)
  key_sql <- sprintf("CASE WHEN NOT %s THEN GID_2 WHEN NOT %s THEN GID_1 ELSE GID_0 END",
                     blank("GID_2"), blank("GID_1"))
  unit_sql <- sprintf("DENSE_RANK() OVER (ORDER BY %s)", key_sql)

  # ---- units: one per level-2 division (or the finest level a country has, to 2)
  if (!quiet) message("Reading GADM attributes ...")
  attrs <- sf::st_drop_geometry(sf::st_read(
    gpkg, quiet = TRUE,
    query = sprintf('SELECT %s AS unit, GID_0, COUNTRY, GID_1, NAME_1, GID_2, NAME_2 FROM "%s"', unit_sql, gadm_layer)
  ))
  isblank <- function(x) is.na(x) | !nzchar(x) | x == "NA"
  first <- attrs[!duplicated(attrs$unit), , drop = FALSE]
  first <- first[order(first$unit), , drop = FALSE]
  stopifnot(identical(as.integer(first$unit), seq_len(nrow(first))))
  units <- data.frame(
    unit = as.integer(first$unit), gid_0 = first$GID_0, country = first$COUNTRY,
    gid_1 = ifelse(isblank(first$GID_1), NA_character_, first$GID_1),
    name_1 = ifelse(isblank(first$GID_1), NA_character_, first$NAME_1),
    gid_2 = ifelse(isblank(first$GID_2), NA_character_, first$GID_2),
    name_2 = ifelse(isblank(first$GID_2), NA_character_, first$NAME_2),
    stringsAsFactors = FALSE
  )
  nanoparquet::write_parquet(units, gvs_index_path("units", dir), compression = "gzip")
  if (!quiet) message("  ", nrow(attrs), " GADM features in ", nrow(units), " units (", el(), " min)")

  # ---- burn unit numbers with GDAL, streaming from the GeoPackage
  units_tif <- gvs_index_path("raster", dir)
  unlink(units_tif)
  if (!quiet) message("Rasterizing GADM units at ", round(resolution * 3600), " arc-seconds ...")
  sf::gdal_utils("rasterize", gpkg, units_tif, options = c(
    "-sql", sprintf('SELECT %s AS unit, geom FROM "%s"', unit_sql, gadm_layer),
    "-a", "unit", "-a_nodata", "0", "-init", "0",
    "-tr", format(resolution, digits = 15), format(resolution, digits = 15),
    "-te", "-180", "-90", "180", "90", "-ot", "Int32",
    "-co", "COMPRESS=DEFLATE", "-co", "TILED=YES", "-co", "BIGTIFF=YES"
  ), quiet = TRUE)
  if (!quiet) message("  done (", el(), " min)")

  # ---- boundary cells
  if (!quiet) message("Marking boundary cells ...")
  gvs_mark_edges(units_tif, gvs_index_path("edges", dir), radius = edge_radius, block_rows = block_rows)
  if (!quiet) message("  done (", el(), " min)")

  provenance <- list(
    source = "index", full_name = "GVS raster location index (GADM level 2 at 30 arc-seconds)",
    version = gvs_builtin_registry()$gadm$version, downloaded = as.character(Sys.Date()),
    resolution_arcsec = round(resolution * 3600), edge_radius = edge_radius,
    n_features = nrow(attrs), n_units = nrow(units), built_minutes = el()
  )
  saveRDS(provenance, gvs_provenance_path("gadmindex", dir))
  invisible(provenance)
}

#' Mark cells whose square neighbourhood holds more than one unit
#'
#' Internal.  Reads the unit raster in blocks of rows (with \code{radius} rows of
#' overlap), takes running maxima and minima over the neighbourhood, and writes 1
#' where they differ.  No-division cells count as a value (0), so coastal cells
#' are boundary cells.  Columns do not wrap at the antimeridian.
#' @keywords internal
#' @noRd
gvs_mark_edges <- function(units_tif, edges_tif, radius = 1, block_rows = 500) {
  r <- terra::rast(units_tif)
  nr <- terra::nrow(r)
  nc <- terra::ncol(r)
  shift_ext <- function(m, fun) {
    # running fun over columns then rows, window 2 * radius + 1, edges replicated
    out <- m
    for (k in seq_len(radius)) {
      left <- cbind(m[, -seq_len(k), drop = FALSE], m[, rep(nc, k), drop = FALSE])
      right <- cbind(m[, rep(1L, k), drop = FALSE], m[, seq_len(nc - k), drop = FALSE])
      out <- fun(out, left, right)
    }
    m2 <- out
    nrm <- nrow(m2)
    for (k in seq_len(radius)) {
      up <- rbind(m2[-seq_len(k), , drop = FALSE], m2[rep(nrm, k), , drop = FALSE])
      down <- rbind(m2[rep(1L, k), , drop = FALSE], m2[seq_len(nrm - k), , drop = FALSE])
      out <- fun(out, up, down)
    }
    out
  }
  terra::readStart(r)
  on.exit(terra::readStop(r), add = TRUE)
  tmpl <- terra::rast(r)
  unlink(edges_tif)
  b <- terra::writeStart(tmpl, edges_tif, overwrite = TRUE,
                         wopt = list(datatype = "INT1U", names = "edge",
                                     gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")))
  for (s in seq(1L, nr, by = block_rows)) {
    e <- min(nr, s + block_rows - 1L)
    rs <- max(1L, s - radius)
    re <- min(nr, e + radius)
    v <- terra::readValues(r, row = rs, nrows = re - rs + 1L, col = 1L, ncols = nc, mat = FALSE)
    v[is.na(v)] <- 0L
    m <- matrix(as.integer(v), nrow = re - rs + 1L, ncol = nc, byrow = TRUE)
    rm(v)
    edge <- shift_ext(m, pmax) != shift_ext(m, pmin)
    keep <- (s - rs + 1L):(e - rs + 1L)
    terra::writeValues(tmpl, as.integer(t(edge[keep, , drop = FALSE])), s, length(keep))
    rm(m, edge)
    gc(verbose = FALSE)
  }
  terra::writeStop(tmpl)
  invisible(edges_tif)
}

#' Locate points in current GADM divisions
#'
#' Internal.  Raster lookup, with exact point-in-polygon for points in boundary
#' cells or in cells no division covers, when \code{gpkg} is given.  Points are
#' processed as unique coordinates.
#'
#' @return data.frame with gid_0, gid_1, gid_2, country, name_1, name_2, and
#'   \code{locate_method}: "raster", "exact", "exact: outside all divisions", or
#'   "raster (boundary cell, not verified)" / "outside raster (not verified)" when
#'   no GeoPackage was given.
#' @keywords internal
#' @noRd
gvs_locate_current <- function(lon, lat, dir = gvs_cache_dir(), gpkg = NULL, gadm_layer = "gadm_410",
                               tile_deg = 1) {
  units <- as.data.frame(nanoparquet::read_parquet(gvs_index_path("units", dir)))
  ru <- terra::rast(gvs_index_path("raster", dir))
  re <- terra::rast(gvs_index_path("edges", dir))

  dd0 <- gvs_dedupe(lon, lat)
  ulon <- lon[dd0$first]
  ulat <- lat[dd0$first]
  ok <- is.finite(ulon) & is.finite(ulat) & abs(ulat) <= 90 & abs(ulon) <= 180
  xy <- cbind(ulon[ok], ulat[ok])

  unit <- rep(NA_integer_, length(ulon))
  edge <- rep(NA, length(ulon))
  unit[ok] <- as.integer(terra::extract(ru, xy)[, 1])
  unit[ok & unit %in% 0L] <- NA_integer_
  edge[ok] <- as.integer(terra::extract(re, xy)[, 1]) %in% 1L

  method <- rep(NA_character_, length(ulon))
  method[ok] <- "raster"
  needs_exact <- ok & (is.na(unit) | edge)

  if (any(needs_exact)) {
    if (is.null(gpkg)) {
      method[needs_exact & !is.na(unit)] <- "raster (boundary cell, not verified)"
      method[needs_exact & is.na(unit)] <- "outside raster (not verified)"
    } else {
      idx <- which(needs_exact)
      exact <- gvs_exact_units(ulon[idx], ulat[idx], units, gpkg, gadm_layer, tile_deg)
      unit[idx] <- exact
      method[idx] <- ifelse(is.na(exact), "exact: outside all divisions", "exact")
    }
  }

  m <- dd0$group
  un <- unit[m]
  data.frame(
    gid_0 = units$gid_0[un], country = units$country[un],
    gid_1 = units$gid_1[un], name_1 = units$name_1[un],
    gid_2 = units$gid_2[un], name_2 = units$name_2[un],
    locate_method = method[m], stringsAsFactors = FALSE
  )
}

#' Exact point-in-polygon against the GeoPackage, one tile at a time
#' @keywords internal
#' @noRd
gvs_exact_units <- function(lon, lat, units, gpkg, gadm_layer, tile_deg = 1) {
  old_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  ukey <- ifelse(!is.na(units$gid_2), units$gid_2, ifelse(!is.na(units$gid_1), units$gid_1, units$gid_0))
  out <- rep(NA_integer_, length(lon))
  tx <- floor(lon / tile_deg)
  ty <- floor(lat / tile_deg)
  tiles <- unique(data.frame(tx, ty))
  for (t in seq_len(nrow(tiles))) {
    i <- which(tx == tiles$tx[t] & ty == tiles$ty[t])
    x0 <- tiles$tx[t] * tile_deg
    y0 <- tiles$ty[t] * tile_deg
    bb <- sf::st_as_sfc(sf::st_bbox(c(xmin = x0, ymin = y0, xmax = x0 + tile_deg, ymax = y0 + tile_deg),
                                     crs = sf::st_crs(4326)))
    g <- suppressMessages(suppressWarnings(sf::st_read(
      gpkg, quiet = TRUE, wkt_filter = sf::st_as_text(bb),
      query = sprintf('SELECT GID_0, GID_1, GID_2, geom FROM "%s"', gadm_layer)
    )))
    if (!nrow(g)) next
    blank <- function(x) is.na(x) | !nzchar(x) | x == "NA"
    gkey <- ifelse(!blank(g$GID_2), g$GID_2, ifelse(!blank(g$GID_1), g$GID_1, g$GID_0))
    p <- sf::st_as_sf(data.frame(lon = lon[i], lat = lat[i]), coords = c("lon", "lat"), crs = 4326)
    hit <- suppressMessages(sf::st_intersects(p, g))
    first <- vapply(hit, function(h) if (length(h)) h[1] else NA_integer_, integer(1))
    out[i] <- match(gkey[first], ukey)
  }
  out
}
