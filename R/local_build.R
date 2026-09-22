#' Install the reference data GVS_local() resolves against
#'
#' Builds the local copy of the reference data, so that coordinates can be
#' validated with no calls to the GVS API.  Two components:
#'
#' \code{"gadm"} is the GADM release the divisions come from: the world
#' GeoPackage (a large download), from which the attributes of levels 0 to 2 are
#' kept.  This component is shared with the GNRS package and is skipped if that
#' package has already built it.  A GeoPackage already on disk can be used
#' instead of downloading one, with \code{gpkg}.
#'
#' \code{"centroids"} is what the resolver reads: for every country,
#' state/province and county/parish, the six centroid types the service uses
#' (centre of mass, point on surface and bounding-box centre, each for the whole
#' division and for its largest part) and, for each, the greatest distance from
#' that centroid to the division's boundary.  Relative distances are measured
#' against those maxima, which is how a point 2 km from the centroid of a small
#' county and a point 2 km from the centroid of a large country are told apart.
#' Deriving them needs the GADM geometry, so the GeoPackage must be present (or
#' downloadable) even though only these tables are kept.
#'
#' @param sources Components to build.  Defaults to both.
#' @param dir Cache directory.  Defaults to the standard user cache location,
#'   shared with GNRS.
#' @param gpkg Optional path to a GADM world GeoPackage already on disk.  Used
#'   instead of downloading, and not deleted afterwards.
#' @param gadm_layer Layer within the GeoPackage.  GADM 4.1 ships one layer of
#'   all levels.
#' @param overwrite Rebuild components that are already built?
#' @param keep_archive Keep the downloaded GADM archive after building?
#' @param quiet Suppress progress messages?
#' @return The status table, invisibly.
#' @seealso \code{\link{GVS_local}}, \code{\link{GVS_local_status}}
#' @export
#' @examples \dontrun{
#' # Using a GeoPackage already on disk
#' GVS_local_build(gpkg = "gadm_410.gpkg")
#'
#' # Downloading GADM (about 1.4 GB)
#' GVS_local_build()
#' }
GVS_local_build <- function(sources = c("gadm", "centroids"),
                            dir = gvs_cache_dir(create = TRUE),
                            gpkg = NULL,
                            gadm_layer = "gadm_410",
                            overwrite = FALSE,
                            keep_archive = FALSE,
                            quiet = FALSE) {
  registry <- gvs_builtin_registry()
  unknown <- setdiff(sources, names(registry))
  if (length(unknown) > 0) {
    stop("Unknown source(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  for (pkg in c("sf", "terra", "nanoparquet")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Building the local reference data needs the '", pkg, "' package. ",
        "Install it with install.packages(\"", pkg, "\") and build again.",
        call. = FALSE
      )
    }
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # The GeoPackage is needed by both components; it is downloaded once, used by
  # whichever of them runs, and removed afterwards unless the caller supplied it
  supplied <- !is.null(gpkg)
  if (supplied && !file.exists(gpkg)) {
    stop("No GeoPackage at: ", gpkg, call. = FALSE)
  }
  needs_geometry <- "centroids" %in% sources &&
    (overwrite || !gvs_is_built("centroids", dir))
  needs_attributes <- "gadm" %in% sources &&
    (overwrite || !gvs_is_built("gadm", dir))

  if (!supplied && (needs_geometry || needs_attributes)) {
    gpkg <- gvs_fetch_gadm(dir = dir, quiet = quiet)
    on.exit({
      unlink(gpkg)
      if (!keep_archive) unlink(gvs_gadm_archive_path(dir))
    }, add = TRUE)
  } else if (!needs_geometry && !needs_attributes && !quiet) {
    message("Already built; nothing to do (use overwrite = TRUE to rebuild).")
  }

  if (needs_attributes) {
    gvs_build_gadm_attributes(gpkg, gadm_layer, dir = dir, quiet = quiet)
  }
  if (needs_geometry) {
    gvs_build_centroids(gpkg, gadm_layer, dir = dir, quiet = quiet)
  }

  invisible(GVS_local_status(dir))
}

#' Download and extract the GADM world GeoPackage
#'
#' Internal.  Returns the path of the extracted GeoPackage.
#' @keywords internal
#' @noRd
gvs_fetch_gadm <- function(dir = gvs_cache_dir(), quiet = FALSE) {
  spec <- gvs_builtin_registry()$gadm
  archive <- gvs_gadm_archive_path(dir)

  if (file.exists(archive)) {
    if (!quiet) message("Using cached download of GADM ", spec$version)
  } else {
    if (!quiet) {
      message(
        "Downloading ", spec$full_name, " ", spec$version,
        " (about ", spec$download_mb, " MB) ..."
      )
    }
    partial <- paste0(archive, ".part")
    old <- options(timeout = max(7200, getOption("timeout")))
    on.exit(options(old), add = TRUE)
    status <- utils::download.file(spec$url, partial, mode = "wb", quiet = quiet, cacheOK = FALSE)
    if (status != 0 || !file.exists(partial)) {
      unlink(partial)
      stop("Download failed for GADM.", call. = FALSE)
    }
    file.rename(partial, archive)
  }

  if (!quiet) message("Extracting the GeoPackage ...")
  members <- utils::unzip(archive, list = TRUE)$Name
  member <- members[grepl("\\.gpkg$", members)][1]
  if (is.na(member)) {
    stop("No GeoPackage found in the GADM archive.", call. = FALSE)
  }
  utils::unzip(archive, files = member, exdir = dir, overwrite = TRUE)
  file.path(dir, member)
}

#' Keep the attributes of GADM levels 0 to 2
#'
#' Internal.  The same table GNRS builds, written to the same path, so that
#' either package can install it.  Read without touching the geometry.
#' @keywords internal
#' @noRd
gvs_build_gadm_attributes <- function(gpkg, gadm_layer, dir = gvs_cache_dir(), quiet = FALSE) {
  if (!quiet) message("Reading the GADM attributes ...")
  fields <- c("GID_0", "COUNTRY", "GID_1", "NAME_1", "GID_2", "NAME_2")
  d <- sf::st_read(
    gpkg,
    query = sprintf('SELECT %s FROM "%s"', paste(fields, collapse = ", "), gadm_layer),
    quiet = TRUE
  )
  d <- sf::st_drop_geometry(d)
  blank <- function(x) is.na(x) | !nzchar(x) | x == "NA"
  divisions <- rbind(
    unique(data.frame(
      level = 0L, gid = d$GID_0, name = d$COUNTRY,
      gid_0 = d$GID_0, gid_1 = NA_character_, gid_2 = NA_character_
    )),
    unique(data.frame(
      level = 1L, gid = d$GID_1, name = d$NAME_1,
      gid_0 = d$GID_0, gid_1 = d$GID_1, gid_2 = NA_character_
    )[!blank(d$GID_1), ]),
    unique(data.frame(
      level = 2L, gid = d$GID_2, name = d$NAME_2,
      gid_0 = d$GID_0, gid_1 = d$GID_1, gid_2 = d$GID_2
    )[!blank(d$GID_2), ])
  )
  divisions <- divisions[!duplicated(divisions$gid), ]
  nanoparquet::write_parquet(divisions, gvs_gadm_path(dir), compression = "gzip")

  spec <- gvs_builtin_registry()$gadm
  provenance <- list(
    source = "gadm", full_name = spec$full_name, version = spec$version,
    url = spec$url, license = spec$license, publisher = spec$publisher,
    downloaded = as.character(Sys.Date()),
    n_admin0 = sum(divisions$level == 0L),
    n_admin1 = sum(divisions$level == 1L),
    n_admin2 = sum(divisions$level == 2L)
  )
  saveRDS(provenance, gvs_provenance_path("gadm", dir))
  if (!quiet) {
    message(
      "  ", provenance$n_admin0, " countries, ", provenance$n_admin1,
      " level-1 and ", provenance$n_admin2, " level-2 divisions"
    )
  }
  invisible(provenance)
}

#' The six centroids of one set of dissolved divisions
#'
#' Internal.  \code{geom} is one dissolved division per element.  Column order
#' follows the service's own: centre of mass, point on surface, bounding-box
#' centre, then the same three computed on the division's largest part.  For a
#' single-part division the two halves coincide.
#'
#' Each centroid carries the greatest distance from it to the division's
#' boundary, which relative distances are measured against.  Distances are
#' Euclidean in degrees, as the service's relative distances are, and the
#' farthest boundary point of a polygon is always a vertex.  They are measured
#' to the WHOLE division even for the largest-part centroids, as the service
#' measures them (checked against its own tables: for the United States 279.2
#' degrees to the farthest vertex of the country, not 32.6 within the largest
#' part).  Distances and centroids are deliberately planar in WGS84 longitude and
#' latitude, including for divisions that span the antimeridian (Fiji's maximum is
#' 357.97 degrees).  The service is built to detect centroids that users assigned
#' naively, and a naive planar calculation for a dateline-straddling division puts
#' its centroid far from the division (near longitude 0); correcting for the
#' antimeridian here would stop those centroids from being recognised.  The
#' point-on-surface and largest-part centroid types cover users who avoided that
#' mistake.
#' @keywords internal
#' @noRd
gvs_six_centroids <- function(geom) {
  n <- length(geom)
  out <- matrix(NA_real_, n, 18)

  main <- gvs_largest_part(geom)

  for (half in 1:2) {
    g <- if (half == 1) geom else main
    cent <- suppressWarnings(sf::st_coordinates(sf::st_centroid(g)))
    pos <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(g)))
    bb <- vapply(g, function(x) {
      b <- sf::st_bbox(x)
      c((b[["xmin"]] + b[["xmax"]]) / 2, (b[["ymin"]] + b[["ymax"]]) / 2)
    }, numeric(2))
    types <- list(cent[, 1:2, drop = FALSE], pos[, 1:2, drop = FALSE], t(bb))
    for (k in seq_along(types)) {
      col <- (half - 1) * 9 + (k - 1) * 3 + 1
      out[, col] <- types[[k]][, 1]
      out[, col + 1] <- types[[k]][, 2]
      out[, col + 2] <- gvs_max_vertex_distance(geom, types[[k]])
    }
  }

  # c1..c6 as the service orders them: 1 std, 2 pos, 3 bb, 4 std_main, 5 pos_main, 6 bb_main
  colnames(out) <- paste0("c", rep(1:6, each = 3), c("_lon", "_lat", "_dmax"))
  as.data.frame(out)
}

#' The largest part of each (multi)polygon, by planar area
#' @keywords internal
#' @noRd
gvs_largest_part <- function(geom) {
  parts <- lapply(geom, function(g) {
    pieces <- suppressWarnings(sf::st_cast(sf::st_sfc(g), "POLYGON"))
    if (length(pieces) <= 1) {
      return(g)
    }
    areas <- suppressWarnings(as.numeric(sf::st_area(sf::st_set_crs(pieces, NA))))
    pieces[[which.max(areas)]]
  })
  sf::st_sfc(parts, crs = sf::st_crs(geom))
}

#' Greatest Euclidean-degree distance from each centroid to its division's vertices
#' @keywords internal
#' @noRd
gvs_max_vertex_distance <- function(geom, centres) {
  vapply(seq_along(geom), function(i) {
    xy <- sf::st_coordinates(sf::st_sfc(geom[[i]]))
    if (!nrow(xy) || any(is.na(centres[i, ]))) {
      return(NA_real_)
    }
    max(sqrt((xy[, 1] - centres[i, 1])^2 + (xy[, 2] - centres[i, 2])^2))
  }, numeric(1))
}

#' Derive the centroid tables from the GADM geometry
#'
#' Internal.  Countries are read one at a time, so peak memory is one country's
#' geometry rather than the world's.  GADM ships one row per finest division, so
#' the divisions of each level are dissolved from those rows: a level-2 division
#' is the union of its level-3 and finer children, and a country the union of
#' everything in it.  Dissolving matters for the largest-part centroids, where
#' the largest part of a country is an island rather than whichever administrative
#' piece of it happens to be biggest.
#' @keywords internal
#' @noRd
gvs_build_centroids <- function(gpkg, gadm_layer, dir = gvs_cache_dir(), quiet = FALSE) {
  old_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  countries <- sf::st_read(
    gpkg,
    query = sprintf('SELECT DISTINCT GID_0 FROM "%s" ORDER BY GID_0', gadm_layer),
    quiet = TRUE
  )
  countries <- sf::st_drop_geometry(countries)$GID_0
  if (!quiet) message("Deriving centroids for ", length(countries), " countries ...")

  levels <- gvs_levels()
  field <- c(country = "GID_0", state = "GID_1", county = "GID_2")
  acc <- stats::setNames(vector("list", length(levels)), levels)
  t0 <- Sys.time()

  for (i in seq_along(countries)) {
    g0 <- countries[i]
    v <- terra::vect(
      gpkg,
      query = sprintf(
        'SELECT GID_0, GID_1, GID_2, geom FROM "%s" WHERE GID_0 = \'%s\'',
        gadm_layer, g0
      )
    )
    if (!nrow(v)) next

    for (lev in levels) {
      key <- field[[lev]]
      ids <- as.character(terra::values(v)[[key]])
      keep <- !is.na(ids) & nzchar(ids) & ids != "NA"
      if (!any(keep)) next
      vk <- v[keep, ]
      # dissolve to one geometry per division of this level
      agg <- terra::aggregate(vk, by = key)
      s <- sf::st_as_sf(agg)
      gid <- as.character(s[[key]])
      geom <- sf::st_geometry(s)
      cents <- gvs_six_centroids(geom)
      acc[[lev]][[length(acc[[lev]]) + 1L]] <- cbind(data.frame(gid = gid), cents)
    }

    if (!quiet && (i %% 25 == 0 || i == length(countries))) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      message(sprintf(
        "  %d/%d countries | %.1f min | %s counties so far",
        i, length(countries), el,
        format(sum(vapply(acc$county, nrow, integer(1))), big.mark = ",")
      ))
    }
  }

  provenance <- list(
    source = "centroids",
    full_name = gvs_builtin_registry()$centroids$full_name,
    version = gvs_builtin_registry()$gadm$version,
    gadm_version = gvs_builtin_registry()$gadm$version,
    downloaded = as.character(Sys.Date()),
    built_minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  )
  for (lev in levels) {
    tab <- do.call(rbind, acc[[lev]])
    tab <- tab[!duplicated(tab$gid), ]
    nanoparquet::write_parquet(tab, gvs_centroid_path(lev, dir), compression = "gzip")
    provenance[[paste0("n_", lev)]] <- nrow(tab)
    if (!quiet) message("  ", lev, ": ", format(nrow(tab), big.mark = ","), " divisions")
  }
  saveRDS(provenance, gvs_provenance_path("centroids", dir))
  invisible(provenance)
}
