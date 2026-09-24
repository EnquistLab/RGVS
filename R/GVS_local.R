#' Validate coordinates without an internet connection, with historical divisions
#'
#' Offline Geocoordinate Validation Service.  For each coordinate: the GADM
#' country, state/province and county/parish containing it; its distance to the
#' centroids of those divisions, and whether it is likely a centroid, as the
#' service computes them; the same centroid test against the historical countries
#' containing it (CShapes 2.0, 1886-2019), so that a record placed at the USSR's
#' centroid is detected; and, when a GNRS resolution is supplied, whether the point
#' lies in the political division the record names, now or at any time.
#'
#' @param occurrence_dataframe A data.frame with numeric \code{latitude} and
#'   \code{longitude} (WGS84), and optionally \code{date} (years or ISO dates),
#'   used when \code{history = "at_date"}.
#' @param history How historical countries count towards geovalidity.
#'   \code{"all"} (default): a point is geovalid if it lies in the named division
#'   as it was at any time (1886 onwards).  \code{"at_date"}: only versions valid
#'   at the record's date (± \code{tolerance_years}) count; records without a date
#'   are treated as under \code{"all"}.  Centroid detection always uses every
#'   historical version, since a centroid copied from an old gazetteer can appear
#'   in a record of any date.
#' @param gnrs Optional output of \code{GNRS::GNRS_local()} for the same rows, in
#'   the same order (with \code{history = "all"} or \code{"at_date"}, so it carries
#'   \code{entity_key}).  Needed for the geovalidity columns.
#' @param dir Cache directory, shared with GNRS.
#' @param gpkg Optional path to the GADM GeoPackage the index was built from.
#'   With it, points in boundary cells are located exactly; without it they keep
#'   the raster answer and are flagged in \code{locate_method}.
#' @param thr_abs,thr_rel Centroid thresholds: named numeric vectors with any of
#'   \code{country}, \code{state}, \code{county}, absolute in km and relative
#'   (distance over the division's maximum distance from that centroid).  Either may
#'   be \code{NULL}, to test only the other kind of distance; a level missing from a
#'   vector is not tested with that kind of distance.  At least one must be given.
#'   The defaults are \code{GVS_thresholds("spec995")}: a point is a centroid if it is
#'   within both the absolute and the relative threshold of its country's or county's
#'   centroid.  On BIEN's own records this catches 66\% of the occurrences BIEN declares
#'   centroids while flagging 1.6\% of all occurrences, against 39\% and 0.53\% for the
#'   service's historical rule (relative 0.002).  \code{\link{GVS_thresholds}} holds the
#'   other sets, including the TSS-optimised ones the GVS manuscript reports.
#' @param combine When a level has both an absolute and a relative threshold:
#'   \code{"or"} flags a point meeting either, \code{"and"} only a point meeting
#'   both.  Levels are always combined with OR.
#' @param max_uncertainty_m Coordinates whose inherent uncertainty (from the decimal
#'   places submitted) is at least this are flagged \code{is_low_precision}; \code{NULL}
#'   switches the flag off.  The default, 11 km, is one decimal place or fewer.  This is
#'   a separate flag from \code{is_centroid}: a rounded coordinate is imprecise, which is
#'   not the same claim as sitting on a division's centroid, though in practice gazetteer
#'   points are both.  On the GVS benchmark it catches 74\% of the state-level records
#'   (rounded gazetteer points a median 23 km from any state centroid, which centroid
#'   distance cannot catch) at specificity 0.998.
#' @param tolerance_years Tolerance either side of a historical version's dates
#'   when \code{date} is given.
#' @return A data.frame, one row per input row.
#' @export
GVS_local <- function(occurrence_dataframe, gnrs = NULL, dir = gvs_cache_dir(), gpkg = NULL,
                      thr_abs = c(country = 38.466, county = 0.24727),
                      thr_rel = c(country = 0.026656, county = 0.0025027),
                      combine = c("and", "or"), max_uncertainty_m = 11000,
                      history = c("all", "at_date"), tolerance_years = 1) {
  history <- match.arg(history)
  combine <- match.arg(combine)
  gvs_check_thresholds(thr_abs, thr_rel)
  if (!inherits(occurrence_dataframe, "data.frame") ||
      !all(c("latitude", "longitude") %in% names(occurrence_dataframe))) {
    stop("occurrence_dataframe should be a data.frame with latitude and longitude columns", call. = FALSE)
  }
  if (!is.null(gnrs) && nrow(gnrs) != nrow(occurrence_dataframe)) {
    stop("gnrs should have one row per row of occurrence_dataframe", call. = FALSE)
  }
  dp_lat <- gvs_decimal_places(occurrence_dataframe$latitude)
  dp_lon <- gvs_decimal_places(occurrence_dataframe$longitude)
  lon <- suppressWarnings(as.numeric(occurrence_dataframe$longitude))
  lat <- suppressWarnings(as.numeric(occurrence_dataframe$latitude))
  n <- length(lon)
  dates <- if (history == "at_date" && "date" %in% names(occurrence_dataframe)) {
    gvs_parse_record_date(occurrence_dataframe$date)
  } else NULL

  # ---- 1. current GADM divisions -------------------------------------------------
  loc <- gvs_locate_current(lon, lat, dir = dir, gpkg = gpkg)

  # ---- 2. centroids of the current divisions (the service's computation) ---------
  ct <- lapply(stats::setNames(gvs_levels(), gvs_levels()), function(l)
    as.data.frame(nanoparquet::read_parquet(gvs_centroid_path(l, dir))))
  thr_of <- function(thr, level) if (is.null(thr) || !level %in% names(thr)) NULL else unname(thr[[level]])
  dc <- gvs_level_distances(lon, lat, loc$gid_0, ct$country, thr = thr_of(thr_abs, "country"))
  ds <- gvs_level_distances(lon, lat, loc$gid_1, ct$state, thr = thr_of(thr_abs, "state"))
  dp <- gvs_level_distances(lon, lat, loc$gid_2, ct$county, thr = thr_of(thr_abs, "county"))
  out <- data.frame(
    latitude = lat, longitude = lon,
    gid_0 = loc$gid_0, country = loc$country, gid_1 = loc$gid_1, state = loc$name_1,
    gid_2 = loc$gid_2, county = loc$name_2, locate_method = loc$locate_method,
    country_cent_dist = dc$dist, country_cent_dist_relative = dc$rel, country_cent_type = dc$type,
    state_cent_dist = ds$dist, state_cent_dist_relative = ds$rel, state_cent_type = ds$type,
    county_cent_dist = dp$dist, county_cent_dist_relative = dp$rel, county_cent_type = dp$type,
    stringsAsFactors = FALSE
  )
  rel_m <- cbind(dc$rel, ds$rel, dp$rel)
  dist_m <- cbind(dc$dist, ds$dist, dp$dist)
  type_m <- cbind(dc$type, ds$type, dp$type)
  allna <- rowSums(!is.na(rel_m)) == 0
  k <- max.col(-replace(rel_m, is.na(rel_m), Inf), ties.method = "first")
  ix <- cbind(seq_len(n), k)
  out$centroid_dist <- ifelse(allna, NA, dist_m[ix])
  out$centroid_dist_relative <- ifelse(allna, NA, rel_m[ix])
  out$centroid_type <- ifelse(allna, NA, type_m[ix])
  out$centroid_poldiv <- ifelse(allna, NA, c("country", "state", "county")[k])
  out$is_centroid <- as.integer(
    gvs_centroid_flag(dc$dist, dc$rel, thr_of(thr_abs, "country"), thr_of(thr_rel, "country"), combine) |
    gvs_centroid_flag(ds$dist, ds$rel, thr_of(thr_abs, "state"), thr_of(thr_rel, "state"), combine) |
    gvs_centroid_flag(dp$dist, dp$rel, thr_of(thr_abs, "county"), thr_of(thr_rel, "county"), combine))

  # ---- 2b. precision of the submitted coordinates ---------------------------------
  out$coordinate_decimal_places <- pmin(dp_lat, dp_lon)
  out$coordinate_inherent_uncertainty_m <- gvs_coordinate_uncertainty_m(lat, dp_lat, dp_lon)
  out$is_low_precision <- if (is.null(max_uncertainty_m)) NA_integer_ else {
    z <- out$coordinate_inherent_uncertainty_m >= max_uncertainty_m
    as.integer(ifelse(is.na(z), FALSE, z))
  }

  # ---- 3. centroids of historical countries (CShapes) -----------------------------
  hc <- gvs_historical_centroids(lon, lat, NULL, dir, thr_of(thr_abs, "country"), thr_of(thr_rel, "country"),
                                 tolerance_years, combine)
  out$hist_centroid_version <- hc$version_id
  out$hist_centroid_name <- hc$country_name
  out$hist_centroid_valid_from <- hc$valid_from
  out$hist_centroid_valid_to <- hc$valid_to
  out$hist_centroid_dist <- hc$dist
  out$hist_centroid_dist_relative <- hc$rel
  out$hist_centroid_type <- hc$type
  out$is_centroid_historical <- as.integer(!is.na(hc$version_id))
  out$is_centroid_any <- as.integer(out$is_centroid == 1L | out$is_centroid_historical == 1L)

  # ---- 4. geovalidity against the named division ----------------------------------
  if (!is.null(gnrs)) {
    gv <- gvs_geovalidity(lon, lat, dates, loc, gnrs, dir, tolerance_years)
    out <- cbind(out, gv)
  }
  rownames(out) <- NULL
  out
}

#' Group identical rows of several vectors without pasting them
#'
#' Internal.  \code{first} indexes one representative row per group (in sort
#' order, not order of appearance); \code{group} maps every row to its
#' representative's position in \code{first}.  NAs compare equal to each other.
#' @keywords internal
#' @noRd
gvs_dedupe <- function(...) {
  cols <- list(...)
  n <- length(cols[[1]])
  if (!n) return(list(first = integer(0), group = integer(0)))
  o <- do.call(order, c(unname(cols), list(na.last = TRUE, method = "radix")))
  same <- rep(TRUE, n - 1L)
  for (x in cols) {
    a <- x[o][-n]
    b <- x[o][-1L]
    eq <- a == b
    eq[is.na(eq)] <- is.na(a[is.na(eq)]) & is.na(b[is.na(eq)])
    same <- same & eq
  }
  start <- c(TRUE, !same)
  gid <- cumsum(start)
  group <- integer(n)
  group[o] <- gid
  list(first = o[start], group = group)
}

#' Centroid-detection threshold sets
#'
#' The threshold sets fitted to the GVS benchmark of 300,000 known centroids (BIEN
#' records georeferenced as country, state or county centroids) and 300,000
#' iNaturalist records, described in \code{gvs_ms/R_scripts/04b_centroid_threshold_approaches.R}.
#' Use with \code{\link{GVS_local}} as \code{thr_abs}, \code{thr_rel} and
#' \code{combine}.
#'
#' \describe{
#'   \item{spec99}{Maximum sensitivity at specificity 0.99, requiring a point to be
#'     within both the absolute and the relative threshold of a division's centroid
#'     (\code{combine = "and"}).  The best of the rules tried at this specificity, by
#'     both weightings.  Held-out sensitivity per record 0.55-0.65; per distinct
#'     location 0.79-0.81, made up of 0.94-0.95 of county centroids, 0.34-0.40 of
#'     country centroids and almost no state centroids, which at this specificity cost
#'     more good records than they save, so the state test is off.  (Per record, country
#'     detection reads 0.83-0.92, but the benchmark's 100,000 country records sit on 132
#'     distinct coordinates, so that figure reflects a few well-replicated places; the
#'     country thresholds are fitted on those 132 locations and should be treated as
#'     provisional.)  Flags 2.9\% of BIEN occurrences.  State-level gazetteer points are
#'     better caught by \code{max_uncertainty_m}, not by centroid distance.}
#'   \item{spec995}{As \code{spec99} at specificity 0.995 (held-out sensitivity
#'     0.51-0.54).}
#'   \item{spec999}{Specificity 0.999; county centroids only (sensitivity 0.29-0.33).}
#'   \item{tss_abs, tss_rel}{The TSS-optimised sets of the GVS manuscript, each used
#'     on its own with \code{combine = "or"}: specificity 0.84 and 0.80, flagging 26\%
#'     and 22\% of BIEN occurrences.  \code{tss_abs} with \code{tss_rel} and
#'     \code{combine = "or"} (all six at once) was never validated: specificity 0.73.}
#'   \item{bien_default}{The service's historical rule, relative distance 0.002 at any
#'     level (specificity 0.997, sensitivity 0.29).}
#' }
#' @return A list with \code{thr_abs}, \code{thr_rel} and \code{combine}, or, with no
#'   arguments, the names of the available sets.
#' @param set One of "spec99", "spec995", "spec999", "tss_abs", "tss_rel", "bien_default".
#' @export
#' @examples
#' GVS_thresholds()
#' GVS_thresholds("spec99")
GVS_thresholds <- function(set = NULL) {
  sets <- list(
    spec99 = list(thr_abs = c(country = 32.911, county = 1.0357),
                  thr_rel = c(country = 0.070671, county = 0.0048643), combine = "and"),
    spec995 = list(thr_abs = c(country = 38.466, county = 0.24727),
                   thr_rel = c(country = 0.026656, county = 0.0025027), combine = "and"),
    spec999 = list(thr_abs = NULL, thr_rel = c(county = 0.0066129), combine = "and"),
    tss_abs = list(thr_abs = c(country = 69.99529, state = 31.75486, county = 1.01550),
                   thr_rel = NULL, combine = "or"),
    tss_rel = list(thr_abs = NULL,
                   thr_rel = c(country = 0.01606376, state = 0.15369636, county = 0.03163571),
                   combine = "or"),
    bien_default = list(thr_abs = NULL, thr_rel = c(country = 0.002, state = 0.002, county = 0.002),
                        combine = "or")
  )
  if (is.null(set)) return(names(sets))
  set <- match.arg(set, names(sets))
  sets[[set]]
}

#' Decimal places of submitted coordinates
#'
#' Internal.  As the service counts them: trailing zeros do not count, so "12.50" is one
#' decimal place.  A coordinate passed as a number has already lost its trailing zeros,
#' which is why \code{\link{GVS_local}} keeps the column as supplied; pass character
#' coordinates to reproduce the service exactly.
#' @keywords internal
#' @noRd
gvs_decimal_places <- function(x) {
  if (is.numeric(x)) x <- formatC(x, format = "f", digits = 12, drop0trailing = FALSE)
  x <- trimws(as.character(x))
  sci <- grepl("[eE]", x)
  if (any(sci, na.rm = TRUE)) {
    x[which(sci)] <- formatC(suppressWarnings(as.numeric(x[which(sci)])), format = "f",
                             digits = 12, drop0trailing = FALSE)
  }
  frac <- sub("^[^.]*[.]?", "", x)
  frac[!grepl("[.]", x)] <- ""
  frac <- sub("0+$", "", frac)
  out <- nchar(frac)
  out[is.na(x)] <- NA_integer_
  as.integer(out)
}

#' Inherent uncertainty (m) of a coordinate, from its decimal places
#'
#' Internal.  The service's \code{coordinate_inherent_uncertainty_m}: the diagonal of the
#' cell the rounding leaves the point in, each axis taking its own decimal places, at
#' 111,320 m per degree with longitude converging as cos(latitude).  Reproduces the
#' service on its benchmark to within 1\% for every record but one (a coordinate written
#' in scientific notation).
#' @keywords internal
#' @noRd
gvs_coordinate_uncertainty_m <- function(lat, dp_lat, dp_lon) {
  m_per_deg <- 111320
  sqrt((10^(-dp_lat) * m_per_deg)^2 + (10^(-dp_lon) * m_per_deg * cos(lat * pi / 180))^2)
}

#' Check user-supplied centroid thresholds
#' @keywords internal
#' @noRd
gvs_check_thresholds <- function(thr_abs, thr_rel) {
  if (is.null(thr_abs) && is.null(thr_rel)) {
    stop("Give thr_abs, thr_rel or both.", call. = FALSE)
  }
  for (nm in c("thr_abs", "thr_rel")) {
    thr <- get(nm)
    if (is.null(thr)) next
    if (!is.numeric(thr) || is.null(names(thr)) || !all(names(thr) %in% gvs_levels()) ||
        anyDuplicated(names(thr)) || any(!is.finite(thr)) || any(thr < 0)) {
      stop(nm, " should be NULL or a named vector of non-negative numbers with names among ",
           "'country', 'state', 'county'.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Centroid test at one level: absolute, relative, or both combined
#'
#' Internal.  A threshold of NULL means that kind of distance is not tested; with
#' neither, nothing is flagged.  Missing distances never flag.
#' @keywords internal
#' @noRd
gvs_centroid_flag <- function(dist, rel, a = NULL, r = NULL, combine = c("or", "and")) {
  combine <- match.arg(combine)
  le <- function(x, t) { z <- x <= t; z[is.na(z)] <- FALSE; z }
  fa <- if (is.null(a)) NULL else le(dist, a)
  fr <- if (is.null(r)) NULL else le(rel, r)
  if (is.null(fa) && is.null(fr)) return(rep(FALSE, length(dist)))
  if (is.null(fa)) return(fr)
  if (is.null(fr)) return(fa)
  if (combine == "or") fa | fr else fa & fr
}

#' Parse an optional date column (years, ISO dates, Dates)
#' @keywords internal
#' @noRd
gvs_parse_record_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  year <- !is.na(x) & grepl("^[0-9]{3,4}$", x)
  if (any(year)) out[year] <- as.Date(sprintf("%04d-07-01", as.integer(x[year])))
  iso <- !is.na(x) & !year & grepl("^[0-9]{4}-[0-9]{2}", x)
  if (any(iso)) out[iso] <- suppressWarnings(as.Date(substr(paste0(x[iso], "-01"), 1, 10)))
  out
}

#' Distances from points to the six centroids of their division at one level
#'
#' Internal.  As the service: the nearest of the six centroid types by absolute
#' (ellipsoidal) distance is reported, with its relative distance (planar degrees
#' over that centroid's maximum distance within the division).  Where a base type
#' and its _main counterpart coincide the _main label is reported.
#'
#' \code{m} indexes the row of \code{tab} for each point (default: matched on
#' \code{gids}).  Distances within 0.1\% of an absolute threshold in \code{thr}
#' are recomputed with \code{geosphere::distGeo}, so comparisons with the
#' thresholds are those of the service; elsewhere the Andoyer-Lambert
#' approximation is used (see \code{gvs_distance_km}).
#' @keywords internal
#' @noRd
gvs_level_distances <- function(lon, lat, gids, tab, m = match(gids, tab$gid), thr = NULL) {
  n <- length(lon)
  eval_order <- c(4, 5, 6, 1, 2, 3)
  eval_label <- c("std_main", "pos_main", "bb_main", "std", "pos", "bb")
  D <- R <- matrix(NA_real_, n, 6)
  has <- which(!is.na(m))
  mh <- m[has]
  for (col in seq_len(6)) {
    i <- eval_order[col]
    clon <- tab[[paste0("c", i, "_lon")]][mh]
    clat <- tab[[paste0("c", i, "_lat")]][mh]
    dm <- tab[[paste0("c", i, "_dmax")]][mh]
    d <- gvs_distance_km(lon[has], lat[has], clon, clat)
    thr <- thr[is.finite(thr)]
    if (length(thr) && requireNamespace("geosphere", quietly = TRUE)) {
      near <- which(Reduce(`|`, lapply(thr, function(t) abs(d - t) <= 1e-5 * t)))
      if (length(near)) {
        d[near] <- geosphere::distGeo(cbind(lon[has][near], lat[has][near]), cbind(clon[near], clat[near])) / 1000
      }
    }
    D[has, col] <- d
    R[has, col] <- sqrt((lon[has] - clon)^2 + (lat[has] - clat)^2) / dm
  }
  k <- max.col(-replace(D, is.na(D), Inf), ties.method = "first")
  idx <- cbind(seq_len(n), k)
  ok <- !is.na(m)
  data.frame(dist = ifelse(ok, D[idx], NA), rel = ifelse(ok, R[idx], NA),
             type = ifelse(ok, eval_label[k], NA_character_), stringsAsFactors = FALSE)
}

#' Ellipsoidal (WGS84) distance in km
#'
#' Internal.  Andoyer-Lambert: the central angle on the auxiliary sphere of
#' reduced latitudes, corrected for flattening to first order.  Vectorised, and
#' within about 1e-5 of the geodesic (Karney) distance at the distances that
#' matter for centroids; exact comparisons with thresholds are made in
#' \code{gvs_level_distances}.
#' @keywords internal
#' @noRd
gvs_distance_km <- function(lon1, lat1, lon2, lat2) {
  if (!length(lon1)) return(numeric(0))
  a <- 6378.137
  f <- 1 / 298.257223563
  r <- pi / 180
  b1 <- atan((1 - f) * tan(lat1 * r))
  b2 <- atan((1 - f) * tan(lat2 * r))
  h <- sin((b2 - b1) / 2)^2 + cos(b1) * cos(b2) * sin((lon2 - lon1) * r / 2)^2
  sig <- 2 * asin(pmin(1, sqrt(h)))
  P <- (b1 + b2) / 2
  Q <- (b2 - b1) / 2
  X <- (sig - sin(sig)) * sin(P)^2 * cos(Q)^2 / cos(sig / 2)^2
  Y <- (sig + sin(sig)) * cos(P)^2 * sin(Q)^2 / sin(sig / 2)^2
  d <- a * (sig - f / 2 * (X + Y))
  d[sig == 0] <- 0
  d
}

#' Pairs of points and centroids within a latitude/longitude window
#'
#' Internal.  First a coarse mask (0.1 degree cells) of the union of all windows
#' discards the points that are near no centroid, in one vectorised lookup; the
#' rest are sorted by latitude and each centroid takes the slice within
#' \code{rlat} of it, keeping those within \code{rlon} in longitude.
#' @return data.frame(p = point index, j = centroid index)
#' @keywords internal
#' @noRd
gvs_pairs_within <- function(lon, lat, clon, clat, rlat, rlon, cell = 0.1) {
  none <- data.frame(p = integer(0), j = integer(0))
  good <- is.finite(clon) & is.finite(clat)
  ok <- which(is.finite(lon) & is.finite(lat) & abs(lon) <= 180 & abs(lat) <= 90)
  if (!length(ok) || !any(good)) return(none)

  nx <- round(360 / cell)
  ny <- round(180 / cell)
  mask <- matrix(FALSE, nrow = nx, ncol = ny)
  for (j in which(good)) {
    x0 <- max(1L, floor((clon[j] - rlon[j] + 180) / cell))
    x1 <- min(nx, floor((clon[j] + rlon[j] + 180) / cell) + 1L)
    y0 <- max(1L, floor((clat[j] - rlat[j] + 90) / cell))
    y1 <- min(ny, floor((clat[j] + rlat[j] + 90) / cell) + 1L)
    if (x1 >= x0 && y1 >= y0) mask[x0:x1, y0:y1] <- TRUE
  }
  cx <- pmin(nx, floor((lon[ok] + 180) / cell) + 1L)
  cy <- pmin(ny, floor((lat[ok] + 90) / cell) + 1L)
  ok <- ok[mask[cbind(cx, cy)]]
  if (!length(ok)) return(none)

  o <- ok[order(lat[ok])]
  sl <- lat[o]
  ps <- js <- vector("list", length(clon))
  for (j in which(good)) {
    lo <- findInterval(clat[j] - rlat[j], sl, left.open = TRUE) + 1L
    hi <- findInterval(clat[j] + rlat[j], sl)
    if (hi < lo) next
    idx <- o[lo:hi]
    idx <- idx[abs(lon[idx] - clon[j]) <= rlon[j]]
    if (length(idx)) {
      ps[[j]] <- idx
      js[[j]] <- rep.int(j, length(idx))
    }
  }
  data.frame(p = as.integer(unlist(ps)), j = as.integer(unlist(js)))
}

gvs_session <- new.env(parent = emptyenv())

#' Read the CShapes versions and geometry from the cache, or NULL
#'
#' Internal.  Cached for the session, keyed on the files' paths and times.
#' @keywords internal
#' @noRd
gvs_read_cshapes <- function(dir, geometry = TRUE) {
  vf <- file.path(dir, "cshapes-versions.gz.parquet")
  gf <- file.path(dir, "cshapes-geometry.gpkg")
  if (!file.exists(vf) || (geometry && !file.exists(gf))) return(NULL)
  stamp <- paste(normalizePath(vf), file.mtime(vf), if (geometry) file.mtime(gf))
  hit <- gvs_session$cshapes
  if (!is.null(hit) && identical(hit$stamp, stamp)) return(hit$value)
  v <- as.data.frame(nanoparquet::read_parquet(vf))
  v$valid_from <- as.Date(v$valid_from)
  v$valid_to <- as.Date(v$valid_to)
  if (geometry) {
    g <- sf::st_read(gf, quiet = TRUE)
    g <- g[match(v$version_id, g$version_id), ]
    attr(v, "geometry") <- sf::st_geometry(g)
  }
  gvs_session$cshapes <- list(stamp = stamp, value = v)
  v
}

#' Which (point, version) pairs have the point inside the version's polygon
#'
#' Internal.  Planar (GEOS) point-in-polygon, as for GADM.
#' @return logical, one per pair
#' @keywords internal
#' @noRd
gvs_pairs_inside <- function(lon, lat, p, j, geom) {
  if (!length(p)) return(logical(0))
  old_s2 <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  up <- unique(p)
  uj <- unique(j)
  pts <- sf::st_as_sf(data.frame(lon = lon[up], lat = lat[up]), coords = c("lon", "lat"), crs = sf::st_crs(geom))
  hits <- suppressMessages(sf::st_intersects(pts, geom[uj]))
  hp <- rep(up, lengths(hits))
  hj <- uj[unlist(hits, use.names = FALSE)]
  # pair keys as doubles: p < 2^31 and j < 2^20
  (as.numeric(p) * 1048576 + j) %in% (as.numeric(hp) * 1048576 + hj)
}

#' Centroid test against historical countries containing each point
#'
#' Internal.  A point is tested against a CShapes version only if it is within
#' reach of one of that version's six centroids under the country thresholds
#' (reach in latitude = the larger of the absolute threshold at 110.5 km per
#' degree and the relative threshold times that centroid's maximum distance; in
#' longitude the absolute part is widened by 1/cos(latitude)).  Qualifying pairs
#' are then required to have the point inside the version's polygon and, when the
#' record has a date, the version to be valid at that date (± tolerance).  Of
#' several qualifying versions the earliest is reported.  As for current GADM
#' divisions, windows do not wrap the antimeridian.
#' @keywords internal
#' @noRd
gvs_historical_centroids <- function(lon, lat, dates, dir, thr_abs, thr_rel, tolerance_years,
                                     combine = "or") {
  n <- length(lon)
  out <- data.frame(version_id = rep(NA_character_, n), country_name = NA_character_,
                    valid_from = as.Date(NA), valid_to = as.Date(NA),
                    dist = NA_real_, rel = NA_real_, type = NA_character_, stringsAsFactors = FALSE)
  if (is.null(thr_abs) && is.null(thr_rel)) return(out)
  v <- gvs_read_cshapes(dir)
  if (is.null(v)) return(out)

  # unique coordinates (and dates) only
  dd0 <- if (is.null(dates)) gvs_dedupe(lon, lat) else gvs_dedupe(lon, lat, as.numeric(dates))
  u <- dd0$first
  ulon <- lon[u]
  ulat <- lat[u]
  udate <- if (is.null(dates)) rep(as.Date(NA), length(u)) else dates[u]

  pairs <- list()
  for (i in 1:6) {
    clon <- v[[paste0("c", i, "_lon")]]
    clat <- v[[paste0("c", i, "_lat")]]
    rel_deg <- if (is.null(thr_rel)) 0 else thr_rel * v[[paste0("c", i, "_dmax")]]
    abs_deg <- if (is.null(thr_abs)) 0 else thr_abs / 110.5
    rlat <- pmax(abs_deg, rel_deg)
    rlon <- pmax(abs_deg / pmax(cos(pmin(abs(clat) + rlat, 89.9) * pi / 180), 0.01), rel_deg)
    pairs[[i]] <- gvs_pairs_within(ulon, ulat, clon, clat, rlat, rlon)
  }
  pr <- do.call(rbind, pairs)
  pr <- pr[!duplicated(as.numeric(pr$p) * 1048576 + pr$j), , drop = FALSE]
  if (!nrow(pr)) return(out)

  # dates
  tol <- 365.25 * tolerance_years
  d <- udate[pr$p]
  pr <- pr[is.na(d) | (v$valid_from[pr$j] - tol <= d & v$valid_to[pr$j] + tol >= d), , drop = FALSE]
  if (!nrow(pr)) return(out)

  # distances to that version's centroids, and the thresholds
  dd <- gvs_level_distances(ulon[pr$p], ulat[pr$p], NULL, v, m = pr$j, thr = thr_abs)
  q <- gvs_centroid_flag(dd$dist, dd$rel, thr_abs, thr_rel, combine)
  pr <- cbind(pr, dd)[q, , drop = FALSE]
  if (!nrow(pr)) return(out)

  pr <- pr[gvs_pairs_inside(ulon, ulat, pr$p, pr$j, attr(v, "geometry")), , drop = FALSE]
  if (!nrow(pr)) return(out)
  pr <- pr[order(pr$p, v$valid_from[pr$j]), , drop = FALSE]
  pr <- pr[!duplicated(pr$p), , drop = FALSE]

  res <- out[seq_along(u), , drop = FALSE]
  res$version_id[pr$p] <- v$version_id[pr$j]
  res$country_name[pr$p] <- v$country_name[pr$j]
  res$valid_from[pr$p] <- v$valid_from[pr$j]
  res$valid_to[pr$p] <- v$valid_to[pr$j]
  res$dist[pr$p] <- pr$dist
  res$rel[pr$p] <- pr$rel
  res$type[pr$p] <- pr$type
  res <- res[dd0$group, , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' Is each point inside the political division its record names, now or ever?
#'
#' Internal.  Uses the GNRS resolution of the same rows.
#' \describe{
#'   \item{geovalid_country_current}{the point's current GADM country is the
#'     resolved country (GNRS gid_0).  NA when GNRS resolved a former country or
#'     no country.}
#'   \item{geovalid_country_any}{the point is in its current country as above, or
#'     in a CShapes version of the named entity (only versions valid at the
#'     record's date when it has one), or, for a former country without CShapes
#'     geometry (e.g. the Netherlands Antilles), in one of its current successors.}
#'   \item{geovalid_basis}{"current GADM", the CShapes version id, "successor:
#'     <keys>", or NA.}
#'   \item{geovalid_state_current, geovalid_county_current}{the point's current
#'     GADM state / county are the resolved ones.}
#'   \item{alt_division_status}{for a division belonging to another system (a
#'     Swedish landskap, a Watsonian vice-county, a Norwegian county from after the
#'     2018 or 2020 reform): "valid" when the point falls in one of the GADM units
#'     that division covers, "invalid" when it does not, and "unverifiable" when
#'     those units are not known. NA when no such division was declared.}
#'   \item{subnational_status}{"valid", "invalid", or "unverifiable": GNRS could
#'     not place the named state/county in a successor, or they disagree with
#'     today's divisions while the country is valid only historically.}
#'   \item{geovalid}{the default for BIEN uses: the country was ever accurate and
#'     the sub-national divisions are not contradicted (valid or unverifiable).}
#' }
#' @keywords internal
#' @noRd
#' The GADM units an alternative division covers, as GNRS wrote them to the cache
#'
#' Internal.  NULL when the alternative-division component has not been built, in
#' which case such a division can only be reported as unverifiable.
#' @keywords internal
#' @noRd
gvs_altdiv_extent <- function(dir) {
  f <- file.path(dir, "altdiv-extent.gz.parquet")
  if (!file.exists(f)) return(NULL)
  as.data.frame(nanoparquet::read_parquet(f))
}

gvs_geovalidity <- function(lon, lat, dates, loc, gnrs, dir, tolerance_years) {
  n <- length(lon)
  s <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  g0 <- s(gnrs$gid_0); g1 <- s(gnrs$gid_1); g2 <- s(gnrs$gid_2)
  ek <- if ("entity_key" %in% names(gnrs)) s(gnrs$entity_key) else rep("", n)
  hist <- if ("is_historical" %in% names(gnrs)) gnrs$is_historical %in% TRUE else rep(FALSE, n)
  succ <- if ("successors" %in% names(gnrs)) s(gnrs$successors) else rep("", n)
  gsub_status <- if ("subnational_status" %in% names(gnrs)) s(gnrs$subnational_status) else rep("", n)
  named <- nzchar(ek) | nzchar(g0)

  cur_country <- ifelse(hist | !nzchar(g0), NA, s(loc$gid_0) == g0)
  any_country <- cur_country %in% TRUE
  basis <- ifelse(any_country, "current GADM", NA_character_)

  # CShapes versions of the named entity containing the point
  need <- which(!any_country & nzchar(ek) & is.finite(lon) & is.finite(lat))
  pf <- file.path(dir, "history-periods.gz.parquet")
  v <- if (length(need) && file.exists(pf)) gvs_read_cshapes(dir) else NULL
  if (!is.null(v)) {
    per <- as.data.frame(nanoparquet::read_parquet(pf))
    per$from <- as.Date(per$from)
    per$to <- as.Date(per$to)
    dd0 <- if (is.null(dates)) gvs_dedupe(lon[need], lat[need], ek[need]) else
      gvs_dedupe(lon[need], lat[need], ek[need], as.numeric(dates[need]))
    u <- need[dd0$first]
    # candidate versions: those whose gwcode ever carried the entity
    cand <- merge(data.frame(p = u, entity_key = ek[u]), per[, c("entity_key", "gwcode", "from", "to")],
                  by = "entity_key")
    if (nrow(cand)) {
      cand <- merge(cand, data.frame(j = seq_len(nrow(v)), gwcode = v$gwcode), by = "gwcode")
      # the version overlaps a period in which the gwcode was that entity
      cand <- cand[v$valid_from[cand$j] <= cand$to & v$valid_to[cand$j] >= cand$from, , drop = FALSE]
      if (!is.null(dates) && nrow(cand)) {
        tol <- 365.25 * tolerance_years
        d <- dates[cand$p]
        cand <- cand[is.na(d) | (v$valid_from[cand$j] - tol <= d & v$valid_to[cand$j] + tol >= d), , drop = FALSE]
      }
      cand <- cand[!duplicated(as.numeric(cand$p) * 1048576 + cand$j), c("p", "j"), drop = FALSE]
      if (nrow(cand)) {
        cand <- cand[gvs_pairs_inside(lon, lat, cand$p, cand$j, attr(v, "geometry")), , drop = FALSE]
        cand <- cand[order(cand$p, v$valid_from[cand$j]), , drop = FALSE]
        cand <- cand[!duplicated(cand$p), , drop = FALSE]
        hit <- match(u[dd0$group], cand$p)
        ok <- !is.na(hit)
        any_country[need[ok]] <- TRUE
        basis[need[ok]] <- v$version_id[cand$j[hit[ok]]]
      }
    }
  }

  # a former country without CShapes geometry: its current successors
  with_geometry <- if (file.exists(pf)) unique(nanoparquet::read_parquet(pf)$entity_key) else character(0)
  via <- which(!any_country & hist & nzchar(succ) & !(ek %in% with_geometry) & !is.na(loc$gid_0))
  if (length(via) && requireNamespace("countrycode", quietly = TRUE)) {
    in_succ <- vapply(via, function(i) {
      keys <- strsplit(succ[i], ";", fixed = TRUE)[[1]]
      iso3 <- suppressWarnings(countrycode::countrycode(keys, "iso2c", "iso3c", warn = FALSE))
      iso3[keys == "XK"] <- "XKO"
      loc$gid_0[i] %in% iso3
    }, logical(1))
    any_country[via[in_succ]] <- TRUE
    basis[via[in_succ]] <- paste("successor:", succ[via[in_succ]])
  }

  st_cur <- ifelse(!nzchar(g1), NA, s(loc$gid_1) == g1)
  co_cur <- ifelse(!nzchar(g2), NA, s(loc$gid_2) == g2)

  # A declared division that belongs to ANOTHER division system - a Swedish landskap,
  # a Watsonian vice-county, a Norwegian county from after the 2018 or 2020 reform -
  # is not a GADM unit, so GNRS leaves gid_1 and gid_2 empty and names the unit in
  # alt_division instead. Where the GADM units it covers are known, the point must
  # fall in one of them; where they are not, the claim cannot be checked and the
  # record is unverifiable rather than invalid. Before this, such a name was matched
  # to the nearest-looking GADM unit and the record then looked invalid although it
  # was correctly georeferenced and correctly labelled in its own system.
  alt <- if ("alt_division" %in% names(gnrs)) s(gnrs$alt_division) else rep("", n)
  alt_status <- rep(NA_character_, n)
  if (any(nzchar(alt))) {
    ex <- gvs_altdiv_extent(dir)
    for (k in unique(alt[nzchar(alt)])) {
      i <- which(alt == k)
      e <- if (is.null(ex)) NULL else ex[ex$entity_key == k, , drop = FALSE]
      if (is.null(e) || !nrow(e)) {
        alt_status[i] <- "unverifiable"
        next
      }
      lg <- if (e$level[1] == 1L) s(loc$gid_1)[i] else s(loc$gid_2)[i]
      alt_status[i] <- ifelse(!nzchar(lg), "unverifiable",
                              ifelse(lg %in% e$gid, "valid", "invalid"))
    }
  }

  sub_given <- nzchar(g1) | nzchar(g2) | nzchar(gsub_status) | nzchar(alt)
  contradicted <- (st_cur %in% FALSE) | (co_cur %in% FALSE)
  sub_status <- ifelse(!sub_given, NA_character_,
                ifelse(gsub_status == "unverifiable", "unverifiable",
                ifelse(!contradicted, "valid",
                ifelse(any_country & !(cur_country %in% TRUE), "unverifiable", "invalid"))))
  # what the alternative division says stands for the division it names
  sub_status <- ifelse(!is.na(alt_status), alt_status, sub_status)
  data.frame(
    geovalid_country_current = cur_country,
    geovalid_country_any = ifelse(named, any_country, NA),
    geovalid_basis = basis,
    geovalid_state_current = st_cur,
    geovalid_county_current = co_cur,
    alt_division_status = alt_status,
    subnational_status = sub_status,
    geovalid = ifelse(named, any_country & !(sub_status %in% "invalid"), NA),
    stringsAsFactors = FALSE
  )
}
