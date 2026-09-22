# Offline GVS: helpers that need no cache, then stages that need a built cache
# (skipped unless the option GVS.test_cache names a directory holding one).

test_that("gvs_dedupe groups identical rows, NAs equal", {
  set.seed(2)
  n <- 5000
  a <- sample(c(1.5, 2.25, NA, 3), n, TRUE)
  b <- sample(c("x", "y", NA), n, TRUE)
  g <- GVS:::gvs_dedupe(a, b)
  k <- paste(a, b)
  expect_equal(length(g$first), length(unique(k)))
  expect_true(all(k == k[g$first][g$group]))
})

test_that("Andoyer-Lambert distances agree with the geodesic", {
  skip_if_not_installed("geosphere")
  set.seed(3)
  n <- 2000
  lon1 <- runif(n, -180, 180)
  lat1 <- runif(n, -85, 85)
  p2 <- geosphere::destPoint(cbind(lon1, lat1), runif(n, 0, 360), runif(n, 0, 3e6))
  g <- geosphere::distGeo(cbind(lon1, lat1), p2) / 1000
  l <- GVS:::gvs_distance_km(lon1, lat1, p2[, 1], p2[, 2])
  expect_lt(max(abs(l - g) / pmax(g, 1e-9)), 1e-5)
  expect_equal(GVS:::gvs_distance_km(10, 10, 10, 10), 0)
})

test_that("record dates parse from years and ISO strings", {
  d <- GVS:::gvs_parse_record_date(c("1985", "1991-12-01", "1991-12", "", NA, "c. 1900"))
  expect_equal(d, as.Date(c("1985-07-01", "1991-12-01", "1991-12-01", NA, NA, NA)))
})

test_that("window pair search matches brute force", {
  set.seed(4)
  lon <- runif(3000, -180, 180)
  lat <- runif(3000, -90, 90)
  clon <- runif(40, -170, 170)
  clat <- runif(40, -80, 80)
  rlat <- runif(40, 0.5, 5)
  rlon <- runif(40, 0.5, 8)
  got <- GVS:::gvs_pairs_within(lon, lat, clon, clat, rlat, rlon)
  bf <- which(outer(abs(lat), rep(1, 40)) >= 0 &
                abs(outer(lat, clat, "-")) <= rep(rlat, each = 3000) &
                abs(outer(lon, clon, "-")) <= rep(rlon, each = 3000), arr.ind = TRUE)
  expect_setequal(paste(got$p, got$j), paste(bf[, 1], bf[, 2]))
})

test_that("level distances pick the nearest centroid and threshold comparisons are exact", {
  skip_if_not_installed("geosphere")
  tab <- data.frame(gid = "A")
  for (i in 1:6) {
    tab[[paste0("c", i, "_lon")]] <- 10 + i / 10
    tab[[paste0("c", i, "_lat")]] <- 50
    tab[[paste0("c", i, "_dmax")]] <- 2
  }
  r <- GVS:::gvs_level_distances(c(10.1, 10.62, 0), c(50, 50, 0), c("A", "A", "B"), tab, thr = 5)
  expect_equal(r$type, c("std", "bb_main", NA))
  expect_equal(r$dist[1], 0)
  expect_equal(r$rel[2], 0.02 / 2, tolerance = 1e-9)
  expect_true(is.na(r$dist[3]))
  # a point exactly at the threshold distance is compared with the geodesic value
  p <- geosphere::destPoint(c(10.1, 50), 180, 5000)
  r2 <- GVS:::gvs_level_distances(p[1], p[2], "A", tab, thr = 5)
  expect_equal(r2$dist, geosphere::distGeo(c(10.1, 50), p) / 1000)
})

cache <- getOption("GVS.test_cache", "")

test_that("historical centroids and geovalidity (needs a built cache)", {
  skip_if(!nzchar(cache) || !dir.exists(cache), "no test cache (option GVS.test_cache)")
  skip_if_not_installed("sf")
  v <- nanoparquet::read_parquet(file.path(cache, "cshapes-versions.gz.parquet"))
  su <- v[v$gwcode == 365 & v$valid_from > as.Date("1950-01-01") & v$valid_to < as.Date("1992-01-01"), ][1, ]
  hc <- GVS:::gvs_historical_centroids(c(su$c1_lon, 2.35), c(su$c1_lat, 48.86), NULL, cache,
                                       69.99529, 0.01606376, 1)
  expect_match(hc$version_id[1], "^cshapes:365:")
  expect_true(is.na(hc$version_id[2]))

  # GNRS-shaped input: USSR record in Vilnius; Czechoslovakia record in Dresden;
  # Netherlands Antilles record on Curacao (no CShapes geometry: successors)
  gn <- data.frame(gid_0 = c(NA, NA, NA), gid_1 = NA, gid_2 = NA,
                   entity_key = c("SUHH", "CSHH", "ANHH"), is_historical = TRUE,
                   successors = c("RU;LT", "CZ;SK", "CW;SX;BQ;AW"), subnational_status = NA)
  loc <- data.frame(gid_0 = c("LTU", "DEU", "CUW"), gid_1 = NA, gid_2 = NA)
  lon <- c(25.28, 13.74, -68.93)
  lat <- c(54.69, 51.05, 12.12)
  gv <- GVS:::gvs_geovalidity(lon, lat, NULL, loc, gn, cache, 1)
  expect_equal(gv$geovalid, c(TRUE, FALSE, TRUE))
  expect_match(gv$geovalid_basis[1], "^cshapes:365:")
  expect_match(gv$geovalid_basis[3], "^successor:")
  # at a date after the USSR, only under history = "all"
  gd <- GVS:::gvs_geovalidity(lon, lat, as.Date(c("2005-07-01", NA, NA)), loc, gn, cache, 1)
  expect_false(gd$geovalid[1])
})

test_that("centroid thresholds: absolute, relative, or both", {
  d <- c(10, 50, 10, NA)
  r <- c(0.5, 0.01, 0.01, 0.01)
  expect_equal(GVS:::gvs_centroid_flag(d, r, a = 20), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(GVS:::gvs_centroid_flag(d, r, r = 0.02), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(GVS:::gvs_centroid_flag(d, r, a = 20, r = 0.02, combine = "or"), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(GVS:::gvs_centroid_flag(d, r, a = 20, r = 0.02, combine = "and"), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(GVS:::gvs_centroid_flag(d, r), rep(FALSE, 4))
  expect_error(GVS:::gvs_check_thresholds(NULL, NULL), "thr_abs, thr_rel or both")
  expect_error(GVS:::gvs_check_thresholds(c(province = 1), NULL), "named vector")
  expect_error(GVS:::gvs_check_thresholds(c(1, 2), NULL), "named vector")
  expect_true(GVS:::gvs_check_thresholds(NULL, c(county = 0.03)))
})

test_that("GVS_thresholds returns usable sets", {
  expect_true(all(c("spec99", "tss_abs", "bien_default") %in% GVS_thresholds()))
  s <- GVS_thresholds("spec99")
  expect_equal(s$combine, "and")
  expect_true(GVS:::gvs_check_thresholds(s$thr_abs, s$thr_rel))
  expect_false("state" %in% names(s$thr_abs))
  for (nm in GVS_thresholds()) {
    x <- GVS_thresholds(nm)
    expect_true(GVS:::gvs_check_thresholds(x$thr_abs, x$thr_rel))
    expect_true(x$combine %in% c("or", "and"))
  }
})

test_that("coordinate precision and inherent uncertainty follow the service", {
  expect_equal(GVS:::gvs_decimal_places(c("12.50", "12.5", "12", "-9.1839", "0.0001", NA)),
               c(1L, 1L, 0L, 4L, 4L, NA_integer_))
  expect_equal(GVS:::gvs_decimal_places(c(12.5, 12, -9.1839)), c(1L, 0L, 4L))
  # per-axis: latitude to 4 places, longitude to 2, at 9.18 S -> longitude cell dominates
  u <- GVS:::gvs_coordinate_uncertainty_m(-9.1839, 4L, 2L)
  expect_equal(round(u), 1099)
  # whole degrees at 45 N
  expect_equal(round(GVS:::gvs_coordinate_uncertainty_m(45, 0L, 0L)), 136339)
  # more decimals means less uncertainty
  expect_lt(GVS:::gvs_coordinate_uncertainty_m(45, 4L, 4L), GVS:::gvs_coordinate_uncertainty_m(45, 2L, 2L))
})

test_that("the default thresholds are the fitted spec995 set", {
  f <- formals(GVS_local)
  s <- GVS_thresholds("spec995")
  expect_equal(eval(f$thr_abs), s$thr_abs)
  expect_equal(eval(f$thr_rel), s$thr_rel)
  expect_equal(eval(f$combine)[1], s$combine)
  expect_equal(eval(f$max_uncertainty_m), 11000)
})
