context("geovalidity of divisions belonging to another division system")

# Self-contained: gvs_geovalidity() is given the located divisions and the GNRS
# resolution directly, and an extent table of the kind GNRS writes to the cache.

dir <- file.path(tempdir(), "gvs-altdiv")
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE, showWarnings = FALSE)
nanoparquet::write_parquet(
  data.frame(entity_key = c("NO-VIKEN", "NO-VIKEN", "NO-VIKEN"), level = 1L,
             gid = c("NOR.1_1", "NOR.4_1", "NOR.2_1"), stringsAsFactors = FALSE),
  file.path(dir, "altdiv-extent.gz.parquet"), compression = "gzip")

# one row per case: inside the extent, outside it, and a system with no extent
loc <- data.frame(gid_0 = rep("NOR", 3), gid_1 = c("NOR.1_1", "NOR.12_1", "NOR.1_1"),
                  gid_2 = NA_character_, stringsAsFactors = FALSE)
# gid_0 is the GADM country code GNRS resolves to, not the ISO 3166-1 alpha-2 code
gnrs <- data.frame(gid_0 = rep("NOR", 3), gid_1 = "", gid_2 = "",
                   alt_division = c("NO-VIKEN", "NO-VIKEN", "SE-LS-Uppland"),
                   stringsAsFactors = FALSE)

test_that("a point in one of the units the division covers is valid, outside it invalid", {
  r <- gvs_geovalidity(c(11.05, 10.75, 17.64), c(59.75, 59.91, 59.86), NULL, loc, gnrs,
                       dir, tolerance_years = 1)
  expect_equal(r$alt_division_status, c("valid", "invalid", "unverifiable"))
  expect_equal(r$subnational_status, c("valid", "invalid", "unverifiable"))
  # only a contradicted division makes a record not geovalid
  expect_equal(r$geovalid, c(TRUE, FALSE, TRUE))
})

test_that("with no extent table at all, such a division is unverifiable, never invalid", {
  empty <- file.path(tempdir(), "gvs-altdiv-empty")
  unlink(empty, recursive = TRUE)
  dir.create(empty, recursive = TRUE, showWarnings = FALSE)
  r <- gvs_geovalidity(c(11.05, 10.75), c(59.75, 59.91), NULL, loc[1:2, ], gnrs[1:2, ],
                       empty, tolerance_years = 1)
  expect_equal(r$alt_division_status, c("unverifiable", "unverifiable"))
  expect_false(any(r$subnational_status %in% "invalid"))
})

test_that("a record with no alternative division is unaffected", {
  g <- data.frame(gid_0 = "NOR", gid_1 = "NOR.1_1", gid_2 = "", alt_division = "",
                  stringsAsFactors = FALSE)
  r <- gvs_geovalidity(11.05, 59.75, NULL, loc[1, ], g, dir, tolerance_years = 1)
  expect_true(is.na(r$alt_division_status))
  expect_equal(r$subnational_status, "valid")
})
