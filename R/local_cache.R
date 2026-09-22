#' Directory holding the local GVS reference data
#'
#' The reference data is downloaded and derived on demand rather than shipped
#' with the package, and is kept in the standard user cache directory so that it
#' survives package updates and can be removed with \code{GVS_local_remove()}.
#'
#' GVS and GNRS derive their reference data from the same GADM release, so they
#' share one cache directory: whichever package builds GADM first, the other
#' finds it.  The location follows GNRS's, and either package can install it.
#' Override with \code{options(GVS.cache_dir = )}, or for both packages at once
#' with \code{options(GNRS.cache_dir = )}.
#'
#' @param create Should the directory be created if it does not exist?
#' @return Path to the cache directory.
#' @keywords internal
#' @noRd
gvs_cache_dir <- function(create = FALSE) {
  dir <- getOption(
    "GVS.cache_dir",
    getOption("GNRS.cache_dir", tools::R_user_dir("GNRS", which = "cache"))
  )
  if (create && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' Data sources a local build can fetch or derive
#'
#' Internal.  \code{"gadm"} is the release the divisions and centroids come
#' from, shared with GNRS: the world GeoPackage, from which GNRS keeps the
#' attributes of levels 0 to 2 and GVS derives its centroid tables.  It is a
#' large download, so a GeoPackage already on disk can be passed to
#' \code{GVS_local_build()} instead.
#'
#' \code{"centroids"} is what GVS itself resolves against: for every country,
#' state/province and county/parish, the six centroid types the service uses and
#' the maximum distance within the division that each is measured against.  It is
#' derived from the GADM geometry and is a few megabytes.
#'
#' @return A named list of source definitions.
#' @keywords internal
#' @noRd
gvs_builtin_registry <- function() {
  list(
    gadm = list(
      source = "gadm",
      full_name = "GADM administrative areas",
      publisher = "GADM",
      version = "4.1",
      url = "https://geodata.ucdavis.edu/gadm/gadm4.1/gadm_410-gpkg.zip",
      license = "Free for academic and other non-commercial use; redistribution and commercial use need permission (gadm.org/license.html)",
      citation = "GADM (2022). Database of Global Administrative Areas, version 4.1. https://gadm.org/",
      # The world GeoPackage. Only the attributes of levels 0 to 2 are kept, a
      # few megabytes; extracting it needs about twice its size free.
      download_mb = 1400,
      disk_mb = 5
    ),
    centroids = list(
      source = "centroids",
      full_name = "GVS political division centroids",
      publisher = "Botanical Information and Ecology Network",
      url = "https://gvsapi.xyz/gvs_api.php",
      license = "GPL-3 (software); GADM terms apply to the data",
      citation = paste(
        "Maitner B. S., Boyle B. L. & Enquist B. J. Geocoordinate Validation",
        "Service (GVS). https://github.com/ojalaquellueva/gvs"
      ),
      # Derived from the GADM geometry, not downloaded
      download_mb = 0,
      disk_mb = 9
    )
  )
}

#' Paths of the files a built source consists of
#'
#' Internal.  Everything a source writes is prefixed with its name, so that a
#' component can be rebuilt or removed on its own, and the \code{.gz.parquet}
#' suffix records the codec.  The GADM files are the ones GNRS writes, and are
#' read here if that package built them first.
#' @keywords internal
#' @noRd
gvs_centroid_path <- function(level, dir = gvs_cache_dir()) {
  file.path(dir, paste0("centroids-", level, ".gz.parquet"))
}

#' @keywords internal
#' @noRd
gvs_gadm_archive_path <- function(dir = gvs_cache_dir()) {
  file.path(dir, paste0("gadm-", gvs_builtin_registry()$gadm$version, ".zip"))
}

#' @keywords internal
#' @noRd
gvs_gadm_path <- function(dir = gvs_cache_dir()) {
  file.path(dir, "gadm-divisions.gz.parquet")
}

#' @keywords internal
#' @noRd
gvs_provenance_path <- function(source, dir = gvs_cache_dir()) {
  file.path(dir, paste0(source, "-provenance.rds"))
}

#' The three levels the service resolves
#' @keywords internal
#' @noRd
gvs_levels <- function() c("country", "state", "county")

#' Has a source been built?
#'
#' Internal.  Built means the tables the resolver reads exist, not that a
#' download completed: an interrupted build can leave one without the other.
#' @keywords internal
#' @noRd
gvs_is_built <- function(source, dir = gvs_cache_dir()) {
  switch(source,
    gadm = file.exists(gvs_gadm_path(dir)),
    centroids = all(file.exists(gvs_centroid_path(gvs_levels(), dir))),
    FALSE
  )
}

#' Files a source occupies in the cache
#' @keywords internal
#' @noRd
gvs_source_files <- function(source, dir = gvs_cache_dir()) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  files <- list.files(dir, pattern = paste0("^", source, "-"), full.names = TRUE)
  files[!dir.exists(files)]
}

#' How a set of source names would be written in a call
#' @keywords internal
#' @noRd
gvs_source_arg <- function(sources) {
  quoted <- paste0('"', sources, '"')
  if (length(quoted) == 1) quoted else paste0("c(", paste(quoted, collapse = ", "), ")")
}

#' Report on the locally cached GVS reference data
#'
#' Shows each component of the local reference data, whether it has been built
#' for offline use, and for those that have, which version it is and how much
#' space it occupies.  A local result can be cited using the versions reported
#' here.
#'
#' The cache is shared with the GNRS package, which derives its political
#' division names from the same GADM release, so a GADM component built by
#' either package is reported as built by both.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @return A data.frame with one row per component.  \code{version} and
#'   \code{downloaded} describe what is installed and are NA for a component
#'   that has not been built.  \code{size_mb} is what the component occupies on
#'   disk now; \code{download_mb} is what fetching it costs.
#' @seealso \code{\link{GVS_local_build}}, \code{\link{GVS_local}}
#' @export
#' @examples {
#'   status <- GVS_local_status()
#' }
GVS_local_status <- function(dir = gvs_cache_dir()) {
  registry <- gvs_builtin_registry()
  sources <- names(registry)

  built <- vapply(sources, function(s) gvs_is_built(s, dir), logical(1))

  provenance <- lapply(sources, function(s) {
    path <- gvs_provenance_path(s, dir)
    if (file.exists(path)) readRDS(path) else NULL
  })
  names(provenance) <- sources

  from_record <- function(field, empty) {
    vapply(sources, function(s) {
      record <- provenance[[s]]
      if (!built[[s]] || is.null(record)) {
        return(empty)
      }
      value <- record[[field]]
      if (is.null(value) || length(value) == 0) empty else as.character(value)[1]
    }, empty)
  }

  size_mb <- vapply(
    sources,
    function(s) round(sum(file.size(gvs_source_files(s, dir))) / 1024^2, 1),
    numeric(1)
  )

  out <- data.frame(
    source = sources,
    full_name = vapply(registry, function(x) x$full_name, character(1)),
    built = unname(built),
    version = unname(from_record("version", NA_character_)),
    downloaded = unname(from_record("downloaded", NA_character_)),
    size_mb = unname(size_mb),
    download_mb = vapply(registry, function(x) as.numeric(x$download_mb), numeric(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  absent <- out$source[!out$built]
  if (length(absent) == length(sources)) {
    message(
      "No local reference data built yet in:\n  ", dir,
      "\nRun GVS_local_build() to set it up."
    )
  } else if (length(absent) > 0) {
    message(
      "Not built: ", paste(absent, collapse = ", "),
      ". Add with GVS_local_build(", gvs_source_arg(absent), ")."
    )
  }

  out
}

#' Delete the locally cached GVS reference data
#'
#' Removes the reference data GVS derived, or one component of it.  It can be
#' rebuilt at any time with \code{GVS_local_build()}.
#'
#' The GADM component is shared with the GNRS package: removing it leaves that
#' package without its GADM layer too, and is refused unless
#' \code{shared = TRUE}.
#'
#' @param dir Cache directory.  Defaults to the standard user cache location.
#' @param sources NULL, the default, removes the centroid tables.  Otherwise the
#'   components to remove.
#' @param shared Allow removal of components shared with GNRS?
#' @param ask Ask for confirmation before deleting? Defaults to TRUE in an
#'   interactive session.
#' @return TRUE if anything was removed, FALSE otherwise, invisibly.
#' @export
#' @examples \dontrun{
#' GVS_local_remove()
#' }
GVS_local_remove <- function(dir = gvs_cache_dir(), sources = "centroids",
                             shared = FALSE, ask = interactive()) {
  if (!dir.exists(dir)) {
    message("Nothing to remove; no cache directory at:\n  ", dir)
    return(invisible(FALSE))
  }
  unknown <- setdiff(sources, names(gvs_builtin_registry()))
  if (length(unknown) > 0) {
    message("Unknown source(s): ", paste(unknown, collapse = ", "))
    return(invisible(FALSE))
  }
  if ("gadm" %in% sources && !shared) {
    message(
      "The GADM component is shared with the GNRS package. ",
      "Remove it with shared = TRUE, or with GNRS_local_remove(sources = \"gadm\")."
    )
    sources <- setdiff(sources, "gadm")
    if (length(sources) == 0) {
      return(invisible(FALSE))
    }
  }

  files <- unlist(lapply(sources, gvs_source_files, dir = dir), use.names = FALSE)
  size_mb <- round(sum(file.size(files), na.rm = TRUE) / 1024^2, 1)

  if (ask) {
    answer <- readline(paste0(
      "Delete ", paste(sources, collapse = ", "), " (", size_mb, " MB) in\n  ",
      dir, "\n? [y/N] "
    ))
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      message("Nothing removed.")
      return(invisible(FALSE))
    }
  }

  unlink(files)
  for (s in sources) unlink(gvs_provenance_path(s, dir))
  message("Removed ", size_mb, " MB from ", dir)
  invisible(TRUE)
}

#' Default value for NULL
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
