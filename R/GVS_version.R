#'Get metadata on current GVS version
#'
#'Return metadata about the current GVS version
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing current GVS version number, build date, and code version.
#' @export
#' @examples{
#' NSR_version_metadata <- GVS_version()
#' }
#'
GVS_version <- function(...){

  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }

  return(gvs_core(mode = "meta",...))

}#NSR version
