#'Get information on sources used by the GVS
#'
#'Return metadata about the current GVS sources
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing information about the sources used in the current GVS version.
#' @export
#' @examples {
#' sources <- GVS_sources()
#' }
#'
GVS_sources <- function(...){

  # Check for internet access
  if (!check_internet()) {
    message("This function requires internet access, please check your connection.")
    return(invisible(NULL))
  }

  return(gvs_core(mode = "sources",...))

}#NSR sources
