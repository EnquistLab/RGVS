#'Get metadata on current GVS version
#'
#'Return metadata about the current NSR version
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing current NSR version number, build date, and code version.
#' @export
#' @examples{
#' NSR_version_metadata <- GVS_version()
#' }
#'
GVS_version <- function(...){

  return(gvs_core(mode = "meta",...))

}#NSR version
