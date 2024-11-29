#'Get collaborator information
#'
#'Returns information on GVS collaborators
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing bibtex-formatted citation information
#' @export
#' @examples {
#' collaborator_info <- GVS_collaborators()
#' }
#'
GVS_collaborators <- function(...){

  return(gvs_core(mode="collaborators",...))

}#NSR citations
