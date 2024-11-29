#'Get citation information
#'
#'Returns information needed to cite the GVS
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing bibtex-formatted citation information
#' @note This function provides citation information in bibtex format that can be used with reference manager software (e.g. Paperpile, Zotero). Please do remember to cite both the sources and the NSR, as the NSR couldn't exist without these sources!
#' @export
#' @examples {
#' citation_info <- GVS_citations()
#' }
#'
GVS_citations <- function(...){

  return(gvs_core(mode="citations",...))

}#NSR citations
