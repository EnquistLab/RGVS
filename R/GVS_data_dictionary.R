#'Get data dictionary
#'
#'Returns the GVS data dictionary
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing bibtex-formatted citation information
#' @export
#' @examples {
#' data_dictionary <- GVS_data_dictionary()
#' }
#'
GVS_data_dictionary <- function(...){

  return(gvs_core(mode="dd",...))

}#NSR citations
