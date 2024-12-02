#'Check the validity of coordinates
#'
#'GVS returns information on coordinate validity.
#' @param occurrence_dataframe A properly formatted dataframe, see `gvs_testfile`
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing GVS results.
#' @export
#' @examples \dontrun{
#'
#'results <- GVS(occurrence_dataframe = gvs_testfile)
#'
#' }
GVS <- function(occurrence_dataframe,
                ...){

  #check that input is a data.frame

    if(!inherits(occurrence_dataframe,"data.frame")){
      stop("occurrence_dataframe should be a data.frame")
    }

  #check for two columns

  if(ncol(occurrence_dataframe)!=2){

    warning("Occurrence dataframe should have exactly two columns: one for latitude and one for longitude (in that order).")
    return(invisible(NULL))

  }


  # Convert the data to JSON
  data_json <- toJSON(unname(occurrence_dataframe))

  results <- gvs_core(mode = "resolve",
                       data_json = data_json, ...)

  # results <- gvs_core(mode = "resolve",
  #                     data_json = data_json)
  #

  #If the API didn't work, return a null
  if(is.null(results)){
    return(invisible(NULL))
  }

  #Re-order results to match original data
  # results <- results[match(table = results$user_id,
  #                          x = occurrence_dataframe$user_id),]

  #reset the row numbers
  rownames(results) <- NULL

  return(results)

}






