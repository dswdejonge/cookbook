#' Combine data sources
#'
#' The function merges data from different columns in a pre-defined preferred order.
#' @param mytable (required) A dataframe with all variables (columns) you would like to combine.
#' @param new_column_name (optional) The name of the new column that will created that has incorporated
#' data from other columns.
#' @param order_of_preference (required) A character vector with the names of the columns you would like
#' to merge, in the order of preference (so the first column will be added first).
#' @return This function returns the input table with an extra column with the name you provided.
#' @details This function is useful if you have the same type of information from different sources, which you
#' would like to combine in a new column. For example, if you have water depth measured on board the ship,
#' water depth taken from high resolution bathymetry, and water depth taken from low resolution bathymetry.
#' As the water depth from the ship is most reliable, all available data is copied into the new column.
#' Then, all observations that remain empty (so water depth was not measured on board), are filled with
#' corresponding data from the high resolution bathymetry column because it has a higher confidence than the
#' low resolution bathymetry dataset.
#' Then, if still empty fields remain, these fields are filled with information from the low resolution
#' bathymetery column.
#' @export
combine_data_sources <- function(mytable, new_column_name = "new", order_of_preference){
  #TODO: check if the names in order_of_preference exist
  mytable$new <- dplyr::pull(mytable, order_of_preference[1])
  mytable$source <- order_of_preference[1]
  for(i in 2:length(order_of_preference)){
    NA_i <- which(is.na(mytable$new))
    if(length(NA_i) == 0){
      break
    }
    mytable$new[NA_i] <- dplyr::pull(mytable,order_of_preference[i])[NA_i]
    mytable$source[NA_i] <- order_of_preference[i]
  }
  colnames(mytable)[colnames(mytable) == "new"] <- new_column_name
  colnames(mytable)[colnames(mytable) == "source"] <- paste0("source_",new_column_name)
  return(mytable)
}
