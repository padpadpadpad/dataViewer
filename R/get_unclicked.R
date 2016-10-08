#' subsets the rows in the data that were not selected in dataViewer
#'
#' subsets the rows in the data that were not selected in dataViewer
#'
#' @param data the dataframe
#' @param clicked_data the dataframe of clicked points
#' @export

get_unclicked <- function(data, clicked_data){
  x.1p <- do.call("paste", data)
  x.2p <- do.call("paste", clicked_data)
  data[!x.1p %in% x.2p, ]
}
