#' top10percent
#'
#' @param dataset the dataset to be treated, the top 10 percent runners in terms of their finishing time will be extracted
#'
#' @return the top 10 percent runners in terms of their finishing time
#' @export
#' @import dplyr
#' @import utils
#'
#' @examples
#' \dontrun{
#' top10percent(data_all)
#' }
top10percent <- function(dataset){
  dataset %>% arrange(Official.Time.1) %>% head(round(length(dataset$Official.Time.1)*0.1))
}
