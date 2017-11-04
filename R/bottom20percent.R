#' bottom20percent
#'
#' @param dataset to be treated, the bottom 20 percent of the runners in terms of their finishing time will be returned
#'
#' @return the bottom 20 percent of the runners in terms of their finishing time
#' @export
#' @import dplyr
#' @import utils
#'
#' @examples
#' \dontrun{
#' bottom20percent(data_all)
#' }
bottom20percent <- function(dataset){
  dataset %>% arrange(desc(Official.Time.1)) %>% head(round(length(dataset$Official.Time.1)*0.2))

}
