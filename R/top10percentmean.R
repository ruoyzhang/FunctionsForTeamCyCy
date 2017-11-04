#' top10percentmean
#'
#' @param dataset the data set to be transformed
#'
#' @return a data frame with the mean time at the specified milestones of the marathon for the top 10 runners in the dataset
#' @export
#' @import dplyr
#' @import utils
#' @importFrom data.table setDT
#'
#' @examples
#' \dontrun{
#' top10percent(data_all)
#' }
top10percentmean <- function(dataset){
  top10 <- dataset %>% arrange(Official.Time.1) %>% head(round(length(dataset$Official.Time.1)*0.1)) %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>%  data.table::setDT(keep.rownames=TRUE) %>% mutate(Label = rep("Top10"))
  top10$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(top10)[1]<-("milestone_km")
  return(top10)
}
