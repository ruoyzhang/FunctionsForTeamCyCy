#' bottom20percentmean
#'
#' @param dataset the dataset to be treated, the mean time at the specified milestones for the bottom 20 percent runners will be calcuated
#' @return the treated dataset in the format of a dataframe
#' @export
#' @import dplyr
#' @import utils
#' @importFrom data.table setDT
#'
#' @examples
#' \dontrun{
#' bottom20percentmean(data_all)
#' }
bottom20percentmean <- function(dataset){
  bottom20 <- dataset %>% arrange(desc(Official.Time.1)) %>% head(round(length(dataset$Official.Time.1)*0.2)) %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>%  data.table::setDT(keep.rownames=TRUE) %>% mutate(Label = rep("Bottom20"))
  bottom20$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(bottom20)[1]<-("milestone_km")
  return(bottom20)
}
