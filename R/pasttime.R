#' pasttime
#'
#' @param pasttime the time you took to reach the finishing line in a previous marathon race
#' @param dataset the dataset to be treated
#'
#' @return the average time at the specified milestones based on the group of people who finished within the 15 mins range centered around your past time
#' @export
#' @import dplyr
#' @importFrom data.table setDT
#'
#' @examples
#' \dontrun{
#' pasttime(data_all)
#' }
pasttime <- function(pasttime, dataset){
  past_time <- dataset %>% filter(Official.Time.1 > pasttime-7.5 & Official.Time.1 < pasttime+7.5) %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>% setDT(keep.rownames=TRUE) %>% mutate(Label = rep("past_time"))
  past_time$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(past_time)[1]<-("milestone_km")
  return(past_time)
}
