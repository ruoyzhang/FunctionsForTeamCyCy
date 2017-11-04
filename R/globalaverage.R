#' globalaverage
#'
#' @param dataset the data set to be treated, the global mean time at the specified milestones will be calculated for all runners in the data set
#'
#' @return the mean time at the specified milestones for all the runners in the data set
#' @export
#' @import dplyr
#' @importFrom data.table setDT
#'
#' @examples
#' \dontrun{
#' globalaverage(data_all)
#' }
globalaverage <- function(dataset){
  global <- dataset %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>%  setDT(keep.rownames=TRUE) %>% mutate(Label = rep("Global"))
  global$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(global)[1]<-("milestone_km")
  return(global)
}
