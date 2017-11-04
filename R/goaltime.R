#' goaltime
#'
#' @param goaltime your projected time spent to reach the finish line
#' @param dataset the dataset from which information is extracted
#'
#' @return the mean time at the specified milestones for 3 groups of people, the group who are 15 to 30 mins faster than your goal time, the gorup no 15 mins faster than your goal time and the group no 15 mins slower than your goal time
#' @export
#' @import dplyr
#' @importFrom data.table setDT
#'
#' @examples
#' \dontrun{
#' goaltime(data_all)
#' }
goaltime <- function(goaltime, dataset){
  goal_time_1 <- dataset %>% filter(Official.Time.1 > goaltime-30 & Official.Time.1 < goaltime-15) %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>% setDT(keep.rownames=TRUE) %>% mutate(Label = rep("Goal_time_15to30_mins_Faster"))
  goal_time_1$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(goal_time_1)[1]<-("milestone_km")

  goal_time_2 <- dataset %>% filter(Official.Time.1 > goaltime-15 & Official.Time.1 < goaltime) %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>% setDT(keep.rownames=TRUE) %>% mutate(Label = rep("Goal_time_0to15_mins_Faster"))
  goal_time_2$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(goal_time_2)[1]<-("milestone_km")

  goal_time_3 <- dataset %>% filter(Official.Time.1 > goaltime & Official.Time.1 < goaltime+15) %>% mutate(X0K.1 = rep(0)) %>% select("X0K.1", c(13:22)) %>% lapply(mean) %>% as.data.frame() %>% `row.names<-`(c("mean_time")) %>% t() %>% as.data.frame() %>% setDT(keep.rownames=TRUE) %>% mutate(Label = rep("Goal_time_0to15_mins_Slower"))
  goal_time_3$rn <- c(0, 5, 10, 15, 20, 21.10, 25, 30, 35, 40, 42.20)
  colnames(goal_time_3)[1]<-("milestone_km")

  goal_time <- rbind(goal_time_1, goal_time_2, goal_time_3)

  return(goal_time)
}
