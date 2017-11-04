#' agefilter
#'
#' @param age input value age
#' @param dataset dataset to be treated
#'
#' @return a treated dataset after the appropriate age brackets have been filtered out
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' agefilter(34, data_all)
#' }
agefilter <- function(age, dataset){ #input value of age, dataset is the data to be filtered
  dataset <- agebracketcol(dataset)
  agebracket <- age %>% agebracket()
  dataset <- dataset %>% filter(Age_Bracket == agebracket)
  return(dataset)
}
