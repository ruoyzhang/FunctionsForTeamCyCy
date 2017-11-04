#' agebracketcol
#'
#' @param dataset the dataset on which an agebracket column will be added
#'
#' @return the treated dataset
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' agebracketcol(data_all)
#'}

agebracketcol <- function(dataset){
  dataset["Age_Bracket"] <- dataset["Age"] %>% agebracket() %>% as.vector()
  return(dataset)
}
