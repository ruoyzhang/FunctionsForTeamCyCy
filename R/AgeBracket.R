#' agebracket
#'
#' @param age input value age
#'
#' @return the appropriate age bracket that the input age belongs to
#' @export
#'
#' @examples
#' \dontrun{
#' agebracket(42)
#'}
#'
agebracket <- function(age){
  ifelse (age < 20, "<20",
          ifelse (age < 25, "20 - 24",
                  ifelse (age < 30, "25 - 29",
                          ifelse (age < 35, "30 - 34",
                                  ifelse (age < 40, "35 - 39",
                                          ifelse (age < 45, "40 - 44",
                                                  ifelse (age < 50, "45 - 49",
                                                          ifelse (age < 55, "50 - 54",
                                                                  ifelse (age < 60, "55 - 59", "> 60")))))))))
}
