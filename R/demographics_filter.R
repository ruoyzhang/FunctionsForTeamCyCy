#' demographics_filter
#'
#' @param dataset the dataset to be treated, compulsory argument, cannot be empty
#' @param age input value age, the data will be filtered according to the appropriate age group of the input value. Optional argument
#' @param gender input value = "M" or "F", the data will be filtered accordingly. Optional argument
#' @param nationality input value nationality, the data will be filtered accordingly. Optional argument
#'
#' @return filtered dataset with only desired data points remaining
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' demographics_filter(data_all, age = 34, gender = "F", nationality ="USA")
#' demographics_filter(data_all, gender = "F")
#' }
demographics_filter <- function(dataset, age, gender, nationality){ #demographic data, dataset = raw data
  if(missing(age)) {

    if(missing(gender)){
      if(missing(nationality)){
        dataset
      }
      else{
        dataset %>% filter(Country == nationality)
      }
    } else if(missing(nationality)){
      dataset %>% filter(M.F == gender)
    }
    else {
      dataset %>% filter(Country == nationality, M.F == gender)
    }




  } else if(missing(gender)){
    if(missing(nationality)){
      agefilter(age, dataset)
    }
    else{
      agefilter(age,dataset) %>% filter(Country == nationality)
    }
  } else if(missing(nationality)){
    agefilter(age,dataset) %>% filter(Age == age, M.F == gender)
  }
  else{
    agefilter(age, dataset) %>% filter(Age == age, Country == nationality, M.F == gender)
  }
}

