#' Prorate Not Reported Observations Based on Education Levels
#'
#' This function prorates the number of "Not Reported" observations across different education levels
#' based on the given proportions in the data.
#'
#' @param db A data frame containing at least two columns: `EDUCATION_C3` and `n`.
#' `EDUCATION_C3` indicates the education level, and `n` indicates the count of observations.
#'
#' @return A data frame with prorated "Not Reported" observations distributed across the existing education levels.
#' @export
#'
#' @examples
#' db <- data.frame(EDUCATION_C3 = c(1, 2, 3, 0), n = c(10, 15, 20, 12))
#' prorate_values(db)
prorate_values <- function(db) {
  probs <- db %>%
    filter(EDUCATION_C3 != 0) %>%
    count(EDUCATION_C3, wt = n) %>%
    mutate(probs = n / sum(n)) %>%
    pull(probs)
  
  total_not_reported <- db[db$EDUCATION_C3 == 0, ]$n
  
  if (dim(db[db$EDUCATION_C3 != 0, ])[1] == 0) {
    db$EDUCATION_C3 <- sample(x = c(1, 2, 3), size = 1, prob = c(0.3748773, 0.3658816, 0.2592411))
  } else if (length(total_not_reported) == 0) {
    db <- db
  } else {
    distributed <- round(total_not_reported * probs + 1e-8)
    
    while (sum(distributed) != total_not_reported) {
      difference <- total_not_reported - sum(distributed)
      if (difference > 0) {
        distributed[which.min(distributed %% 1)] <- distributed[which.min(distributed %% 1)] + 1
      } else {
        distributed[which.max(distributed %% 1)] <- distributed[which.max(distributed %% 1)] - 1
      }
    }
    db <- db %>%
      filter(EDUCATION_C3 != 0) %>%
      mutate(n = n + distributed)
  }
  return(db)
}
#' Calculate Exponential Growth Rate
#'
#' This function estimates the exponential growth rate between two time points.
#'
#' @param db A data frame containing at least two columns: `YEAR` and `N`.
#' `YEAR` indicates the year of the observation, and `N` indicates the count of observations.
#' @param t0 The initial time point (default is 2002).
#' @param t1 The final time point (default is 2017).
#'
#' @return A data frame with an additional column `r` representing the calculated growth rate.
#' @export
#'
#' @examples
#' db <- data.frame(YEAR = c(2002, 2017), N = c(100, 200))
#' calculate_r(db)
calculate_r <- function(db, t0 = 2002, t1 = 2017) {
  db$r <- (1 / (t1 - t0)) * log(db[db$YEAR == t1, ]$N / db[db$YEAR == t0, ]$N)
  return(db)
}


#' Calculate Exponential Growth Rate
#'
#' This function estimates the exponential growth rate between two time points.
#'
#' @param db A data frame containing at least two columns: `YEAR` and `N`.
#' `YEAR` indicates the year of the observation, and `N` indicates the count of observations.
#' @param t0 The initial time point (default is 2002).
#' @param t1 The final time point (default is 2017).
#'
#' @return A data frame with an additional column `r` representing the calculated growth rate.
#' @export
#'
#' @examples
#' db <- data.frame(YEAR = c(2002, 2017), N = c(100, 200))
#' calculate_r(db)
calculate_r <- function(db, t0 = 2002, t1 = 2017) {
  db$r <- (1 / (t1 - t0)) * log(db[db$YEAR == t1, ]$N / db[db$YEAR == t0, ]$N)
  return(db)
}



