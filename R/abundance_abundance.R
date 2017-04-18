#' My title
#'
#' @description My description
#'
#' Mortality is the main function, and is constructed like
#' growth and recruitment. It requires two complete datasets, one per census,
#' with dbh, pom, and date for every individual of all species in at least 2 censuses (see Data Format).
#'
#' Output of the mortality function is a list with components:
#' * N, the number of individuals alive in the census 1 per category selected
#' * D, the number of individuals no longer alive in census 2
#' * rate, the mean annualized mortality rate constant per category selected, calculated as (log(N)-log(S))/time
#' * upper, upper confidence limit of mean rate
#' * lower, lower confidence limit of mean rate
#' * time, mean time interval in years
#' * date1, mean date included individuals were measured in census 1, as julian object (R displays as date, but treats as integer)
#' * date2, mean date in census 2
#' * dbhmean, mean dbh in census 1 of individuals included
#'
#'
#' Pass the list to assemble.demography (in utilities.r) with type="m" to convert the list a data.frame.
#'
#'
#'
#'
#' @details My details
#' @section My section:
#' My cool section
#' @author R. C.
#' @aliases other
"abundance"


