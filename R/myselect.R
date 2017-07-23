#' Source of parameter x
#'
#'
#' @param x Documented in x
#' @param y Documented in x
#' @export
from1 <- function(x, y) {paste0(x, y)}

#' Source of parameter y
#'
#'
#' @param y Documented in y
#' @export
from2 <- function(y) {y}


#' Source of parameter z
#'
#' @inheritParams from2
#' @inheritParams from1
#' @param z Documented in to0
#'
#' @return Stuff
#' @export
to0 <- function(x, y, z) {paste0(x, y, z)}
