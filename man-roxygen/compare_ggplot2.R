#' @section Comparison with ggplot2:
#' These functions wrap functions from the \strong{ggplot2} package. For more
#' control you can use \strong{ggplot2} directly or smaller wrappers in
#' \strong{fgeo.map} (see the sections See Also and Examples). \strong{ggplot2}
#' will give you maximum control and is your best choice if you are already
#' familiar with it. The wrapper functions in \strong{fgeo.map} focus on
#' ForestGEO's data: For example, their names commonly mention the variable of a
#' ForestGEO dataset they work with.
#' 
#' The most important difference between \strong{ggplot2} and \strong{fgeo.map}
#' is in the way you compose multiple functions to create complex plots. With
#' \strong{ggplot2} you compose multiple functions with \code{+}. For example,
#' to apply functions \code{f()} then \code{g()} you do something like this:
#' \code{f(data) + g()}. With \strong{fgeo.map} you compose multiple functions
#' in the usual way with \code{g(f(data))}, or in the modern way with the pipe:
#' \code{f(data)} \code{\%>\%} \code{g()} -- it is up to you. To learn more
#' about the pipe see http://r4ds.had.co.nz/pipes.html.
