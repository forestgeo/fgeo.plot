#' Plot objects of class "tt_df".
#'
#' @param x An object of class "tt_df".
#' @param ... Other arguments passed to methods.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(fgeo.tool)
#' library(fgeo.habitat)
#' census <- filter(luquillo_top3_sp, status == "A", dbh >= 10)
#' habitat <- luquillo_habitat
#' species <- c("CASARB", "PREMON", "SLOBER")
#' 
#' tt_df <- to_df(tt_test(census, species, habitat))
#' plot(tt_df)
#' }
plot.tt_df <- function(x, ...) {
  ggplot(x, aes(sp, .data$probability)) + 
    geom_col(aes(fill = .data$distribution), position = "dodge") +
    geom_label(aes(label = .data$stem_count)) +
    facet_wrap(vars(.data$habitat)) +
    labs(
      title = "Probability of species distribution by habitat",
      x = "species",
      y = "probability",
      fill = "distribution",
      caption = "The number on each bar indicates the count of stems."
    ) +
    coord_flip()
}
