#' classify_fgeo_tables ForestGEO-like data or return unknown.
#'
#' @param .data A list or a dataframe.
#'
#' @return A string.
#' @export
#'
#' @examples
#' fgeo(fgeo.data::luquillo_elevation)
#' fgeo(fgeo.data::luquillo_elevation$col)
#' fgeo(fgeo.data::luquillo_stem6_random)
#' fgeo(fgeo.data::luquillo_tree6_random)
#' fgeo(list(1))
#' fgeo(data.frame(x = 1))
classify_fgeo_tables <- function(.data) {
  UseMethod("classify_fgeo_tables")
}

classify_fgeo_tables.list <- function(.data) {
  if (!is_known_elevation(.data)) return("unknown")
  "elevation_lst"
}

classify_fgeo_tables.data.frame <- function(.data) {
  if (is_known_elevation(.data)) return("elevation_df")
  if (has_stem_names(.data)) return("stem")
  if (has_tree_names(.data)) return("tree")
  "unknown"
}

# Helpers -----------------------------------------------------------------

has_elevation_names <- fgeo.base::has_table_names(
  fgeo.tool::fgeo_elevation(fgeo.data::luquillo_elevation$col)
)

has_stem_names <- fgeo.base::has_table_names(fgeo.data::luquillo_stem6_1ha)
has_tree_names <- fgeo.base::has_table_names(fgeo.data::luquillo_tree6_1ha)

is_known_elevation <- function(.data) {
  out <- TRUE
  out <- tryCatch(
    has_elevation_names(fgeo.tool::fgeo_elevation(.data)),
    error = function(e) FALSE
  )
  
  out
}
