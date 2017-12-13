# Flag inline helpers as global variables so R CMD check doesn't warn
utils::globalVariables(
  c(".data", "qx", "qy", "status_tree", "subquadrat", "elev", "..level..")
)
