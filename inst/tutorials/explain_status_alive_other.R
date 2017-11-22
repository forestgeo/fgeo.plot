# The variable status refers to a stem; not to a tree. But map_tag() is designed
# to map a single point per tag, so it can not deal with a tag with multiple
# values of the variable status. Thus I create a variable status_tree, and
# I assign to it the value "alive" if any stem for that tag is alive on the last
# census; else the value of status is "other".
s <- tibble::as_tibble(sinharaja::sinh_vft) %>% set_names(tolower)
last_cns <- s %>%
  filter(censusid == max(unique(censusid)))

is_duplicated <- last_cns %>%
  pull(tag) %>%
  duplicated()

duplicated_tags <- unique(last_cns[is_duplicated, "tag"] %>% pull())

last_cns %>%
  filter(tag %in% duplicated_tags) %>%
  select(censusid, tag, matches("status"), everything()) %>%
  arrange(tag)
