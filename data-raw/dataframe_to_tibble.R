dataframe_to_tibble <- data.frame(x = 1:10)
dataframe_to_tibble
dataframe_to_tibble <- tibble::as_tibble(dataframe_to_tibble)
use_data(dataframe_to_tibble)
