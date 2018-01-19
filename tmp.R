library(stringr)
library(purrr)
library(dplyr)

# # Issue 33

# tricky_quad <- c("0100", "0101", "1000")
# expect_nms <- map(tricky_quad, paste0, "_", 1:4) %>% reduce(c)
# 
# # Good order
# tricky_quad %>% sort()
# 
# # Bad order if tricky_quad ever becomes a number
# tricky_quad %>% as.numeric() %>% sort()
# tricky_quad %>% as.numeric() %>% as.character() %>% sort()
# 
# # Good order again if numbers are padded with 0.
# tricky_quad %>% as.numeric() %>% str_pad(4, pad = 0) %>% sort()


