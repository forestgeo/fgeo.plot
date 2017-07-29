# Plot species specialization.

# Packages

library(readr)
library(dplyr)
library(ggplot2)




sig_spp <- read_csv("./data/spp_specialization.csv")
sig_spp <- select(sig_spp, 1, 2)
sig_spp <- mutate(
  sig_spp, 
  spp_spec_4 = rep(c("LS", "P", "G", "F"), 5)
)


ggplot(
  sig_spp, 
  aes(x = sp, y = PN.sq.change.N1, fill = spp_spec_4)
) +
  geom_bar(stat = "identity")





