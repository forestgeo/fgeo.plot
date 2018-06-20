# Few quadrats based on Luquillo

library(fgeo)

vft_4quad <- fgeo.data::luquillo_vft_4quad %>% pick_top(CensusID)
use_data(vft_4quad, overwrite = TRUE)

vft_1quad <- vft_4quad %>% pick_top(QuadratID, 1)
use_data(vft_1quad, overwrite = TRUE)
