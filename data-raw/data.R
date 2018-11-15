# Few quadrats based on Luquillo

vft_1quad <- fgeo.data::luquillo_vft_4quad %>% 
  fgeo.tool::pick_top(CensusID) %>% 
  fgeo.tool::pick_top(QuadratID, 1)

use_data(vft_1quad, overwrite = TRUE)
