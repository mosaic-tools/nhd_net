
library(nhdplusTools)
library(tidyverse)
library(dplyr)
library(magrittr)


#to download full nhdplusV2 on my computer
nhdplus_path("D:/projects/TSS/data/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb")
nhd_paths <- stage_national_data()

network <- readRDS(nhd_paths$flowline)

#to download nhdplusv2 to your machine, specify the filepath in outdir argument

#network <- download_nhdplusv2(outdir=)

#function to create network for tinkertoy model given the outlet reach (i.e. comid)
makeNetwork <- function(network, comid) {
  
  up_id <- get_UT(network,comid )
  
  net <- network %>% 
    filter(StreamOrde == StreamCalc) %>%
    filter(COMID %in% up_id) %>%
    left_join(select(network %>%
                       as_tibble %>%
                       filter(StreamOrde == StreamCalc) %>%
                       filter(COMID %in% up_id) %>% 
                       select(-Shape),
                     toCOMID = COMID, FromNode),
              by = c("ToNode" = "FromNode")) %>%
    left_join(select(network %>%
                       as_tibble %>%
                       filter(StreamOrde == StreamCalc) %>%
                       filter(COMID %in% up_id) %>%
                       select(-Shape),
                     fromCOMID = COMID, ToNode),
              by = c("FromNode" = "ToNode"))  %>%
    group_by(COMID) %>%
    mutate( fromCOMID = as.character(list(unique(fromCOMID)))) %>%
    ungroup() %>%
    st_set_geometry(NULL) %>%
    distinct_at(vars(COMID:Enabled), .keep_all = T)
  
  return(net)
  
}


savannah <- makeNetwork(network, comid =  18242767)

write_csv(savannah, "D:/Dropbox/projects/mosaics/savannah_river_v2.csv")


############### MUNGE #################
# sav_1 <- network %>% 
#   filter(StreamOrde == StreamCalc) %>%
#   filter(COMID %in% up_id_sav) %>%
#   left_join(select(network %>%
#                      as_tibble %>%
#                      filter(StreamOrde == StreamCalc) %>%
#                      filter(COMID %in% up_id_sav) %>% 
#                      select(-Shape),
#                     toCOMID = COMID, FromNode),
#              by = c("ToNode" = "FromNode")) %>%
#   left_join(select(network %>%
#                      as_tibble %>%
#                      filter(StreamOrde == StreamCalc) %>%
#                      filter(COMID %in% up_id_sav) %>%
#                      select(-Shape),
#                    fromCOMID = COMID, ToNode),
#             by = c("FromNode" = "ToNode"))  %>%
#   group_by(COMID) %>%
#   mutate( fromCOMID = as.character(list(unique(fromCOMID)))) %>%
#   ungroup() %>%
#   st_set_geometry(NULL) %>%
#   distinct_at(vars(COMID:Enabled), .keep_all = T)
# 
# write_csv(sav_1, "D:/Dropbox/projects/mosaics/savannah_river_v2.csv")

