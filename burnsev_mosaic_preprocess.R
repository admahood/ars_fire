# unzip 100ish BS mosaics

# setup =============
library(tidyverse)
library(terra)

# file unzip (data downloaded from mtbs.gov) =====
years <- list.files("data/composite_data/MTBS_BSmosaics/")

# for(y in years){
#   files<-list.files(file.path("data/composite_data/MTBS_BSmosaics", y), 
#                     pattern = ".zip", full.names = TRUE)
#   for(f in files){
#   system(paste("unzip", f, "-d /home/a/projects/ars_fire/data/unzipped"))}
# }

# stacking them all together ===========
all_tifs <- list.files("data/unzipped", pattern = ".tif", full.names=TRUE)
CO_tif_files <- list.files("data/unzipped", pattern = "CO_\\d{4}.tif", full.names=TRUE)
NM_tif_files <- list.files("data/unzipped", pattern = "NM_\\d{4}.tif", full.names=TRUE)
WY_tif_files <- list.files("data/unzipped", pattern = "WY_\\d{4}.tif", full.names=TRUE)

terra::rast(CO_tif_files[1]) -> r1

crs <- terra::crs(r1)
tf <- all_tifs[1]
df_it <- function(tf){
  year <- str_extract(tf, "\\d{4}") %>% as.numeric()
  df <- tf %>% 
    terra::rast() %>% 
    as.data.frame(xy=TRUE) %>%
    mutate(year = year)
}

exts <- data.frame(xmin=NA, xmax=NA, ymin=NA, ymax=NA)
for(i in 1:length(CO_tif_files)){
  v<- terra::rast(CO_tif_files[i]) %>% ext() %>% as.vector()
  exts[i,1] <- v["xmin"] %>% as.numeric
  exts[i,2] <- v["xmax"] %>% as.numeric
  exts[i,3] <- v["ymin"] %>% as.numeric
  exts[i,4] <- v["ymax"] %>% as.numeric
}

new_extent <- c(max(exts$xmin), max(exts$xmax), max(exts$ymin), max(exts$ymax))

rastlist <- list()

for(i in 1:length(CO_tif_files)){
  terra::rast(CO_tif_files[i]) -> r1
  
  r22 <- terra::rast(vals=values(r1),
                     crs=crs(r1))
  ext(r22) <- new_extent
  names(r22) <- names(r1)
  
}