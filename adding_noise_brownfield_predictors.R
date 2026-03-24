easypackages::packages ("sf", "sp", "spdep", "spatialreg", "GWmodel", "tmap", "mapview", "car", "RColorBrewer", 
                        "cowplot", "leafsync", "leaflet.extras2", "mapview", "tidyverse", "readr", "this.path")

setwd(this.path::here())


data <- st_read("data/ward_poly.gpkg")
brownfield.data <- read.csv("data/brownfield_and_noise/areas/brownfield.csv")
noise.data <- read.csv("data/brownfield_and_noise/areas/noise_polution.csv")


data <- left_join(data, brownfield.data, by = join_by(New.ward.code == GSS_CODE), suffix = c("", ""))
data <- left_join(data, noise.data, by = join_by(New.ward.code == GSS_CODE), suffix = c("", ""))

st_write(data, dsn = 'data/ward_poly.gpkg', delete_dsn = TRUE)
