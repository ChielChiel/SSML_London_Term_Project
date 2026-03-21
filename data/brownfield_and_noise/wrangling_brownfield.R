easypackages::packages ("sf", "sp", "spdep", "spatialreg", "GWmodel", "tmap", "mapview", "car", "RColorBrewer", 
                        "cowplot", "leafsync", "leaflet.extras2", "mapview", "tidyverse", "readr", "this.path")

setwd(this.path::here())


dir <- "brownfield"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE, recursive = TRUE)

ward.polygons <- read_sf("../london_wards.gpkg")

ward.polygons <- ward.polygons |>
  select(GSS_CODE, geom)|>
  mutate(ward_area = st_area(ward.polygons))


brownfield.data <- read_sf(ff)

brownfield.data.valid.geoms <- st_make_valid(brownfield.data)

final <- st_intersection(brownfield.data.valid.geoms, ward.polygons)|>
  mutate(areaBrownfield = st_area(geometry))|>
  group_by(GSS_CODE)|>
  summarise(proportion_brownfield = as.numeric(sum(areaBrownfield)/unique(ward_area)))|>
  st_drop_geometry()


ward.polygons <- st_drop_geometry(ward.polygons)
final <- left_join(ward.polygons[1], final, by = join_by(GSS_CODE))
final[is.na(final)] <- 0

write.csv(final, "areas/brownfield.csv", row.names = FALSE)

