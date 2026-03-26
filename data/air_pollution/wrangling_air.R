easypackages::packages ("sf", "sp", "spdep", "spatialreg", "GWmodel", "tmap", "mapview", "car", "RColorBrewer", 
                        "cowplot", "leafsync", "leaflet.extras2", "mapview", "tidyverse", "readr", "this.path",
                        "janitor")

setwd(this.path::here())



dir <- "Geopackage_GPKG"
ff <- list.files(dir, pattern="\\.gpkg$", full.names=TRUE, recursive = TRUE)

ward.polygons <- read_sf("../london_wards.gpkg")

ward.polygons <- ward.polygons |>
  select(GSS_CODE, geom)|>
  mutate(ward_area = st_area(ward.polygons))


air.data <- lapply(ff, read_sf)
air.data <- lapply(air.data, clean_names)

air.data.valid.geoms <- lapply(air.data, st_make_valid)
#air.data.valid.geoms <- air.data.valid.geoms[-3]

st_crs(ward.polygons) <- st_crs(air.data.valid.geoms[[1]])



air.data.valid.geoms[[3]] <- air.data.valid.geoms[[3]] |>
  mutate(all_sources_2013 = rowSums(across(ends_with("2013"))))




hey <- function(x){
  x |>
    mutate(areaPol = st_area(geom))|>
    st_intersection(ward.polygons)|>
    mutate(areaInt = st_area(geom),
           pollution_intersection = (areaInt/areaPol) * all_sources_2013,)|>
    group_by(GSS_CODE, pollutant)|>
    summarise(interpolated_relative_pollution = as.numeric(sum(pollution_intersection)/unique(ward_area)))|>
    st_drop_geometry()
}


intersections <- lapply(air.data.valid.geoms, hey)


final <- Reduce(function(x, y) merge(x,y, all= TRUE), intersections)
final.wide <- pivot_wider(final, values_from = interpolated_relative_pollution, names_from = c(pollutant))


ward.polygons <- st_drop_geometry(ward.polygons)
final.wide <- left_join(ward.polygons[1], final.wide, by = join_by(GSS_CODE))

final.wide[is.na(final.wide)] <- 0


write.csv(final.wide, "air_polution.csv", row.names = FALSE)
