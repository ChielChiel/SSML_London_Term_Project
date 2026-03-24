easypackages::packages ("sf", "sp", "spdep", "spatialreg", "GWmodel", "tmap", "mapview", "car", "RColorBrewer", 
                        "cowplot", "leafsync", "leaflet.extras2", "mapview", "tidyverse", "readr", "this.path")

setwd(this.path::here())


dir <- "noise"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE, recursive = TRUE)

ward.polygons <- read_sf("../london_wards.gpkg")

ward.polygons <- ward.polygons |>
  select(GSS_CODE, geom)|>
  mutate(ward_area = st_area(ward.polygons))


noise.data <- lapply(ff, read_sf)

noise.data.valid.geoms <- lapply(noise.data, st_make_valid)

rm(noise.data)


st_crs(ward.polygons) <- st_crs(noise.data.valid.geoms[[1]])
#st_crs(noise.data.valid.geoms[[1]]) <- st_crs(ward.polygons)


hey <- function(x){
  st_intersection(x, ward.polygons)|>
    mutate(areaNoise = st_area(geometry))|>
    group_by(GSS_CODE, NoiseClass)|>
    summarise(proportion_noise = as.numeric(sum(areaNoise)/unique(ward_area)))
  
}

intersections <- lapply(noise.data.valid.geoms, hey)



names(intersections) <- ff

hoi <- function(x){
  st_drop_geometry(intersections[[x]])|>
    mutate(noise_category = gsub(".*\\/(.*)\\.shp$", "\\1", x))
  
}


intersections.df <- lapply(names(intersections), hoi)


final <- Reduce(function(x, y) merge(x,y, all= TRUE), intersections.df)
final.wide <- pivot_wider(final, values_from = proportion_noise, names_from = c(noise_category, NoiseClass))

ward.polygons <- st_drop_geometry(ward.polygons)
final.wide <- left_join(ward.polygons[1], final.wide, by = join_by(GSS_CODE))

final.wide[is.na(final.wide)] <- 0


write.csv(final.wide, "areas/noise_polution.csv", row.names = FALSE)



