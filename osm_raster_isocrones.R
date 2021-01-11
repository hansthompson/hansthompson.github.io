library(mapboxapi)
library(tidyverse)
library(mapdeck)
library(osmdata)
library(leaflet)
library(sf)
library(raster)
library(rgeos)
library(fasterize)
library(rgdal)
library(sf)
library(dplyr)

#mapboxapi::mb_access_token("my_token",
#                           install = TRUE, overwrite = TRUE)


x <- getbb("Anchorage, AK") %>% opq() %>%
  add_osm_feature("name", "Costco") %>%
  osmdata_sf()

all_latlon <- c()
for (i in x$osm_polygons$osm_id) {
  
  latlon <-
    paste0(x$osm_polygons$geometry[[i]][1][[1]][1,1], ", ",
           x$osm_polygons$geometry[[i]][1][[1]][1,2])
  
  all_latlon <- c(latlon, all_latlon)
}

all_isos <- data.frame()

for(j in all_latlon) {
  isos <- mb_isochrone(
    location = j,
    profile = "driving",
    time = 1:10,
  )  
  print(j)
  all_isos <- rbind(all_isos, isos)
  
}


iso_sf <- st_as_sf(all_isos)

iso_union <- iso_sf %>%
  group_by(time) %>%
  summarise()

isos_proj <- st_transform(iso_sf, 32615)

template <- raster(isos_proj, resolution = 100)

iso_surface <- fasterize(isos_proj, template, field = "time", fun = "min")

pal <- colorNumeric("viridis", isos_proj$time, na.color = "transparent")
leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addRasterImage(iso_surface, colors = pal, opacity = 0.5) %>%
  addLegend(values = isos_proj$time, pal = pal,
            title = "Drive Time (Minutes)") %>%
  addPolygons(data = x$osm_polygons, popup = ~name,
              opacity = 1.0, fillOpacity = 0.5, 
              color = "orange")

#writeRaster(iso_surface, filename = "Costco.tif")

