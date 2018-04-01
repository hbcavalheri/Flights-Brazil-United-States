library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(plotly)
library(maptools)

################################################################################
# I will need this function to speed up plotting
# Function code got from:
#https://gis.stackexchange.com/questions/236340/simplifying-and-plotting-polygons-in-leaflet-package-in-r/236465#236465

getSmallPolys <- function(poly, minarea=0.01) {
    areas <- lapply(poly@polygons, 
                    function(x) sapply(x@Polygons, function(y) y@area))
    
    print(quantile(unlist(areas)))
    
    bigpolys <- lapply(areas, function(x) which(x > minarea))
    length(unlist(bigpolys))
    
    for(i in 1:length(bigpolys)){
        if(length(bigpolys[[i]]) >= 1 && bigpolys[[i]] >= 1){
            poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
            poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
        }
    }
    return(poly)
}
################################################################################

r.lights <- readShapePoly("ne_10m_urban_areas.shp") %>% 
            getSmallPolys(., minarea=0.01)
r.lights.simple <- gSimplify(r.lights, tol = 0.01, topologyPreserve = TRUE)

urb <- c(geom_polygon(aes(long, lat, group = group), size = 0.3, color = "#eae607",
                      fill = "#eae607", alpha = 0.5, data = r.lights.simple))

mapa1 <- map_data("world")
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#1e2b8b", 
                     fill="#1e2b8b", alpha=0.8, data=mapa1))

airp <- read.table('airports.dat.txt', sep = ',') %>% 
    dplyr::select(V2, V3, V4, V5, V7, V8) %>% 
    dplyr::rename(airport.name = V2, city = V3, country = V4, IATA = V5, latitude = V7,
           longitude = V8)


route <- read.table('routes.dat.txt', sep = ',') %>% 
    dplyr::select(V1, V2, V3, V5) %>% 
    dplyr::rename(airline = V1, airline.id = V2, airp.from = V3, airp.to = V5) %>% 
    inner_join(airp, by = c('airp.from' = 'IATA')) %>% 
    dplyr::rename(airp.name.from = airport.name, city.from = city, 
           country.from = country, latitude.from = latitude,
           longitude.from = longitude) %>% 
    inner_join(airp, by = c('airp.to' = 'IATA')) %>% 
    dplyr::rename(airp.name.to = airport.name, city.to = city, 
           country.to = country, latitude.to = latitude,
           longitude.to = longitude) %>% 
    filter(country.from == 'Brazil') %>% 
    filter(country.to != 'Brazil') %>% 
    mutate(group = 1:NROW(.))

route.f <- data.frame('airline' = route$airline, 'airline.id' = route$airline.id, 
               'airport' = c(route$airp.from, route$airp.to), 
               'name' = c(route$airp.name.from, route$airp.name.to), 
               'city' = c(route$city.from, route$city.to), 
               'country' = c(route$country.from, route$country.to), 
               'latitude' = c(route$latitude.from, route$latitude.to), 
               'longitude' = c(route$longitude.from, route$longitude.to), 
               'group' = rep(route$group, 2), 'origin' = rep(route$city.from, 2))

h <- ggplot() + wrld + urb +
    theme(panel.background = element_rect(fill='#10174a',colour='#10174a'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    coord_map("ortho", orientation=c(-7, -55, 0)) +
    geom_path(data = route.f, aes(longitude, latitude, group = group, 
                                  color = origin), alpha = 0.3, 
              show.legend = FALSE)
h



