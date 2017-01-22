# This script is dedicated to analyze scraped Airbnb webpages in order
# to provide some insights about Airbnb pricing, Airbnb location and
# even daily monitor dashboards. 

library(ggmap)
library(geosphere)

args <- commandArgs(TRUE)
kCityName <- args[1]
kBoxWidth <- as.numeric(args[2])
kMile <- 1609.34
kUnit <- 0.1
main <- function() {
    coords <- geocode(kCityName)
    lat_unit <- distGeo(coords, coords + c(0, kUnit)) / (kMile * kUnit)
    lng_unit <- distGeo(coords, coords + c(kUnit, 0)) / (kMile * kUnit)
    sw_lat <- coords$lat - kBoxWidth / lat_unit
    sw_lng <- coords$lon - kBoxWidth / lng_unit
    ne_lat <- coords$lat + kBoxWidth / lat_unit
    ne_lng <- coords$lon + kBoxWidth / lng_unit
    cat(sprintf(
        '
        {"sw_lat" : %s,
         "sw_lng" : %s,
         "ne_lat" : %s,
         "ne_lng" : %s}',
                  sw_lat,
                  sw_lng,
                  ne_lat,
                  ne_lng))
}

main()
