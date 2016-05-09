# This script is dedicated to analyze scraped Airbnb webpages in order
# to provide some insights about Airbnb pricing, Airbnb location and
# even daily monitor dashboards. 

library(dplyr)
library(ggplot2)
library(RgoogleMaps)
library(ggmap)
library(ggrepel)
library(maps)
library(sp)
library(gstat)
library(gridExtra)

CITYNAME = 'Houston TX'
ZOOM = 11
WORKINGDIR = '/media/sean/disk2/desktop/airbnb/AirbnbScrape'

setwd(WORKINGDIR)
# Get Airbnb listing data
dat <- read.csv('data/OutputFile.csv', header = TRUE)

# Get Airbnb (listing, day) level data
datDaily <- read.csv('data/OutputFileDaily.csv', header = TRUE)
datDaily <- datDaily[as.Date(datDaily$date) >= Sys.Date(), ]

dat$ListingID <- as.integer(dat$ListingID)
datSliced <- dat[, c('ListingID', 'Price', 'Lat', 'Long')]

# Get city cooridnated based on city name
center <- t(us.cities[us.cities$name == CITYNAME, c('lat', 'long')])
# center <- c(median(dat$Lat), median(dat$Long))

# Join listing level data with daily level data
datJoined <- datDaily %>%
    group_by(ListingID = listingID) %>%
    summarise(occupancyRate = mean(ifelse(availability == 'True', FALSE, TRUE)),
              listingPrice = mean(price)) %>%
    inner_join(datSliced)

map <- get_googlemap(center = center[2:1], zoom = ZOOM, maptype = 'roadmap')

midRate <- median(datJoined$listingPrice)
rateLimits <- c(0, 2 * midRate)
midOccupancy <- median(datJoined$occupancyRate)
occupancyLimits <- c(max(0, 2 * midOccupancy - 1),
                     min(1, 2 * midOccupancy))

# Some useful metrics and charts
# Histogram of occupancy rate
hist(datJoined$occupancyRate)
ggplot(aes(x = Price, y = listingPrice), data = datJoined) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)

# Price
gg1 <- ggplot(data = datJoined, aes(y = Lat, x = Long,
                                    color = listingPrice)) 
g1 <- ggmap(map, base_layer = gg1, alpha = 0.1, darken = c(0.2, 'black')) +
    geom_point(alpha = 1, size = 3) +
    scale_colour_gradient2(midpoint = midRate,
                           low = 'blue', high = 'red', mid = 'white',
                           limits = rateLimits) +
    theme(legend.position = 'top',
          legend.key.width = unit(2, 'cm'))

# Occupancy Rate
gg2 <- ggplot(data = datJoined, aes(y = Lat, x = Long,
                                    color = occupancyRate)) 
g2 <- ggmap(map, base_layer = gg2, alpha = 0.1, darken = c(0.3, 'black')) +
    geom_point(alpha = 1, size = 3) +
    scale_colour_gradient2(midpoint = midOccupancy,
                           low = 'blue', high = 'red', mid = 'white',
                           limits = occupancyLimits) +
    theme(legend.position = 'top',
          legend.key.width = unit(2, 'cm'))

g <- grid.arrange(g1, g2, ncol = 2)



ggsave(plot = g, filename = 'plot/houston.png', width = 15, height = 10)

# Kriging
datKriging <- data.frame(datJoined)
coordinates(datKriging) = ~ Long + Lat
coords <- attributes(map)$bb
xlim <- c(coords$ll.lon, coords$ur.lon)
ylim <- c(coords$ll.lat, coords$ur.lat)

gridx <- (xlim[1] * (0:420) + xlim[2] * (420:0))/420
gridy <- (ylim[1] * (0:420) + ylim[2] * (420:0))/420
grids <- data.frame(expand.grid(x = gridx, y = gridy))

gridded(grids) = ~ x + y

m <- vgm(.5, "Sph", 1, .04)
x <- krige(Price ~ 1, datKriging, grids, model = m)
xx = data.frame(x = x@coords[,1], y = x@coords[,2], Price = x@data$var1.pred)

ggmap(map, base_layer = gg, alpha = 0.1, darken = c(0.3, 'black')) +
    geom_point(alpha = 1, size = 3) +
    scale_colour_gradient2(midpoint = midRate,
                           low = 'blue', high = 'red', mid = 'white',
                           limits = rateLimits) +
    geom_raster(aes(x = x, y = y, fill = Price), data = xx, alpha = 0.5) +
    coord_cartesian()

spplot(x["var1.pred"], main = "ordinary kriging predictions")


gg <- ggplot(data = xx, aes(y = y, x = x,
                            color = Price, fill = Price)) 

ggmap(map, base_layer = gg, alpha = 0.1, darken = c(0.3, 'black')) +
    geom_point(aes(x = Long, y = Lat, color = Price),
               data = datJoined, alpha = 1, size = 3) +
    scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
    ## scale_colour_gradient2(midpoint = midRate,
    ##                        low = 'blue', high = 'red', mid = 'white',
    ##                        limits = rateLimits) +
    geom_raster(alpha = 0.5) +
    coord_cartesian()








