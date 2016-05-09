MyMap <- GetMap(center=center, zoom=12, destfile = "MyTile1.png");
PlotOnStaticMap(MyMap, lat = dat.sliced$Lat, 
                       lon = dat.sliced$Long, 
                       destfile = "MyTile1.png",
                       cex=1.5,pch=20, add=FALSE); 
g1 <- ggmap(map1) +
    geom_point(data = datListing, aes(y = Lat, x = Long), alpha = 0.5, size = 1) +
    geom_label_repel(data = datListing,
              aes(x = Long, y = Lat, label = Price),
              nudge_x = 0.001,
              alpha = 0.5,
              color = 'blue',
              size = 2.5,
              force = 0.003,
              box.padding = unit(0.25, "lines"),
              point.padding = unit(0.5, "lines"))
