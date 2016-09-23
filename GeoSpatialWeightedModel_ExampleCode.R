
#Geo Spatial Weighted Models paper: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf

library("GWmodel")
library("rgdal")
library("RColorBrewer")
data("EWHP")

head(ewhp)

listLatLon <- listing[,c("latitude", "longitude")]
listLatLon$ID <- 1:nrow(listLatLon)
#Create coordinate variables
coords <- cbind(Latitude =  as.numeric(as.character(listLatLon$latitude)),
                Longitude = as.numeric(as.character(listLatLon$longitude)))
#Create Spatial DF
df <- SpatialPointsDataFrame(coords,
                             data = data.frame(listing$price),
                             proj4string = CRS("+init=epsg:4326")) #interpreting latlong

# plot(df, col = "red")

# Convert from Longitude & Latitude to Eastings and Northings to Latitude and Longitude
GP_SP_LL <- spTransform(df, CRS("+init=epsg:27700"))
# we also need to rename the columns
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Longitude"] <- "Easting"
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Latitude"] <- "Northing"
east_northing <- as.data.frame(GP_SP_LL@coords)

#Variables for holding coordinate system types
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"
?SpatialPointsDataFrame


# data("DubVoter")
# summary(Dub.voter)
# class(Dub.voter)

gwssbx <- gwss(df,
               vars = "listing.price",
               kernel = "boxcar",
               adaptive = TRUE,
               bw = 48,    #bandwidth
               quantile = TRUE)






map.na = list("SpatialPolygonsRescale",
              layout.north.arrow(),
              offset = c(400000, 300000),
              scale = 4000,
              col = 1)
map.scale1 <- list("SpatialPolygonsRescale",
                   layout.north.arrow(),
                   offset = c(400000, 300000),
                   scale = 4000,
                   col = 1,
                   fill = c("transparent", "blue"))
map.layout <- list(map.na, map.scale1)



mypalette1 <- brewer.pal(4, "Reds")

X11(width = 10, height = 12)
spplot(gwssbx$SDF,
       "listing.price_LSD",
       key.space = "right",
       col.regions = mypalette1,
       cuts = 2,
       main = "GW standard deviations for price",
       sp.layout = map.layout)
