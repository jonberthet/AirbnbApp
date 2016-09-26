
#Geo Spatial Weighted Models paper: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf


#Get distance matrix for airbnb price data
library("GWmodel")
library("rgdal")
library("RColorBrewer")
data("EWHP")

head(ewhp)

LatLon2EastNorthing <- function(latitude, longitude){
  df <- as.data.frame(latitude, longitude)   #Create df of coordinates
  df$ID <- 1:nrow(df)                        #create unique ID per coordinate
  coords <- cbind(Latitude =  as.numeric(as.character(df$latitude)),
                  Longitude = as.numeric(as.character(df$longitude)))
  #Create Spatial DF
  df2 <- SpatialPointsDataFrame(coords,
                                data = data.frame(listing[,c("price")]),
                                proj4string = CRS("+init=epsg:4326")) #interpreting latlong
}




#########################WORKING CODE BELOW
listLatLon <- listing[,c("latitude", "longitude")]
listLatLon$ID <- 1:nrow(listLatLon)
# data <- listing[,c("price", "weekly_price", "availability_30")]
#Create coordinate variables
data <- cbind(Latitude =  as.numeric(as.character(listLatLon$latitude)),
              Longitude = as.numeric(as.character(listLatLon$longitude)),
              listing[,c("price", "weekly_price", "availability_30")])   #Variables to Use
data_complete <- data[complete.cases(data),]

#Create Spatial DF
df <- SpatialPointsDataFrame(data_complete[,c(1,2)],
                             data = data.frame(data_complete[,c(3:ncol(data_complete))]),
                             proj4string = CRS("+init=epsg:4326")) #interpreting latlong

#Convert from Longitude & Latitude to Eastings and Northings to Latitude and Longitude
GP_SP_LL <- spTransform(df, CRS("+init=epsg:27700"))
# we also need to rename the columns
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Longitude"] <- "Easting"
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Latitude"] <- "Northing"
east_northing <- as.data.frame(GP_SP_LL@coords)

#Variables for holding coordinate system types
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"



#####EXAMPLE


# gw.ss.bx <- gwss(Dub.voter, vars = c("GenEl2004", "LARent", "Unempl"),kernel = "boxcar", adaptive = TRUE, bw = 48, quantile = TRUE)
gwssbx <- gwss(df,
               vars = c("price", "weekly_price"),   #"availability_30"
               kernel = "bisquare",    #bocar relates to un-weighted moving window. bisquare relates to weighted moving window. Also try tricube, gaussian, exponential
               adaptive = TRUE,
               bw = 0.15 * nrow(df),    #roughly 15% of df data
               quantile = TRUE)    #median, IQR, and quantile imbalance calculated


map.na = list("SpatialPolygonsRescale",
              layout.north.arrow(),
              offset = c(100000, 300000),
              scale = 4000,
              col = 1)

map.scale1 <- list("SpatialPolygonsRescale",
                   layout.north.arrow(),
                   offset = c(200000, 300000),
                   scale = 4000,
                   col = 1,
                   fill = c("transparent", "blue"))

map.layout <- list(map.na, map.scale1)

mypalette1 <- brewer.pal(4, "Reds")

X11(width = 12, height = 15)
spplot(gwssbx$SDF,
       "price_LSD",
       key.space = "right",
       col.regions = mypalette1,
       cuts = 2,
       main = "GW standard deviations for price",
       sp.layout = map.layout)
