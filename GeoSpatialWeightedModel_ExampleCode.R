
#Geo Spatial Weighted Models paper: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf


#Get distance matrix for airbnb price data
library("GWmodel")
library("rgdal")
library("RColorBrewer")
library(maps); library(ggplot2); library(mapproj)

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

#########################LATITUDE / LONGITUDE
#Create coordinate variables
data_latlon <- cbind(Latitude =  as.numeric(as.character(listLatLon$latitude)),
                     Longitude = as.numeric(as.character(listLatLon$longitude)),
                     listing[,c("price", "weekly_price", "availability_30")])   #Variables to Use
data_complete_latlon <- data_latlon[complete.cases(data_latlon),]

#Create Spatial DF : Source: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
#Latitude / Longitude
df_latlong <- SpatialPointsDataFrame(data_complete_latlon[,c(1,2)],
                                     data = data.frame(data_complete_latlon[,c(3:ncol(data_complete_latlon))]),
                                     proj4string = CRS("+init=epsg:32051")) #interpreting latlong

###################Easting / Northing
#Convert from Longitude & Latitude to Eastings and Northings to Latitude and Longitude
GP_SP_LL <- spTransform(df_latlong[,c(1,2)], CRS("+init=epsg:27700"))
# we also need to rename the columns
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Latitude"] <- "Northing"
colnames(GP_SP_LL@coords)[colnames(GP_SP_LL@coords) == "Longitude"] <- "Easting"
east_northing <- as.data.frame(GP_SP_LL@coords)

#Latitude / Longitude
df_EastNorth <- SpatialPointsDataFrame(east_northing[,c(1,2)],
                                     data = data.frame(data_complete_latlon[,c(3:ncol(data_complete_latlon))]),
                                     proj4string = CRS("+init=epsg:32051")) #interpreting latlong   3855




library(rgdal)
library(sp)
#Projection:
projInfo(type = "proj")
#Datum:
projInfo(type = "datum")
#Ellipsoid:
projInfo(type = "ellps")

#Get DF of EPSG
EPSG <- make_EPSG()

#Query EPSG
EPSG[grep("lat_2=38.8", EPSG$prj4),]


#Variables for holding coordinate system types
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"


#####EXAMPLE
#STRANGELY: ONLY boxcar kernel only works. Other kernels throw error.
gwssbx_eastnorth <- gwss(df_EastNorth,
               vars = c("price", "weekly_price"),   #"availability_30"
               kernel = "boxcar",    #bocar relates to un-weighted moving window. bisquare relates to weighted moving window. Also try tricube, gaussian, exponential
               adaptive = TRUE,
               bw = 0.15 * nrow(df),    #roughly 15% of df data
               quantile = TRUE)    #median, IQR, and quantile imbalance calculated

gwssbx_latlong <- gwss(df_latlong,
                       vars = c("price", "weekly_price"),   #"availability_30"
                       kernel = "boxcar",    #bocar relates to un-weighted moving window. bisquare relates to weighted moving window. Also try tricube, gaussian, exponential
                       adaptive = TRUE,
                       bw = 0.15 * nrow(df),    #roughly 15% of df data
                       quantile = TRUE)    #median, IQR, and quantile imbalance calculated



#LatLong
map.na = list("SpatialPolygonsRescale",layout.north.arrow(),offset = c(-77.1, 38.95),scale = 1000,col = 1)
map.scale1 <- list("SpatialPolygonsRescale", layout.north.arrow(),offset = c(-77.04, 38.97),scale = 2000,col = 1,fill = c("transparent", "blue"))
map.scale2 <- list("sp.text",c(-77, 38.85),"0",cex = 0.4,col = 1)
map.scale3 <- list("sp.text", c(-77,38.85), "5km", cex = 0.9, col = 1)
map.layout <- list(map.na, map.scale1, map.scale2, map.scale3)




mypalette1 <- brewer.pal(9, "Reds")
mypalette2 <- brewer.pal(5, "Blues")
mypalette3 <- brewer.pal(6, "Greens")

# X11(width = 25, height = 25)

#price_LSD : Price variation varies more in northern, southern, and central regions
spplot(gwssbx_latlong$SDF,   #or gwssbx_latlong
       "price_LSD",       #LSD = Least Significant Difference
       key.space = "right",
       col.regions = mypalette1,
       cuts = 3,
       main = "GW standard deviations for price (basic)",
       sp.layout = map.layout)

spplot(gwssbx_eastnorth$SDF,   #or gwssbx_latlong
       "price_LSD",       #LSD = Least Significant Difference
       key.space = "right",
       col.regions = mypalette1,
       cuts = 3,
       main = "GW standard deviations for price (basic)",
       sp.layout = map.layout)



#price_IQR
spplot(gwssbx_latlong$SDF,
       "price_IQR",
       key.space = "right",
       col.regions = mypalette2,
       cuts = 7,
       main = "GW standard deviations for price (robust)",
       sp.layout = map.layout)

#price & Weekly_price
spplot(gwssbx_latlong$SDF,
       "Spearman_rho_price.weekly_price",   #Corr_price.weekly_price
       key.space = "right",
       col.regions = mypalette3,
       at = c(-1, -0.8, -0.6, -0.4, -0.2, 0),
       main = "GW correlations: price and weekly_price (box-car kernel)",
       sp.layout = map.layout)


#####PCA
data.scaled <- scale(as.matrix(df_latlong@data))
df_latlong@data
