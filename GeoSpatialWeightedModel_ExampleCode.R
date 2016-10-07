
#Geo Spatial Weighted Models paper: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf


#Get distance matrix for airbnb price data
library("GWmodel"); library("rgdal"); library("sp"); library("RColorBrewer");
library(maps); library(ggplot2); library(mapproj); library("plotly");library("ggmap"); library("shapefiles")
library(rgdal); library(rworldmap)
library(data.table)
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
data_complete_latlon <- data_latlon[complete.cases(data_latlon),]         ######USE AS POINTS TO PLOT ON TOP OF POINTS

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
                       #boxcar relates to un-weighted moving window. bisquare relates to weighted moving window. Also try tricube, gaussian, exponential
                       kernel = "boxcar",
                       adaptive = TRUE,
                       bw = 0.15 * nrow(df),    #roughly 15% of df data
                       quantile = TRUE)    #median, IQR, and quantile imbalance calculated


#LatLong
map.na = list("SpatialPolygonsRescale", layout.north.arrow(),offset = c(-77.1, 38.95),scale = 1000,col = 1)
map.scale1 <- list("SpatialPolygonsRescale", layout.north.arrow(),offset = c(-77.04, 38.97),scale = 2000,col = 1,fill = c("transparent", "blue"))
map.scale2 <- list("sp.text",c(-77, 38.85),"0",cex = 0.4,col = 1)
map.scale3 <- list("sp.text", c(-77,38.85), "5km", cex = 0.9, col = 1)
map.layout <- list(map.na, map.scale1, map.scale2, map.scale3)


mypalette1 <- brewer.pal(9, "Reds")
mypalette2 <- brewer.pal(5, "Blues")
mypalette3 <- brewer.pal(6, "Greens")

# X11(width = 25, height = 25)

#price_LSD : Price variation varies more in northern, southern, and central regions
# ? https://rpubs.com/nickbearman/r-google-map-making
df_SDF <-setDT(as.data.frame(gwssbx_latlong$SDF), keep.rownames = TRUE)[]  #USE TO PLOT
df_latlong #Google Maps use sp data
plot(df_latlong, pch = ".", col = "darkred")   #USES sp data  #WORKS

# map2 <- qmap("Washington", zoom = 10, maptype = 'hybrid')
# map2 + geom_point(data = df_SDF, aes(x = "Longitude", y = "Latitude"),
                  color = "red",size = 3, alpha = 0.4)

qmplot(df_SDF$Longitude, df_SDF$Latitude, data = df_SDF, colour = I('red'), size = I(3), darken = 0.3)

mapPoints <- ggmap(map);mapPoints
gwssbx_latlong$SDF

map2 + spplot(gwssbx_latlong$SDF,   #or gwssbx_latlong
       "Corr_price.weekly_price",       #LSD = Least Significant Difference
       key.space = "right",
       col.regions = mypalette1,
       cuts = 3,
       main = "GW standard deviations for price (basic)",
       sp.layout = map.layout)
points(data_complete_latlon$Longitude, data_complete_latlon$Latitude, col = "red", cex = 0.6)



list.files('~/C:/Users/jberthet001/Desktop/AiA/Airbnb/AirbnbApp', pattern='\\.shp$')
file.exists('C:/Users/jberthet001/Desktop/AiA/Airbnb/AirbnbApp/district_of_columbia_administrative.shp')
washDC <- readOGR(dsn=".",layer="district_of_columbia_administrative.shp")
washDC <- readOGR(dsn=".",layer="cb_2015_us_state_5m.shp")

s <- shapefile(dsn=path.expand("C:/Users/jberthet001/Desktop/AiA/Airbnb/AirbnbApp/district_of_columbia_administrative"), layer = "district_of_columbia_administrative")


getwd()

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



#PLot US (No luck w/ DC)
library("maptools", lib.loc="~/R/win-library/3.2")
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
states = readShapePoly("C:/Users/jberthet001/Desktop/AiA/Airbnb/cb_2015_us_state_5m.shp", proj4string = crswgs84, verbose = TRUE)
par(mfrow=c(1,1))
plot(states)

# https://pakillo.github.io/R-GIS-tutorial/
# http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/
#Check if coordinates exist in map file
library(rgeos)
p <- SpatialPoints(list(data_complete_latlon$Longitude, data_complete_latlon$Latitude), proj4string = crswgs84)
gContains(fdg, pt)

#####PCA
#GW PCA can assess how effective data dimensionality varies spatially and how the original variables influence each spatially-varying component

#Main challenges: finding scale at which each localised PCA should operate and visualizing & interpreting output.
#pca basic
data.scaled <- scale(as.matrix(df_latlong@data))
pca.basic <- princomp(data.scaled, cor = F)
(pca.basic$sdev^2 / sum(pca.basic$sdev ^2)) * 100
pca.basic$loadings

#pca robust: reduces anomalous observations as outputs. Outliers effects are reduced cuz each local covariance matrix is estimated using robust minimum covariance determinant (MCD) estimator, which searches for a subset of h data points that has the smallest determinant for their basic sample covariance matrix. h estimator set to default values: h = 0.75n
R.Cov <- covMcd(data.scaled, cor=F, alpha = 0.75)
pca.robust <- princomp(data.scaled, covmat = R.Cov, cor = F)
pca.robust$sdev^2 / sum(pca.robust$sdev ^2)
pca.robust$loadings

#Kernel bandwidths for GW PCA found automatically using LOOCV. A LOOCV score is computed for all possible bandwidths and an optimal bandwidth is the smallest CV score found. Good start for a kernel (k) values is arbitrary. But start with bi-square kernel.
Coords <- as.matrix(cbind(data_complete_latlon$Longitude,
                          data_complete_latlon$Latitude))
Data.scaled.spdf <- SpatialPointsDataFrame(Coords,
                                           as.data.frame(data.scaled))
bw.gwpca.basic <-bw.gwpca(Data.scaled.spdf,
                          vars = colnames(Data.scaled.spdf@data),
                          k = 3,
                          robust = FALSE,
                          adaptive = TRUE)
bw.gwpca.robust <- bw.gwpca(Data.scaled.spdf,
                            vars = colnames(Data.scaled.spdf@data),
                            k = 3,   #Number of Principal components to use
                            robust = TRUE,
                            adaptive = TRUE)
#COMPARE
bw.gwpca.basic
bw.gwpca.robust
#CONCLUSION: IF values similar, then similar optimal bandwidths will be used to calibrate the respective basic and robust GW PCA Fits. Very different values shows that optimal bandwidths are very different.

#Now, use gwpca() to get the GW PCA for each gwpca
gwpca.basic <- gwpca(Data.scaled.spdf,
                     vars = colnames(Data.scaled.spdf@data),
                     bw = bw.gwpca.basic,
                     k = 3,
                     robust = FALSE,
                     adaptive = TRUE)

gwpca.robust <- gwpca(Data.scaled.spdf,
                     vars = colnames(Data.scaled.spdf@data),
                     bw = bw.gwpca.robust,
                     k = 3,
                     robust = TRUE,
                     adaptive = TRUE)

#PLOT
#prop.var() finds PTV data, and adds it to long/lat attitude
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components])/
            rowSums(gwpca.obj$var))*100)
  }

var.gwpca.basic <- ((rowSums(gwpca.basic$var[, 1:3])/rowSums(gwpca.basic$var))*100)
var.gwpca.robust <- ((rowSums(gwpca.robust$var[, 1:3])/rowSums(gwpca.robust$var))*100)

gwssbx_latlong$var.gwpca.basic <- var.gwpca.basic
gwssbx_latlong$var.gwpca.robust <- var.gwpca.robust


#data must be 'sp' class
mypalette.4 <-brewer.pal(3, "YlGnBu")
X11(width = 10,height = 12)
spplot(gwssbx_latlong,
       "var.gwpca.basic", key.space = "right",
          col.regions = mypalette.4, cuts = 3,
          main = "PTV for local components 1 to 3 (basic GW PCA)",
          sp.layout = map.layout)

X11(width = 10,height = 12)
spplot(Dub.voter, "var.gwpca.robust", key.space = "right",
          col.regions = mypalette.4, cuts = 3,
          main = "PTV for local components 1 to 3 (robust GW PCA)",
          sp.layout = map.layout)
# Figure 5 presents the local PTV
