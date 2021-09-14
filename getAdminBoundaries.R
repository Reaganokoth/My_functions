#install.packages("gbif")
#devtools::install_github("81747/rgbif")

library(raster)
library(sf)
library(sp)

library(rgbif)
getAdminBoundaries <- function(path,country=NULL, region=NULL, dissolve=NULL){
   library(rgeos)
   library(rgdal)
   setwd(path)
   download.file(url = "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip",destfile ="ne_10m_admin_0_countries.zip" )
   unzip(zipfile = "ne_10m_admin_0_countries.zip")
   shp <- readOGR("ne_10m_admin_0_countries.shp")
   if(is.null(country)){
      if(dissolve==TRUE){
         continent <- shp[shp$REGION_UN == region, ]
         return(gUnaryUnion(continent))
      }
      continent <- shp[shp$REGION_UN == region, ]
      return(continent)
   } else{
      country <- shp[shp$SOVEREIGNT == country, ]
      return(country)
   }
}

Africa <- getAdminBoundaries(path = "/Users/rragankonywa/Desktop/data",region = "Africa", dissolve = F)
plot(kenya)
library(rgeos)
plot(gUnaryUnion(kenya))





kenya <- getAdminBoundaries(path = "/Users/rragankonywa/Desktop/data", country = "Kenya", dissolve = T)

crs(kenya)

setwd("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/Time_Series_ET")

writeOGR(Germany, '.', 'Germany_Mask', 'ESRI Shapefile')




point_data <- as.data.frame(read.csv2("/Users/rragankonywa/Downloads/EAGLE_BEECHDECLINE_2021.csv", sep = ",")[1:25, ])
names(point_data)
cols.num <- c("Latitude...." ,"Longitude....")




point_data[cols.num] <- sapply(point_data[cols.num],as.double)
sapply(point_data, class)
str(point_data)
plot(Germany)


coordinates(point_data) <- point_data[, 7:8]
str(point_data)

crs(point_data) <- crs(Germany)

points_projected_tranformed <- spTransform(point_data, CRS("+init=epsg:4839"))

crs(points_projected_tranformed)
#spTransform(point_data, CRS("+init=epsg:4839"))
Germany_proj <- spTransform(Germany, CRS("+init=epsg:4839"))
crs(Germany_proj)
# make the data a spatial point dataframe


#spatial_point_dataframe <- SpatialMultiPointsDataFrame(point_data[7:8],point_data, as.character(crs(Germany)))

setwd("/Users/rragankonywa/OneDrive/UniWurzburg/EAGLES/Semester2/Time_Series_ET")

writeOGR(points_projected_tranformed, '.', 'ROI_Points3', 'ESRI Shapefile')

writeOGR(Germany_proj, '.', 'Germany_Mask2', 'ESRI Shapefile')

plot(point_data)



points(point_data)
