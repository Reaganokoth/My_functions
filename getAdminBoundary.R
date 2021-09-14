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
# Examples
# get continent
# Africa <- getAdminBoundaries(path = "/Users/rragankonywa/Desktop/data",region = "Africa", dissolve = F)

# get country
# kenya <- getAdminBoundaries(path = "/Users/rragankonywa/Desktop/data", country = "Kenya", dissolve = T)