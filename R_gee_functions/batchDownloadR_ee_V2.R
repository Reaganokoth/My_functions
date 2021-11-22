library(getPass)

batchDownloadR_ee <- function(email=getPass("Enter your gmail address"),dataset,start_date, end_date, bands,resolution, roi, progress=NULL, folderName=NULL, Islandcover=NULL, landcover_class=NULL){
  # check if rgee and getpass are installed already, if not then install them.
  list.of.packages <- c("rgee")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(rgee)
  
  
  
  
  # initialize GEE and specify your GEE email address.
  ee_Initialize(email = email, drive = TRUE)
  
  # Index EE object the R way, i.e allows user to index specific image based on the image index.
  index_ee_ic <- function(image_collection, index){
    return(ee$Image(image_collection$toList(image_collection$size())$get(index-1)))
  }
  # # check the length of a gee images the R way 
  length_ee <- function(x){
    return(x$size()$getInfo())
  }
  
  
  if(is.null(Islandcover)){
    image_collection <-  ee$ImageCollection(dataset)$
      filterDate(start_date, end_date)$
      select(bands)$
      map(function(image){
        return (image$clip(roi))
      })
  } else {
    image_collection <-  ee$ImageCollection(dataset)$
      filterDate(start_date, end_date)$
      select(bands)$
      map(function(image){
        return (image$clip(roi))
      })$map(function(image){
        return (image$eq(landcover_class))
      })
  }
  
  for(i in 1:length_ee(image_collection)){
    image <- index_ee_ic(image_collection, i)
    title_name <- image$get('system:index')$getInfo()
    
    extents = ee$Image$pixelLonLat()$updateMask(image$select(c(bands))$mask())$
      reduceRegion(ee$Reducer$minMax(), roi, resolution)
    
    bounds = ee$Geometry$Rectangle(extents$values(c("longitude_min", "latitude_min", "longitude_max", "latitude_max")))
    
    
    downConfig = list(scale = resolution, maxPixels = 1.0E13, 
                      region = bounds, 
                      fileFormat = 'GeoTIFF', 
                      driveFolder = folderName)
    
    task = ee$batch$Export$image(image,title_name, 
                                 downConfig)
    
    task$start()
    ee_monitoring()
  }
  if(progress==T){
    task$start()
    ee_monitoring()
  } else{
    task$start()
    # ee_monitoring()
  }
  
}

# sample implementation for a landcover classification dataset.

# Filter the images to obtain data from desired start and end dates
# Select the land cover type one band (represents annual Land Cover Type from
# International Geosphere-Biosphere Program (IGBP) classification)
# crop each image in the image collection to the extent of the study area (Kenya),
# Then retain only land cover class 12 which is the crop cover mask. 
# NOTE: the eq() function is single image function, that's why we use the map function to iterate throu the Imagecollection.

#NOTE: ignore the Islandcover and landcover_clas parameters if you are working with non lodcover type datasets i.e Sentinel and Landsat series.
batchDownloadR_ee(dataset = "MODIS/006/MCD12Q1",start_date = "2000-01-01", end_date = "2016-12-31", bands = "LC_Type1",resolution = 500,roi = roi,progress = T,folder="test_2", Islandcover = T, landcover_class = 12) 

# notes: if you have trouble installing rgee, check the installation instructions based on your system from
# the following links: https://csaybar.github.io/rgee-examples/
