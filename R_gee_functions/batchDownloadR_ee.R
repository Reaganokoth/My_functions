batchDownloadR_ee <- function(image_collection, bands,resolution, roi, progress=NULL, folderName=NULL){
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

# Example how to run
#batchDownload_ee(image_collection = cropland_Mask,bands = "LC_Type1",resolution = 500,roi = Kenya,progress = T)