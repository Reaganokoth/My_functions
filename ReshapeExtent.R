ReshapeExtent <- function(files, path){
  setwd(path)
  for (i in 1:length(files)){
    if(i==1){
      my_raster <- raster(files[i])
    } else {
      new_raster <- raster(files[i])
      extent(new_raster) <- extent(my_raster)
      my_raster <- stack(my_raster, new_raster)
    }
  }
  
  return(my_raster)
}
