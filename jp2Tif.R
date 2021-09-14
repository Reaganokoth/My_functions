jp2Tif <- function(input_path, output_path){
  library(stringr)
  library(raster)
  library(rgdal)
  in.files <- list.files(paste0(input_path),"jp2$")
  out.files <- gsub(".jp2", ".tif", in.files)
  for(i in 1:length(in.files)){
    writeRaster(raster(readGDAL(in.files[i])),
                file.path(output_path, out.files[i]),
                overwrite = TRUE)
  }
  
  converted_files <- list.files(paste0(output_path))
  
  files.tiff_toresample <- c(converted_files[1],
                             converted_files[5],
                             converted_files[6], 
                             converted_files[7],
                             converted_files[13], 
                             converted_files[10], 
                             converted_files[11], 
                             converted_files[12])
  
  files.tiff_notresampled <-c(converted_files[2],
                              converted_files[3],
                              converted_files[4], 
                              converted_files[8])
  
  for (i in 1:length(files.tiff_toresample)){
    resampleband <- raster(files.tiff [2])
    if (i == 1){
      current_tiff <- raster(files.tiff_toresample[i])
      resample_current <- resample(toresample, resampleband, method="bilinear")
      my_raster <- stack(resample_current)
      
    } else {
      # ... and fill it with each additional run with another layer
      current_tiff <- raster(files.tiff_toresample[i])
      #current_ascii <- read.asciigrid(filenames[i])
      current_resample <- resample(current_tiff, resampleband, method="bilinear")
      my_raster <- stack(my_raster, current_resample)
      # Delete all variables except for the raster stack "my_raster"
      rm(i, current_resample, current_tiff)
    }
    
  }
  
  raster_notresampled <- stack(files.tiff_notresampled)
  final_raster <- stack(files.tiff_notresampled, my_raster)
  writeRaster(final_raster,
              file.path(output_path, "Raster_Image.tif"),
              overwrite = TRUE)
  
}

jp2Tif(input_path = paste0(getwd()), output_path = "/Users/rragankonywa/Documents/S2_sample_DATA/test/")








