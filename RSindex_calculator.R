library(raster)
library(RStoolbox)

#modify the landsat section to work as the sentinel
#find images both landsat and sentinel with complete band information and test the other indexes.
index_calc <- function(sensor, raster_stack, Index, Outdir){
  if(tolower(sensor) == "sentinel" & missing(Index)){
  NIR <- raster_stack[[8]]
  RED <- raster_stack[[4]]
  BLUE <- raster_stack[[2]]
  SWIR <- raster_stack[[11]]
  NDVI <- (NIR-RED)/(NIR + RED)
  RED_EDGE1 <- raster_stack[[5]]
  G  <- 2.5
  C1 <- 6
  C2 <- 7.5
  L <- 1
  EVI <- G * ((NIR - RED) / (NIR + C1 * RED-C2 * BLUE + L))
  NDBI <- (SWIR-NIR) / (SWIR + NIR)
  NDWI <- (NIR-SWIR) / (NIR + SWIR)
  NDCI <- (RED_EDGE1-RED)/(RED_EDGE1+RED)
  rasterstack <- stack(NDVI, EVI,NDBI,NDWI, NDCI)
  layernames <- c("NDVI", "EVI", "NDBI","NDWI", "NDCI")
  names(rasterstack) <- layernames
  writeRaster(rasterstack,
              file.path(Outdir, "indices.tif"),
              overwrite = TRUE)
  #writeRaster(rasterstack, filemame = paste0(Outdir, "Indices.tif"), format = "GTiff")
  return(plot(rasterstack))
  } else {
    if (tolower(sensor) == "sentinel") {
      NIR <- raster_stack[[8]]
      RED <- raster_stack[[4]]
      BLUE <- raster_stack[[2]]
      SWIR <- raster_stack[[11]]
      RED_EDGE1 <- raster_stack[[5]]
      NDVI <- (NIR-RED)/(NIR + RED)
      #raster <- noquote(toupper(Index))
      G  <- 2.5
      C1 <- 6
      C2 <- 7.5
      L <- 1
      NDBI <- (SWIR-NIR) / (SWIR + NIR)
      NDWI <- (NIR-SWIR) / (NIR + SWIR)
      NDCI <- (RED_EDGE1-RED)/(RED_EDGE1+RED)
      EVI <- G * ((NIR - RED) / (NIR + C1 * RED-C2 * BLUE + L))
      layernames <- c("NDVI", "EVI", "NDBI", "NDWI", "NDCI")
      rasterstack <- stack(NDVI, EVI, NDBI, NDWI, NDCI)
      names(rasterstack) <- layernames
      return(plot(rasterstack[[toupper(Index)]]))
    }
    else {
      if(tolower(sensor) == "landsat" & missing(Index)) {
        NIR <- raster_stack[[4]]
        RED <- raster_stack[[3]]
        SWIR <- raster_stack[[5]]
        NDVI <- (NIR-RED)/(NIR + RED)
        NDBI <- (SWIR-NIR) / (SWIR + NIR)
        NDWI <- (NIR-SWIR) / (NIR + SWIR)
        rasterstack <- stack(NDVI, NDWI,NDBI)
        return(rasterstack)
        
      }
      else {
        NIR <- raster_stack[[4]]
        RED <- raster_stack[[3]]
        SWIR <- raster_stack[[5]]
        NDVI <- (NIR-RED)/(NIR + RED)
        NDBI <- (SWIR-NIR) / (SWIR + NIR)
        NDWI <- (NIR-SWIR) / (NIR + SWIR)
        return(Index)
      }
    }
  }
}


outdir <- "/Users/rragankonywa/Documents/S2_sample_DATA/lake_vitoria_crop/"
raster <- stack(paste0(outdir, "lake_victoria_crop.tif"))
unique(names(raster))
layernames <- paste0("Band_", 1:length(names(raster)))
names(raster) <- layernames

#raster <- stack("/Users/rragankonywa/Documents/S2_sample_DATA/test/Raster_Image.tif")
#outdir <- "/Users/rragankonywa/Documents/S2_sample_DATA/test/"
index_calc(sensor = "sentinel", raster_stack =raster_croped, Outdir = outdir)
index_calc(sensor = "sentinel", raster_stack = raster, Index = "NDCI")

ggRGB(raster_croped,4,3,2, maxpixels = 10000)
