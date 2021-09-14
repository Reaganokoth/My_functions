index_ee_ic <- function(image_collection, index){
  return(ee$Image(image_collection$toList(image_collection$size())$get(index-1)))
}
