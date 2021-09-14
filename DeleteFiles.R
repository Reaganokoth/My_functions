
deleteFiles <- function(path, extension){
  files_list <- list.files(path, extension)
  sapply(tif, function(each){
    unlink(each)
  })
  return("Done")
}

