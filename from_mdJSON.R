#read a mdEditor json file extract and display the json formatted metadata content

from_mdJSON <- function(file=NA, which = 1, what = "json", ...){
  library(rjson)
  myjson <- fromJSON(file = file, ...)
  myjson <- myjson$data[[which]][["attributes"]][[what]]
  if( what == "json") { myjson <- fromJSON(myjson)} 
  return(myjson)
}

#test
# x <- from_mdJSON(file = "mdeditor-20240307-160377.json")
#works

replace_mdJSON <- function(x=NULL, y=NULL, what = NULL, metadata = TRUE, resource = TRUE){
  library(rjson)
  if(metadata == TRUE){
    if(resource == TRUE){
      if( is.null(y[["metadata"]][["resourceInfo"]][[what]]) ) 
        stop(paste0("Object '", what, "' not found!"))
      x[["metadata"]][["resourceInfo"]][[what]] <- y[["metadata"]][["resourceInfo"]][[what]]
    }else{
      if( is.null(y[["metadata"]][["metadataInfo"]][[what]]) ) 
        stop(paste0("Object '", what, "' not found!"))
      x[["metadata"]][["metadataInfo"]][[what]] <- y[["metadata"]][["metadataInfo"]][[what]]
    }
  }else{
    if( is.null(y[["schema"]]) ) 
      stop(paste0("Object 'schema' not found!"))
    x[["schema"]] <-  y[["schema"]]
  }
  return(x)
}
#test
# xx <- x
# xx[["metadata"]][["resourceInfo"]][["taxonomy"]] <- list("test" = "worked")
# y <- replace_mdJSON(x = x, y = xx, what = "taxonomy")
# y <- replace_mdJSON(x = x, y = xx, what = "taxonomyhahahah")

#Example: load two mdEditor files and compare attributes, here taxonomy:
# x <- from_mdJSON(file = "mdeditor-20240307-160377.json")
# y <- from_mdJSON(file = "ACP_mdeditor-20240307-150309.json", which = 5)
# x <- unlist(x$metadata$resourceInfo$taxonomy)
# y <- unlist(y$metadata$resourceInfo$taxonomy)
# length(x) == length(y)
# sum(x == y) == length(x)

