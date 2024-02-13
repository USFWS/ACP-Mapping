#read and chop up transects from point files
# code originally from https://examples.distancesampling.org/dsm-data-formatting/dsm-data-formatting.html
trans2seg <- function(
  transects=NA, 
  transectID = "Transect", #label for the Transect id, often "ORIGID" in shapefiles
  segLength=1000, 
  utm=6332, #'+proj=utm +zone=6 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  proj=4326, 
  year=NA,
  area=NA){
  
  library(sf)
  library(dplyr)
  library(lwgeom)
  
  
  
  #transform to UTMs
  transects <- st_transform(transects, st_crs(utm)) %>%
    rename(ORIGID = !!transectID )

  # do the segmenting
  segs <- st_segmentize(transects, dfMaxLength=units::set_units(segLength, "metres"))
  # transform back to lat/long
  segs <- st_transform(segs, proj)
  transects <- st_transform(transects, proj)
  
  #load function from https://examples.distancesampling.org/dsm-data-formatting/dsm-data-formatting.html
  # originally from https://dieghernan.github.io/201905_Cast-to-subsegments/
  stdh_cast_substring <- function(x, to = "MULTILINESTRING") {
    ggg <- st_geometry(x)
    
    if (!unique(st_geometry_type(ggg)) %in% c("POLYGON", "LINESTRING")) {
      stop("Input should be  LINESTRING or POLYGON")
    }
    for (k in 1:length(st_geometry(ggg))) {
      sub <- ggg[k]
      geom <- lapply(
        1:(length(st_coordinates(sub)[, 1]) - 1),
        function(i)
          rbind(
            as.numeric(st_coordinates(sub)[i, 1:2]),
            as.numeric(st_coordinates(sub)[i + 1, 1:2])
          )
      ) %>%
        st_multilinestring() %>%
        st_sfc()
      
      if (k == 1) {
        endgeom <- geom
      }
      else {
        endgeom <- rbind(endgeom, geom)
      }
    }
    endgeom <- endgeom %>% st_sfc(crs = st_crs(x))
    if (class(x)[1] == "sf") {
      endgeom <- st_set_geometry(x, endgeom)
    }
    if (to == "LINESTRING") {
      endgeom <- endgeom %>% st_cast("LINESTRING")
    }
    return(endgeom)
  }
  # apply the function
  segs <- stdh_cast_substring(segs, to="LINESTRING")
  # set segs length
  segs$Effort <- st_length(segs)
  # create a dummy column that we can fill in as we go
  segs$Sample.Label <- NA
  
  # loop over the transect IDs
  for(this_transect in unique(segs$ORIGID)){
    # how many segments in this transect?
    n_segs <- nrow(subset(segs, ORIGID==this_transect))
    # generate the n_segs labels that we need
    segs$Sample.Label[segs$ORIGID==this_transect] <- paste(year, area, this_transect,
                                                           1:n_segs, sep="-")
  }
  # save the line version of segs for plotting later
  segs_lines <- segs
  
  # project segs
  segs <- st_transform(segs, st_crs(utm))
  
  # find centroids
  segs <- st_centroid(segs)
  
  # project back to lat/long and rename to original transect ID
  segs <- st_transform(segs, proj) %>%
    rename( !!transectID := ORIGID)
  
  return(segs)
}

# function that returns a dataframe from a sf object
segs_df <- function(segs){
  cbind(as.data.frame(sf::st_drop_geometry(segs)),
                 sf::st_coordinates(segs))
}
