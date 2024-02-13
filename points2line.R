#make a linestring for each transect
points2line <- function(x, Year=unique(x$Year), Transect=unique(x$Transect), crs=4326){
  #this function accepts sf object of points and returns an sf linestring
  # sf object define ONE linestring, not more than one!
  #accepted sf object should have an attribute for named Year and Transect, 
  # if not, supply as a parameter value
  linestring <- x %>% cbind(st_coordinates(.)) %>%
    as.data.frame() %>% 
    select(-geometry) %>%
    arrange(X, Y) %>% 
    select(X, Y) %>%
    as.matrix() %>%
    st_linestring() %>%
    st_sfc(crs=4326) %>%
    st_sf(geometry=.) %>%
    mutate(Year = Year, Transect = Transect)
  return(linestring)
}
#add a version of function that also uses Time to sort points
points2line2 <- function(x, Year=unique(x$Year), Transect=unique(x$Transect), crs=4326){
  #this function accepts sf object of points and returns an sf linestring
  # sf object define ONE linestring, not more than one!
  #accepted sf object should have an attribute for named Year and Transect, 
  # if not, supply as a parameter value
  linestring <- x %>% cbind(st_coordinates(.)) %>%
    as.data.frame() %>% 
    select(-geometry) %>%
    arrange(Time, X, Y) %>% #added Time on 20240102 as test for non-E-W transects
    select(X, Y) %>%
    as.matrix() %>%
    st_linestring() %>%
    st_sfc(crs=4326) %>%
    st_sf(geometry=.) %>%
    mutate(Year = Year, Transect = Transect)
  return(linestring)
}
