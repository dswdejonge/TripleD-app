create_map <- function(database){
  
  mymap <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(
      lng=database$Lon_DD, 
      lat=database$Lat_DD
    )
  
  return(mymap)
}
