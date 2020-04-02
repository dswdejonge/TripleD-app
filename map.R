create_map <- function(database, all_stations){
  
  mymap <- leaflet() %>%
    #fitBounds(lng1 = lons[1], lng2 = lons[2], lat1 = lats[1], lat2 = lats[2]) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(
      lng=database$Lon_DD, 
      lat=database$Lat_DD,
      radius = 10,
      fillOpacity = 0.2,
      stroke = FALSE
    ) %>%
    addCircleMarkers(
      lng = all_stations$Lon_DD,
      lat = all_stations$Lat_DD,
      radius = 3,
      fillOpacity = 1.0,
      color = "black",
      stroke = FALSE
    )
  
  return(mymap)
}
