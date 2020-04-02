create_map <- function(my_subset, all_stations, pal){
  
  mymap <- leaflet() %>%
    #fitBounds(lng1 = lons[1], lng2 = lons[2], lat1 = lats[1], lat2 = lats[2]) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(
      lng=my_subset$Lon_DD, 
      lat=my_subset$Lat_DD,
      color = "red",
      #color = ~pal(value),
      radius = 20,
      fillOpacity = 0.5,
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
