create_map <- function(my_subset, all_stations, my_pal, map_type){
  # Map title
  if(map_type == "pa"){
    map_title <- HTML("<p>Presence - Absence</p>")
  }else if(map_type == "dens"){
    map_title <- HTML("<p>Density (count m<sup>-2</sup>)</p>")  
  }else if(map_type == "biom"){
    map_title <- HTML("<p>Biomass (g AFDW m<sup>-2</sup>)</p>") 
  }
  
  mymap <- leaflet() %>%
    #fitBounds(lng1 = lons[1], lng2 = lons[2], lat1 = lats[1], lat2 = lats[2]) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    #addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(
      lng=my_subset$Lon_DD, 
      lat=my_subset$Lat_DD,
      #color = "red",
      color = my_pal(my_subset$Value),
      radius = 15,
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
    ) %>%
    addLegend("bottomright",
              pal = my_pal,
              values = my_subset$Value,
              title = map_title,
              opacity = 1)
  
  return(mymap)
}
