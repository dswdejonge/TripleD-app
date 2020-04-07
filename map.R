create_map <- function(my_subset, all_stations, my_pal, map_type, show_bathy){
  # Map title
  if(map_type == "pa"){
    map_title <- HTML("<p>Presence - Absence</p>")
  }else if(map_type == "dens"){
    map_title <- HTML("<p>Density (count m<sup>-2</sup>)</p>")  
  }else if(map_type == "biom"){
    map_title <- HTML("<p>Biomass (g AFDW m<sup>-2</sup>)</p>") 
  }
  
  # Bathymetry palette
  if(show_bathy){
    bathy_pal <- colorNumeric(
      palette = RColorBrewer::brewer.pal(9, "Blues"),
      domain = c(
        min(my_contours_df$depth, na.rm = T), # Minimum range
        max(my_contours_df$depth, na.rm = T)), # Maximum range
      reverse = F # Use the scale in reverse (dark blue is deeper)
    ) 
  }
  
  my_map <- leaflet() %>%
    # Esri ocean base map
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addScaleBar(position = "bottomleft")
  
  # Add extra bathymetry
  if(show_bathy){
    my_map <- my_map %>%
      addPolylines(
        data = my_contours_df, # SpatialLinesDataFrame object from sp package
        color = bathy_pal(my_contours_df$depth),
        weight = 2,
        opacity = 0.8,
        label = paste0(my_contours_df$depth, " m."))
  }
    
  # Add data markers and legend if there is data
  if(nrow(my_subset) > 0){
    # Palette
    my_pal <- colorNumeric(
      palette = RColorBrewer::brewer.pal(11, "Spectral"), # Spectral palette
      domain = c(
        min(my_subset$Value, na.rm = T), # Minimum range
        max(my_subset$Value, na.rm = T)), # Maximum range
      reverse = T # Use the scale in reverse (blue is low, red is high)
    )
    # Add markers
    my_map <- my_map %>%
      addCircleMarkers(
        lng = my_subset$Lon_DD, 
        lat = my_subset$Lat_DD,
        color = my_pal(my_subset$Value),
        radius = 15,
        fillOpacity = 0.5,
        stroke = FALSE
      ) %>%
      addLegend("bottomright",
                pal = my_pal,
                values = my_subset$Value,
                title = map_title,
                opacity = 1)
  }
  # Add station markers
  my_map <- my_map %>% 
    addCircleMarkers(
      lng = all_stations$Lon_DD,
      lat = all_stations$Lat_DD,
      radius = 3,
      fillOpacity = 1.0,
      color = "black",
      stroke = FALSE,
      popup = htmltools::htmlEscape(
        paste0("StationID: ",all_stations$StationID,
               "\n Date: ",  all_stations$Date))
    )
  
  return(my_map)
}
