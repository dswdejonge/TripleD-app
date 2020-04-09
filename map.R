create_map <- function(my_subset, all_stations, 
                       my_pal, map_type, show_incomplete_data, html_legend,
                       show_bathy, my_contours_df, 
                       show_roi, regions_of_interest){
  # Fixed parameters
  station_radius <- 7
  sample_radius <- 20
  
  ############
  # Map title
  ############
  if(map_type == "pa"){
    map_title <- HTML("<p>Presence - Absence</p>")
  }else if(map_type == "dens"){
    map_title <- HTML("<p>Density (count m<sup>-2</sup>)</p>")  
  }else if(map_type == "biom"){
    map_title <- HTML("<p>Biomass (g AFDW m<sup>-2</sup>)</p>") 
  }
  
  #############
  # Add layers
  #############
  # Base layer
  my_map <- leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    addScaleBar(position = "topright")
  
  # Bathymetry
  if(show_bathy){
    # Bathymetry palette
    bathy_pal <- colorNumeric(
      palette = RColorBrewer::brewer.pal(9, "Blues"),
      domain = c(
        min(my_contours_df$depth, na.rm = T), # Minimum range
        max(my_contours_df$depth, na.rm = T)), # Maximum range
      reverse = F # Use the scale in reverse (dark blue is deeper)
    ) 
    # Add to map
    my_map <- my_map %>%
      addPolylines(
        data = my_contours_df, # SpatialLinesDataFrame object from sp package
        color = bathy_pal(my_contours_df$depth),
        weight = 2,
        opacity = 0.8,
        label = paste0(my_contours_df$depth, " m."))
  }
  
  # Regions of interest
  if(show_roi){
    my_map <- my_map %>%
      addPolygons(
        data = regions_of_interest,
        popup = htmltools::htmlEscape(paste0(
          regions_of_interest$name, 
          " (", regions_of_interest$type,"). ",
          "Area: ", regions_of_interest$area_ha, " ha."))
      )
  }
  
  ###########
  # Add data
  ###########
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
    
    # Add legends
    my_map <- my_map %>%
      addLegend("bottomright",
                pal = my_pal,
                values = my_subset$Value,
                title = map_title,
                opacity = 1) %>%
      addControl(html = html_legend, position = "bottomleft")
    
    # Add complete markers
    complete_points <- my_subset[!my_subset$do_not_include,]
    my_map <- my_map %>%
      addCircleMarkers(
        lng = complete_points$Lon_DD, 
        lat = complete_points$Lat_DD,
        color = my_pal(complete_points$Value),
        radius = sample_radius,
        fillOpacity = 1.0,
        stroke = FALSE,
        popup = htmltools::htmlEscape(paste0("Value: ",complete_points$Value))
      )
    # Add incomplete data points
    if(show_incomplete_data){
      incomplete_points <- my_subset[my_subset$do_not_include,]
      if(nrow(incomplete_points) > 0){
        my_map <- my_map %>%
          addCircleMarkers(
            lng = incomplete_points$Lon_DD,
            lat = incomplete_points$Lat_DD,
            radius = sample_radius,
            fillColor = my_pal(incomplete_points$Value),
            fillOpacity = 0.3,
            stroke = TRUE,
            color = my_pal(incomplete_points$Value),
            opacity = 1.0,
            popup = htmltools::htmlEscape(paste0("Value: ",incomplete_points$Value))
          )
      } 
    }
  }
  # Add station markers
  my_map <- my_map %>% 
    addCircleMarkers(
      lng = all_stations$Lon_DD,
      lat = all_stations$Lat_DD,
      radius = station_radius,
      fillOpacity = 1.0,
      color = "black",
      stroke = FALSE,
      popup = htmltools::htmlEscape(
        paste0("StationID: ",all_stations$StationID,
               " Station name: ",all_stations$Station_name,
               " Date: ", all_stations$Date))
    )
  
  return(my_map)
}
