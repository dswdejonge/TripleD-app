# Subset data based on input
get_subset <- function(taxonomic_level, map_type, database){
  # Filter on taxonomic level
  if(taxonomic_level == "all"){
    my_subset <- database
  }else{
    my_subset <- database %>%
      dplyr::filter(phylum == taxonomic_level)
  }
  
  # Get data based on map_type
  if(map_type == "pa"){
    my_subset <- my_subset %>%
      dplyr::select(StationID, Lat_DD, Lon_DD) %>%
      dplyr::distinct()
    my_subset$Value <- 1
  }else if(map_type == "dens"){
    my_subset <-  my_subset %>%
      dplyr::select(StationID, Density_nr_per_m2, Lat_DD, Lon_DD) %>%
      dplyr::group_by(StationID, Lat_DD, Lon_DD) %>%
      dplyr::summarise(Value = sum(Density_nr_per_m2))
  }else if(map_type == "biom"){
    my_subset <- my_subset %>%
      dplyr::select(StationID, Biomass_g_per_m2, Lat_DD, Lon_DD) %>%
      dplyr::group_by(StationID, Lat_DD, Lon_DD) %>%
      dplyr::summarise(Value = sum(Biomass_g_per_m2))
  }
  
  return(my_subset)
}
