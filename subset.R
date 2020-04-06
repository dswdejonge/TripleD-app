# Subset data based on input
get_subset <- function(taxonomic_level, taxon, map_type, database, dates_input){
  # Filter on taxonomic level
  if(taxonomic_level == "all_data"){
    my_subset <- database
  }else if(taxonomic_level == "species"){
    my_subset <- database %>%
      dplyr::filter(rank == "Species") %>%
      dplyr::filter(valid_name == taxon)
  }else{
    my_subset <- database %>%
      dplyr::filter(!!sym(taxonomic_level) == taxon)
  }
  
  # Get data within date range
  my_subset <- my_subset %>%
    dplyr::filter(Date >= dates_input[1] & Date <= dates_input[2])
  
  # Get data based on map_type
  if(map_type == "pa"){
    my_subset <- my_subset %>%
      dplyr::select(StationID, Lat_DD, Lon_DD) %>%
      dplyr::distinct()
    if(nrow(my_subset) > 0){
      my_subset$Value <- 1
    }
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