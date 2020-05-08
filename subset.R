subset_environment <- function(dataset, dates_input, depth_range, cruise_id){
  my_subset <- dataset %>%
    # Get data within date range
    dplyr::filter(Date >= dates_input[1] & Date <= dates_input[2]) %>%
    # Get data within depth range
    dplyr::filter(Water_depth_m >= depth_range[1] & Water_depth_m <= depth_range[2])
  
  # Filter based on CruiseID
  if(cruise_id != "all"){
    my_subset <- dplyr::filter(my_subset, CruiseID == cruise_id)
  }
  return(my_subset)
}

# Subset data based on input
subset_taxon <- function(dataset, taxonomic_level, taxon){
  # Filter on taxonomic level
  if(taxonomic_level == "all_data"){
    my_subset <- dataset
  }else if(taxonomic_level == "species"){
    my_subset <- dataset %>%
      dplyr::filter(rank == "Species") %>%
      dplyr::filter(valid_name == taxon)
  }else{
    my_subset <- dataset %>%
      dplyr::filter(!!sym(taxonomic_level) == taxon)
  }
  return(my_subset)
}

# Get data based on map_type (do last, because info lost with select)
subset_data_type <- function(dataset, map_type){
  if(nrow(dataset) > 0){
    if(map_type == "pa"){
      my_subset <- dataset %>%
        dplyr::select(StationID, Lat_DD, Lon_DD) %>%
        dplyr::distinct()
      if(nrow(my_subset) > 0){
        my_subset$Value <- 1
      }
      my_subset$do_not_include <- FALSE
    }else if(map_type == "dens"){
      my_subset <-  dataset %>%
        # Remove tracks not suitable for quantification
        dplyr::filter(is_Quantitative == 1) %>%
        # Identify incomplete data points
        dplyr::mutate(is_incomplete = ifelse(incomplete_count, 1, 0)) %>%
        dplyr::select(StationID, Density_nr_per_m2, Lat_DD, Lon_DD, is_incomplete) %>%
        dplyr::group_by(StationID, Lat_DD, Lon_DD) %>%
        # Sum densities for this station
        dplyr::summarise(
          Value = sum(Density_nr_per_m2, na.rm = T),
          is_incomplete = sum(is_incomplete)) %>%
        dplyr::mutate(do_not_include = ifelse(is_incomplete > 0, TRUE, FALSE)) %>%
        dplyr::select(-is_incomplete)
    }else if(map_type == "biom"){
      my_subset <- dataset %>%
        # Remove tracks not suitable for quantification
        dplyr::filter(is_Quantitative == 1) %>%
        # Identify incomplete data points
        dplyr::mutate(is_incomplete = ifelse(incomplete_biomass, 1, 0)) %>%
        dplyr::select(StationID, Biomass_g_per_m2, Lat_DD, Lon_DD, is_incomplete) %>%
        dplyr::group_by(StationID, Lat_DD, Lon_DD) %>%
        # Sum biomasses for this station
        dplyr::summarise(
          Value = sum(Biomass_g_per_m2, na.rm = T),
          is_incomplete = sum(is_incomplete)) %>%
        dplyr::mutate(do_not_include = ifelse(is_incomplete > 0, TRUE, FALSE)) %>%
        dplyr::select(-is_incomplete)
      my_subset$Value[which(my_subset$Value == 0)] <- NA
    }
  }else{
    my_subset <- dataset
  }
  return(my_subset)
}
  
