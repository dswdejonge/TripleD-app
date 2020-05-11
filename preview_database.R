preview_database <- function(){
  n_stations <- 100
  n_species <- 60
  preview_database <- data.frame(
    StationID = paste0("Station", rep(1:n_stations, each = n_species)),
    valid_name = rep(paste0("sp",1:n_species),n_stations),
    File = rep("File_name", n_stations*n_species),
    rank = rep("Species", n_stations*n_species),
    phylum = paste0("phylum",sample(1:3, n_stations*n_species,replace = T)),
    class = paste0("class",sample(1:5, n_stations*n_species,replace = T)),
    order = paste0("order",sample(1:5, n_stations*n_species,replace = T)),
    family = paste0("family",sample(1:5, n_stations*n_species,replace = T)),
    genus = paste0("genus",sample(1:5, n_stations*n_species,replace = T)),
    isFuzzy = FALSE,
    incomplete_count = c(FALSE,TRUE)[sample(1:2, n_stations*n_species, prob = c(0.8,0.2),replace = T)],
    incomplete_biomass = c(FALSE,TRUE)[sample(1:2, n_stations*n_species, prob = c(0.7,0.3),replace = T)],
    #phylum = rep(rep(c("phylum1", "phylum2", "phylum"), each = n_species/3),n_stations)
    CruiseID = "CruiseID",
    Cruise_name = NA,
    Date = rep(c("2018-01-01", "2017-01-01", "2017-06-01", "2016-01-01"), each = n_stations/4*60),
    Region = NA,
    Blade_width_cm = 20,
    Blade_depth_cm = 20,
    is_Quantitative = TRUE,
    Station_objective = "Complete", Focus = NA, Excluded = NA,
    Comment = NA, Vessel = NA, Bearing = NA,
    Station_name = NA, Tow_speed_knots = NA,
    Time_start = NA, Time_stop = NA,
    Lat_DD = rep(runif(n_stations,min=53.5,max=56), each = n_species),
    Lon_DD = rep(runif(n_stations,min=0.5,max=4.5), each = n_species),
    Water_depth_m = rep(runif(n_stations,min=5,max=40), each = n_species),
    Track_length_m = 100,
    Sample_area_m2 = 20,
    Sample_volume_m3 = 0.4,
    source_Lat_DD = NA, source_Lon_DD = NA, 
    source_Water_depth_m = NA, source_Track_length_m = NA,
    Density_nr_per_m2 = rexp(n_stations*n_species),
    Density_nr_per_m3 = NA,
    Biomass_g_per_m2 = rexp(n_stations*n_species),
    Biomass_g_per_m3 = NA,
    stringsAsFactors = FALSE
  )
  asubset <- sample(1:nrow(preview_database),0.75*nrow(preview_database))
  preview_database <- preview_database[asubset,]
  return(preview_database)
}
