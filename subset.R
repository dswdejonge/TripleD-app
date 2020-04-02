# Subset data based on input
subset_db <- function(taxonomic_level, database){
  if(taxonomic_level == "all"){
    my_subset <- database
  }else{
    my_subset <- database %>%
      filter(phylum == taxonomic_level)
  }
  return(my_subset)
}
