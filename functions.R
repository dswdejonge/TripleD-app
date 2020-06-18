get_palette <- function(df){
  if(all(is.na(df$Value))){
    min_range <- 0
    max_range <- 1
  }else{
    min_range <- min(df$Value, na.rm = T)
    max_range <- max(df$Value, na.rm = T)
  }
  my_pal <- colorNumeric(
    palette = RColorBrewer::brewer.pal(11, "Spectral"), # Spectral palette
    domain = c(
      min_range, # Minimum range
      max_range), # Maximum range
    reverse = T # Use the scale in reverse (blue is low, red is high)
  )
  return(my_pal)
}