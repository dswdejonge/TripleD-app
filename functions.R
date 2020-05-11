get_palette <- function(df){
  my_pal <- colorNumeric(
    palette = RColorBrewer::brewer.pal(11, "Spectral"), # Spectral palette
    domain = c(
      min(df$Value, na.rm = T), # Minimum range
      max(df$Value, na.rm = T)), # Maximum range
    reverse = T # Use the scale in reverse (blue is low, red is high)
  )
  return(my_pal)
}