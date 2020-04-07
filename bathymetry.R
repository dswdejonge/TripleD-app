library(contoureR)
library(sp)
# load bathymetry

# Define depths
my_depths <- seq(from = -10, to = -200, by = -20)

# Get contour lines
cont <- getContourLines(bathymetry, levels = my_depths)

# Create list: each element one depth contour
# i.e. can be multiple contour groups with same depth
# depth_10{
#     group1{
#       df(x, y)
#      },
#     group2{
#       df(x, y)
#      }
# } etc.
my_list <- list()
for(i in 1:length(my_depths)){
  my_list[[i]] <- cont %>%
    dplyr::filter(z == my_depths[i]) %>%
    dplyr::group_by(Group) %>%
    dplyr::select(Group, x, y) %>%
    dplyr::group_split(keep = F)
  my_list[[i]] <- lapply(my_list[[i]], Line)
  my_list[[i]] <- Lines(my_list[[i]], paste0("depth_",abs(my_depths[i])))
}

# Create 'SpatialLines' file
my_contours <- SpatialLines(my_list)

# Create 'SpatialLinesDataFrame' object
df <- data.frame(
  depth = abs(my_depths), 
  row.names = paste0("depth_",abs(my_depths)) # the unique identifiers for each 'Lines' object
)
my_contours_df <- SpatialLinesDataFrame(my_contours, df)

# Save
save(my_contours_df, file = "data/contours.rda")


#ggplot() +
#geom_point(data = bathymetry, mapping = aes(x = x, y = y, color = z)) +
#geom_path(data = cont, mapping = aes(x = x, y = y, group = Group, color = z))