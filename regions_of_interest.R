library(sp)

# Raw data (Rob Witbaard)
NCP <- data.frame(
  x = c( 3.21,  2.50,  3.18,  3.18,  2.97,  2.77,  3.37,  5.00,  5.00,  6.00,  6.42),
  y = c(51.34, 51.83, 52.62, 52.83, 53.63, 54.38, 55.77, 55.00, 54.62, 54.18, 53.62)
)

FF <- data.frame(
  x = c( 5.23,  5.23,  4.22,  4.21, 5.23),
  y = c(54.22, 53.83, 53.42, 53.8, 54.22)
)

KB <- data.frame(
  x = c( 3.32,  3.32,  2.90,  2.87,  2.81,  3.32),
  y = c(54.20, 53.83, 53.83, 53.96, 54.20, 54.20)
)

OYS <- data.frame(
  x = c( 4.53,  4.49,  3.96,  3.98,  4.53),
  y = c(54.96, 54.08, 54.08, 54.96, 54.96)
)

DB <- data.frame(
  x = c( 3.64,  4.26,  2.76,  2.90,  3.21,  3.64),
  y = c(55.64, 55.37, 54.38, 54.62, 55.38, 55.64)
)

# Polygons
NCP_poly <- Polygons(list(Polygon(NCP)), "NCP")
FF_poly <-  Polygons(list(Polygon(FF)), "FF")
KB_poly <-  Polygons(list(Polygon(KB)), "KB")
OYS_poly <- Polygons(list(Polygon(OYS)), "OYS")
DB_poly <-  Polygons(list(Polygon(DB)), "DB")

# DF with characteristics
df <- data.frame(
  name = c("Netherlands Continental Plate", "Frysian Front", "Clover Bank", "Oyster Grounds", "Dogger Bank"),
  type = c("Economic Zone", "Birds Directive", "Habitats Directive", "No Natura2000 status", "Habitats Directive"),
  area_ha = c(NA, 288261.35, 153954.17, NA, sum(c(1234015.84, 473992.36, 169552.66))),
  row.names = c("NCP", "FF", "KB", "OYS", "DB") # unique id in polygons
)

# Spatial df
regions_of_interest <- SpatialPolygonsDataFrame(
  SpatialPolygons(list(NCP_poly, FF_poly, KB_poly, OYS_poly, DB_poly)), 
  df)

# Save
save(regions_of_interest, file = "data/regions_of_interest.rda")
