#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("shiny)
# install.packages("leaflet")
library(shiny)
library(leaflet)
library(dplyr)

# --------------
# Load Database
# --------------
load("data/2020-04-02_database.rda")
source("map.R")
source("subset.R")
# -----------------------
# Define fixed parameters
# -----------------------
# North Sea boundaries lat and lon
#lats <- c(55, 59)
#lons <- c(-1, 4)

# All station points
all_stations <- dplyr::select(database, Lat_DD, Lon_DD) %>%
  distinct()

# ---------------
# User interface
# ---------------
ui <- navbarPage( # page with tabs to navigate to different pages
  "NIOZ TripleD Data",

  # ------------------------------------
  # Page with interactive North Sea map
  # ------------------------------------
  tabPanel(
    "Map",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "taxonomic_level",
          label = h3("Select a taxonomic group:"),
          choices = as.list(
            c("all", unique(database$phylum))),
          selected = 1)
      ),
      mainPanel(
        leafletOutput("mymap", height = "800px")
      )
    )
  ),

  # ------------------------------------
  # Page with database statistics
  # ------------------------------------
  tabPanel(
    "Stats",

    h1("Factsheet"),
    p(paste0("This database contains ",dim(database)[1]," entries, collected over
             ",length(unique(database$StationID))," sample stations,
             during ",length(unique(database$CruiseID))," cruises.")),
    p(paste0("The oldest sample was taken on ",min(database$Date),
             " and the most recent sample was taken on ",max(database$Date),".")),
    #p(paste0("Total suface area that has been sampled with the TripleD is ",
    #         sum(stations_final$Sample_area_m2), "m2 and the total volume of sediment
    #         sampled is ", sum(stations_final$Sample_volume_m3), "m3.")),
    #p(paste0("In total ", sum(as.integer(database$Count)), " specimens have been counted from ",
    #         length(unique(database$valid_name))," taxa.")),
    hr()
    ),

  # -----------
  # About page
  # -----------
  tabPanel("About",
           p("This interactive page is created by Danielle de Jonge."),
           img(src = "Diagram_TripleD.png", height = 400))
)

# ---------------------------------------
# Server, i..e R-code that renders output
# ---------------------------------------
server <- function(input, output) {

  # --------------
  # North Sea map
  # --------------
  output$mymap <- renderLeaflet({
    my_subset <- subset_db(input$taxonomic_level, database)
    create_map(my_subset, all_stations)
  })

  # ------------
  # Statistics page
  # ------------
}

# Run the application
shinyApp(ui = ui, server = server)

