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
library(RColorBrewer)

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
          label = h3("Select a taxonomic level:"),
          choices = as.list(c("all_data", "phylum", "class", "order", 
                              "family", "genus", "species")),
          #choices = as.list(c("phylum", "class", "order","family", "genus", "species")),
          selected = 1),
        selectInput(
          "taxon",
          label = h3("Select a taxon:"),
          choices = NULL,
          selected = 1
        ),
        radioButtons(
          "map_type",
          label = h3("Map type:"),
          choiceNames = list(
            HTML("<p>Presence - Absence</p>"),
            HTML("<p>Density (count m<sup>-2</sup>)</p>"),
            HTML("<p>Biomass (g AFDW m<sup>-2</sup>)</p>")
          ),
          choiceValues = list("pa","dens","biom"),
          selected = "pa"),
        dateRangeInput(
          "dates_input",
          label = h3("Show data between:"),
          start = min(database$Date),
          end = max(database$Date),
          min = min(database$Date),
          max = max(database$Date)
        )
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
server <- function(input, output, session) {

  # --------------
  # North Sea map
  # --------------
  # Update selection box based on taxonomic level
  observeEvent(input$taxonomic_level,{
    if(input$taxonomic_level == "all_data"){
      my_choices <- as.list(NA)
    }else if(input$taxonomic_level == "species"){
      temp <- database %>%
        dplyr::filter(rank == "Species") %>%
        dplyr::pull(valid_name)
      my_choices <- as.list(sort(unique(temp)))
    }else{
      my_choices <- as.list(sort(unique(dplyr::pull(database,input$taxonomic_level))))
    }
    updateSelectInput(
      session,
      inputId = "taxon",
      choices = my_choices
    )
  })
  
  output$mymap <- renderLeaflet({
    # Subset data
    my_subset <- get_subset(
      taxonomic_level = input$taxonomic_level,
      taxon = input$taxon, 
      map_type = input$map_type,
      database = database,
      dates_input = input$dates_input)
    # Palette
    my_pal <- colorNumeric(
      palette = RColorBrewer::brewer.pal(11, "Spectral"), # Spectral palette
      domain = c(
        min(my_subset$Value, na.rm = T), # Minimum range
        max(my_subset$Value, na.rm = T)), # Maximum range
      reverse = T # Use the scale in reverse (blue is low, red is high)
      )
    # Create map
    create_map(
      my_subset = my_subset, 
      all_stations = all_stations,
      my_pal = my_pal,
      map_type = input$map_type)
  })

  # ------------
  # Statistics page
  # ------------
}

# Run the application
shinyApp(ui = ui, server = server)

