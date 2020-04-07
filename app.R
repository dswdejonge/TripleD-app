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
library(leaflet) # for interactive map
library(dplyr) # for data manipulation
library(RColorBrewer) # for palette map
library(htmltools) # for HTLMescape in popups

# --------------
# Load Database
# --------------
load("data/2020-04-02_database.rda")
load("data/contours.rda")
load("data/regions_of_interest.rda")
source("map.R")
source("subset.R")

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
        h3("Additional layers:"),
        checkboxInput(
          "show_bathy",
          label = p("Show bathymetry"),
          value = FALSE
        ),
        checkboxInput(
          "show_roi",
          label = p("Show regions of interest"),
          value = FALSE
        ),
        h3("Filter data"),
        selectInput(
          "taxonomic_level",
          label = p("Select a taxonomic level:"),
          choices = as.list(c("all_data", "phylum", "class", "order", 
                              "family", "genus", "species")),
          selected = 1),
        selectInput(
          "taxon",
          label = p("Select a taxon:"),
          choices = NULL,
          selected = 1
        ),
        dateRangeInput(
          "dates_input",
          label = p("Show data within time range:"),
          start = min(database$Date),
          end = max(database$Date),
          min = min(database$Date),
          max = max(database$Date)
        ),
        sliderInput(
          "depth_range",
          label = p("Show data within depth range:"),
          min = floor(min(database$Water_depth_m)),
          max = ceiling(max(database$Water_depth_m)),
          value = c(
            floor(min(database$Water_depth_m)),
            ceiling(max(database$Water_depth_m))),
          step = 1,
          round = TRUE,
          dragRange = TRUE
        ),
        selectInput(
          "cruise_id",
          label = p("Select a CruiseID:"),
          choices = c("all",sort(unique(as.character(dplyr::pull(database,CruiseID))))),
          selected = 1
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
  
  # Create map
  output$mymap <- renderLeaflet({
    # Get all station points (black markers)
    all_stations <- dplyr::select(database, StationID, Date, Lat_DD, Lon_DD) %>%
      distinct()
    # Subset data
    my_subset <- get_subset(
      database = database,
      taxonomic_level = input$taxonomic_level,
      taxon = input$taxon, 
      map_type = input$map_type,
      dates_input = input$dates_input,
      cruise_id = input$cruise_id,
      depth_range = input$depth_range)
    # Create map
    create_map(
      my_subset = my_subset, 
      all_stations = all_stations,
      my_pal = my_pal,
      map_type = input$map_type,
      show_bathy = input$show_bathy,
      show_roi = input$show_roi)
  })

  # ------------
  # Statistics page
  # ------------
}

# Run the application
shinyApp(ui = ui, server = server)

