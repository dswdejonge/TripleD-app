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
# install.packages("dplyr")
# install.packages("RColorBrewer")
# install.packages("htmltools")
library(shiny)
library(leaflet) # for interactive map
library(dplyr) # for data manipulation
library(RColorBrewer) # for palette map
library(htmltools) # for HTLMescape in popups

# --------------
# Load Database
# --------------
load("data/database.rda")
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
        checkboxInput(
          "show_incomplete_data",
          label = p("Also show incomplete data points (underestimations)"),
          value = TRUE
        ),
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
        h3("Filter data:"),
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
        textOutput("warnings"),
        tags$head(tags$style("#warnings{color: red}")),
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
  output$warnings <- renderText(
    if(input$show_incomplete_data){
      "Notifications: The map also shows incomplete data points, i.e. for some specimens in the filtered taxon no density or biomass value could be determined and the corresponding values are an underestimation."
    }else{
      "Notifications: NA"
    }
  )
  output$mymap <- renderLeaflet({
    # Get all station points (black markers)
    my_stations <- subset_environment(
      dataset = database,
      dates_input = input$dates_input,
      cruise_id = input$cruise_id,
      depth_range = input$depth_range
    )
    plot_stations <- dplyr::select(my_stations, 
                                   StationID, Station_name, Date, 
                                   Lat_DD, Lon_DD) %>%
                     dplyr::distinct()
    # Subset data based on bio info
    my_subset <- my_stations %>%
      subset_taxon(., 
                   taxonomic_level = input$taxonomic_level,
                   taxon = input$taxon) %>%
      subset_data_type(., map_type = input$map_type)
    # Import marker legend images
    html_legend <- "<img src='Station.png'style='width:30px;height:30px;'>Sampled station, filtered taxon not found.<br/><img src='Complete.png'style='width:30px;height:30px;'>Complete data for filtered taxon.<br/><img src='Incomplete.png'style='width:30px;height:30px;'>Incomplete data for filtered taxon."
    # Create map
    create_map(
      my_subset = my_subset, 
      all_stations = plot_stations,
      my_pal = my_pal,
      map_type = input$map_type,
      show_incomplete_data = input$show_incomplete_data,
      html_legend = html_legend,
      show_bathy = input$show_bathy,
      my_contours_df = my_contours_df,
      show_roi = input$show_roi,
      regions_of_interest = regions_of_interest)
  })

  # ------------
  # Statistics page
  # ------------
}

# Run the application
shinyApp(ui = ui, server = server)

