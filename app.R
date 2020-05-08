#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

################################################
# 1. Install the packages below if not present
################################################
# install.packages("shiny)
# install.packages("leaflet")
# install.packages("dplyr")
# install.packages("RColorBrewer")
# install.packages("htmltools")

################################################
# 2. Load the package below
################################################
library(shiny)
library(leaflet) # for interactive map
library(dplyr) # for data manipulation
library(RColorBrewer) # for palette map
library(htmltools) # for HTLMescape in popups

################################################
# 3. Load data and functions
################################################
load("data/database.rda")
load("data/contours.rda")
load("data/regions_of_interest.rda")
source("map.R")
source("subset.R")

################################################
# 4. User interface
################################################
ui <- navbarPage( # page with tabs to navigate to different pages
  "NIOZ TripleD Data",
 
   # -----------------------
   # A. Data exploration
  # -----------------------
  tabPanel(
    "Data exploration",
    sidebarLayout(
      # --------------------
      # A.1. Subset data
      # --------------------
      sidebarPanel(
        # --------------------------------------------------------
        # Radiobuttons: Presence-absence, density, or biomass data
        # --------------------------------------------------------
        radioButtons(
          "map_type",
          label = h3("Data type:"),
          choiceNames = list(
            HTML("<p>Presence - Absence</p>"),
            HTML("<p>Density (count m<sup>-2</sup>)</p>"),
            HTML("<p>Biomass (g AFDW m<sup>-2</sup>)</p>")
          ),
          choiceValues = list("pa","dens","biom"),
          selected = "pa"),
        # --------------------------------------------------------
        # Checkbox: With or without incomplete data?
        # --------------------------------------------------------
        checkboxInput(
          "show_incomplete_data",
          label = p("Also show incomplete data points (underestimations)"),
          value = TRUE
        ),
        h3("Filter data:"),
        # --------------------------------------------------------
        # Select: Taxonomic level and taxon
        # --------------------------------------------------------
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
        # --------------------------------------------------------
        # Select: Date range
        # --------------------------------------------------------
        dateRangeInput(
          "dates_input",
          label = p("Show data within time range:"),
          start = min(database$Date),
          end = max(database$Date),
          min = min(database$Date),
          max = max(database$Date)
        ),
        # --------------------------------------------------------
        # Select: Depth range
        # --------------------------------------------------------
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
        # --------------------------------------------------------
        # Select: CruiseID
        # --------------------------------------------------------
        selectInput(
          "cruise_id",
          label = p("Select a CruiseID:"),
          choices = c("all",sort(unique(as.character(dplyr::pull(database,CruiseID))))),
          selected = 1
        )
      ),
      mainPanel(
        textOutput("notifications"),
        tags$head(tags$style("#notifications{color: blue}")),
        tabsetPanel(type = "tabs",
                    # ------------------------------
                    # A.2. Interactive North Sea map
                    # ------------------------------
                    tabPanel("Map",
                             leafletOutput("mymap", height = "800px")),
                    # ------------------------------
                    # A.3. Time series
                    # ------------------------------
                    tabPanel("Timeseries",
                             plotOutput("plot1")),
                    # ------------------------------
                    # A.3. Ordination
                    # ------------------------------
                    tabPanel("NMDS",
                             plotOutput("plot2"))
        )
      )
    )
  ),

  # -----------
  # B. About page
  # -----------
  tabPanel("About",
           h3("This interactive page is created by Danielle de Jonge."),
           img(src = "TripleD_2011.jpg", height = 400),
           h1("Factsheet"),
           p(paste0("This database contains ",dim(database)[1]," entries, collected over
                    ",length(unique(database$StationID))," sample stations,
                    during ",length(unique(database$CruiseID))," cruises.")),
           p(paste0("The oldest sample was taken on ",min(database$Date),
                    " and the most recent sample was taken on ",max(database$Date),"."))
  )
)




################################################
# 5. Server, i.e. R-code that renders output
################################################
server <- function(input, output, session) {

  # -----------------------------------------------
  # Update selection box based on taxonomic level
  # -----------------------------------------------
  observeEvent(input$taxonomic_level,{
    # Get list of choices
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
   # Update the select box
    updateSelectInput(
      session,
      inputId = "taxon",
      choices = my_choices
    )
  })
  
  # -----------------------------------------------
  # Render notifications about data
  # -----------------------------------------------
  output$notifications <- renderText(
    if(input$show_incomplete_data){
      "Notifications: The data includes incomplete data points, i.e. these values are underestimations."
    }else{
      "Notifications: NA"
    }
  )
  
  # -----------------------------------------------
  # Reactive functions to filter data
  # -----------------------------------------------
  filter_on_station_metadata <- reactive({
    subset_environment(
      dataset = database,
      dates_input = input$dates_input,
      cruise_id = input$cruise_id,
      depth_range = input$depth_range
    )
  })
  
  filter_on_taxonomy <- reactive({
    filter_on_station_metadata() %>%
      subset_taxon(., 
                   taxonomic_level = input$taxonomic_level,
                   taxon = input$taxon) %>%
      subset_data_type(., map_type = input$map_type)
  })
  
  get_map_title <- reactive({
    if(input$map_type == "pa"){
      HTML("<p>Presence - Absence</p>")
    }else if(input$map_type == "dens"){
      HTML("<p>Density (count m<sup>-2</sup>)</p>")  
    }else if(input$map_type == "biom"){
      HTML("<p>Biomass (g AFDW m<sup>-2</sup>)</p>") 
    }
  })
  
  # -----------------------------------------------
  # Render base map
  # -----------------------------------------------
  output$mymap <- renderLeaflet({
    # Get palette for bathymetry
    bathy_pal <- colorNumeric(
      palette = RColorBrewer::brewer.pal(9, "Blues"),
      domain = c(
        min(my_contours_df$depth, na.rm = T), # Minimum range
        max(my_contours_df$depth, na.rm = T)), # Maximum range
      reverse = F # Use the scale in reverse (dark blue is deeper)
    )
    
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Basemap") %>%
      addScaleBar(position = "topright") %>%
      setView(lng = 4.0, lat = 55, zoom = 6) %>% #55°18'24.6"N 3°53'42.4"E
      # Add bathymetry layer
      addPolylines(
        group = "Bathymetry",
        data = my_contours_df, # SpatialLinesDataFrame object from sp package
        color = bathy_pal(my_contours_df$depth),
        weight = 2,
        opacity = 0.8,
        label = paste0(my_contours_df$depth, " m.")) %>%
      # Add regions of interest layer
      addPolygons(
        group = "Regions of interest",
        data = regions_of_interest,
        popup = htmltools::htmlEscape(paste0(
          regions_of_interest$name, 
          " (", regions_of_interest$type,"). ",
          "Area: ", regions_of_interest$area_ha, " ha."))
      ) %>%
      # Add layer control (show/hide layers)
      addLayersControl(
        overlayGroups = c("Bathymetry", "Regions of interest"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # -----------------------------------------------
  # Render and update layer with station markers
  # -----------------------------------------------
  observe({
    # Get station marker
    station_marker <- makeIcon(
      iconUrl = "Station.png", 
      iconWidth = 12, iconHeight = 12,
      iconAnchorX = 6, iconAnchorY = 6)
    # Subset data
    mysubset <- filter_on_station_metadata()
    plot_stations <- dplyr::select(
      mysubset, 
      StationID, Station_name, Date, 
      Lat_DD, Lon_DD) %>% 
      dplyr::distinct()
    # Update layer with proxy
    leafletProxy("mymap") %>%
      clearGroup(group = "Station markers") %>%
      addMarkers(
        group = "Station markers",
        lng = plot_stations$Lon_DD,
        lat = plot_stations$Lat_DD,
        icon = station_marker,
        popup = htmltools::htmlEscape(
          paste0("StationID: ",plot_stations$StationID,
                 " Station name: ",plot_stations$Station_name,
                 " Date: ", plot_stations$Date))
      )
  })
  
  # -----------------------------------------------
  # Render and update layer with data markers
  # -----------------------------------------------
  observe({
    # Image as legend:
    html_legend <- "<img src='Station.png'style='width:30px;height:30px;'>Sampled station, filtered taxon not found.<br/><img src='Complete.png'style='width:30px;height:30px;'>Complete data for filtered taxon.<br/><img src='Incomplete.png'style='width:30px;height:30px;'>Incomplete data for filtered taxon."
    map_title <- get_map_title()
    # Subset data
    my_subset <- filter_on_taxonomy()
    if(nrow(my_subset) > 0){
      my_pal <- colorNumeric(
        palette = RColorBrewer::brewer.pal(11, "Spectral"), # Spectral palette
        domain = c(
          min(my_subset$Value, na.rm = T), # Minimum range
          max(my_subset$Value, na.rm = T)), # Maximum range
        reverse = T # Use the scale in reverse (blue is low, red is high)
      )
    }
  
    # Update layer with proxy
    leafletProxy("mymap") %>%
      removeControl(layerId = "Legend") %>%
      addLegend(
        layerId = "Legend",
        "bottomright",
        pal = my_pal,
        values = my_subset$Value,
        title = map_title,
        opacity = 1)
  })
  
  #output$mymap <- renderLeaflet({
    # Create map
    #create_map(
    #  my_subset = my_subset, 
    #  all_stations = plot_stations,
    #  station_marker = station_marker,
    #  my_pal = my_pal,
    #  map_type = input$map_type,
    #  show_incomplete_data = input$show_incomplete_data,
    #  html_legend = html_legend,
    #  show_bathy = input$show_bathy,
    #  my_contours_df = my_contours_df,
    #  show_roi = input$show_roi,
    #  regions_of_interest = regions_of_interest)
  #})
}

# Run the application
shinyApp(ui = ui, server = server)

