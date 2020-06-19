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
# install.packages("ggplot2")

################################################
# 2. Load the package below
################################################
library(shiny)
library(leaflet) # for interactive map
library(dplyr) # for data manipulation
library(RColorBrewer) # for palette map
library(htmltools) # for HTLMescape in popups
library(ggplot2) # for plotting

################################################
# 3. Load data and functions
################################################
if("database.rda" %in% list.files("data")){
  load("data/database.rda")
  preview_warning <- FALSE
}else{
  source("preview_database.R")
  database <- preview_database()
  preview_warning <- TRUE
}
load("data/contours.rda")
load("data/regions_of_interest.rda")
source("functions.R")
source("subset.R")
source("about_text.R")

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
        checkboxInput(
          "log_transform",
          label = p("Log10-transform data"),
          value = FALSE
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
          choices = as.list(NA),
          selected = 1
        ),
        # --------------------------------------------------------
        # Select: Date range
        # --------------------------------------------------------
        dateRangeInput(
          "dates_input",
          label = p(paste0("Display data from time range. (Available dates: ",
                           min(database$Date)," to ",max(database$Date),"):")),
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
        textOutput("preview_message"),
        tags$head(tags$style("#preview_message{color: red}")),
        textOutput("notifications"),
        tags$head(tags$style("#notifications{color: blue}")),
        tabsetPanel(type = "tabs",
                    # ------------------------------
                    # A.2. Interactive North Sea map
                    # ------------------------------
                    tabPanel("Map",
                             leafletOutput("mymap", height = "700px")), #800
                    # ------------------------------
                    # A.3. Time series
                    # ------------------------------
                    tabPanel("Timeseries",
                             plotOutput("times_series_plot"),
                             selectInput(
                               "smooth_method",
                               label = p("Smoothing method:"),
                               choices = list(Linear = "lm", Loess = "loess"),
                               selected = 1
                             )),
                    # ------------------------------
                    # A.3. Ordination
                    # ------------------------------
                    # https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
                    tabPanel("NMDS",
                             plotOutput("NMDS_plot"))
        )
      )
    )
  ),

  # -----------
  # B. About page
  # -----------
  tabPanel("About",
           h3("This Shiny app is created by Daniëlle de Jonge"),
           h4("May 2020"),
           h1("The TripleD"),
           p(TripleD_text()),
           img(src = "TripleD_2011.jpg", height = 400),
           p("Model of the TripleD in 2011. Image provided by Rob Witbaard (NIOZ)."),
           h1("The TripleD package"),
           p(package_text()),
           h1("Database dashboard"),
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
  get_taxon_choices <- reactive({
    if(input$taxonomic_level == "all_data"){
      as.list(NA)
    }else if(input$taxonomic_level == "species"){
      temp <- database %>%
        dplyr::filter(rank == "Species") %>%
        dplyr::pull(valid_name)
      as.list(sort(unique(temp)))
    }else{
      as.list(sort(unique(dplyr::pull(database,input$taxonomic_level))))
    }
  })
  observe({
    updateSelectInput(
      session,
      inputId = "taxon",
      choices = get_taxon_choices()
    )
  })
  
  # -----------------------------------------------
  # Render notifications about data
  # -----------------------------------------------
   if(preview_warning){
     output$preview_message <- renderText(
       "You are using random generated preview data to test the app!"
     )
   }
  
   get_notification_text <- reactive({
    default <- "Notifications: Polychaetes cannot be studied quantitatively with TripleD data!"
    data_filter <- paste("Showing data of:",input$taxonomic_level,input$taxon,".",sep = " ")
    if(input$show_incomplete_data){
      incomplete_message <- "The data includes incomplete data points, i.e. these values are underestimations."
    }else{
      incomplete_message <- ""
    }
    if(input$log_transform){
      log_message <- "Data is Log10 transformed."
    }else{
      log_message <- ""
    }
    paste(default, data_filter, incomplete_message, log_message, sep = " ")
  })
  
  output$notifications <- renderText(
    get_notification_text()
  )
  
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

    # Make legend and station marker image
    html_legend <- "<img src='Station_legend.png'style='width:30px;height:30px;'>Stations<br/><img src='Complete.png'style='width:30px;height:30px;'>Complete data<br/><img src='Incomplete.png'style='width:30px;height:30px;'>Underestimations"
    station_marker <- makeIcon(iconUrl = "Station.png",iconWidth = 12, iconHeight = 12,iconAnchorX = 6, iconAnchorY = 6)
    
    # Build map
    stations_to_plot <- dplyr::select(database,StationID, Station_name, Date,Lat_DD, Lon_DD) %>% dplyr::distinct()
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Basemap") %>%
      addScaleBar(position = "topright") %>%
      addSimpleGraticule(
        group = "Graticule",
        interval = 0.5,
        showOriginLabel = T
      ) %>%
      setView(lng = 4.0, lat = 55, zoom = 6) %>%
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
        overlayGroups = c("Bathymetry", "Regions of interest", "Graticule"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addControl(html = html_legend, position = "bottomleft") %>%
      addMarkers(
        group = "Station markers",
        lng = stations_to_plot$Lon_DD,
        lat = stations_to_plot$Lat_DD,
        icon = station_marker,
        popup = htmltools::htmlEscape(
          paste0("StationID: ",stations_to_plot$StationID,
                 " Station name: ",stations_to_plot$Station_name,
                 " Date: ", stations_to_plot$Date,
                 " Lat: ", stations_to_plot$Lat_DD,
                 " Lon: ", stations_to_plot$Lon_DD))
      )
  })
  
  # -----------------------------------------------
  # Reactive functions to filter data
  # # -----------------------------------------------
  filter_on_station_metadata <- reactive({
    subset_environment(
      dataset = database,
      dates_input = input$dates_input,
      cruise_id = input$cruise_id,
      depth_range = input$depth_range
    )
  })
  
  # -----------------------------------------------
  # Render and update layer with station markers
  # -----------------------------------------------
  observe({
    mysubset <- filter_on_station_metadata()
    station_marker <- makeIcon(iconUrl = "Station.png",iconWidth = 12, iconHeight = 12,iconAnchorX = 6, iconAnchorY = 6)
    stations_to_plot <- dplyr::select(mysubset,StationID, Station_name, Date,Lat_DD, Lon_DD) %>% dplyr::distinct()
    leafletProxy("mymap") %>%
      clearGroup(group = "Station markers") %>%
      addMarkers(
        group = "Station markers",
        lng = stations_to_plot$Lon_DD,
        lat = stations_to_plot$Lat_DD,
        icon = station_marker,
        popup = htmltools::htmlEscape(
          paste0("StationID: ",stations_to_plot$StationID,
                 " Station name: ",stations_to_plot$Station_name,
                 " Date: ", stations_to_plot$Date,
                 " Lat: ", stations_to_plot$Lat_DD,
                 " Lon: ", stations_to_plot$Lon_DD))
      )
  })
  
  # -----------------------------------------------
  # Reactive functions biodata filtering
  # -----------------------------------------------
  get_map_title <- reactive({
    if(input$map_type == "pa"){
      HTML("<p>Presence - Absence</p>")
    }else if(input$map_type == "dens"){
      if(input$log_transform){
        HTML("<p>Density [log10(count m<sup>-2</sup>)]</p>")
      }else{
        HTML("<p>Density [count m<sup>-2</sup>]</p>")
      }
    }else if(input$map_type == "biom"){
      if(input$log_transform){
        HTML("<p>Biomass [log10(g AFDW m<sup>-2</sup>)]</p>")
      }else{
        HTML("<p>Biomass [g AFDW m<sup>-2</sup>]</p>")
      }
    }
  })
  
  filter_on_taxonomy <- reactive({
    filter_on_station_metadata() %>%
      subset_taxon(.,
                   taxonomic_level = input$taxonomic_level,
                   taxon = input$taxon) %>%
      subset_data_type(., map_type = input$map_type)
  })
  
  transformed_subset <- reactive({
    if(input$log_transform & input$map_type != "pa"){
      my_subset <- filter_on_taxonomy()
      if(nrow(my_subset) > 0){
        dplyr::mutate(my_subset, Value = log10(Value))
      }else(
        my_subset
      )
    }else{
      filter_on_taxonomy()
    }
  })
  
  # -----------------------------------------------
  # Render and update data marker points and legend
  # -----------------------------------------------
  observe({
    # New subset
    my_subset <- transformed_subset()
    # Remove all markers and legend
    proxy <- leafletProxy("mymap") %>%
      removeControl(layerId = "Legend") %>%
      clearGroup(group = "Complete markers") %>%
      clearGroup(group = "Incomplete markers")
    # Only add new data if data in subset
    if(nrow(my_subset) > 0){
      complete_points <- my_subset[!my_subset$do_not_include,]
      incomplete_points <- my_subset[my_subset$do_not_include,]
      # When all data incl. incomplete is shown,
      # use a palette based on all data and
      # add complete and incomplete markers separately
      if(input$show_incomplete_data){
        if(all(is.na(my_subset$Value))){
          min_range <- 0
          max_range <- 1
        }else if(length(unique(my_subset$Value) == 1)){
          min_range <- 0
          max_range <- unique(my_subset$Value)
        }else{
          min_range <- min(my_subset$Value, na.rm = T)
          max_range <- max(my_subset$Value, na.rm = T)
        }
        my_pal <- get_palette(min_range, max_range)
        proxy <- proxy %>%
          addLegend(
            layerId = "Legend",
            "bottomright",
            pal = my_pal,
            values = c(min_range, max_range), #my_subset$Value,
            title = get_map_title(),
            opacity = 1)
        # Add markers if data exists
        if(nrow(complete_points) > 0){
          proxy <- proxy %>%
            addCircleMarkers(
              group = "Complete markers",
              lng = complete_points$Lon_DD,
              lat = complete_points$Lat_DD,
              color = my_pal(complete_points$Value),
              radius = 13,
              fillOpacity = 1.0,
              stroke = FALSE,
              popup = htmltools::htmlEscape(paste0("Value: ",complete_points$Value))
            )
        }
        # Add markers if data exists
        if(nrow(incomplete_points) > 0){
          proxy <- proxy %>%
            addCircleMarkers(
              group = "Incomplete markers",
              lng = incomplete_points$Lon_DD,
              lat = incomplete_points$Lat_DD,
              radius = 13,
              fillColor = my_pal(incomplete_points$Value),
              fillOpacity = 0.3,
              stroke = TRUE,
              color = my_pal(incomplete_points$Value),
              opacity = 1.0,
              popup = htmltools::htmlEscape(paste0("Value: ",incomplete_points$Value))
            )
        }
      }else{
      # If only complete points are shown,
      # use a palette based on the complete data and
      # add only complete markers
        if(nrow(complete_points) > 0){
          if(all(is.na(complete_points$Value))){
            min_range <- 0
            max_range <- 1
          }else if(length(unique(complete_points$Value) == 1)){
            min_range <- 0
            max_range <- unique(complete_points$Value)
          }else{
            min_range <- min(complete_points$Value, na.rm = T)
            max_range <- max(complete_points$Value, na.rm = T)
          }
          my_pal <- get_palette(min_range, max_range)
          proxy <- proxy %>%
            addLegend(
              layerId = "Legend",
              "bottomright",
              pal = my_pal,
              values = c(min_range, max_range),#complete_points$Value,
              title = get_map_title(),
              opacity = 1) %>%
            addCircleMarkers(
              group = "Complete markers",
              lng = complete_points$Lon_DD,
              lat = complete_points$Lat_DD,
              color = my_pal(complete_points$Value),
              radius = 13,
              fillOpacity = 1.0,
              stroke = FALSE,
              popup = htmltools::htmlEscape(paste0("Value: ",complete_points$Value))
            )
        }
      }
      # Return updated proxy
      proxy
    }else{
    # If there is no data in subset
      leafletProxy("mymap") %>%
        removeControl(layerId = "Legend") %>%
        clearGroup(group = "Complete markers") %>%
        clearGroup(group = "Incomplete markers")
    }
  })
  
  #########################
  # Render time series plot
  #########################
  # output$times_series_plot <- renderPlot({
  #   my_subset <- transformed_subset()
  #   if(nrow(my_subset) > 0){
  #     ggplot(my_subset, aes(x = Date, y = Value)) +
  #       geom_point() +
  #       geom_smooth(
  #         method = input$smooth_method #"lm"
  #       )
  #   }
  # })
  
  #########################
  # Render NMDS plot
  #########################
  #output$NMDS_plot <- renderPlot({
  #  my_subset <- transformed_subset()
  #  community_matrix <- tidyr::pivot_wider(
  #    data = my_subset,
  #    id_cols = StationID,
  #    names_from = taxon,
  #    values_from = Value,
  #    values_fill = list(Value = 0)
  #  )
  #})
}

# Run the application
shinyApp(ui = ui, server = server)

