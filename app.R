rm(list=ls())
library(leaflet)
library(geojsonsf)
library(sf)
library(tidyverse)
library(shiny)
library(ggmap)
library(classInt)
library(RColorBrewer)



rsconnect::setAccountInfo(name='janeli',
                          token='YOUR API TOKEN',
                          secret='YOUR API secret')




typology <-  geojson_sf("bg.geojson") 

typology$urban_types <- as.factor(typology$urban_types)
levels(typology$urban_types)

levels(typology$urban_types) <- c("Low development with natural vegetation",
                                  "Dams, reservoirs, and wetlands",
                                  "Foothill areas",
                                  "Urban parks and open space",
                                  "Valley arterial areas",
                                  "Valley less developed areas",
                                  "Basin less developed areas",
                                  "Most developed areas",
                                  "Furthest from regional parks with natural vegetation")


####ggmap to display the sites#####
library(ggmap)
register_google(key = api_key)


#######################################
#######################################
#######################################
#######################################

server <- function(input, output) {
  
  # Get latitude and longitude
  geocode_origin <- eventReactive(input$go,{
    geocode(input$Location)
  })
  
  return_urban_type<- 
    function(x) {
      x <- geocode_origin()
      pt_sf <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326, agr = "constant")
      result_types<- st_intersection(pt_sf, typology) 
      return(as.character(result_types$urban_types))
    }
  
  
  get_urban_type<- 
    function(x) {
      x <- geocode_origin()
      pt_sf <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326, agr = "constant")
      intersect_df<- st_intersection(pt_sf, typology) 
      coords <- st_coordinates(intersect_df)
      intersection_result <- cbind(intersect_df, coords)
    }
  
  output$type <- renderPrint({return_urban_type(geocode_origin)})
  
  output$histgram <- renderPlot({
    pt_df <- 
      get_urban_type(geocode_origin) %>%
      st_set_geometry(NULL) %>%
      select(input$Variable)
    names(pt_df)[1] <- "variable"
    
    hist_df <- 
      typology %>%
      st_set_geometry(NULL) %>%
      select(input$Variable)
    names(hist_df)[1] <- "variable"
    
    hist_df %>%
      ggplot(aes(x=variable)) +
      geom_histogram(bins= input$bins, fill = "lightblue", color= "white") +
      geom_vline(aes(xintercept = pt_df$variable), color="red", linetype="dashed", size=1 )+
      theme_classic() +
      xlab(str_replace_all(paste0(input$Variable), "_", " ") ) +
      ylab("frequency")
  })
  
  
  
  
  
  output$map <- renderLeaflet({
    
    # generate base leaflet map
    newcol <- colorRampPalette(brewer.pal(9,"Spectral"))
    newcolor <- rev (newcol(9))
    newcolor
    pal <- colorFactor(newcolor, levels = levels(typology$urban_types))
    
    popup <- paste0( "<br><strong> urban type:  </strong>", typology$urban_types,
                     "<br><strong> population:  </strong>", typology$population,
                     "<br><strong> annual mean precipitation:  </strong>", typology$annual_precipitation,
                     "<br><strong> annual mean temperature:  </strong>", typology$average_temperature,
                     "<br><strong> elevation:  </strong>", typology$elevation,
                     "<br><strong> percentage of impervious surface:  </strong>", typology$percentage_of_impervious_surface,
                     "<br><strong> percentage of tree canopy coverage:  </strong>", typology$percentage_of_tree_canopy_coverage,
                     "<br><strong> percentage of water and wetlands:  </strong>", typology$percentage_of_water_and_wetlands,
                     "<br><strong> distance to natural areas:  </strong>", typology$distance_to_natural_areas,
                     "<br><strong> traffic density:  </strong>", typology$traffic_density,
                     "<br><strong> traffic noise:  </strong>", typology$traffic_noise)
    
    
    
    leaflet(typology) %>%
      addProviderTiles(providers$Stamen.Toner,group="Stamen Toner") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,group="Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery,group="Esri WorldImagery") %>%
      addProviderTiles(providers$Stamen.Terrain,group="Stamen Terrain") %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                  fillColor = ~pal(urban_types), popup =  popup,
                  highlightOptions = highlightOptions(fillColor = "yellow", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomleft", pal = pal, opacity = 1,
                values = ~urban_types, 
                title = "Urban Type") %>%
      addLayersControl(
        baseGroups = c("Stamen Toner", "Open Street Map", "Esri WorldImagery", "Stamen Terrain")) %>%
      clearPopups()
    
  })
  
  
  
  observeEvent(input$go,{
    df<-get_urban_type(geocode_origin)
    leafletProxy("map", data = df) %>% 
      setView( lng = df$X, lat=df$Y,zoom=15) %>%
      clearMarkers() %>%
      addMarkers(lng = ~X, lat = ~Y, 
                 popup = paste0( "<br><strong> urban type:  </strong>", df$urban_types,
                                 "<br><strong> population:  </strong>", df$population,
                                 "<br><strong> annual mean precipitation:  </strong>", df$annual_precipitation,
                                 "<br><strong> annual mean temperature:  </strong>", df$average_temperature,
                                 "<br><strong> elevation:  </strong>", df$elevation,
                                 "<br><strong> percentage of impervious surface:  </strong>", df$percentage_of_impervious_surface,
                                 "<br><strong> percentage of tree canopy coverage:  </strong>", df$percentage_of_tree_canopy_coverage,
                                 "<br><strong> percentage of water and wetlands:  </strong>", df$percentage_of_water_and_wetlands,
                                 "<br><strong> distance to natural areas:  </strong>", df$distance_to_natural_areas,
                                 "<br><strong> traffic density:  </strong>", df$traffic_density,
                                 "<br><strong> traffic noise:  </strong>", df$traffic_noise) 
      )
  })
  
}


ui <- fluidPage(
  titlePanel("LA urban habitat classification"),
  sidebarLayout(
    sidebarPanel( textInput("Location", "Type your adress here:", ""),
                  actionButton("go", "Enter"),
                  br(),
                  h4("This location is in type:"),
                  textOutput("type"),
                  br(),
                  br(),
                  selectInput(inputId = "Variable", 
                              label = "Display histogram of:", 
                              choices = sort(unique(names(typology %>% 
                                                            st_set_geometry(NULL) %>%
                                                            select(-c(urban_types, bg_id)))))),
                  sliderInput(inputId = "bins",
                              label = "Number of bins:",
                              min = 1,
                              max = 50,
                              value = 30),
                  plotOutput(outputId = "histgram", height="320px"),
                  h6("Blue histogram is the distribution of 
                     selected variable for the whole study area;
                     red line represents the data of the entered location")),
    mainPanel(leafletOutput("map",  width="100%",height="800px"))
                  ))

shinyApp(ui = ui, server = server)



