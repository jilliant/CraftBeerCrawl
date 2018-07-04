# Global ----
library(shiny)
library(dplyr)
library(googleway)

load("ShinyData/CraftBeer.rdata")
load("ShinyData/closestCraftBeer.rdata")
load("ShinyData/closestCraftBeerDT.rdata")

source("~/Desktop/SomethingWithNumbers/key.R")

# https://stackoverflow.com/questions/49473094/plotting-waypoint-routes-with-googleway-in-shiny-app 


# Map things
# IconCraft <- makeIcon(iconUrl = "Icons/beer-1.png",
#                       iconAnchorX = 18, iconAnchorY = 12)
mystyle<-'[{"elementType":"geometry","stylers":[{"color":"#f5f5f5"}]},{"elementType":"geometry.fill","stylers":[{"color":"#feffff"}]},{"elementType":"labels.icon","stylers":[{"visibility":"off"}]},{"elementType":"labels.text.fill","stylers":[{"color":"#616161"}]},{"elementType":"labels.text.stroke","stylers":[{"color":"#f5f5f5"}]},{"featureType":"administrative.land_parcel","elementType":"labels","stylers":[{"visibility":"off"}]},{"featureType":"administrative.land_parcel","elementType":"labels.text.fill","stylers":[{"color":"#bdbdbd"}]},{"featureType":"administrative.locality","elementType":"labels.text.fill","stylers":[{"color":"#000000"}]},{"featureType":"poi","elementType":"geometry","stylers":[{"color":"#eeeeee"}]},{"featureType":"poi","elementType":"labels.text","stylers":[{"visibility":"off"}]},{"featureType":"poi","elementType":"labels.text.fill","stylers":[{"color":"#757575"}]},{"featureType":"poi.business","stylers":[{"visibility":"off"}]},{"featureType":"poi.park","elementType":"geometry","stylers":[{"color":"#e5e5e5"}]},{"featureType":"poi.park","elementType":"labels.text.fill","stylers":[{"color":"#9e9e9e"}]},{"featureType":"road","elementType":"geometry","stylers":[{"color":"#ffffff"}]},{"featureType":"road","elementType":"labels.icon","stylers":[{"visibility":"off"}]},{"featureType":"road.arterial","elementType":"geometry.fill","stylers":[{"color":"#aaaaaa"}]},{"featureType":"road.arterial","elementType":"labels.text.fill","stylers":[{"color":"#000000"}]},{"featureType":"road.highway","elementType":"geometry","stylers":[{"color":"#dadada"}]},{"featureType":"road.highway","elementType":"geometry.fill","stylers":[{"color":"#919191"}]},{"featureType":"road.highway","elementType":"labels.text.fill","stylers":[{"color":"#000000"}]},{"featureType":"road.local","elementType":"geometry.fill","stylers":[{"color":"#d6d6d6"}]},{"featureType":"road.local","elementType":"labels","stylers":[{"visibility":"off"}]},{"featureType":"road.local","elementType":"labels.text.fill","stylers":[{"color":"#000000"},{"weight":0.5}]},{"featureType":"transit","stylers":[{"visibility":"off"}]},{"featureType":"transit.line","elementType":"geometry","stylers":[{"color":"#e5e5e5"}]},{"featureType":"transit.line","elementType":"labels.icon","stylers":[{"visibility":"on"}]},{"featureType":"transit.station","elementType":"geometry","stylers":[{"color":"#eeeeee"}]},{"featureType":"water","elementType":"geometry","stylers":[{"color":"#c9c9c9"}]},{"featureType":"water","elementType":"labels.text.fill","stylers":[{"color":"#9e9e9e"}]}]'

# UI ---- 
ui <- fluidPage(
  
  titlePanel("Crafty Crawl"),
  
  mainPanel(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    google_mapOutput(outputId = "map")),
  
  sidebarPanel(
    tags$style(".well {background-color:#e6e6e6;}"),
    tags$style(type = "text/css", "{height: calc(100vh - 80px) !important;}"),
    
    selectInput("startPoint", "Enter a starting point",
                CraftBeer$Venue,
                selected = "Stomping Ground Brewing Co"),
    #uiOutput("endPoint"),
    numericInput("waypointsN", "Number of Venues", value = 5, min = 2, max = 12),
    tableOutput("itineraryTable"),
    tags$br(),
    actionButton(inputId = "getRoute", label = "Get Route"),
    tags$br(),
    tags$br(),
    helpText("An optimal Craft Beer Crawl, using the results from 
             Beer Cartel's Australian Craft Beer Survey 2017 (where the
             Brewery or Bar achieved at least 1% of the vote for it's state).")
    ))

server <- function(input, output, session){
  
  # Map
  
  output$map <- renderGoogle_map({
    google_map(key = MapKey, 
               styles = mystyle,
               search_box = TRUE, 
               scale_control = TRUE)
  })
  
  observeEvent(input$getRoute,{

    print("getting route")
    
    # Get the data looking nice
    Itinerary <- closestCraftBeerDT %>% 
       filter(Origin == input$startPoint) 
       # filter(Origin == "Stomping Ground Brewing Co")
    Itinerary <- Itinerary[, 1:input$waypointsN]
    # tinerary <- Itinerary[, 1:6]
    
    Itinerary <- as.vector(paste(Itinerary)) # mash it back into shape
    Itinerary <- data.frame(Itinerary)
    colnames(Itinerary) <- "Venue" 
    Itinerary$Venue <- as.character(Itinerary$Venue) 
    
    Locations <- Itinerary %>%
      inner_join(CraftBeer, by = c( "Venue" = "Venue" )) %>% 
      mutate(id = seq.int(nrow(Itinerary))) %>% 
      mutate(latlon = paste0(lat,",",lon))
    Locations
    
    # Origin
    o <- Locations[1,9] # first row last col

    # Waypoints
    # w <- Locations[3,12]
    # q <- Locations[4,12]
    stops_list <- as.list(Locations$Address)
    n <-rep("stop",length(stops_list)) 
    names(stops_list) <- paste(n)
    stops_list <- head(stops_list, -1) # remove last one
      
    # Destination  
    d <- Locations[nrow(Locations),9] # last row last col

    res <- google_directions(key = DirectionsKey,
                             origin = o,
                             waypoints = stops_list,
                             # waypoints = list(stop = w,
                             #                  stop = q),
                             destination = d,
                             optimise_waypoints = TRUE,
                             mode = "driving")

    df_route <- data.frame(route = res$routes$overview_polyline$points)

    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address)
    )

    df_way$order <- as.character(1:nrow(df_way))
    
    Locations <- Locations %>% select(Venue, Type, Address)
    
    df_way <- df_way %>%
       inner_join(Locations, by = c("address" = "Address"))

    google_map_update(map_id = "map") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#3252fc",
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    #info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way,
                  #info_window = df_way$Venue,
                  label = "order")
  })

}

shinyApp(ui, server)

