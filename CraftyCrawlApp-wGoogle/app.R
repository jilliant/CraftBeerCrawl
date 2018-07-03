# Global ----
library(shiny)
library(dplyr)
library(sp)
library(tspmeta)
library(googleway)

load("ShinyData/CraftBeer.rdata")
load("ShinyData/closestCraftBeer.rdata")
load("ShinyData/closestCraftBeerDT.rdata")

source("~/Desktop/SomethingWithNumbers/key.R")

# https://stackoverflow.com/questions/49473094/plotting-waypoint-routes-with-googleway-in-shiny-app 


# Map things
# IconCraft <- makeIcon(iconUrl = "Icons/beer-1.png",
#                       iconAnchorX = 18, iconAnchorY = 12)

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
    uiOutput("endPoint"),
    numericInput("waypointsN", "Number of Venues", value = 5, min = 4, max = 12),
    #tableOutput("itineraryT"),
    tags$br(),
    actionButton(inputId = "getRoute", label = "Get Route"),
    tags$br(),
    tags$br(),
    helpText("An optimal Craft Beer Crawl, using the results from 
             Beer Cartel's Australian Craft Beer Survey 2017 (where the
             Brewery or Bar achieved at least 1% of the vote for it's state).")
    ))

server <- function(input, output, session){
  
  # Allow user to select an endpoint from the 12 nearest neighbours
  output$endPoint <- renderUI({
    
    endOptions <- closestCraftBeerDT %>% 
      filter(Origin == input$startPoint) 
    
    endOptions <- as.character(as.vector(endOptions[1,]))
    
    endOptions <- endOptions[-1] # remove start point
  
    selectInput("endPoints", "Choose an end point", endOptions)
  })
  
  # Map
  
  output$map <- renderGoogle_map({
    google_map(key = MapKey, 
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
    #Itinerary <- Itinerary[, 1:6]
    Itinerary <- Itinerary[,-ncol(Itinerary)] # remove last stop
    last_stop <- c(input$endPoint)
    #last_stop <- c("Pirate Life")
    Itinerary <- append(Itinerary, last_stop) # add destination row
    Itinerary <- as.vector(paste(Itinerary)) # mash it back into shape
    Itinerary <- data.frame(Itinerary)
    colnames(Itinerary) <- "Venue" # to lazy join them
    Itinerary$Venue <- as.character(Itinerary$Venue) # always factors so change back to char
    
    Locations <- Itinerary %>%
      inner_join(CraftBeer, by = c( "Venue" = "Venue" )) %>% 
      mutate(id = seq.int(nrow(Itinerary)))
    Locations
    

    origin <- Locations[1,]
    o <- paste0(origin$lat,",",origin$lon)

    # w <- input$waypoint
    # q <- input$waypoint2

    destination <- Locations[nrow(Locations),]
    d <- paste0(destination$lat,",",destination$lon)
    
    # o1 <- CraftBeer %>% 
    #   filter(Venue == input$startPoint)
    # o <- paste0(o1$lat,",",o1$lon)
    # 
    # d1 <- CraftBeer %>% 
    #   filter(Venue == input$endPoint)
    # d <- paste0(d1$lat,",",d1$lon)


    res <- google_directions(key = DirectionsKey,
                             origin = o,
                             # waypoints = list(stop = w,
                             #                  stop = q),
                             destination = d,
                            # optimise_waypoints = TRUE,
                             mode = "driving")

    df_route <- data.frame(route = res$routes$overview_polyline$points)

    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address)
    )

    df_way$order <- as.character(1:nrow(df_way))
    
    # Test map
    # g <- google_map(key = MapKey,
    #                 search_box = TRUE,
    #                 scale_control = TRUE) %>%
    #   add_polylines(data = df_route,
    #                 polyline = "route",
    #                 stroke_colour = "#FF33D6",
    #                 stroke_weight = 7,
    #                 stroke_opacity = 0.7,
    #                 info_window = "New route",
    #                 load_interval = 100) %>%
    #   add_markers(data = df_way,
    #               info_window = "end_address",
    #               label = "order")

    google_map_update(map_id = "map") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way,
                  info_window = "end_address",
                  label = "order")
  })



  # output$map <- renderLeaflet({
  #   
  #   # Get the data looking nice
  #   Itinerary <- closestCraftBeerDT %>% 
  #     filter(Origin == input$venueInput) 
  #   
  #   Itinerary <- Itinerary[, 1:input$stops]
  #   Itinerary <- as.data.frame(t(Itinerary))
  #   colnames(Itinerary) <- "Venue"
  #   Itinerary$Venue <- as.character(Itinerary$Venue)
  #   
  #   Locations <- Itinerary %>%
  #     inner_join(CraftBeer, by = c( "Venue" = "Venue" )) %>% 
  #     mutate(id = seq.int(nrow(Itinerary)))
  #   Locations
  #   
  #   # TSP 
  #   
  #   ## turn the co-ords into a matrix
  #   coords.df <- data.frame(long=Locations$lon, lat=Locations$lat)
  #   coords.mx <- as.matrix(coords.df)
  #   
  #   ## Compute great-circle distance matrix
  #   dist.mx <- spDists(coords.mx, longlat=TRUE)
  #   
  #   ## Create TSP object
  #   tsp.ins <- tsp_instance(coords.mx, dist.mx )
  #   
  #   ## Solve
  #   tour <- run_solver(tsp.ins, method="nn", start = 1)
  #   
  #   ## Get Permutation Vector
  #   tour_order <- as.integer(tour)
  #   
  #   ## reorder data frame
  #   Locations_tour <- Locations[match(tour_order, Locations$id),]
  #   
  #   ## Cast for Mapping
  #   Locations_tour$lat <- as.numeric(Locations_tour$lat)
  #   Locations_tour$lon <- as.numeric(Locations_tour$lon)
  #   
  #   Itinerary <- Locations_tour
  #   
  #   leaflet(Itinerary) %>%
  #     addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
  #              attribution = attribution1) %>%
  #     addMarkers(~as.numeric(lon), ~as.numeric(lat),
  #                icon = IconCraft,
  #                #popup = ~as.character(V1),#add in address or votes or link to untappd?
  #                label = ~as.character(Venue)) %>% 
  #     addPolylines(lng = ~as.numeric(lon), lat = ~as.numeric(lat), 
  #                  color = "#3252fc", opacity = 0.9)
  # })
  # 
  # output$itineraryT <- renderTable({
  #   ShowItineraryT <- closestCraftBeerDT %>% 
  #     filter(Origin == input$venueInput) 
  #   ShowItineraryT <- ShowItineraryT[, 1:input$stops]
  #   ShowItineraryT <- as.data.frame(t(ShowItineraryT))
  #   colnames(ShowItineraryT) <- "Itinerary"
  #   ShowItineraryT
  # })
  
}

shinyApp(ui, server)

