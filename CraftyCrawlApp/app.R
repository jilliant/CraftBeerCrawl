# Global ----
library(shiny)
library(leaflet)
library(dplyr)
library(sp)
library(tspmeta)

load("ShinyData/CraftBeer.rdata")
load("ShinyData/closestCraftBeer.rdata")
load("ShinyData/closestCraftBeerDT12.rdata")

# Map things
attribution1 <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | Icons made by <a href="http://www.flaticon.com/authors/nas-ztudio" title="Nas Ztudio">Nas Ztudio</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC BY 3.0 </a>'  

IconCraft <- makeIcon(iconUrl = "Icons/beer-1.png",
                      iconAnchorX = 18, iconAnchorY = 12)

# UI ---- 
ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel("Crafty Crawl"),

mainPanel(
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  leafletOutput("map")),

sidebarPanel(
  tags$style(".well {background-color:#e6e6e6;}"),
  tags$style(type = "text/css", "{height: calc(100vh - 80px) !important;}"),

    selectInput("venueInput", "Enter a starting point",
                CraftBeer$Venue,
                selected = "Stomping Ground Brewing Co"),
    # radioButtons("stops", "Number of Venues",
    #              choices = list("4 Venues" = 4, "5 Venues" = 5,
    #                             "6 Venues" = 6), selected = 5),
    numericInput("stops", "Number of Venues (choose between 4-12)", value = 5, min = 4, max = 12),
    tableOutput("itineraryT"),
    helpText("An optimal Craft Beer Crawl, using the results from 
              Beer Cartel's Australian Craft Beer Survey 2017 (where the
              Brewery or Bar achieved at least 1% of the vote for it's state)."),
    br(),
    a(actionButton(inputId = "email1", label = "Contact", 
                 icon = icon("envelope", lib = "font-awesome")),
      href="mailto:thingswithnumbers@gmail.com")
  ))

server <- function(input, output, session){

  output$map <- renderLeaflet({

    # Get the data looking nice
      Itinerary <- closestCraftBeerDT %>% 
        filter(Origin == input$venueInput) 
      
      Itinerary <- Itinerary[, 1:input$stops]
      Itinerary <- as.data.frame(t(Itinerary))
      colnames(Itinerary) <- "Venue"
      Itinerary$Venue <- as.character(Itinerary$Venue)

      Locations <- Itinerary %>%
        inner_join(CraftBeer, by = c( "Venue" = "Venue" )) %>% 
        mutate(id = seq.int(nrow(Itinerary)))
      Locations
    
    # TSP 
    
    ## turn the co-ords into a matrix
    coords.df <- data.frame(long=Locations$lon, lat=Locations$lat)
    coords.mx <- as.matrix(coords.df)

    ## Compute great-circle distance matrix
    dist.mx <- spDists(coords.mx, longlat=TRUE)
    
    ## Create TSP object
    tsp.ins <- tsp_instance(coords.mx, dist.mx )
    
    ## Solve
    tour <- run_solver(tsp.ins, method="nn", start = 1)
    
    ## Get Permutation Vector
    tour_order <- as.integer(tour)
    
    ## reorder data frame
    Locations_tour <- Locations[match(tour_order, Locations$id),]
    
    ## Cast for Mapping
    Locations_tour$lat <- as.numeric(Locations_tour$lat)
    Locations_tour$lon <- as.numeric(Locations_tour$lon)

    Itinerary <- Locations_tour
    
    leaflet(Itinerary) %>%
      addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = attribution1) %>%
      addMarkers(~as.numeric(lon), ~as.numeric(lat),
                 icon = IconCraft,
                 #popup = ~as.character(V1),#add in address or votes or link to untappd?
                 label = ~as.character(Venue)) %>% 
    addPolylines(lng = ~as.numeric(lon), lat = ~as.numeric(lat), 
                 color = "#3252fc", opacity = 0.9)
  })

  output$itineraryT <- renderTable({
    ShowItineraryT <- closestCraftBeerDT %>% 
      filter(Origin == input$venueInput) 
    ShowItineraryT <- ShowItineraryT[, 1:input$stops]
    ShowItineraryT <- as.data.frame(t(ShowItineraryT))
    colnames(ShowItineraryT) <- "Itinerary"
    ShowItineraryT
  })

}

shinyApp(ui, server)

