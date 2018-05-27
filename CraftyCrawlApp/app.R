# Global ----
library(shiny)
library(leaflet)
library(dplyr)
library(sp)
library(tspmeta)
library(RANN)

setwd("~/Desktop/SomethingWithNumbers/CraftBeerCrawl")
load("Data/CraftBeer.rdata")

# Map things
attribution1 <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | Icons made by <a href="http://www.flaticon.com/authors/nas-ztudio" title="Nas Ztudio">Nas Ztudio</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC BY 3.0 </a>'  

IconCraft <- makeIcon(iconUrl = "Icons/beer-1.png",
                      iconAnchorX = 18, iconAnchorY = 12)
IconBrew <- makeIcon(iconUrl = "Icons/barrel-3.png",
                     iconAnchorX = 18, iconAnchorY = 12)
IconBar <- makeIcon(iconUrl = "Icons/beer-2.png",
                    iconAnchorX = 18, iconAnchorY = 12)

CraftBeer  <- CraftBeer %>% 
  mutate(ID = as.integer(rownames(CraftBeer)))

# Find nearest neighbours
CraftBeerNN <- data.frame(cbind(CraftBeer$lat, CraftBeer$lon))
closestCraftBeer <- nn2(data = CraftBeerNN, k=6)[[1]]
closestCraftBeer <- data.frame(closestCraftBeer)

# Convert index to names
closestCraftBeerDT <- closestCraftBeer %>%
  mutate_all(funs(CraftBeer$Venue[match(., CraftBeer$ID)])) %>%
  rename(Origin = X1, "Stop 2" = X2, "Stop 3" = X3, "Stop 4" = X4, "Stop 5" = X5, "Stop 6" = X6) %>% 
  arrange(Origin)
closestCraftBeerDT

# TODO Save as rdata and load

# UI ---- 
ui <- fluidPage(
titlePanel("Crafty Crawl"),

mainPanel(
  tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
  br(), br(),
  tableOutput("chk"),
  leafletOutput("map")),
sidebarPanel(
  tags$style(".well {background-color:#e6e6e6;}"),
  tags$style(type = "text/css", "{height: calc(100vh - 80px) !important;}"),
  #tags$style(".well {background-color:#FCDC32;}"), # yellow sidebar

    selectInput("venueInput", "Enter a starting point",
                CraftBeer$Venue,
                selected = "Stomping Ground Brewing Co"),
    radioButtons("stops", "Number of Venues",
                 choices = list(4,5,6), selected = 5),
    tableOutput("itineraryT")
  ))

server <- function(input, output, session){

  output$map <- renderLeaflet({

    
      Itinerary <- closestCraftBeerDT %>% 
        filter(Origin == input$venueInput) 
      Itinerary <- Itinerary[, 1:input$stops]
      Itinerary <- as.data.frame(t(Itinerary))
      colnames(Itinerary) <- "Venue"
      Itinerary$Venue <- as.character(Itinerary$Venue)
      
      # Itinerary <- closestCraftBeerDT %>% 
      #   filter(Origin == "Stomping Ground Brewing Co") 
      # Itinerary <- Itinerary[, 1:5]
      # Itinerary <- as.data.frame(t(Itinerary))
      # colnames(Itinerary) <- "Itinerary"
        
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
      #setView(max(Itinerary$lon), min(Itinerary$lat),, zoom = 12) %>% 
      #fitBounds(min(Itinerary$lon), min(Itinerary$lat), max(Itinerary$lon), max(Itinerary$lat)) %>% 
      addMarkers(~as.numeric(lon), ~as.numeric(lat),
                 icon = IconCraft,
                 #popup = ~as.character(V1),#add in address or votes or link to untappd?
                 label = ~as.character(Venue)) %>% 
    addPolylines(lng = ~as.numeric(lon), lat = ~as.numeric(lat))
  })
  

  output$itineraryT <- renderTable({
    ShowItineraryT <- closestCraftBeerDT %>% 
      filter(Origin == input$venueInput) 
    ShowItineraryT <- ShowItineraryT[, 1:input$stops]
    ShowItineraryT <- as.data.frame(t(ShowItineraryT))
    colnames(ShowItineraryT) <- "Itinerary"
    ShowItineraryT
  })
  
  # output$chk <- renderTable({
  #   ShowItinerary <- closestCraftBeerDT %>% 
  #     filter(Origin == input$venueInput)# %>% 
  #     #select(1:input$stops)
  #   Locations <- as.data.frame(t(ShowItinerary))
  #   Locations <- Locations %>%
  #     inner_join(CraftBeer, by = c( "V1" = "Venue" )) %>%
  #     select("V1", "lat", "lon") %>% 
  #     mutate(id = seq.int(nrow(Locations)))
  #   Locations
# })
}

shinyApp(ui, server)

