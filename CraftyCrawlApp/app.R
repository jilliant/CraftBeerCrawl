# Global
library(shiny)
library(leaflet)
library(dplyr)

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

# TODO Save as rdata and load
CraftBeer  <- CraftBeer %>% 
  mutate(ID = as.integer(rownames(CraftBeer)))

# Find nearest neighbours
CraftBeerNN <- data.frame(cbind(CraftBeer$lat, CraftBeer$lon))
closestCraftBeer <- nn2(data = CraftBeerNN, k=6)[[1]]

closestCraftBeer <- data.frame(closestCraftBeer)

closestCraftBeerDT <- closestCraftBeer %>%
  mutate_all(funs(CraftBeer$Venue[match(., CraftBeer$ID)])) %>%
  rename(Origin = X1, "Stop 2" = X2, "Stop 3" = X3, "Stop 4" = X4, "Stop 5" = X5, "Stop 6" = X6) %>% 
  arrange(Origin)
closestCraftBeerDT

# add gomap.js
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
# 
#   Sample <- Sample %>% 
#     inner_join(CraftBeer, by = c( "V1" = "Venue" )) %>% 
#     select("id","V1", "lat", "lon")
#   
#   Sample_tour <- Sample
#   
#   # # turn the co-ords into a matrix 
#   # coords.df <- data.frame(long=Sample$lon, lat=Sample$lat)
#   # coords.mx <- as.matrix(coords.df)
#   # 
#   # # Compute great-circle distance matrix
#   # dist.mx <- spDists(coords.mx, longlat=TRUE)
#   # 
#   # # TSP object
#   # tsp.ins <- tsp_instance(coords.mx, dist.mx )
#   # 
#   # # Solve 
#   # tour <- run_solver(tsp.ins, method="nn", start = 1)
#   # 
#   # # Permutation Vector 
#   # tour_order <- as.integer(tour)
#   # 
#   # # Plot
#   # autoplot(tsp.ins, tour)
#   # 
#   # # reorder
#   # Sample_tour <- Sample[match(tour_order, Sample$id),]
#   # 
#   # On a Map
#   Sample_tour$lat <- as.double(Sample_tour$lat)
#   Sample_tour$lon <- as.double(Sample_tour$lon)
#   })


  

# A Basic App
ui <- fluidPage(
  titlePanel("Crafty Crawl"),
  mainPanel(
    leafletOutput("map"),
    br(), br(),
    #tableOutput("results"),
    tableOutput("itinerary")),
  sidebarPanel(
    ### User chooses the tour to map
    selectInput("venueInput", "Enter a starting point",
                CraftBeer$Venue,
                selected = "Boatrocker Brewers & Distillers"),
    selectInput("stops", "Number of Stops",
                c(2,3,4,5,6),
                selected = "5")
  ))

server <- function(input, output, session){

  output$map <- renderLeaflet({

    ShowItinerary <- closestCraftBeerDT %>% 
      filter(Origin == input$venueInput) %>% 
      select(1:input$stops)
    Locations <- as.data.frame(t(ShowItinerary))
    Locations <- Locations %>%
      inner_join(CraftBeer, by = c( "V1" = "Venue" )) %>%
      select("V1", "lat", "lon") %>% 
      mutate(id = seq.int(nrow(Locations)))
    Locations
    
    filtered <- Locations
    
    leaflet(filtered) %>%
      addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = attribution1) %>%
      addMarkers(~as.numeric(lon), ~as.numeric(lat),
                 icon = IconCraft,
                 #popup = ~as.character(V1),#add in address or votes or link to untappd?
                 label = ~as.character(V1))
  })

  output$itinerary <- renderTable({
    ShowItinerary <- closestCraftBeerDT %>% 
      filter(Origin == input$venueInput) %>% 
      select(1:input$stops)
    ShowItinerary
  })
}

shinyApp(ui, server)

