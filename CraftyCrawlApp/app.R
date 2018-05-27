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
# # Define UI for application that draws a histogram
# #ui <- fluidPage(
# ui <- bootstrapPage(
#     tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#     leafletOutput("map", width = "100%", height = "100%"),
#     absolutePanel(top = 10, right = 10,
#                   selectizeInput("Origin",
#                                  label = ("Origin"),
#                                  multiple = FALSE,
#                                  choices = closestCraftBeerDT$Origin,
#                                  options = list(
#                                    placeholder = 'Select starting venue, type to search',
#                                    onInitialize = I('function() { this.setValue(""); }')
#                                  )),
#                   
#                   selectInput("stops", "Number of Stops",
#                               c(2,3,4,5,6)
#                   ),
#                   submitButton("Go!")
#     )
#   )
# 
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   
#   filteredData <- reactive({
#   # Take one
#   Sample <- closestCraftBeerDT %>%
#     filter(Origin == input$Origin)
#   # Sample <- closestCraftBeerDT %>% 
#   #   filter(Origin == "10 Toes Brewery")
#   
#   # pivot and bring back the coordinates
#   Sample <- as.data.frame(t(Sample))
#   Sample$id <- seq.int(nrow(Sample))
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
#   
#   
#   
#   
#   output$map <- renderLeaflet({
#     leaflet()%>%
#       setView(133.8807, -27.6980,4) %>% 
#       addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
#                attribution = attribution1
#       )
#   })
#   
#   observe({
#     leafletProxy("map", data = filteredData()) %>%  
#       addMarkers(lng = Sample_tour$lon, lat = Sample_tour$lat,
#                  icon = IconCraft,
#                  popup = paste(Sample_tour$V1),
#                  group = "Location"
#       ) %>% 
#       addPolylines(lng = Sample_tour$lon, lat = Sample_tour$lat)
#   })
#   
#       
#   # Data Table
# 
#   
# 
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# 
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
                CraftBeer$Venue),
    selectInput("stops", "Number of Stops",
                c(2,3,4,5,6)
    )
  ))

server <- function(input, output, session){

  output$map <- renderLeaflet({

    filtered <- CraftBeer %>%
      filter(Venue == input$venueInput)
    
    # StartTour <- closestCraftBeerDT %>% 
    #   filter(Origin == filtered)
    
    leaflet(filtered) %>%
      addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
               attribution = attribution1) %>%
      addMarkers(~as.numeric(lon), ~as.numeric(lat),
                 icon = IconCraft,
                 popup = ~as.character(Venue),
                 label = ~as.character(Venue))
  })

  # output$results <- renderTable({
  #   filtered <- CraftBeer %>%
  #     filter(Venue == input$venueInput) %>%
  #     select("Venue", "Type", "Percent Vote", "Total votes", "Address")
  #   filtered
  # })

  output$itinerary <- renderTable({
    StartTour <- closestCraftBeerDT %>% 
      filter(Origin == input$venueInput)
    StartTour
  })
  
}

shinyApp(ui, server)

