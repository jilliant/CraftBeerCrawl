# Setup
setwd("~/Desktop/SomethingWithNumbers/CraftBeerCrawl")
load("Data/CraftBeer.rdata")

CraftBeer  <- CraftBeer %>% 
  mutate(ID = as.integer(rownames(CraftBeer)))

# Duplicates <- CraftBeer %>% 
#   mutate(latlong = paste0(lat,lon)) %>% 
#   group_by(latlong) %>% 
#   filter(n()>1)

# Find nearest neighbours
CraftBeerNN <- data.frame(cbind(CraftBeer$lat, CraftBeer$lon))
closestCraftBeer <- nn2(data = CraftBeerNN, k=5)[[1]]

closestCraftBeer <- data.frame(closestCraftBeer)

# Pretty up for QA
closestCraftBeerChk <- closestCraftBeer %>% 
  mutate_all(funs(CraftBeer$Venue[match(., CraftBeer$ID)])) %>% 
  rename(Orgin = X1, First = X2, Second = X3, Third = X4, Fourth = X5, Fifth = X6)
closestCraftBeerChk

# Take one
Sample <- closestCraftBeerChk %>% 
  slice(1) 

# pivot and bring back the coordinates
Sample <- as.data.frame(t(Sample))
Sample$id <- seq.int(nrow(Sample))

Sample <- Sample %>% 
  inner_join(CraftBeer, by = c( "V1" = "Venue" )) %>% 
  select("id","V1", "lat", "lon")


# turn the co-ords into a matrix 
coords.df <- data.frame(long=Sample$lon, lat=Sample$lat)
coords.mx <- as.matrix(coords.df)

# Compute great-circle distance matrix
dist.mx <- spDists(coords.mx, longlat=TRUE)
dist.mx

# TSP object
tsp.ins <- tsp_instance(coords.mx, dist.mx )
tsp.ins

# Solve 
tour <- run_solver(tsp.ins, method="2-opt")
tour

# Compare Solvers
tours = sapply(c("nn", "cheapest_insertion", "arbitrary_insertion", "2-opt"), function(solver) {
  list(solver = run_solver(tsp.ins, method = solver))
})
tours
# 2-opt is pretty good!

# Permutation Vector 
tour_order <- as.integer(tour)

# Plot
autoplot(tsp.ins, tour)

# reorder
Sample_tour <- Sample[match(tour_order, Sample$id),]
Sample_tour


# On a Map
Sample_tour$lat <- as.double(Sample_tour$lat)
Sample_tour$lon <- as.double(Sample_tour$lon)

attribution1 <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | Icons made by <a href="http://www.flaticon.com/authors/nas-ztudio" title="Nas Ztudio">Nas Ztudio</a> from <a href="http://www.flaticon.com" title="Flaticon">www.flaticon.com</a> licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC BY 3.0 </a>'  

IconCraft <- makeIcon(iconUrl = "Icons/beer-1.png",
                      iconAnchorX = 18, iconAnchorY = 12)
IconBrew <- makeIcon(iconUrl = "Icons/barrel-3.png",
                     iconAnchorX = 18, iconAnchorY = 12)
IconBar <- makeIcon(iconUrl = "Icons/beer-2.png",
                    iconAnchorX = 18, iconAnchorY = 12)


map <- leaflet()%>%
  setView(145.000017, -37.815943, 10) %>% 
  addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
           attribution = attribution1
  ) %>%
  addMarkers(lng = Sample_tour$lon, lat = Sample_tour$lat,
             icon = IconCraft,
             popup = paste(Sample_tour$V1),
             group = "Location"
  ) %>% 
  addPolylines(lng = Sample_tour$lon, lat = Sample_tour$lat)
map

# map <- leaflet()%>%
#   setView(145.000017, -37.815943, 5) %>% 
#   addTiles(urlTemplate = 'http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png', 
#            attribution = attribution1
#   ) %>%
#   addMarkers(lng = Duplicates$lon2, lat = Duplicates$lat,
#              icon = IconCraft,
#              popup = paste(Duplicates$Venue),
#              group = "Location"
#   )
# map

