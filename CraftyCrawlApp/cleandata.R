library(shiny)
library(leaflet)
library(dplyr)
library(sp)
library(tspmeta)
library(RANN)

setwd("~/Desktop/SomethingWithNumbers/CraftBeerCrawl")
load("Data/CraftBeer.rdata")

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

save(CraftBeer, file = "Data/CraftBeer.rdata")

save(closestCraftBeer, file = "Data/closestCraftBeer.rdata")

save(closestCraftBeerDT, file = "Data/closestCraftBeerDT.rdata")
