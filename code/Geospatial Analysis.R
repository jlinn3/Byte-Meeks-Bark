locations <- read.csv(CountyLocations.csv)
merged_data <- inner_join(rabies, locations, by = "County")
#Check for Missing Values
anyNA
colSums(is.na(merged_data))

#Map of Texas
positive_cases <- subset(merged_data, TestResult == "Positive")

#Create a Map
rabiesmap <- leaflet(data = positive_cases) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 7,  
    color = "red",
    fillOpacity = 0.5,
    popup = ~paste("County: ", County, "<br>",
                   "Rabies Cases: ", rabies)
  )

# Display the map
rabiesmap
