library(pacman)
p_load(dplyr, 
       broom, 
       haven)

## After unloading the CSV's, replace the file path with wherever it is on your computer

april14 <- read.csv( "D:/Economics/Data/Uber NYC Pickups 2014/uber-raw-data-apr14.csv" )
may14 <- read.csv( "D:/Economics/Data/Uber NYC Pickups 2014/uber-raw-data-may14.csv" )
june14 <- read.csv( "D:/Economics/Data/Uber NYC Pickups 2014/uber-raw-data-jun14.csv" )
july14 <- read.csv( "D:/Economics/Data/Uber NYC Pickups 2014/uber-raw-data-jul14.csv" )
august14 <- read.csv( "D:/Economics/Data/Uber NYC Pickups 2014/uber-raw-data-aug14.csv" )
sept14 <- read.csv( "D:/Economics/Data/Uber NYC Pickups 2014/uber-raw-data-sep14.csv" )

## Binding together the data set

uber_df <- bind_rows( april14 , 
                      may14 , 
                      june14, 
                      july14 , 
                      august14 , 
                      sept14 )

## Getting to mapping

p_load(ggmap) # This is the package to fetch the maps

## For this part you need to set up your google maps APIs. If you don't have a Google account, you'll need one.

wmap <- get_map( "new york city, new york" )

## (This part takes a while) 

ggmap(wmap) + 
  geom_point( data = uber_df , 
              aes(x = uber_df$Lon , y = uber_df$Lat , color = 'red') , alpha = 0.5) +
  scale_x_continuous( name = 'Longitude') +
  scale_y_continuous( name = 'Latitude') + 
  ggtitle("Uber Pick-ups, April through September 2014") +
  theme( legend.position = "none")
