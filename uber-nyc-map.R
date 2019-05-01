library(pacman)
p_load(dplyr , 
       broom , 
       haven , 
       ggmap 
       )

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
                      sept14 
                      )

## Getting to mapping

nymap <- get_map( c( left = -74.4, 
                     bottom = 40.495, 
                     right = -73.6, 
                     top = 40.89
)
)

## Monthly

# April

ggmap(nymap) + 
  geom_point( data = april14 , 
              aes(x = april14$Lon , 
                  y = april14$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, April 2014") +
  theme( legend.position = "none")

# May

ggmap(nymap) + 
  geom_point( data = may14 , 
              aes(x = may14$Lon , 
                  y = may14$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, May 2014") +
  theme( legend.position = "none")

# June

ggmap(nymap) + 
  geom_point( data = june14 , 
              aes(x = june14$Lon , 
                  y = june14$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, June 2014") +
  theme( legend.position = "none")

# July

ggmap(nymap) + 
  geom_point( data = july14 , 
              aes(x = july14$Lon , 
                  y = july14$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, July 2014") +
  theme( legend.position = "none")

# August

ggmap(nymap) + 
  geom_point( data = august14 , 
              aes(x = august14$Lon , 
                  y = august14$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, August 2014") +
  theme( legend.position = "none")

# Septempber

ggmap(nymap) + 
  geom_point( data = sept14 , 
              aes(x = sept14$Lon , 
                  y = sept14$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, September 2014") +
  theme( legend.position = "none")



## POOLED SAMPLE!! (This part takes a while) 

ggmap(wmap) + 
  geom_point( data = uber_df , 
              aes(x = uber_df$Lon , 
                  y = uber_df$Lat , 
                  color = 'red') , 
              alpha = 0.5) +
  ggtitle("Uber Pick-ups, April through September 2014") +
  theme( legend.position = "none")

