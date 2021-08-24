library(sf)
library(tidyverse)

# Load and filter data to florida
states <- st_read("X:/Collaboration/EPMO/Data Strategy/GIS/Data/Boundaries/State_and_Territory_Boundaries/CensusStateBoundaries_2019/tl_2019_us_state.shp")

florida <- states %>% filter(STUSPS =="FL")


# Project and CRS stuff (I do not full understand this anymore)
#set crs of the current map
st_set_crs( florida, 5070 )
#transform to WGS84-coordinates
st_transform( florida, 5070 )

# Quick plot!
plot(florida$geometry)

# Read in the lat/long points
temple_terrace <- read_csv("X:/Collaboration/EPMO/Data Strategy/GIS/Projects/Enterprise/Carrier_Service_Areas/Temple_Terrace_ERATE.csv") %>% 
  janitor::clean_names()

# And then convert them to SF objects
tt <- st_as_sf( temple_terrace, coords = c( "recipient_longitude", "recipient_latitude" ), crs =  "+proj=longlat +datum=NAD83")



# Do the same projection and crs stuff (again not quite sure how you're supposed to do this)
#set crs of the current map
st_set_crs( tt, 5070 )
#transform to WGS84-coordinates
st_transform( tt, 5070 )

# Take a look!
mapview::mapview( list( florida, tt ) )

# Look again (but with ggplot!)
ggplot() +
  geom_sf(data = florida) +
  geom_sf(data = tt)

ggsave("test.png")
