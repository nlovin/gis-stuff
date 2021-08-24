library(dplyr)
library(readr)
library(ggplot2)
library(sf)

# Load and filter data to florida
states <- st_read("X:/Collaboration/EPMO/Data Strategy/GIS/Data/Boundaries/State_and_Territory_Boundaries/CensusStateBoundaries_2019/tl_2019_us_state.shp")

florida <- states %>% filter(STUSPS =="FL")


# Project and CRS stuff (I do not full understand this anymore)
#set crs of the current map
st_set_crs( florida, 4326 )
#transform to projected-coordinates
st_transform( florida, 5070 )

#Reading in E-Rate Data
df_e_rate <- read_csv("X:/Collaboration/EPMO/Data Strategy/GIS/Projects/Enterprise/Carrier_Service_Areas/sl_hoco_merged.csv") %>% 
  janitor::clean_names() # this creates "pretty" column names (makes everything lower case and removes spaces in names)


# COMBINE ATT AND CENTURY LINK BY OVERWRITING THEIR HOLDING CO NAME
df_e_rate %>% 
  select(-x1) %>% # this is dropping the x1 column which is just an index. This isn't necessary, just a data cleaning step I did
  filter(!is.na(recipient_latitude)) %>% 
  mutate(same = case_when(holdingcompanyname=='AT&T INC'|holdingcompanyname=='AT&T, Inc.' ~ 1,
                          holdingcompanyname=='CENTURYLINK'|holdingcompanyname=='CenturyLink Inc.' ~ 2,
                          TRUE ~ 0
                          ),
         holdingcompanyname = case_when(same == 1 ~ 'AT&T INC',
                                        same == 2 ~ 'CENTURYLINK',
                                        TRUE ~ holdingcompanyname)) -> df_e_rate

# split the DF by holdingcompany
df_e_rate %>% 
  group_by(holdingcompanyname) %>% # group by hold co
  group_split() -> hco # and split into 39 dataframes contained within a list

glimpse(hco)

# THE LOOP
for (i in 1:length(hco)) {
  
  # we're going to pull out the hold co name for each dataframe
  # and save those names as character vectors
  # and then use those names later to create "nice" file names
  hco[[i]] %>% 
    select(holdingcompanyname) %>% # keep ONLY the holdingcompanyname column
    distinct() %>% # and remove repeats (so this will just keep one observation)
    pull() %>% # this pulls that holdingcompanyname column and creates a vector similar to distinct(hco[[1]]$holdingcompanyname)
    janitor::make_clean_names() -> name # save as object
  
  pt <- st_as_sf( hco[[i]], coords = c( "recipient_longitude", "recipient_latitude" ), crs =  "+proj=longlat +datum=NAD83")
  
  # Do the same projection and crs stuff (again not quite sure how you're supposed to do this)
  #set crs of the current map
  #st_set_crs( pt, 4326 ) # don't need this
  #transform to projected-coordinates
  st_transform( pt, 5070 )
  
  ggplot() +
    geom_sf(data = florida) +
    geom_sf(data = pt)
  
  # YOU NEED TO HAVE A FOLDER NAMED MAPS IN YOUR PROJECT FOLDER
  # OTHERWISE, EDIT THIS PATH NAME
  ggsave(paste0('maps/', name,'.png')) # now we're going to use paste0() to create nice file names
  
  
}
