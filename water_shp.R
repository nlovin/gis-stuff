# dmv rivers
library(tidyverse)
library(sf)

# load boundary boxes
source("fct/_crops.R")

# Get the Water

### Beltcrop ---

## VIRGINIA --
va_water_lou <- tigris::area_water(county = "Loudoun", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

va_water_arl <- tigris::area_water(county = "Arlington", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

va_water_fax <- tigris::area_water(county = "Fairfax County", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

va_water_alx <- tigris::area_water(county = "Alexandria", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

va_water_pw <- tigris::area_water(county = "Prince William", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

va_water_stf <- tigris::area_water(county = "Stafford", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

## MARYLAND --
md_water_pg <- tigris::area_water(county = "Prince George", state ="MD") %>% 
  filter(FULLNAME == "Potomac Riv" |
           FULLNAME == "Anacostia Riv" | 
           FULLNAME == "Broad Crk" |
           #FULLNAME == "Potomac River Tributary" |
           FULLNAME == "Northwest Br" |
           FULLNAME == "Sligo Crk" |
           (FULLNAME == "Northeast Br" & AWATER == 111088)) %>% 
  belt_crop

md_water_cha <- tigris::area_water(county = "Charles", state ="MD") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

# md_water <- tigris::area_water(county = "Montgomery", state ="MD") %>% 
#   belt_crop

md_water_pot <-
  tigris::area_water(county = "Montgomery", state = "MD") %>%
  mutate(FULLNAME = ifelse(is.na(FULLNAME), "Missing", FULLNAME)) %>%
  filter(FULLNAME == "Potomac Riv" | MTFCC == "H3010") %>%
  filter(
    FULLNAME != "Cabin John Crk" &
      FULLNAME != "Coal Spring Br" &
      FULLNAME != "Booze Crk" &
      FULLNAME != "Rock Crk" & 
      FULLNAME != "Coquelin Run" &
      FULLNAME != "Sligo Crk" &
      FULLNAME != "Northwest Br" &
      FULLNAME != "Muddy Br" &
      FULLNAME != "Long Draft Br" &
      FULLNAME != "Dry Seneca Crk" &
      FULLNAME != "Great Seneca Crk" &
      FULLNAME != "Whetstone Run" &
      FULLNAME != "Watts Br" &
      FULLNAME != "Seneca Crk" &
      FULLNAME != "Patuxent Riv" &
      FULLNAME != "Old Farm Crk" &
      FULLNAME != "Sandy Br" &
      FULLNAME != "Buck Br" &
      FULLNAME != "Paint Branch Crk" &
      FULLNAME != "Rock Creek North Br" & 
      FULLNAME != "James Crk" &
      FULLNAME != "Hawlings Riv"
  ) %>%
  group_by(FULLNAME) %>% 
  mutate(max = max(AWATER),
         biggest = ifelse(AWATER == max,1,0),
         drop = ifelse(FULLNAME == "Missing" & biggest == 0,1,0)) %>% 
  ungroup() %>% 
  filter(drop == 0) %>% 
  belt_crop

dc_water2 <-
  tigris::area_water(county = "District of Columbia", state = "DC") %>%
  filter(
    FULLNAME == "Potomac Riv" |
      FULLNAME == "Anacostia Riv" |
      FULLNAME == "Reflecting Pool" | 
      #FULLNAME == "Georgetown Reservoir" |
      FULLNAME == "Channel" |
      FULLNAME == "C and O Cnl" |
      FULLNAME == "Kingman Lk" |
      FULLNAME == "Lagoon"
  )



# testing water shapes
# ggplot() + 
#   geom_sf(data = md_water_pot, color = "steelblue",fill="steelblue") +
#   ggrepel::geom_text_repel(
#     data = md_water_pot,
#     aes(label = FULLNAME, geometry = geometry),
#     stat = "sf_coordinates",
#     min.segment.length = 0,
#     size = 3
#     # ,
#     # nudge_y = -.015,
#     # nudge_x = -.015
#   )
