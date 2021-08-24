library(tidyverse)
library(sf)

tibble(name = c("Nathan", "Abe", "Maddie", "Amit", "Zach", "Ben", "Emily"),
       lat = c(38.756466576658205, 38.969521078436784, 38.89447218316051, 38.9096376877314, 38.94506428225417, 38.90126995087962, 38.932919689164),
       lon = c(-77.15438437492702, -77.38586884523622, -76.99631725589192, -77.02975075961918, -77.03724421272813, -77.0005972100985, -76.94763975440956)
) -> dt

states <- tigris::states()

dmv <- states %>% filter(STUSPS=="DC" | STUSPS=="MD" | STUSPS == "VA")

dmv_crop <- st_crop(dmv, xmin = -77.398979, xmax = -76.896355,
                    ymin = 38.722665, ymax = 39.012429)

pt <- st_as_sf( dt, coords = c( "lon", "lat" ), crs =  "+proj=longlat +datum=NAD83")

dt %>% 
  summarise(name = "Mean Center",
            lat = sum(lat)/n(),
            lon = sum(lon)/n()) %>% 
  st_as_sf( ., coords = c( "lon", "lat" ), crs =  "+proj=longlat +datum=NAD83")-> ct


dt %>% 
  summarise(name = "Mean Center",
            lat = median(lat),
            lon = median(lon)) %>% 
  st_as_sf( ., coords = c( "lon", "lat" ), crs =  "+proj=longlat +datum=NAD83")-> mt



ggplot() + 
  #geom_sf(data = dmv_crop) +
  geom_sf(data = pt) +
  geom_sf(data = dc_water2, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_arl, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_fax, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_alx, color = "steelblue",fill="steelblue") +
  geom_sf(data = md_water_pg, color = "steelblue",fill="steelblue") +
  geom_sf(data = md_water_pot, color = "steelblue",fill="steelblue") +
  #geom_sf(data = ct, color = "blue", size = 3, shape = 8) +
  #geom_sf(data = mt, color = "green", size = 3, shape = 8) +
  #geom_sf_text(aes(label = name),data = ct, colour = "white")+
  # ggrepel::geom_label_repel(
  #   data = ct,
  #   aes(label = name, geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   size = 3,
  #   nudge_y = -.015,
  #   nudge_x = -.015
  # ) +
  ggrepel::geom_text_repel(
    data = pt,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 3
    # ,
    # nudge_y = -.015,
    # nudge_x = -.015
  ) +
  theme_minimal() +
  ylab(label = "") +
  xlab(label = "") +
  theme( panel.border=element_blank(),
         panel.grid = element_blank(),
         axis.text.x= element_blank(),
         axis.text.y = element_blank())

# dc_water <- tigris::linear_water(county = "District of Columbia", state ="DC")
# va_water <- tigris::linear_water(county = "Arlington", state ="VA")
dc_water <- tigris::area_water(county = "District of Columbia", state ="DC")

va_water_arl <- tigris::area_water(county = "Arlington", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  belt_crop

va_water_lou <- tigris::area_water(county = "Loudoun", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  st_crop(xmin = -77.398979, xmax = -76.896355,
          ymin = 38.722665, ymax = 39.012429)

va_water_fax <- tigris::area_water(county = "Fairfax County", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
   st_crop(xmin = -77.398979, xmax = -76.896355,
           ymin = 38.722665, ymax = 39.012429)

va_water_alx <- tigris::area_water(county = "Alexandria", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
   st_crop(xmin = -77.398979, xmax = -76.896355,
           ymin = 38.722665, ymax = 39.012429)

va_water_stf <- tigris::area_water(county = "Stafford", state ="VA") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  st_crop(xmin = -77.41678, xmax = -76.687215,
          ymin = 38.603916, ymax = 39.175237)

md_water_pg <- tigris::area_water(county = "Prince George", state ="MD") %>% 
  filter(FULLNAME == "Potomac Riv" | FULLNAME == "Anacostia Riv") %>% 
  st_crop(xmin = -77.398979, xmax = -76.896355,
          ymin = 38.722665, ymax = 39.012429)

md_water_pg <- tigris::area_water(county = "Prince George", state ="MD") %>% 
  filter(FULLNAME == "Potomac Riv" | FULLNAME == "Anacostia Riv") %>% 
  st_crop(xmin = -77.398979, xmax = -76.896355,
          ymin = 38.722665, ymax = 39.012429)

md_water <- tigris::area_water(county = "Montgomery", state ="MD") %>% 
  st_crop(xmin = -77.398979, xmax = -76.896355,
          ymin = 38.722665, ymax = 39.012429)

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
      FULLNAME != "Northwest Br"
  ) %>%
  group_by(FULLNAME) %>% 
  mutate(max = max(AWATER),
         biggest = ifelse(AWATER == max,1,0),
         drop = ifelse(FULLNAME == "Missing" & biggest == 0,1,0)) %>% 
  ungroup() %>% 
  filter(drop == 0) %>% 
  st_crop(
    xmin = -77.398979,
    xmax = -76.896355,
    ymin = 38.722665,
    ymax = 39.012429
  )

md_water_cha <- tigris::area_water(county = "Charles", state ="MD") %>% 
  filter(FULLNAME == "Potomac Riv") %>% 
  st_crop(xmin = -77.398979, xmax = -76.896355,
          ymin = 38.722665, ymax = 39.012429)

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


ggplot() + 
  geom_sf(data = md_water_pot, color = "steelblue",fill="steelblue") +
  ggrepel::geom_text_repel(
    data = md_water_pot,
    aes(label = FULLNAME, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    size = 3
    # ,
    # nudge_y = -.015,
    # nudge_x = -.015
  )

mapview::mapview(y)

leaflet::leaflet(x)

plotly::ggplotly(y)

ggplot() + 
  geom_sf(data = md_water_pot, color = "steelblue",fill="steelblue") -> y
