library(tidyverse)
library(sf)
source("fct/_crops.R")

####### Combine Brewery Data --------
vab_ba <- read_csv("data/output/va_breweries_geocoded.csv")
mdb_ba <- read_csv("data/output/md_breweries_geocoded.csv")
dcb_bd <- read_csv("data/united-states/district-of-columbia.csv") %>% 
  filter(!is.na(latitude)) %>% 
  rename(lat = latitude, lon = longitude, type = brewery_type, website = website_url)

dmv_brew <- vab_ba %>% 
  select(name, type,phone,website,lon,lat) %>% 
  mutate(state = "VA") %>% 
  bind_rows(mdb_ba %>% 
              select(name, type,phone,website,lon,lat) %>% 
              mutate(state = "MD")) %>% 
  bind_rows(dcb_bd %>% 
              select(name, type,phone,website,lon,lat) %>% 
              mutate(phone = as.character(phone),
                     state = "DC")) %>% 
  mutate(type = str_remove(type, "Type: "),
         type = tools::toTitleCase(type)) %>% 
  rowid_to_column(var = "row")
  
dmv_brew <- dmv_brew %>% 
  mutate(been_there = case_when(row == 15 ~ 1,
                                row == 91 ~ 1,
                                row == 164 ~ 1,
                                row == 266 ~ 1,
                                row == 277 ~ 1,
                                row == 279 ~ 1,
                                row == 283 ~ 1,
                                row == 312 ~ 1,
                                row == 308 ~ 1,
                                row == 332 ~ 1,
                                row == 367 ~ 1,
                                row == 368 ~ 1,
                                row == 369 ~ 1,
                                row == 371 ~ 1,
                                row == 376 ~ 1,
                                row == 377 ~ 1,
                                TRUE ~ 0),
         been_there_label = case_when(name == "Calvert Brewing Co" ~ "Haven't been there",
                                      name == "Patuxent Brewing Co" ~ "Been there"))

###### load state shapes -------
library(tigris)
#options(tigris_use_cache = TRUE)
states <- tigris::states()

# filter dmv
dmv <- states %>% filter(STUSPS=="DC" | STUSPS=="MD" | STUSPS == "VA")

# create close crop of dmv
dmv_crop <- dmv %>% 
  close_crop

dmv_beltway_crop <- dmv %>% 
  belt_crop


# make sf obj using brewery lat lon
dmv_brew_pt <- dmv_brew %>% 
  # filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c( "lon", "lat" ), crs =  "+proj=longlat +datum=NAD83")

# viz
ggplot() +
  geom_sf(data = dmv) +
  geom_sf(data = dmv_brew_pt)


# close crop down to dmv (VA only)
# dmv_brew_pt_crop <- st_crop(dmv_brew_pt, xmin = -77.398979, xmax = -76.896355,
#                        ymin = 38.722665, ymax = 39.012429)

# beltway crop
dmv_brew_pt_crop <- dmv_brew_pt %>% 
  belt_crop %>% 
  filter(type != "Contract") %>% 
  filter(!str_detect(name, "Gordon Biersch"))

# beer_emoji <- tibble(image = sample(c("https://www.pngfind.com/pngs/m/32-322804_beer-emoji-png-transparent-png.png"),
#                                size=nrow(dmv_brew_pt_crop), replace = TRUE))

#dmv_brew_pt_crop <- dmv_brew_pt_crop %>% bind_cols(beer_emoji)


####### Rivers and Roads --------
prd_shp <- tigris::primary_roads()
prd_crop_shp <- prd_shp %>% 
  close_crop()


prd_beltway_shp <- prd_shp %>% 
  belt_crop()


beltway_shp <- prd_crop_shp %>% 
  filter(FULLNAME == "Capital Beltway")

beltway_shp <- prd_beltway_shp %>% 
  filter(FULLNAME == "Capital Beltway")

source("water_shp.R")


####### Viz --------
library(extrafont)
#font_import()
loadfonts(device = "win", quiet = TRUE)

ggplot() +
  geom_sf(data = dmv_beltway_crop, fill = 'white') +
  geom_sf(data = dmv_brew_pt_crop, 
          aes(shape = factor(type), color = factor(been_there)),
          size=2) +
  geom_sf(data = beltway_shp, color = 'darkgrey') + 
  geom_sf(data = dc_water2, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_arl, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_fax, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_stf, color = "steelblue",fill="steelblue") +
  geom_sf(data = va_water_alx, color = "steelblue",fill="steelblue") +
  geom_sf(data = md_water_pg, color = "steelblue",fill="steelblue") +
  geom_sf(data = md_water_pot, color = "steelblue",fill="steelblue") +
  geom_sf(data = md_water_cha, color = "steelblue",fill="steelblue") +
  theme_minimal() +
  labs(x="",
       y="",
       title = "DMV Area Breweries",
       shape = "Type",
       color = "Been There") +
  theme( panel.border=element_blank(),
         panel.grid = element_blank(),
         axis.text.x= element_blank(),
         axis.text.y = element_blank(),
         text = element_text(family = "Century Gothic", size = 20)) +
  ggthemes::scale_color_solarized() +
  #hrbrthemes::scale_color_ipsum() +
  guides(color = FALSE) #+
  # ggrepel::geom_label_repel(
  #   data = dmv_brew_pt_crop,
  #   aes(label = been_there_label, geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,
  #   size = 3
  #    ,
  #    nudge_y = .015,
  #    nudge_x = .025
  # )


ggsave(filename = "maps/dmv_brew.png", width = 8, height = 8, units = "in", dpi = 300)

ggsave(filename = "maps/dmv_brew_nolab.png", width = 8, height = 8, units = "in", dpi = 300)

