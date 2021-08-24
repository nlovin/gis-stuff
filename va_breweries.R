library(tidyverse)
library(ggmap)
library(sf)

#raw <- read_lines("data/raw/va_brew_raw.txt")
raw <- read_file("data/raw/va_brew_raw.txt")
raw_sp <- raw %>% str_split("View Map")

raw_tbl <- as.data.frame(do.call(cbind, raw_sp)) %>% 
  as_tibble() %>% 
  rename(raw = 1) %>% 
  slice(1:(n()-1))

clean_stg <- raw_tbl %>% 
  mutate(clean = str_replace_all(raw, "\\r\\n\\r\\n", "SPLITTER"),
         clean = str_replace_all(clean, "\\r\\n", "SPLITTER")) %>% 
  filter(!str_detect(clean, "Greater than ")) %>% 
  mutate(clean = ifelse(str_detect(clean, "1781 Brewing Company"), paste0("SPLITTER",clean), clean)) %>% 
  mutate(cleaner = str_remove(clean, "SPLITTER"))

clean_tbl <- clean_stg %>% 
  select(-raw, -clean) %>% 
  separate(cleaner, into = c("name", "street", "citystatezip", "phone", "type", "website"),  sep = "SPLITTER") %>% 
  mutate(website = ifelse(str_detect(type, "www"), type, website),
         type = ifelse(str_detect(phone, "Type"), phone, type),
         phone = ifelse(str_detect(phone, "Type"), NA, phone),
         website = ifelse(website=="", NA, website),
         addr = paste0(street, ", ", citystatezip)
         ) %>% 
  select(name, addr, everything()) %>% 
  filter(addr != "40950 Flying Ace Lane, Lovettsville, VA 20176")


# va_brew_tidygeocodes <- clean_tbl %>% 
#   tidygeocoder::geocode(addr, method = 'osm', lat = latitude , long = longitude)
  
#va_brew_googlegeocodes <- geocode(clean_tbl$addr,output = "more")

vab_ba <- cbind(clean_tbl, va_brew_googlegeocodes %>% rename(addrtype=type)) %>% as_tibble()

vab_ba <- vab_ba %>% 
  mutate(lon = ifelse(name == "Deadline Brewing Project", -76.0703599306823, lon),
         lat = ifelse(name == "Deadline Brewing Project", 36.91152497755818, lat))


write_csv(vab_ba, "data/output/va_breweries_geocoded.csv")

########### Mapping -----------

# load state shapes
states <- tigris::states()

# filter va
va_shp <- states %>% filter(STUSPS == "VA")

# create close crop of dmv (but VA only)
va_crop_shp <- st_crop(va_shp, xmin = -77.398979, xmax = -76.896355,
                    ymin = 38.722665, ymax = 39.012429)

# filter dmv
dmv <- states %>% filter(STUSPS=="DC" | STUSPS=="MD" | STUSPS == "VA")

# create close crop of dmv
dmv_crop <- st_crop(dmv, xmin = -77.398979, xmax = -76.896355,
                    ymin = 38.722665, ymax = 39.012429)

# make sf obj using brewery lat lon
vab_pt <- vab_ba %>% 
# filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c( "lon", "lat" ), crs =  "+proj=longlat +datum=NAD83")

# viz
ggplot() +
  geom_sf(data = va_shp) +
  geom_sf(data = vab_pt)


# close crop down to dmv (VA only)
vab_pt_crop <- st_crop(vab_pt, xmin = -77.398979, xmax = -76.896355,
                       ymin = 38.722665, ymax = 39.012429)
# viz
ggplot() +
  geom_sf(data = va_crop_shp) +
  geom_sf(data = vab_pt_crop)














# scraps
vab <- read_csv("data/united-states/virginia.csv")
vab <- vab %>% 
  mutate(addr = paste0(street, ", ", city, ", ", state, " ", postal_code))

library(tidygeocoder)

lat_longs <- vab %>%
  select(obdb_id, addr) %>% 
  geocode(addr, method = 'osm', lat = latitude , long = longitude)


x <- vab %>% filter(obdb_id=="fair-winds-brewing-company-lorton")
x <- x %>% mutate(st = "7000 Newington Rd")
x <- x %>% mutate(addr = paste0(st, ", ", city, ", ", state, " ", "22079"))

lat_longs <- x %>%
  select(obdb_id, addr) %>% 
  geocode(addr, method = 'osm', lat = latitude , long = longitude)

vab_pt <- lat_longs %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c( "longitude", "latitude" ), crs =  "+proj=longlat +datum=NAD83")

vab_pt_crop <- st_crop(vab_pt, xmin = -77.398979, xmax = -76.896355,
                       ymin = 38.722665, ymax = 39.012429)

ggplot() +
  geom_sf(data = dmv_crop) +
  geom_sf(data = vab_pt_crop)


library(ggmap)

register_google(key = "[your key]", write = TRUE)

geocode(x$addr,output = "more") -> y3


x %>% 
  mutate(addr = paste0(street, ", ", city, ", ", state, " ", postal_code)) -> addr2

geocode(x$addr,output = "more") -> y2

addr2$addr
