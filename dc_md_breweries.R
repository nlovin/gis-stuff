library(tidyverse)
library(ggmap)
library(sf)

#raw <- read_lines("data/raw/va_brew_raw.txt")
raw <- read_file("data/raw/md_brew_raw.txt")
raw_sp <- raw %>% str_split("View Map")

raw_tbl <- as.data.frame(do.call(cbind, raw_sp)) %>% 
  as_tibble() %>% 
  rename(raw = 1) %>% 
  slice(1:(n()-1))

clean_stg <- raw_tbl %>% 
  mutate(clean = str_replace_all(raw, "\\r\\n\\r\\n", "SPLITTER"),
         clean = str_replace_all(clean, "\\r\\n", "SPLITTER")) %>% 
  filter(!str_detect(clean, "Greater than ")) %>% 
  mutate(clean = ifelse(str_detect(clean, "1623 Brewing Company"), paste0("SPLITTER",clean), clean)) %>% 
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
  select(name, addr, everything())


md_brew_googlegeocodes <- geocode(clean_tbl$addr,output = "more")

mdb_ba <- cbind(clean_tbl, md_brew_googlegeocodes %>% rename(addrtype=type)) %>% as_tibble()

mdb_ba <- mdb_ba %>% 
  mutate(lon = ifelse(name == "Smoketown Creekside", -77.40332569945384, lon),
         lat = ifelse(name == "Smoketown Creekside", 39.41235353002384, lat)) %>% 
  mutate(lon = ifelse(name == "Union Craft Brewing", -76.6444394724721, lon),
         lat = ifelse(name == "Union Craft Brewing", 39.336719744528004, lat))

write_csv(mdb_ba, "data/output/md_breweries_geocoded.csv")

# DC

dcb_bd <- read_csv("data/united-states/district-of-columbia.csv") %>% 
  filter(!is.na(latitude)) %>% 
  rename(lat = latitude, lon = longitude, type = brewery_type, website = website_url)

