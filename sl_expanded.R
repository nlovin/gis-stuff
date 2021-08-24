# install.packages(sp)
# install.packages(tmap)
# install.packages(maptools)
# install.packages("mapview")

library(sp)
library(sf)
library(dplyr)
library(spatstat)
library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(tmap)
library(tmaptools)
library(mapview)

library(tidyverse)


#Reading in Florida shapefile
states <- st_read("X:/Collaboration/EPMO/Data Strategy/GIS/Data/Boundaries/State_and_Territory_Boundaries/CensusStateBoundaries_2019/tl_2019_us_state.shp")

florida <- states %>% filter(STUSPS =="FL")

florida_sp <- as(florida, Class = "Spatial")

florida_prj <- spTransform(florida_sp, CRS("+init=epsg:5070"))

plot(florida_prj)


project_crs <- crs(florida_prj, asText = TRUE)


wgs.84 <- "+proj=longlat +datum=WGS84"
sp::CRS(wgs.84)

#Reading in Temple Terrace
temple_terrace <- read_csv("X:/Collaboration/EPMO/Data Strategy/GIS/Projects/Enterprise/Carrier_Service_Areas/Temple_Terrace_ERATE.csv") %>% 
  janitor::clean_names()

temple_terrace <- SpatialPoints(temple_terrace [,c("recipient_longitude", "recipient_latitude")], proj4string=CRS(wgs.84))

temple_terrace <- spTransform(temple_terrace, CRS("+init=epsg:5070"))

plot(florida_prj)
plot(temple_terrace, add= TRUE)


#Reading in E-Rate Data
df_e_rate <- read_csv("X:/Collaboration/EPMO/Data Strategy/GIS/Projects/Enterprise/Carrier_Service_Areas/sl_hoco_merged.csv") %>% 
  janitor::clean_names()

# --- Converting E-Rate to SpatialPointsDataFrame
att_sl <- df_e_rate %>% 
  filter(holdingcompanyname=='AT&T INC' | holdingcompanyname == "AT&T, Inc.") %>% 
  filter(!is.na(recipient_latitude)) #tidyverse

att_sl <- subset(df_e_rate, subset = holdingcompanyname == 'AT&T INC' | holdingcompanyname == "AT&T, Inc.") #baseR

att_sl <- att_sl %>% filter(!is.na(recipient_latitude))

att_sl <- SpatialPoints(att_sl [,c("recipient_longitude", "recipient_latitude")], proj4string=CRS(wgs.84))

att_sl <- spTransform(att_sl, CRS("+init=epsg:5070"))

plot(att_sl)  #to check the display
plot(florida_prj, add= TRUE)
---
anhp_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Advanced/Newhouse Partnership')

anhp_sl <- anhp_sl %>% filter(!is.na(Recipient.Latitude))

anhp_sl <- SpatialPoints(anhp_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

anhp_sl <- spTransform(anhp_sl, CRS("+init=epsg:5070"))

plot(anhp_sl)  #to check the display 

---
anixter_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Anixter, INc')

anixter_sl <- anixter_sl %>% filter(!is.na(Recipient.Latitude))

anixter_sl <- SpatialPoints(anixter_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

anixter_sl <- spTransform(anixter_sl, CRS("+init=epsg:5070"))

plot(anixter_sl)  #to check the display  
  
---
  
abf_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Atlantic Broadband Finance, LLC')

abf_sl <- abf_sl %>% filter(!is.na(Recipient.Latitude))

abf_sl <- SpatialPoints(abf_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

abf_sl <- spTransform(abf_sl, CRS("+init=epsg:5070"))

plot(abf_sl)
---
  
bci_sl <- subset(df_e_rate, subset = holdingcompanyname == 'BIRCH COMMUNICATIONS INC')

bci_sl <- bci_sl %>% filter(!is.na(Recipient.Latitude))

bci_sl <- SpatialPoints(bci_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

bci_sl <- spTransform(bci_sl, CRS("+init=epsg:5070"))
  
plot(bci_sl)
---
  
ctccp_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Coast to COast Computer Products Inc.')

ctccp_sl <- ctccp_sl %>% filter(!is.na(Recipient.Latitude))

ctccp_sl <- SpatialPoints(ctccp_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

ctccp_sl <- spTransform(ctccp_sl, CRS("+init=epsg:5070"))

plot(ctccp_sl)
---
ccc_sl <- subset(df_e_rate, subset = holdingcompanyname == 'COMCAST CABLE COMMUNICATIONS LLC')

ccc_sl <- ccc_sl %>% filter(!is.na(Recipient.Latitude))

ccc_sl <- SpatialPoints(ccc_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

ccc_sl <- spTransform(ccc_sl, CRS("+init=epsg:5070"))

plot(ccc_sl)  
---
coxcom_sl <- subset(df_e_rate, subset = holdingcompanyname == 'COXCOM, INC.')

coxcom_sl <- coxcom_sl %>% filter(!is.na(Recipient.Latitude))

coxcom_sl <- SpatialPoints(coxcom_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

coxcom_sl <- spTransform(coxcom_sl, CRS("+init=epsg:5070"))

plot(coxcom_sl)  #to check the display  
---
cch_sl <- subset(df_e_rate, subset = holdingcompanyname == 'CONSOLIDATED COMMUNICATIONS HOLDINGS, INC.')

cch_sl <- cch_sl %>% filter(!is.na(Recipient.Latitude))

cch_sl <- SpatialPoints(cch_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

cch_sl <- spTransform(cch_sl, CRS("+init=epsg:5070"))

plot(cch_sl)  #to check the display 

---
ccoc_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Crown Castle Operating Company')

ccoc_sl <- ccoc_sl %>% filter(!is.na(Recipient.Latitude))

ccoc_sl <- SpatialPoints(ccoc_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

ccoc_sl <- spTransform(ccoc_sl, CRS("+init=epsg:5070"))

plot(ccoc_sl)  #to check the display 

---
dti_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Derby Tech Inc')

dti_sl <- dti_sl %>% filter(!is.na(Recipient.Latitude))

dti_sl <- SpatialPoints(dti_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

dti_sl <- spTransform(dti_sl, CRS("+init=epsg:5070"))

plot(dti_sl)  #to check the display
---
dsrtech_sl <- subset(df_e_rate, subset = holdingcompanyname == 'DSR Technologies, Inc')

dsrtech_sl <- dsrtech_sl %>% filter(!is.na(Recipient.Latitude))

dsrtech_sl <- SpatialPoints(dsrtech_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

dsrtech_sl <- spTransform(dsrtech_sl, CRS("+init=epsg:5070"))

plot(dsrtech_sl)  #to check the display  
---
enoa_sl <- subset(df_e_rate, subset = holdingcompanyname == 'EDUCATION NETWORKS OF AMERICA INC')

enoa_sl <- enoa_sl %>% filter(!is.na(Recipient.Latitude))

enoa_sl <- SpatialPoints(enoa_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

enoa_sl <- spTransform(enoa_sl, CRS("+init=epsg:5070"))

plot(enoa_sl)  #to check the display 

---
gttc_sl <- subset(df_e_rate, subset = holdingcompanyname == 'GTT Communications Inc.')

gttc_sl <- gttc_sl %>% filter(!is.na(Recipient.Latitude))

gttc_sl <- SpatialPoints(gttc_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

gttc_sl <- spTransform(gttc_sl, CRS("+init=epsg:5070"))

plot(gttc_sl)  #to check the display  
---
harris_sl <- subset(df_e_rate, subset = holdingcompanyname == 'HARRIS CORPORATION')

harris_sl <- harris_sl %>% filter(!is.na(Recipient.Latitude))

harris_sl <- SpatialPoints(harris_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

harris_sl <- spTransform(harris_sl, CRS("+init=epsg:5070"))

plot(harris_sl)  #to check the display   
---
it_simp_sl <- subset(df_e_rate, subset = holdingcompanyname == 'IT Simplicity, Inc')

it_simp_sl <- it_simp_sl %>% filter(!is.na(Recipient.Latitude))

it_simp_sl <- SpatialPoints(it_simp_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

it_simp_sl <- spTransform(it_simp_sl, CRS("+init=epsg:5070"))

plot(it_simp_sl)  #to check the display   
---
i_venture_sl <- subset(df_e_rate, subset = holdingcompanyname == 'iVenture Solutions, Inc.')

i_venture_sl <- i_venture_sl %>% filter(!is.na(Recipient.Latitude))

i_venture_sl <- SpatialPoints(i_venture_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

i_venture_sl <- spTransform(i_venture_sl, CRS("+init=epsg:5070"))

plot(i_venture_sl)  #to check the display  
---
ld_telecom_sl <- subset(df_e_rate, subset = holdingcompanyname == 'LD TELECOM INC')

ld_telecom_sl <- ld_telecom_sl %>% filter(!is.na(Recipient.Latitude))

ld_telecom_sl <- SpatialPoints(ld_telecom_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

ld_telecom_sl <- spTransform(ld_telecom_sl, CRS("+init=epsg:5070"))

plot(ld_telecom_sl) 
---
nextiva_sl <- subset(df_e_rate, subset = holdingcompanyname == 'NEXTIVA INC')

nextiva_sl <- nextiva_sl %>% filter(!is.na(Recipient.Latitude))

nextiva_sl <- SpatialPoints(nextiva_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

nextiva_sl <- spTransform(nextiva_sl, CRS("+init=epsg:5070"))

plot(nextiva_sl)
---
pnd_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Panhandle Network & Data, LLC')

pnd_sl <- pnd_sl %>% filter(!is.na(Recipient.Latitude))

pnd_sl <- SpatialPoints(pnd_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

pnd_sl <- spTransform(pnd_sl, CRS("+init=epsg:5070"))

plot(pnd_sl)
---
pci_sl <- subset(df_e_rate, subset = holdingcompanyname == 'PC CONNECTION INC')

pci_sl <- pci_sl %>% filter(!is.na(Recipient.Latitude))

pci_sl <- SpatialPoints(pci_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

pci_sl <- spTransform(pci_sl, CRS("+init=epsg:5070"))

plot(pci_sl)
---
pres_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Presidio, Inc.')

pres_sl <- pres_sl %>% filter(!is.na(Recipient.Latitude))

pres_sl <- SpatialPoints(pres_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

pres_sl <- spTransform(pres_sl, CRS("+init=epsg:5070"))

plot(pres_sl)

---
softbank_sl <- subset(df_e_rate, subset = holdingcompanyname == 'SOFTBANK CORP')

softbank_sl <- softbank_sl %>% filter(!is.na(Recipient.Latitude))

softbank_sl <- SpatialPoints(softbank_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

softbank_sl <- spTransform(softbank_sl, CRS("+init=epsg:5070"))

plot(softbank_sl) 

---
sdcs_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Synergetics Diversified Computer Services, Inc')

sdcs_sl <- sdcs_sl %>% filter(!is.na(Recipient.Latitude))

sdcs_sl <- SpatialPoints(sdcs_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

sdcs_sl <- spTransform(sdcs_sl, CRS("+init=epsg:5070"))

plot(sdcs_sl)
---
tmobile_sl <- subset(df_e_rate, subset = holdingcompanyname == 'T MOBILE USA INC')

tmobile_sl <- tmobile_sl %>% filter(!is.na(Recipient.Latitude))

tmobile_sl <- SpatialPoints(tmobile_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

tmobile_sl <- spTransform(tmobile_sl, CRS("+init=epsg:5070"))

plot(tmobile_sl)
---
twce_sl <- subset(df_e_rate, subset = holdingcompanyname == 'TIME WARNER CABLE ENTERPRISES LLC')

twce_sl <- twce_sl %>% filter(!is.na(Recipient.Latitude))

twce_sl <- SpatialPoints(twce_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

twce_sl <- spTransform(twce_sl, CRS("+init=epsg:5070"))

plot(twce_sl)

---
uniti_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Uniti Group Inc.')

uniti_sl <- uniti_sl %>% filter(!is.na(Recipient.Latitude))

uniti_sl <- SpatialPoints(uniti_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

uniti_sl <- spTransform(uniti_sl, CRS("+init=epsg:5070"))

plot(uniti_sl)  
---
verizon_sl <- subset(df_e_rate, subset = holdingcompanyname == 'VERIZON COMMUNICATIONS INC'| holdingcompanyname == 'Verizon Communications, Inc.')

verizon_sl <- verizon_sl %>% filter(!is.na(Recipient.Latitude))

verizon_sl <- SpatialPoints(verizon_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

verizon_sl <- spTransform(verizon_sl, CRS("+init=epsg:5070"))

plot(verizon_sl)   
---
zayo_sl <- subset(df_e_rate, subset = holdingcompanyname == 'ZAYO GROUP HOLDINGS INC')

zayo_sl <- zayo_sl %>% filter(!is.na(Recipient.Latitude))

zayo_sl <- SpatialPoints(zayo_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

zayo_sl <- spTransform(zayo_sl, CRS("+init=epsg:5070"))

plot(zayo_sl)
  
---  
windstream_sl <- subset(df_e_rate, subset = holdingcompanyname == 'Windstream Services, LLC')

windstream_sl <- windstream_sl %>% filter(!is.na(Recipient.Latitude))

windstream_sl <- SpatialPoints(windstream_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

windstream_sl <- spTransform(windstream_sl, CRS("+init=epsg:5070"))

plot(windstream_sl)  #to check the display

---
  centurylink_sl <- subset(df_e_rate, subset = holdingcompanyname == 'CENTURYLINK' | holdingcompanyname == 'CenturyLink Inc.')

centurylink_sl <- centurylink_sl %>% filter(!is.na(Recipient.Latitude))

centurylink_sl <- SpatialPoints(centurylink_sl [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))

centurylink_sl <- spTransform(centurylink_sl, CRS("+init=epsg:5070"))

plot(centurylink_sl)

# ------ Reading in Spatial Points objects as ppp and generating KDE plots 

 #Atlantic Broadband Finance 
abf_sl_ppp <- as.ppp(abf_sl)
w_abf <- as.owin(florida_prj)
Window(abf_sl_ppp)<- w_abf

K_abf_sl <- density(abf_sl_ppp,  kernel = "quartic", sigma=50000) 
plot(K_abf_sl)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #Advanced Newhouse Partnership

anhp_sl_ppp <- as.ppp(anhp_sl)
w_anhp <- as.owin(florida_prj)
Window(anhp_sl_ppp)<- w_anhp

K_anhp_sl <- density(anhp_sl_ppp,  kernel = "quartic", sigma=50000) 
plot(K_anhp_sl)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #Anixter Inc
anixter_sl_ppp <- as.ppp(anixter_sl)
w_anixter <- as.owin(florida_prj)
Window(anixter_sl_ppp)<- w_anixter

K_anixter_sl <- density(anixter_sl_ppp,  kernel = "quartic", sigma=50000) 
plot(K_anixter_sl)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #AT&T 

att_sl_ppp <- as.ppp(att_sl)
w_att <- as.owin(florida_prj)
Window(att_sl_ppp)<- w_att

K_att_sl <- density(att_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_att_sl)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #Birch Communications Inc.
bci_sl_ppp <- as.ppp(bci_sl)
w_bci <- as.owin(florida_prj)
Window(bci_sl_ppp)<- w_bci

K_bci_sl <- density(bci_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_bci_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- # Comcast Cable Communications
ccc_sl_ppp <- as.ppp(ccc_sl)
w_ccc <- as.owin(florida_prj)
Window(ccc_sl_ppp)<- w_ccc

K_ccc_sl <- density(ccc_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_ccc_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- #Consolidate Communications Holdings
cch_sl_ppp <- as.ppp(cch_sl)
w_cch <- as.owin(florida_prj)
Window(cch_sl_ppp)<- w_cch

K_cch_sl <- density(cch_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_cch_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- # Coxcom Inc.

coxcom_sl_ppp <- as.ppp(coxcom_sl)
w_coxcom <- as.owin(florida_prj)
Window(coxcom_sl_ppp)<- w_coxcom

K_coxcom_sl <- density(coxcom_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_coxcom_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 



--- # Crown Castle Operating Company 
ccoc_sl_ppp <- as.ppp(ccoc_sl)
w_ccoc <- as.owin(florida_prj)
Window(ccoc_sl_ppp)<- w_ccoc

K_ccoc_sl <- density(ccoc_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_ccoc_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- # Consolidated Communications Holdings Inc.
  
cch_sl_ppp <- as.ppp(cch_sl)
w_cch <- as.owin(florida_prj)
Window(cch_sl_ppp)<- w_cch

K_cch_sl <- density(cch_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_cch_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- # Derby Tech Inc.
  
dti_sl_ppp <- as.ppp(dti_sl)
w_dti <- as.owin(florida_prj)
Window(dti_sl_ppp)<- w_dti

K_dti_sl <- density(dti_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_dti_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- # DSR Technologies, Inc.
dsrtech_sl_ppp <- as.ppp(dsrtech_sl)
w_dsrtech <- as.owin(florida_prj)
Window(dsrtech_sl_ppp)<- w_dsrtech

K_dsrtech_sl <- density(dsrtech_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_dsrtech_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- #Education Networks of America, Inc.
enoa_sl_ppp <- as.ppp(enoa_sl)
w_enoa <- as.owin(florida_prj)
Window(enoa_sl_ppp)<- w_enoa

K_enoa_sl <- density(enoa_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_enoa_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)  


--- # GTT Communications Inc.
gttc_sl_ppp <- as.ppp(gttc_sl)
w_gttc <- as.owin(florida_prj)
Window(gttc_sl_ppp)<- w_gttc

K_gttc_sl <- density(gttc_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_gttc_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)


--- #Harris Corporation
harris_sl_ppp <- as.ppp(harris_sl)
w_harris <- as.owin(florida_prj)
Window(harris_sl_ppp)<- w_harris

K_harris_sl <- density(harris_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_harris_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)
  
--- #IT Simplicity Inc
it_simp_sl_ppp <- as.ppp(it_simp_sl)
w_it_simp <- as.owin(florida_prj)
Window(it_simp_sl_ppp)<- w_it_simp

K_it_simp_sl <- density(it_simp_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_it_simp_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)
 
--- #iVenture Solutions, Inc.
i_venture_sl_ppp <- as.ppp(i_venture_sl)
w_i_venture <- as.owin(florida_prj)
Window(i_venture_sl_ppp)<- w_i_venture

K_i_venture_sl <- density(i_venture_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_i_venture_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #LD Telecom Inc.
ld_telecom_sl_ppp <- as.ppp(ld_telecom_sl)
w_ld_telecom <- as.owin(florida_prj)
Window(ld_telecom_sl_ppp)<- w_ld_telecom

K_ld_telecom_sl <- density(ld_telecom_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_ld_telecom_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)
--- #Nextiva Inc
nextiva_sl_ppp <- as.ppp(nextiva_sl)
w_nextiva <- as.owin(florida_prj)
Window(nextiva_sl_ppp)<- w_nextiva

K_nextiva_sl <- density(nextiva_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_nextiva_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)
  

--- #Panhandle Network & Data LLC

pnd_sl_ppp <- as.ppp(pnd_sl)
w_pnd <- as.owin(florida_prj)
Window(pnd_sl_ppp)<- w_pnd

K_pnd_sl <- density(pnd_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_pnd_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- #PC COnnection Inc
pci_sl_ppp <- as.ppp(pci_sl)
w_pci <- as.owin(florida_prj)
Window(pci_sl_ppp)<- w_pci

K_pci_sl <- density(pci_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_pci_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 


---#Presidio Inc
pres_sl_ppp <- as.ppp(pres_sl)
w_pres <- as.owin(florida_prj)
Window(pres_sl_ppp)<- w_pres

K_pres_sl <- density(pres_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_pres_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 


--- #Softbank Corp
softbank_sl_ppp <- as.ppp(softbank_sl)
w_softbank <- as.owin(florida_prj)
Window(softbank_sl_ppp)<- w_softbank

K_softbank_sl <- density(softbank_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_softbank_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 
--- #Synergetics Diversified Computer Service Inc.

sdcs_sl_ppp <- as.ppp(sdcs_sl)
w_sdcs <- as.owin(florida_prj)
Window(sdcs_sl_ppp)<- w_sdcs

K_sdcs_sl <- density(sdcs_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_sdcs_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE) 

--- #Tmobile USA INc.
tmobile_sl_ppp <- as.ppp(tmobile_sl)
w_tmobile <- as.owin(florida_prj)
Window(tmobile_sl_ppp)<- w_sdcs

K_tmobile_sl <- density(tmobile_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_tmobile_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)


--- #Time Warner Cable Enterprises LLC
twce_sl_ppp <- as.ppp(twce_sl)
w_twce <- as.owin(florida_prj)
Window(twce_sl_ppp)<- w_twce

K_twce_sl <- density(twce_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_twce_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #Uniti Group Inc.
uniti_sl_ppp <- as.ppp(uniti_sl)
w_uniti <- as.owin(florida_prj)
Window(uniti_sl_ppp)<- w_uniti

K_uniti_sl <- density(twce_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_uniti_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)

--- #Verizon Communications Inc.
verizon_sl_ppp <- as.ppp(verizon_sl)
w_verizon <- as.owin(florida_prj)
Window(verizon_sl_ppp)<- w_verizon

K_verizon_sl <- density(verizon_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_verizon_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)  


--- #Zayo Group Holdings, Inc.
zayo_sl_ppp <- as.ppp(zayo_sl)
w_zayo <- as.owin(florida_prj)
Window(zayo_sl_ppp)<- w_zayo

K_zayo_sl <- density(zayo_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_zayo_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)  
--- #Windstream Services LLC
 
windstream_sl_ppp <- as.ppp(windstream_sl)
w_windstream <- as.owin(florida_prj)
Window(windstream_sl_ppp)<- w_windstream

K_windstream_sl <- density(windstream_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_windstream_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)  

--- #CenturyLink
centurylink_sl_ppp <- as.ppp(centurylink_sl)
w_centurylink <- as.owin(florida_prj)
Window(centurylink_sl_ppp)<- w_centurylink

K_centurylink_sl <- density(centurylink_sl_ppp, kernel= "quartic", sigma=50000)
plot(K_centurylink_sl, axes=F)
plot(florida_prj, border= "black", lwd=3, add=TRUE)  

---

temple_terrace <- data.frame(read.csv("X:/Collaboration/EPMO/Data Strategy/GIS/Projects/Enterprise/Carrier_Service_Areas/Temple_Terrace_ERATE.csv"))


temple_terrace <- SpatialPoints(temple_terrace [,c("Recipient.Longitude", "Recipient.Latitude")], proj4string=CRS(wgs.84))
temple_terrace <- spTransform(temple_terrace, CRS("+init=epsg:5070"))


plot(temple_terrace)
plot(florida_prj, add=TRUE)

  
  

  