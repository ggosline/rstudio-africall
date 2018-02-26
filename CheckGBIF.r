# Check GBIF submission files
library(readxl)

library(sf)
library(sp)
library(data.table)
library(mapview)

# # setwd("T:/Cameroon/Guinea HNG and Redlisting/GBIF data cleaning")
# setwd("T:\\Cameroon\\Database\\Data cleaning")
# 
# guinea <- readRDS("T:/Cameroon/ArcViewData/Borders/GIN_adm0.rds")
# cameroon <- readRDS("T:/Cameroon/ArcViewData/Borders/CMR_adm0.rds")
# 
# guinea <- st_as_sf(guinea, crs=4326)
# cameroon <- st_as_sf(cameroon, crs=4326)
# 
# specs <- read_excel(file.choose(), sheet = "Occurrence", col_names = TRUE)
# specs <- data.table(specs)
# 
# specs[(decimalLongitude==0.0 | decimalLatitude==0.0), error:="zero coordinates"]
# 
# specimens <- st_as_sf(specs, coords=c("decimalLongitude", "decimalLatitude"), crs=4326, agr="constant", na.fail=FALSE, remove=FALSE)
# 
# # ints <- st_intersects(specimens, guinea, sparse=FALSE)
# ints <- st_intersects(camspecs, cameroon, sparse=FALSE)
# 
# notinguinea <- specimens[apply(ints,1,function(r) !any(r)),]
# notincameroon <- camspecs[apply(ints,1,function(r) !any(r)),]
# 
# notinguinea <- data.table(notinguinea)
# 
# specs[notinguinea[is.na(notinguinea$error)], error:="not in country", on="occurrenceID"]
# 
# fwrite(specs, "GBIFInputErrors.csv")

alternatives(campts, world=wrld_simpl, rst = dem, ext="p")
errorcheck(world,,campts,countryfield="countryCode", Species="Species")
GuineaTIPAs <- read_sf("T:\\Cameroon\\Cameroon\\TIPAs\\Guinea\\Guinea_TIPAs.shp")
buffdist <- set_units(3,km)
TIPAsB <- st_buffer(st_transform(GuineaTIPAs, 4087), buffdist)
