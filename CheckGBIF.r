# Check GBIF submission files
library(readxl)

library(sf)
library(sp)
library(data.table)

setwd("T:/Cameroon/Guinea HNG and Redlisting/GBIF data cleaning")

guinea <- readRDS("T:/Cameroon/ArcViewData/Borders/GIN_adm0.rds")

guinea <- st_as_sf(guinea, crs=4326)

specs <- read_excel(file.choose(), sheet = "Occurrence", col_names = TRUE)
specs <- data.table(specs)

specs[(decimalLongitude==0.0 | decimalLatitude==0.0), error:="zero coordinates"]

specimens <- st_as_sf(specs[is.na(error)], coords=c("decimalLongitude", "decimalLatitude"), crs=4326, agr="constant", na.fail=FALSE)

ints <- st_intersects(specimens, guinea, sparse=FALSE)

notinguinea <- specimens[apply(ints,1,function(r) !any(r)),]

specs[notinguinea, error:="not in guinea", on="occurrenceID"]

fwrite(specs, "GBIFInputErrors.csv")


