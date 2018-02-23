library(sf)
library(sp)
library(data.table)
library(mapview)
library("units", lib.loc="C:/Program Files/R/R-3.4.2/library")

GuineaTIPAs <- read_sf("T:\\Cameroon\\Cameroon\\TIPAs\\Guinea\\Guinea_TIPAs.shp")
buffdist <- set_units(5,km)
TIPAsB <- st_buffer(st_transform(GuineaTIPAs, 4087), buffdist)
TIPAsB <- st_transform(TIPAsB, 4326)

con <- dbConnect(RPostgres::Postgres(), dbname="rainbio", port=1433, user="ggosline", password="Uvariops1s", host="kewwta.chh53kepvixq.eu-west-2.rds.amazonaws.com")
ginspecs <- st_read_db(con, query="Select * from africaall where africaall.\"countryCode\" = 'GIN';")
dbDisconnect(con)

ginspecs <- data.table(ginspecs)
ginthreat <- ginspecs[IUCN %in% c("CR", "EN", "VU"),]

ginthreat <- st_sf(ginthreat)
tipasSpecst <- st_join(ginthreat, TIPAsB)
tipasSpecst <- data.table(tipasSpecst)
tipaThreatSpeciesList <- tipasSpecst[!is.na(NAME),.N,keyby=.(NAME,species,IUCN)]
fwrite(tipaThreatSpeciesList, "T:\\Cameroon\\Cameroon\\TIPAs\\Guinea\\tipaThreatSpeciesList.csv")

mapview(GuineaTIPAs) + mapview(ginthreat, zcol="IUCN", cex=3, alpha=0.6) 