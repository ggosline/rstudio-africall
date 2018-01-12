require(sf)
require(data.table)
require(rnaturalearth)
require(leaflet)
require(htmltools)
require(RPostgreSQL)
countries <- rnaturalearth::ne_countries('large')
countries <- st_as_sf(countries)
land <- rnaturalearth::ne_download(scale=10, type='land', category='physical')
land <- st_as_sf(land)

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host ='kewwta.chh53kepvixq.eu-west-2.rds.amazonaws.com', port=1433, dbname='Rainbio', user='ggosline', password ='Uvariops1s')
AfricaAll <- st_read_db(con, 'africaall')
dbDisconnect(con)

AfricaAll <- as.data.table(AfricaAll)
AfricaAll <- AfricaAll[complete.cases(AfricaAll[,.(decimalLatitude,decimalLongitude)])]

AfricaAll[,id:=1:nrow(AfricaAll)]
AfricaAll <- st_as_sf(AfricaAll, coords=c('decimalLongitude','decimalLatitude'), crs=4326, agr="constant")

AfricaAll <- 
NAspecs <- AfricaAll[is.na(AfricaAll$countryCode),]
pttocountry <- st_join(AfricaAll[,1:3], countries[,'iso_a3'])
AfricaAll[NAcountry$id,countryCode := NAcountry$countryCode]

NAcountry[,'countryCode'] <- pttocountry[,'iso_a3']
AfricaAll[NAcountry$id,countryCode := NAcountry$countryCode]

NAspecs <- st_as_sf(NAspecs, coords=c('decimalLongitude','decimalLatitude'), crs=4326, agr="constant")
AcanthAll <- AfricaAll[AfricaAll$family == 'Acanthaceae',]

m <- leaflet(AcanthAll[1:10,]) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions(),
popup = ~((sprintf(
"<div><strong>%s</strong><br/>%s %s <br/> %s</div>",
species, recordedby, recordnumber, eventdate))))
