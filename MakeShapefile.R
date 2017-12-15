require(sf)
require(data.table)
require(rnaturalearth)
require(leaflet)
require(htmltools)
countries <- rnaturalearth::ne_countries('large')
countries <- st_as_sf(countries)
land <- rnaturalearth::ne_download(scale=10, type='land', category='physical')
land <- st_as_sf(land)


AfricaAll <- as.data.table(AfricaAll)
AfricaAll <- AfricaAll[complete.cases(AfricaAll[,.(decimalLatitude,decimalLongitude)])]

AfricaAll[,id:=1:nrow(AfricaAll)]
AfricaAll <- st_as_sf(AfricaAll, coords=c('decimalLongitude','decimalLatitude'), crs=4326, agr="constant")

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
