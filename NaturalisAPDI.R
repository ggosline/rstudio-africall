library("jsonlite", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("data.table", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("rlist", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("stringi", lib.loc="C:/Program Files/R/R-3.4.2/library")


setwd("T:/Cameroon/GGosline/Specimen Data Processing")

count <- fromJSON("http://api.biodiversitydata.nl/v2/specimen/count/?sourceSystem.code=BRAHMS&gatheringEvent.country=Ivory%20Coast&recordBasis=Herbarium%20sheet&gatheringEvent.continent=Africa")

baseURL = "http://api.biodiversitydata.nl/v2/specimen/query/?sourceSystem.code=BRAHMS&gatheringEvent.country=Ivory%20Coast&recordBasis=Herbarium%20sheet&gatheringEvent.continent=Africa"

fields <- paste('unitID','collectorsFieldNumber','assemblageID','gatheringEvent.dateTimeBegin','identifications.scientificName.fullScientificName',sep=',')


getNat <- function(gfrom=0,gsize=10000) {
  specs = fromJSON(paste0(baseURL, "&_fields=", fields, "&_size=", gsize, "&_from=", gfrom))[['resultSet']][['item']]
  specs <- as.data.table(specs)
  eventDates <- specs$gatheringEvent
  specs[,eventdate:=eventDates]
  snames <- list.map(specs[,identifications],.[1])
  snames <- list.map(snames,.$scientificName$fullScientificName)
  specs[,scientificName:=snames]
  collno <- stri_match_first_regex(specs[,collectorsFieldNumber],'(.+)\\s(\\s\\d*|s.n.)')
  specs[,collector := collno[,2]]
  specs[,collno := collno[,3]]
  bid = stri_replace_first_fixed(specs[,assemblageID], "@BRAHMS","")
  specs[,brahmsID:=bid]
  specs[,gatheringEvent:=NULL]
  specs[,identifications:=NULL]
  specs
}

ntl = list()

for (strt in 0:3) {
  print(strt)
  ntl[[strt+1]] = getNat(strt*10000,10000)
}

Natspecs <- rbindlist(ntl)
Natspecs[unlist(lapply(Natspecs[,scientificName],is.null)),scientificName:=as.list(NA)]
Natspecs[,scientificName:=unlist(Natspecs[,scientificName])]
fwrite(Natspecs,'Natspecs.csv')