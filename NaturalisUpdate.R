library("jsonlite", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("data.table", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("rlist", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("stringi", lib.loc="C:/Program Files/R/R-3.4.2/library")


setwd("T:/Cameroon/GGosline/Specimen Data Processing")

#count <- fromJSON("http://api.biodiversitydata.nl/v2/specimen/count/?sourceSystem.code=BRAHMS&gatheringEvent.country=Ivory%20Coast&recordBasis=Herbarium%20sheet&gatheringEvent.continent=Africa")
count <- fromJSON("http://api.biodiversitydata.nl/v2/specimen/count/?sourceSystem.code=BRAHMS&recordBasis=Herbarium%20sheet&gatheringEvent.continent=Africa")
#baseURL = "http://api.biodiversitydata.nl/v2/specimen/query/?sourceSystem.code=BRAHMS&recordBasis=Herbarium%20sheet&gatheringEvent.continent=Africa"
baseURL = "http://api.biodiversitydata.nl/v2/specimen/query/?sourceSystem.code=BRAHMS&recordBasis=Herbarium%20sheet&gatheringEvent.continent=Africa"

readBRURL <- "http://api.biodiversitydata.nl/v2/specimen/query/?sourceSystem.code=BRAHMS&assemblageID=${BRID}@BRAHMS"

fields <- paste('unitID','collectorsFieldNumber','assemblageID','gatheringEvent.dateTimeBegin','identifications.scientificName.fullScientificName',sep=',')

getNat <- function(BRID) {
  URL <- stringr::str_interp(readBRURL)
  specs = fromJSON(paste0(URL, "&_fields=", fields))[['resultSet']][['item']]
  if (is.null(specs)) return(NULL)
  specs <- as.data.table(specs)
  eventDates <- specs$gatheringEvent
  specs[,eventdate:=eventDates]
  if ("identifications" %in% names(specs)) {
    snames <- list.map(specs[,identifications],.[1])
    snames <- list.map(snames,.$scientificName$fullScientificName)
    specs[,scientificName:=snames]
    }
  collno <- stri_match_first_regex(specs[,collectorsFieldNumber],'(.+)\\s(\\s\\d*|s.n.)')
  specs[,collector := collno[,2]]
  specs[,collno := collno[,3]]
  bid = stri_replace_first_fixed(specs[,assemblageID], "@BRAHMS","")
  specs[,brahmsID:=bid]
  specs[,gatheringEvent:=NULL]
  specs[,identifications:=NULL]
  return(specs)
}

print(count)
ntl = list()

for (brid in nodata[191350:200000,BRid]) {
  print(brid)
  gn <- getNat(brid)
  if (!is.null(gn)) {ntl <- list.append(ntl, gn)}
}

Natspecs <- rbindlist(ntl, fill=TRUE)
Natspecs[unlist(lapply(Natspecs[,scientificName],is.null)),scientificName:=as.list(NA)]
Natspecs[,scientificName:=unlist(Natspecs[,scientificName])]
Natspecs <- unique(Natspecs, by=c("collectorsFieldNumber"))
fwrite(Natspecs,'Natspecs3a.csv')

Natspecs <- rbindlist(lapply(list.files(,pattern="Natspecs"), fread)
NS <- unique(Natspecs,, by=c("collectorsFieldNumber")
fwrite(NS,"Natspecs.csv")
