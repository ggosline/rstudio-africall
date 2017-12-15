install.packages("finch")
library("data.table", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("finch", lib.loc="C:/Program Files/R/R-3.4.2/library")

setwd("T:/Cameroon/Database/Data entry files")

guinea <- dwca_read("T:/Cameroon/Database/Data entry files/Guinea-BID-DWCA.zip",read=FALSE)

occs <- fread(guinea$files$data_paths[1],encoding='UTF-8')

occs <- occs[,which(unlist(lapply(occs, function(x)!all(is.na(x))))),with=F] # remove empty cols

mm <- fread(guinea$files$data_paths[2],encoding='UTF-8')

setkey(occs,identifier)
setkey(mm,identifier)

if (nrow(mm) > 0) {merge(occs,mm, all.x=TRUE)}

fwrite(occs, file='guinea.csv')

library("RODBC", lib.loc="C:/Program Files/R/R-3.4.2/library")
con <- odbcConnect("pgWTA", uid="postgres", pwd="Uvaria", case="nochange")
africaall <- sqlFetch(con, "AfricaAll")
 
setkey(africaall,"catalogNumber")
setkey(occs,"catalogNumber")

matches<-africaall[occs]
duplicates <- matches[!is.na(countryCode)]

