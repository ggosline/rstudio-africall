setwd("C:/wta/Rwork/Guinea")
library("ConR", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("RODBCDBI", lib.loc="C:/Program Files/R/R-3.3.3/library")
library("maptools", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("data.table", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("pryr", lib.loc="C:/Program Files/R/R-3.4.2/library")

Cameroon.protected <- readShapePoly("T:\\Cameroon\\ArcViewData\\Protected Areas\\Cameroon_Protected_areas\\Protected_areas.shp",proj4string=CRS("+proj=longlat +datum=WGS84"))

con <- dbConnect(RODBCDBI::ODBC(), dsn="Rainbio")
AfricaAll <- dbReadTable(con,"AfricaAll")
Alldt <- data.table(AfricaAll)
AllSpecies <-Alldt[,.(count=.N),by=.(tax_sp_level)]

GINAll <- AfricaAll[AfricaAll$countryCode=="GIN",]
GINdt <- data.table(GINAll)
GINSpecies <-GINdt[is.na(Cult_Intro),.(count=.N),by=.(tax_sp_level)]
setkey(GINdt,tax_sp_level)
setkey(GINSpecies,tax_sp_level)
setkey(Alldt,tax_sp_level)
AllGINSpecimens <- Alldt[GINSpecies, nomatch=0]

AllGINSpecimens <- AllGINSpecimens[is.na(Cult_Intro)]
EOOin <- AllGINSpecimens[!is.na(tax_sp_level),.(decimalLatitude, decimalLongitude, tax_sp_level, family, substr(eventdate,1,4))]

CamAll <- AfricaAll[AfricaAll$countryCode=="CMR",]
Camdt <- data.table(CamAll)
CamSpecies <-Camdt[is.na(Cult_Intro),.(count=.N),by=.(tax_sp_level)]

setkey(Camdt,tax_sp_level)
setkey(CamSpecies,tax_sp_level)
setkey(Alldt,tax_sp_level)
AllCamSpecimens <- Alldt[CamSpecies, nomatch=0]
AllCamSpecimens <- AllCamSpecimens[is.na(Cult_Intro)]
EOOin <- AllCamSpecimens[!is.na(tax_sp_level),.(decimalLatitude, decimalLongitude, tax_sp_level, family, substr(eventdate,1,4))]

colnames(EOOin)[5] <- "coly"
EOOin <- data.table(EOOin)
EOOin[,'coly'] <- as.numeric(EOOin[,coly])

families <- EOOin[order(family),(.N),by=family][,1]

for (f in families$family[as.character(families$family) =="Acanthaceae"]) {
     print(f)
    if (!dir.exists(as.character(f))) dir.create(as.character(f))
    setwd(as.character(f))
    if (nrow(EOOin[family==f]) > 0) {
     IUCN.eval(EOOin[family==f], verbose=TRUE, showWarnings = TRUE, write_shp = TRUE)}
     #IUCN.eval(EOOin[family==f], protec.areas = Cameroon.protected, ID_shape_PA = "nom_ap", verbose=FALSE, showWarnings = FALSE, write_shp = TRUE)}
   setwd("..")
   print(mem_used())
}

files <- list.files(pattern = "\\IUCN_results.csv$",recursive=T)
DF <- read.csv(files[1])

for (f in files[-1]) DF <- rbind(DF, read.csv(f))   
write.csv(DF, "species_IUCN.csv", row.names=FALSE, quote=FALSE)

species.family <- EOOin[,.(family),by=.(family,tax_sp_level)]

coordinates(AfricaAll) <- ~decimalLongitude + decimalLatitude
crs(AfricaAll) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


