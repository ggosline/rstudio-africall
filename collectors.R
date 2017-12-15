
library("stringi", lib.loc="C:/Program Files/R/R-3.4.2/library")

unescape_html <- function(str){
   xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

flatten <- function(str){
  iconv(str, from='UTF-8', to="ASCII//TRANSLIT")}

setwd("T:/Cameroon/GGosline/Collectors")

collectors[,('canonicalname') := .(stri_extract(text,regex="<i>.*</i>"))]
collectors[,('canonicalname') := .(unescape_html(canonicalname)),by=sequence]

txtcols = c('surname', 'firstnames', 'nameref', 'text', 'references', 'canonicalname')
for (j in txtcols) set(collectors, j = j, value = sapply(collectors[[j]],unescape_html))

collectors[,canonicalname := sapply(canonicalname, flatten)]

fwrite(collectors, file='collectors_out.csv')

library("RODBC", lib.loc="C:/Program Files/R/R-3.4.2/library")
collcon <- RODBC::odbcConnectAccess2007("T:/Cameroon/Database/AfricaCollectors.accdb")

sqlSave(collcon, collectors, 'canonical', varTypes = c(text='LONGCHAR', references='LONGCHAR'))
