library(RPostgreSQL)
try(conn <- dbConnect(PostgreSQL(), dbname = "postgis"))
if (exists("conn") && !inherits(conn, "try-error")) {
  x = st_read_db(conn, "meuse", query = "select * from meuse limit 3;")
  x = st_read_db(conn, table = "public.meuse")
  print(st_crs(x)) # SRID resolved by the database, not by GDAL!
  dbDisconnect(conn)
}

library("data.table", lib.loc="C:/Program Files/R/R-3.4.2/library")
NS[,catalogNumber:=paste("JW", brahmsID, sep="")]
setkey(NS,catalogNumber)

africaall <- as.data.table(africall)
setkey(africaall,catalogNumber)
match <- africaall[NS, nomatch=0]

NS[,eventdate:=str_sub(eventdate,1,10)]
upd <- NS[,.(catalogNumber,collector, collno, eventdate, unitID)]

africaupd[upd, ":="(eventdate=i.eventdate, recordedby=collector, recordnumber=collno, KewBar=unitID), on=.(catalogNumber)]

africaall <- st_sf(africaupd)

con <- dbConnect("PostgreSQL", dbname="Rainbio", port=1433, user="ggosline", password="Uvariops1s", host="kewwta.chh53kepvixq.eu-west-2.rds.amazonaws.com")
st_write_db(con, africaall, table="africaall")
st_write_db(con, africaall, table="africaalln")
Acanthall <- st_read_db(con, query="Select * from africaalln where Family='Acanthaceae';")
dbDisconnect(con)