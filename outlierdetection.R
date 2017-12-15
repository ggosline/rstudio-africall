library("dbscan", lib.loc="C:/Program Files/R/R-3.4.2/library")
library("sp", lib.loc="C:/Program Files/R/R-3.4.2/library")
require('data.table')
require('raster')

plot_glosh <- function(x, glosh){
  plot(x, pch = ".", main = "GLOSH (k=3)")
  points(x, cex = glosh*3, pch = 1, col="red")
  text(x[glosh > 0.80,], labels = round(glosh, 3)[glosh > 0.80], pos = 3)}

projEAC <- crs("+proj=cea +lon_0=Central Meridian+lat_ts=Standard Parallel+x_0=False Easting+y_0=False Northing +ellps=WGS84")

DATA <- Alldt[tax_sp_level=="Whitfieldia lateritia" & !is.na(decimalLatitude) & !is.na(decimalLongitude)]
EOOin <- DATA[,.(decimalLatitude, decimalLongitude, tax_sp_level, family, substr(eventdate,1,4))]

XY <- EOOin[, c(2:1)]

coordEAC <- as.data.frame(matrix(unlist(rgdal::project(as.matrix(XY), proj = as.character(projEAC), inv = FALSE)), ncol = 2))
rownames(coordEAC) <- seq(1, nrow(coordEAC), 1)

cl <- hdbscan(coordEAC, minPts = 5)
plot(coordEAC, col=cl$cluster+1)
DATA[, clus :=cl$cluster]

