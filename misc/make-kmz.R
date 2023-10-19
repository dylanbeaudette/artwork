library(rgdal)
library(sp)
library(plotKML)

# load and transform CRS of soilveg data
sv <- readOGR(dsn='SHP', layer='SoilVeg_3_16_2016_AJP', stringsAsFactors = FALSE)
sv <- spTransform(sv, CRS('+proj=longlat +datum=WGS84'))

# load and transform CRS of geology data
geo <- readOGR(dsn='SHP', layer='CA630_Geology_2_3_2016AJP', stringsAsFactors = FALSE)
geo <- spTransform(geo, CRS('+proj=longlat +datum=WGS84'))

# convert to KML, coloring polygons with selected attributes
kml(sv, folder.name='SoilVeg', file.name='soilveg.kml', alpha=0.5, colour=Parent_Mat, balloon=TRUE)
kml(geo, folder.name='Geology', file.name='geology.kml', alpha=0.5, colour=GROUP, balloon=TRUE)

