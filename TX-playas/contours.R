
## paper from Zamir:
# https://www.sciencedirect.com/science/article/pii/S0140196310001849


library(aqp)
library(terra)
library(soilDB)
library(elevatr)


# TX155: https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.05415,-99.58514,z14
bb <- '-99.6457 34.0217,-99.6457 34.0896,-99.5294 34.0896,-99.5294 34.0217,-99.6457 34.0217'



# https://casoilresource.lawr.ucdavis.edu/soil-properties/?prop=texture_025&lat=34.3774&lon=-101.7197&z=9
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.47387,-102.12719,z13
# https://soilmap2-1.lawr.ucdavis.edu/mike/soilweb/taxa-grid/?lat=34.47387&lon=-102.12719&zoom=9&taxa=vertisols

# TX069
bb <- '-102.2633 34.4032,-102.2633 34.5341,-102.0142 34.5341,-102.0142 34.4032,-102.2633 34.4032'


# another interesting region nearby
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.53435,-101.25026,z14
bb <- '-101.3237 34.5037,-101.3237 34.5711,-101.1752 34.5711,-101.1752 34.5037,-101.3237 34.5037'




wkt <- sprintf("POLYGON((%s))", bb)
a <- vect(wkt, crs = 'epsg:4326')

texture_2550cm <- ISSR800.wcs(aoi = a, var = 'texture_2550cm')
plot(texture_2550cm, axes = FALSE)


m <- mukey.wcs(a, db = 'gSSURGO')

# extract RAT for thematic mapping
rat <- cats(m)[[1]]

# variables of interest
vars <- c("sandtotal_r", "silttotal_r", "claytotal_r")

p <-  get_SDA_property(property = vars,
                       method = "Dominant Component (Numeric)", 
                       mukeys = as.integer(rat$mukey),
                       top_depth = 0,
                       bottom_depth = 25, 
                       include_minors = TRUE, 
                       miscellaneous_areas = FALSE
                       )

# merge aggregate soil data into RAT
rat <- merge(rat, p, by.x = 'mukey', by.y = 'mukey', sort = FALSE, all.x = TRUE)

# requires that grid cell ID (mukey) be numeric
rat$mukey <- as.integer(rat$mukey)
levels(m) <- rat

ssc <- catalyze(m)[[vars]]

texture.class <- ssc[[1]]
names(texture.class) <- 'soil.texture'

# assign soil texture classes for the fine earth fraction
# using sand and clay percentages
values(texture.class) <- ssc_to_texcl(
  sand = values(ssc[['sandtotal_r']]), 
  clay = values(ssc[['claytotal_r']]), 
  droplevels = FALSE
)

plot(texture.class, col = hcl.colors(50), axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 0-25cm (RV)', mar = c(1, 1, 3, 2))


texture.rat <- read.csv('http://casoilresource.lawr.ucdavis.edu/800m_grids/RAT/texture_05.csv')

rat <- cats(texture.class)[[1]]

rat <- merge(rat, texture.rat[, c('class', 'hex', 'names')], by.x = 'label', by.y = 'class', all.x = TRUE, sort = FALSE)

rat <- rat[order(rat$value), ]
rat <- rat[, c('value', 'label', 'names', 'hex')]

levels(texture.rat) <- rat

plot(texture.class, col = hcl.colors(50), axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 0-25cm (RV)', mar = c(1, 1, 3, 2))

coltab(texture.class)
# coltab(texture.class) <- NULL

coltab(texture.class) <- rat[, c('value', 'hex')]

plot(texture.class, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 0-25cm (RV)', mar = c(1, 1, 3, 4), plg = list(cex = 2))

par(mfcol = c(1, 2))
plot(texture_2550cm, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nWeighted Mean, 0-25cm (ISSR-800)', mar = c(1, 1, 1, 4))
plot(texture.class, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 0-25cm (SSURGO)', mar = c(1, 1, 1, 4))



## elevation

# query using GCS / WGS84 BBOX
m.ext <- as.polygons(ext(m))
crs(m.ext) <- 'epsg:5070'
m.ext <- project(m.ext, 'epsg:4326')

# ug, have to work through sf / raster objects
e <- get_elev_raster(sf::st_as_sf(m.ext), z = 14)

# convert to spatRaster and warp to 5070
e <- rast(e)
ee <- project(e, m, method = 'cubicspline', mask = TRUE)


par(mfcol = c(1, 2))

plot(ee, col = hcl.colors(50, palette = 'mako'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(1, 1, 1, 4))
plot(texture.class, axes = FALSE, main = 'Soil Texture Class <2mm Fraction\nDominant Component, 0-25cm (SSURGO)', mar = c(1, 1, 1, 4))

par(mfcol = c(1, 1), bg = 'black', fg = 'white')
plot(ee, col = hcl.colors(50, palette = 'mako'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE)

plot(ee, col = hcl.colors(50, palette = 'spectral'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE)

plot(ee, col = hcl.colors(50, palette = 'blues3'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE)

plot(ee, col = hcl.colors(50, palette = 'oslo', rev = TRUE), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE)


# ragg::agg_png(filename = 'e:/temp/terrain.png', width = 2560, height = 1440)
# 
# plot(ee, col = hcl.colors(50, palette = 'oslo'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE, maxcel = ncell(ee))
# 
# dev.off()



## feeling artistic...

v <- as.contour(ee, levels = quantile(values(ee), na.rm = TRUE, prob = seq(0, 1, by = 0.025)))

par(mfcol = c(1, 1), bg = 'black')
plot(v, col = hcl.colors(n = nrow(v), palette = 'zissou1'), mar = c(0, 0, 0, 0), lwd = 2, axes = FALSE)

ex <- as.polygons(ext(v))
lines(ex, col = 'white', lwd = 0.5)


par(mfcol = c(1, 1), bg = 'black', fg = 'white')
plot(ee, col = hcl.colors(50, palette = 'oslo', rev = TRUE), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE)
lines(v, col = 'black', lwd = 0.25)

plot(ee, col = hcl.colors(50, palette = 'oslo'), axes = FALSE, main = 'Elevation (m) ~4m', legend = FALSE, mar = c(0, 0, 0, 0), smooth = TRUE)
lines(v, col = 'white', lwd = 0.25)


