
# https://www.sciencedirect.com/science/article/pii/S0098300406001828?via%3Dihub

# https://www.cabdirect.org/cabdirect/abstract/20013118404

# https://link.springer.com/content/pdf/10.1007/BF02083652.pdf


# https://dfriend21.github.io/quadtree/index.html

library(soilDB)
library(terra)
library(viridis)
library(sf)
library(wk)
library(data.table)
library(quadtree)


## CONUS

##
r <- terra::rast('E:/gis_data/FY2023-800m-rasters/rasters/ph_025.tif')
r <- as.matrix(r)

# adjust resample_n_side: larger = more resolution = tinyer divisions

qt <- quadtree(r, split_threshold = 0.5, split_method = "sd", adj_type = "resample",  resample_n_side = 512)

par(mar = c(0, 1, 0, 0), bg = 'black')
plot(qt, axes = FALSE, xlab = '', ylab = '', legend = FALSE, col = viridis::viridis(100), border_col = 'white', na_col = 'black', crop = TRUE)


## much faster qt -> sf conversion
# https://github.com/dfriend21/quadtree/issues/12#issuecomment-1033135513
dt <- data.table(as_data_frame(qt))
cr <- st_crs(5070)
v <- dt[, list(bb = st_as_sf(rct(xmin, ymin, xmax, ymax, cr)))]$bb
v <- st_as_sf(v)

# crop v to r
vv <- st_intersection(v, st_as_sf(st_as_sfc(st_bbox(r))))

dev.off()
par(mar = c(0.1, 0.1, 0.1, 0.1) , bg = 'black')
plot(vv, border = 4)



svglite::svglite(filename = 'E:/working_copies/ncss-tech.github.io/AQP/SVG-examples/CONUS-pH-2550cm-quadtree.svg', width = 15, height = 10)

par(mar = c(0.1, 0.1, 0.1, 0.1) , bg = 'black')
plot(vv, border = 4, lwd = 0.5)

dev.off()



## CA subset

# make a bounding box and assign a CRS (4326: GCS, WGS84)
a.CA <- st_bbox(
  c(xmin = -2280334, xmax = -2065433, ymin = 1755361, ymax = 1970262), 
  crs = st_crs(5070)
)

# convert bbox to sf geometry
a.CA <- st_as_sfc(a.CA)


pH_3060cm <- ISSR800.wcs(aoi = a.CA, var = 'ph_3060cm')
plot(pH_3060cm, axes = FALSE, xlab = '', ylab = '')


qt <- quadtree(pH_3060cm, split_threshold = 0.25, split_method = "sd", adj_type = "resample",  resample_n_side = 128)


par(mar = c(0, 1, 0, 0), bg = 'black', mfcol = c(1, 2))
plot(pH_3060cm, axes = FALSE, xlab = '', ylab = '', legend = FALSE, col = viridis::viridis(100))
plot(qt, axes = FALSE, xlab = '', ylab = '', legend = FALSE, col = viridis::viridis(100), border_col = 'white', na_col = 'black', crop = TRUE)

r <- as_raster(qt, pH_3060cm)

par(mar = c(0, 1, 0, 0), bg = 'black', mfcol = c(1, 2))
plot(pH_3060cm, axes = FALSE, xlab = '', ylab = '', legend = FALSE, col = viridis::viridis(100))
plot(r, axes = FALSE, xlab = '', ylab = '', legend = FALSE, col = viridis::viridis(100))


df <- as_data_frame(qt, FALSE)

# iterate over boxes -> single sf object
# ~ 5 seconds with wk
v <- lapply(1:nrow(df), function(i) {
  
  .row <- df[i, ]
  
  ## this is very slow
  # st_bbox -> st_as_sfc -> st_as_sf
  
  # fast BBOX -> SF via wk
  
  # BBOX -> WK
  .bbox <- rct(
    xmin = .row$xmin, xmax = .row$xmax, ymin = .row$ymin, ymax = .row$ymax, 
    crs = st_crs(5070)
  )
  
  # WK -> SF
  return(st_as_sf(.bbox))
})

# rbind.sf is slow for > 1,000 objects
# v <- do.call('rbind', v)library(wk)

# ! 3 seconds
# https://github.com/r-spatial/sf/issues/798
v <- st_as_sf(data.table::rbindlist(v))


dev.off()

par(mar = c(0, 0, 0, 0) + 0.125, bg = 'black')
plot(pH_3060cm, axes = FALSE, xlab = '', ylab = '', legend = FALSE, col = viridis::viridis(100))
plot(v, border = 'white', add = TRUE)





