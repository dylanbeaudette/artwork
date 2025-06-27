## updated from sp -> sf
## original code from Jay Skovlin 2013, maybe from someone else..?

library(deldir)
library(sf)

# read-in example data
# must be in planar CRS
rPoly <- st_read('example-polygon.shp')

# find points on boundary of rPoly
# no idea what happens when there are holes...
rPolyPts <- st_coordinates(rPoly)

# Perform Voronoi tessellation of those points and extract coordinates of tiles
rVoronoi <- tile.list(deldir(x = rPolyPts[, 1], y = rPolyPts[, 2]))

# extract Voronoi tessellation nodes
V.pts <- do.call(rbind, lapply(rVoronoi, function(x) cbind(x$x, x$y)))
V.pts <- data.frame(V.pts)
names(V.pts) <- c('x', 'y')

# upgrade to sf object
rVp <- st_as_sf(V.pts, coords = c('x', 'y'))

# copy CRS information from poly -> points
st_crs(rVp) <- st_crs(rPoly)


# Find the points on the Voronoi tiles that fall inside 
# the linear feature polygon
rLinePts <- st_intersection(rPoly, rVp)

# visualize results

ragg::agg_png(filename = 'example.png', width = 1000, height = 600)

par(mar=c(0,1,0,1))

# original polygon
plot(st_geometry(rPoly), lwd=2)

# Voronoi tessellation
plot(rVoronoi, border='grey', showpoints=FALSE, add=TRUE)

# overplot for clarity
plot(st_geometry(rPoly), lwd=2, add = TRUE)

# all Voronoi tessellation nodes
plot(st_geometry(rVp), cex=0.5, add = TRUE)

# Voronoi tessellation nodes within polygon
plot(st_geometry(rLinePts), cex=0.75, col='RoyalBlue', pch=16, add = TRUE)

dev.off()



