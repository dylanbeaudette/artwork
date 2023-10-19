library(terra)


x <- rast(nrows = 100, ncols = 100, xmin = 0, xmax = 77, ymin = 0, ymax = 100)

x <- rast('E:/working_copies/DSS_simulations/lohr-farm/gis-data/elev_3m_utm.tif')

e <- ecdf(values(x))
p <- x
values(p) <- e(values(x))

plot(p)

p <- classify(p, rcl = c(0, 0.25, 0.5, 0.75, 0.9, 1), include.lowest = TRUE, brackets = TRUE)
p <- as.numeric(p)

plot(p)

s <- spatSample(p, size = 50, as.points = TRUE, method = 'stratified')

plot(s, mar = c(0, 0, 0, 0))

v <- voronoi(s, bnd = ext(s))
d <- delaunay(s)

plot(s)
lines(v)

plot(v, axes = FALSE, mar = c(0, 0, 0, 0))
# points(s, cex = 0.75)


plot(p, axes = FALSE, legend = FALSE, mar = c(0, 0, 0, 0))
contour(x, add = TRUE, drawlabels = FALSE)
lines(v)
points(s, cex = 0.75)

plot(p, mar = c(0, 0, 0, 0))
lines(d)


# https://dfriend21.github.io/quadtree/index.html

library(quadtree)
library(raster)
qt <- quadtree(raster(x), split_threshold = 2, split_method = "sd", adj_type = "resample",  resample_n_side = 128)

par(mar = c(0, 0, 0, 0))
plot(qt, axes = FALSE, xlab = '', ylab = '', legend = FALSE, border_col = 'black', na_col = 'white', crop = TRUE, col = NA)

