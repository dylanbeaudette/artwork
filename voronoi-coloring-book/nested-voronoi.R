library(terra)
library(purrr)

## sampling based on real data

x <- rast('E:/working_copies/DSS_simulations/lohr-farm/gis-data/elev_3m_utm.tif')

e <- ecdf(values(x))
p <- x
values(p) <- e(values(x))

plot(p)

p <- classify(p, rcl = seq(0, 1, by = 0.25), include.lowest = TRUE, brackets = TRUE)
p <- as.numeric(p)

plot(p)


s <- spatSample(p, size = 4, as.points = TRUE, method = 'stratified')

## completely random

p <- rast(nrows = 800, ncols = 1000, xmin = 0, xmax = 1000, ymin = 0, ymax = 800)
values(p) <- runif(ncell(p))
plot(p)

s <- spatSample(p, size = 16, as.points = TRUE, method = 'random')

v <- voronoi(s, bnd = ext(s))

plot(v, axes = FALSE, mar = c(0, 0, 0, 0))


# table(v$elev_3m_utm)
# vv <- aggregate(v, by = 'elev_3m_utm', dissolve = TRUE)
# 
# plot(vv, axes = FALSE, mar = c(0, 0, 0, 0), lwd = 2)
# lines(v)

plot(v, axes = FALSE, mar = c(0, 0, 0, 0))

subV <- function(i, shape, size = 16) {
  .x <- shape[i, ]
  .s <- spatSample(.x, size = size)
  .v <- voronoi(.s, bnd = .x)
  .v <- crop(.v, .x)
  return(.v)
}

# idx <- sample(1:nrow(v), size = nrow(v) / 4)

idx <- 1:nrow(v)
v2 <- map(idx, .f = subV, shape = v, size = 16, .progress = TRUE)
v2 <- do.call('rbind', v2)

idx <- 1:nrow(v2)
v3 <- map(idx, .f = subV, shape = v2, size = 4, .progress = TRUE)
v3 <- do.call('rbind', v3)


svglite::svglite(filename = 'nested.svg', width = 10, height = 8)

plot(v3, lwd = 0.25, axes = FALSE, mar = c(0, 0, 0, 0))
lines(v2, lwd = 0.75)
lines(v, lwd = 2)

dev.off()


svglite::svglite(filename = 'n1.svg', width = 10, height = 8)
plot(v3, lwd = 0.25, axes = FALSE, mar = c(0, 0, 0, 0))
dev.off()

svglite::svglite(filename = 'n2.svg', width = 10, height = 8)
plot(v3, lwd = 0.25, axes = FALSE, mar = c(0, 0, 0, 0), type = 'n')
lines(v2, lwd = 0.75)
dev.off()

svglite::svglite(filename = 'n3.svg', width = 10, height = 8)
plot(v3, lwd = 0.25, axes = FALSE, mar = c(0, 0, 0, 0), type = 'n')
lines(v, lwd = 2)
dev.off()



