library(terra)
library(quadtree)

e <- rast('ah.png')

plot(e)

qt <- quadtree(e, split_threshold = 20, split_method = 'sd', combine_method = 'mean', min_cell_length = 2)

par(fg = 'white', bg = 'black', mar = c(0.25, 0.25, 0.25, 0.25))

plot(qt, border_lwd = 0.1, axes = FALSE, col = hcl.colors(n = 100, palette = 'mako'), xlab = '', ylab = '', crop = TRUE, border_col = 'white', na_col = NA, legend = FALSE, alpha = 0.66)

plot(qt, border_lwd = 0.5, axes = FALSE, col = NA, xlab = '', ylab = '', crop = TRUE, border_col = 'white', na_col = NA, legend = FALSE)


s <- spatSample(e, size = 100, method = 'random', as.points = TRUE)
v <- voronoi(s)

plot(e)
lines(v)
points(s)

plot(s, col = 'white')
