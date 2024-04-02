# https://cran.r-project.org/web/packages/quadtree/vignettes/quadtree-lcp.html

library(terra)
library(quadtree)

e <- rast('ah.png')
coltab(e) <- NULL

plot(e)

qt <- quadtree(e, split_threshold = 20, split_method = 'sd', combine_method = 'mean', min_cell_length = 2)

start_pt <- c(50, 50)
start_pt <- c(97, 115)

lcpf <- lcp_finder(qt, start_pt)
paths <- find_lcps(lcpf)

par(fg = 'white', bg = 'black', mar = c(0.25, 0.25, 0.25, 0.25))

plot(qt, border_lwd = 0.1, axes = FALSE, col = hcl.colors(n = 100, palette = 'mako'), xlab = '', ylab = '', crop = TRUE, border_col = 'white', na_col = NA, legend = FALSE, alpha = 0.66)

lines(lcpf, lwd = 0.5, col = 'white')


plot(e, axes = FALSE, col = hcl.colors(n = 100, palette = 'mako', alpha = 0.5), xlab = '', ylab = '', legend = FALSE)
lines(lcpf, lwd = 0.1, col = 'white')


par(fg = 'white', bg = 'black', mar = c(1, 1, 1, 1))
lines(lcpf, lwd = 0.1, col = 'white', add = FALSE, asp = 1, axes = FALSE, xlab = '', ylab = '')

points(lcpf, pch = '.', col = 'white', add = FALSE, asp = 1, axes = FALSE, xlab = '', ylab = '')


# points(start_pt[1], start_pt[2], col = "black", bg = "red", pch = 21, cex = 1.2)

