


.n <- 50
.res <- 64
.cols <- hcl.colors(n = .n, palette = 'mako', rev = TRUE)



par(mar = c(0, 0, 0, 0), bg = 'black')

a <- b <- seq(-4*pi, 4*pi, len = .res)
r <- sqrt(outer(a^2, b^2, "+"))


f <- outer(sin(a), cos(b), "+")
image(a, b, f, asp = 1, col = .cols, useRaster = TRUE)


f <- r + rnorm(nrow(r)^2)
image(a, b, f, asp = 1, col = .cols, useRaster = TRUE)


f <- cos(r) + sin(r)
image(a, b, f, asp = 1, col = .cols, useRaster = TRUE)


f <- cos(r) + sin(r) + rnorm(n = nrow(r), mean = colMeans(r), sd = 0.1) + rnorm(n = nrow(r), mean = rowMeans(r), sd = 0.1)
image(a, b, f, asp = 1, col = .cols, useRaster = TRUE)




.n <- 10
.res <- 256
.cols <- hcl.colors(n = .n, palette = 'mako', rev = TRUE)




a <- b <- seq(-4*pi, 4*pi, len = .res)


r <- sqrt(outer(a^2, b^2, "+"))

# f <- cos(r^1.5) * exp(1 - 0.5 * r / (2 * pi))

# f <- cos(r^2) * exp(1 - 0.5 * r / (2 * pi))

par(mar = c(0, 0, 0, 0), bg = 'black')

f <- cos(r/sin(r))^3
image(a, b, f, asp = 1, col = .cols)


f <- cos(r/sin(r/tan(r)))^3
image(a, b, f, asp = 1, col = .cols)


f <- cos(r/sin(r/tan(cos(r + rnorm(nrow(r), mean = colMeans(r))))))^3
image(a, b, f, asp = 1, col = .cols)



f <- cos(r/sin(r))^3
contour(a, b, f, axes = FALSE, drawlabels = FALSE, asp = 1, nlevels = .n, col = .cols)


# animate over .res and .n




# https://cran.r-project.org/web/packages/ContourFunctions/vignettes/Introduction_to_the_cf_R_package.html


# library(ContourFunctions)


# cp <- function(...){
#   hcl.colors(palette = 'mako', ...)
# }
# 
# cf_grid(x = a, y = b, z = f, color.palette = cp, axes = FALSE, lines_only = TRUE, nlevels = 10)


