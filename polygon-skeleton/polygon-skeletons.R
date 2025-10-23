# remotes::install_github("thomasp85/boundaries")
# remotes::install_github("thomasp85/polyclid")

# not able to build on linux: https://github.com/thomasp85/euclid/issues/30

# https://boundaries.r-euclid.com/reference/skeleton_interior.html


library(boundaries)
library(euclid)
library(polyclid)
library(soilDB)
library(gifski)
library(purrr)
library(terra)
library(sfheaders)


.gzip <- function(i) {
  length(memCompress(as.character(i), type = 'gzip'))
}


# given the perimeter coordinates of a valid polygon
# convert to a full or inner skeleton
.skeleton <- function(.coords, .inner) {
  
  # init polyclid polygon from raw polygon (part) coordinates
  # source data are {terra} SpatVector records, dumped via terra::geom()
  poly <- polyclid::polygon(.coords[, 1], .coords[, 2])
  
  # skeleton with option
  .s <- skeleton_interior(poly, only_inner = .inner)
  
  # compute some polygon shape indices 
  # seems sensitive to winding direction (may result in negative values)
  .a <- abs(euclid::approx_area(poly))
  .p <- abs(euclid::approx_length(poly))
  
  # one definition of polygon fractal dimension
  .fd <- ( 2.0 * log(0.25 * .p) ) / ( log(.a) )
  
  # save as attributes for later
  attr(.s, 'area') <- .a
  attr(.s, 'perimeter') <- .p
  attr(.s, 'fractal.dimension') <- .fd
  attr(.s, 'gz') <- .gzip(.coords[, 1:2])
  
  return(.s)
}


## Notes:
# * "holes" are encoded 2x
# * setting .dropHoles = TRUE, results in (flagged) duplicate hole geom

skeletonize <- function(x, .inner = TRUE, .dropHoles = TRUE) {
  
  # polygon geometry as raw coordinates
  .g <- data.frame(geom(x))
  
  if(.dropHoles) {
    .g <- .g[which(.g$hole == 0), ]
  }
  
  # split into pieces, based on interaction(id * part number * hole)
  # dropping combinations that do not exist
  .g <- split(.g, list(.g$geom, .g$part, .g$hole), drop = TRUE)
  
  # iterate over parts
  .skel <- map(.g, .progress = TRUE, .f = function(.p) {
     # attempt to skeletonize this part
    .s <- try(.skeleton(.p[, c('x', 'y')], .inner = .inner))
    # trap errors
    if(inherits(.s, 'try-error')) {
      return(NULL)
    } else {
      # flag holes
      attr(.s, 'is.hole') <- ifelse(.p$hole[1] > 0, TRUE, FALSE)
      return(.s)
    }
  })
  
  # remove NULL geometry (invalid, or some kind of error with {polyclid})
  .idx <- which(! sapply(.skel, is.null))
  .skel <- .skel[.idx]
  
  return(.skel)
}



# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=36.37327,-82.53513,z15
# example point, WGS84 coordinates
p <- vect(
  data.frame(x = -82.53513, y = 36.37327),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)


# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=36.96184,-81.96539,z16
p <- vect(
  data.frame(x = -81.96539, y = 36.96184),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=37.98659,-120.38170,z15
p <- vect(
  data.frame(x = -120.38170, y = 37.98659),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)


# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.54471,-121.74457,z15
p <- vect(
  data.frame(x = -121.74457, y = 38.54471),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)


# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=36.74934,-119.47477,z13
p <- vect(
  data.frame(x = -119.47477, y = 36.74934),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)


# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=34.16182,-108.18495,z14
p <- vect(
  data.frame(x = -108.18495, y = 34.16182),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)



# BBOX in 4326
# b <- as.polygons(ext(buffer(p, 2000)), crs = 'epsg:4326')

# circle
b <- buffer(p, 2000, quadsegs = 20)

# 12-sided polygon
# b <- buffer(p, 2000, quadsegs = 3)


b <- vect(
  'POLYGON((-90.4804 38.8577,-90.4804 38.9215,-90.3517 38.9215,-90.3517 38.8577,-90.4804 38.8577))',
  crs = 'epsg:4326'
)


# mu
mu <- SDA_spatialQuery(b, what = 'mupolygon', byFeature = TRUE, geomIntersection = TRUE)

## does this even work?
# mu <- sf::st_as_sf(mu)
# mu <- sf_remove_holes(mu)

mu <- terra::project(mu, 'epsg:5070')

# polygon indices
mu$perim <- perim(mu)
mu$area <- expanse(mu)
mu$fd <- ( 2.0 * log(0.25 * mu$perim) ) / ( log(mu$area) ) 

hist(mu$fd)

# remove holes
# sfheaders::sf_remove_holes()



par(bg = 'black', fg = 'white')
plot(mu, axes = FALSE, border = 'white')

for(i in 1:nrow(mu)) {
  .ct <- centroids(mu[i, ], inside = TRUE)
  .xy <- crds(.ct)
  text(x = .xy[, 1], y = .xy[, 2], labels = i, cex = 0.66)
}


## TODO: expand on this
# p.idx <- 1:nrow(mu)
# plot(mu[p.idx, ], border = 'white', type = 'n')
# sapply(seq(100, 1000, by = 50), function(i) {
#   lines(buffer(mu[p.idx, ], -i), col = 'white', lwd = 0.3)
#   lines(buffer(mu[p.idx, ], i), col = 'white', lwd = 0.3)
# })
 



# skeletonize 2-ways
sk.i <- skeletonize(mu, .inner = TRUE, .dropHoles = TRUE)
sk <- skeletonize(mu, .inner = FALSE, .dropHoles = TRUE)

# holes
table(map_lgl(sk, attr, 'is.hole'))




# a fine color palette
cp <- hcl.colors(n = 100, palette = 'zissou1', rev = TRUE)

## TODO: consider several geometric attributes
# extract fractal dimension, some could be NaN due to degenerate geometry
# v <- log(map_dbl(sk, attr, 'gz'))
# v <- log(map_dbl(sk, attr, 'gz')) / map_dbl(sk, attr, 'fractal.dimension')
v <- map_dbl(sk, attr, 'fractal.dimension')

# color ramp function, accepts [0,1]
cpf <- colorRamp(cp, interpolate = 'spline', space = 'Lab')

## TODO: some outliers can skew the color scale, consider trimmed version

# histogram equalize color scale with ECDF
e <- ecdf(v)
# apply color ramp to quantiles via ECDF
# result are sRGB coordinates [0,255]
.rgb <- cpf(e(v))

# replace NA sRGB coordinates
# this happens when fractal dimension is undefined (degenerate geom)
.rgb[which(is.na(.rgb))] <- 255

# convert to colors
.cols <- rgb(.rgb, maxColorValue = 255)

# check length of colors = length of skeletons
stopifnot(length(.cols) == length(sk))



## colors based on permutation of geometry index 
# rather interesting
# n.cols <- pmax(length(sk.i), length(sk))
# .cols <- hcl.colors(n = n.cols, palette = 'zissou1')[sample(1:n.cols)]

par(fg = 'white', bg = 'black')
plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 4, lwd = 0.5, type = 'n')

walk(seq_along(sk), function(i) {
    plot(sk[[i]], col = .cols[i], add = TRUE, axes = FALSE, lwd = 0.85)
})

lines(mu, col = 'white', lwd = 0.25)


par(fg = 'white', bg = 'black')
plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 4, lwd = 0.5, type = 'n')

walk(seq_along(sk.i), function(i) {
  plot(sk.i[[i]], col = .cols[i], add = TRUE, axes = FALSE, lwd = 1.5)
})

# lines(mu, col = 'white', lwd = 0.25)




ragg::agg_png(file = 'skeletons.png', width = 3000, height = 1000, scaling = 1)

# pdf(file = 'skeletons.pdf', width = 15, height = 5)

par(fg = 'white', bg = 'black', mfcol = c(1, 3))

plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 'white', lwd = 0.5)


plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 4, lwd = 0.5, type = 'n')

walk(seq_along(sk.i), function(i) {
  plot(sk.i[[i]], col = .cols[i], add = TRUE, axes = FALSE, lwd = 1.5)
})


plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 4, lwd = 0.5, type = 'n')

walk(seq_along(sk), function(i) {
  plot(sk[[i]], col = .cols[i], add = TRUE, axes = FALSE, lwd = 0.85)
})

lines(mu, col = 'white', lwd = 0.25)

dev.off()



ragg::agg_png(file = 'skeletons-inv.png', width = 3000, height = 1000, scaling = 1)

# pdf(file = 'skeletons-inv.pdf', width = 15, height = 5)

par(fg = 'black', bg = 'white', mfcol = c(1, 3))

plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 'black', lwd = 0.5)


plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 4, lwd = 0.5, type = 'n')

walk(seq_along(sk.i), function(i) {
  plot(sk.i[[i]], col = .cols[i], add = TRUE, axes = FALSE, lwd = 1.5)
})


plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 4, lwd = 0.5, type = 'n')

walk(seq_along(sk), function(i) {
  plot(sk[[i]], col = .cols[i], add = TRUE, axes = FALSE, lwd = 0.85)
})

lines(mu, col = 'black', lwd = 0.25)

dev.off()



## return to this later

# 
# gifski::save_gif(
#   walk(c(100, 200, 300, 400, 500, 1000), function(i) {
#     
#     mu.b <- makeValid(buffer(mu, i))
#     .coords <- crds(mu.b)[, 1:2]
#     
#     poly <- polyclid::polygon(.coords[, 1], .coords[, 2])
#     
#     s <- try(skeleton_interior(make_valid(poly)), silent = TRUE)
#     
#     par(mar = c(1, 1, 1, 1), bg = 'black', fg = 'white')
#     plot(mu.b, lwd = 2, border = 'white')
#     
#     if(!inherits(s, 'try-error')) {
#       plot(s, col = 4, add = TRUE, axes = FALSE)
#     }
#     
#   }), 
#   gif_file = 'skeleton.gif', 
#   delay = 0.1, 
#   width = 800, 
#   height = 400
# )


