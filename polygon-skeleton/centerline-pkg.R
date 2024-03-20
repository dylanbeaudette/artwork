
## alternative approach

# https://github.com/atsyplenkov/centerline/tree/master

library(centerline)
library(terra)
library(soilDB)

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=36.37327,-82.53513,z15
# example point, WGS84 coordinates
p <- vect(
  data.frame(x = -82.53513, y = 36.37327),
  geom = c('x', 'y'),
  crs = 'epsg:4326'
)


b <- vect(
  'POLYGON((-90.4804 38.8577,-90.4804 38.9215,-90.3517 38.9215,-90.3517 38.8577,-90.4804 38.8577))',
  crs = 'epsg:4326'
)


# circle
b <- buffer(p, 2000, quadsegs = 20)

# mu
mu <- SDA_spatialQuery(b, what = 'mupolygon', byFeature = TRUE, geomIntersection = TRUE)

## does this even work?
# mu <- sf::st_as_sf(mu)
# mu <- sf_remove_holes(mu)

mu <- terra::project(mu, 'epsg:5070')

# sk <- cnt_skeleton(mu, keep = 0.5) 
sk <- cnt_skeleton(mu, keep = 1)
# sk <- cnt_skeleton(mu, keep = 1.5) 


par(fg = 'white', bg = 'black')
plot(mu, axes = FALSE, mar = c(0, 0, 0, 0), border = 'white')
lines(sk, lwd = 1, col = 'royalblue')

plot(sk, axes = FALSE, mar = c(0, 0, 0, 0), col = 'royalblue')




par(fg = 'white', bg = 'black', mfcol = c(3, 3))

sapply(1:9, function(i) {
  .idx <- sample(1:nrow(mu), size = 1)
  print(.idx)
  sk <- cnt_skeleton(mu[.idx, ], keep = 10)
  skl <- cnt_path_guess(mu[.idx, ], keep = 10)
  
  
  plot(mu[.idx, ], axes = FALSE, mar = c(0, 0, 0, 0), border = 'white')
  lines(sk, lwd = 0.5, col = 'royalblue')
  lines(skl, lwd = 1, col = 'firebrick')
})





