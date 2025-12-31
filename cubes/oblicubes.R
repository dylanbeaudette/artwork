# https://github.com/coolbutuseless/isocubes
# https://github.com/trevorld/oblicubes

# remotes::install_github("trevorld/oblicubes")
# remotes::install_github('coolbutuseless/isocubes')

library(isocubes)
library(grid)
library(aqp)
library(soilDB)
library(purrr)


# i:      SPC length 1
# n.sim:  number of perturbations = width of each "slab"
# n.fuzz: jitter applied to thickness of each soil series "slab"

cubit <- function(i, max.depth = 150, n.sim = 40, n.fuzz = 10, ...) {
  
  x <- fetchOSD(i, colorState = 'moist')
  x <- trunc(x, 0, max.depth)
  
  # convert horizon distinctness codes into reasonable depth offsets
  x$hd <- hzDistinctnessCodeToOffset(
    x$distinctness, 
    codes = c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
  )
  
  # number of new IDs sets the number of realizations
  s <- perturb(
    x, 
    id = sprintf("Sim. %02d", 1:n.sim),
    boundary.attr = 'hd', 
    min.thickness = 5
  )
  
  s <- dice(s)
  
  p <- profileApply(s, function(d) {
    
    .id <- match(profile_id(d), profile_id(s))
    
    coords <- data.frame(
      x = .id, 
      y = (max.depth - d$bottom) / 3, 
      z = sample(1:n.fuzz, size = nrow(d), replace = TRUE)
    )
    
    fill <- d$soil_color
    
    return(list(coords = coords, fill = fill))
  }, simplify = FALSE
  )
  
  
  coords <- do.call('rbind',
                    lapply(p, '[[', 'coords')
  )
  
  fill <- do.call('c',
                  lapply(p, '[[', 'fill')
  )
  
  
  
  return(
    list(coords = coords, fill = fill)
  )
  
}

o <- c('leon', 'musick', 'fresno', 'zook', 'amador', 'sierra', 'lucy', 'pierre', 'miami', 'drummer')

.nf <- 5

z <- lapply(o, cubit, n.fuzz = .nf, n.sim = 50)
z <- transpose(z)

# spacing between soil series "slabs"
for(i in seq_along(z$coords)) {
  .inc <- (i-1) * .nf
  z$coords[[i]]$z <- z$coords[[i]]$z - .inc
}


z.c <- do.call('rbind', z$coords)
z.f <- do.call('c', z$fill)

# 2025-12-31: 
#            * use xyplane = 'right' for proper depth orientation
#            * may require tinkering with x, y when plotting to PNG
cubes <- isocubesGrob(
  coords = z.c,
  fill = z.f,
  col = 'black', 
  lwd = 0.1,
  verbosity = 1,
  intensity = c(1, 0.3, 0.7), # top, left, right,
  size = 1.5, 
  default.units = 'mm',
  xyplane = 'right',
  x = 0.5, 
  y = 0
)

ragg::agg_png(filename = 'cubes.png', width = 1200, height = 800, scaling = 2)

grid.newpage()
grid.draw(cubes)

dev.off()

