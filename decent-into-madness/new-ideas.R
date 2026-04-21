# devtools::load_all()
library(aqp)

# ragg::agg_png(file = 'art.png', width = 1600, height = 900, scaling = 1.5)



artF <- function(n, s = 30, mean = 25, sd = 10, log = FALSE, sort = FALSE, method = 'E', thresh = 1, ...) {

  par(mar = c(0.1, 0.1, 0.1, 0.1), bg = 'black', fg = 'white', mfrow = c(n, n))
  
  
  for(i in seq(from = 1, to = n^2)) {
    
    x <- c(1, rnorm(n = s, mean = mean, sd = sd), 50)
    x <- pmax(pmin(x, 50), 1)
    
    ## artistic effects more interesting without pre-sort
    # data should be pre-sorted
    if(sort) {
      x <- sort(x)
    }
    
    cols <- hcl.colors(n = 9, palette = 'zissou 1', rev = TRUE)
    cols <- colorRampPalette(cols)(length(x))
    
    
    ## TODO: argument for uniform spacing attractive force schedule
    ## TODO: argument for pre-sorting
    
    z <- fixOverlap(x, thresh = thresh, method = method, trace = TRUE, ...)
    
    
    if(log) {
      matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1, log = 'x')     
    } else {
      matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = cols, lwd = 1)
    }
    
    
  }
  
}


artF(n = 2, q = 0.1, chargeDecay = -0.02, QkA_GrowthRate = 0, log = TRUE)
artF(n = 2, q = 0.1, chargeDecay = -0.02, QkA_GrowthRate = 0, log = FALSE)


artF(n = 2, q = 0.1, chargeDecay = -0.02, QkA_GrowthRate = 0, log = FALSE, s = 80)
artF(n = 2, q = 0.2, chargeDecay = -0.015, QkA_GrowthRate = 0, log = FALSE, s = 40)


artF(n = 2, q = 0.2, chargeDecay = -0.015, QkA_GrowthRate = 0, log = FALSE, s = 40, maxIter = 120)

artF(n = 2, q = 0.5, chargeDecay = 0, QkA_GrowthRate = 0, log = TRUE, s = 50)
artF(n = 2, q = 0.9, chargeDecay = 0, QkA_GrowthRate = 0, log = TRUE, s = 50)



artF(n = 2, sd = 25, q = 0.45, chargeDecay = 0.01, QkA_GrowthRate = 0, log = TRUE, s = 50, maxIter = 200)



artF(n = 2, sd = 15, log = FALSE, s = 10, maxIter = 800, method = 'S', sort = TRUE, thresh = 2, adj = 4)



