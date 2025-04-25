library(purrr)
library(emdist)
library(scales)

drawIt <- function(n = 50, a = 0.25, shape1 = c(2, 3), shape2 = c(5, 3), ...) {
  
  plot(1, 1, type = 'n', axes = FALSE, xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0, 5))
  
  .x <- seq(0.0001, 1, length.out = 101)
  
  .curves <- map(1:n, .f = function(i) {
    
    .pbar <- abs(rnorm(n = 1, mean = shape1[1],  sd = shape1[2]))
    .theta <- abs(rnorm(n = 1, mean = shape2[1],  sd = shape2[2]))
    .b <- dbeta(.x, .pbar, .theta)
    .b <- zapsmall(.b, digits = 8)
    
    if(any(is.infinite(.b))) {
      return(NULL)
    }
    
    .res <- list(
      dens = .b,
      pars = c(.pbar, .theta)
    )
    
    return(.res)
    
  })
  
  # remove NULL
  .idx <- which(!sapply(.curves, is.null))
  .curves <- .curves[.idx]
  
  
  .shapeMeans <- rowMeans(sapply(.curves, '[[', 'pars'))
  .meanBeta <- dbeta(.x, .shapeMeans[1], .shapeMeans[2])
  
  # notes: https://github.com/s-u/emdist/issues/3
  # A <- [prob. density at, coordinate 1, coordinate 2, coordinate 3, ...]
  # B <- [prob. density at, coordinate 1, coordinate 2, coordinate 3, ...]
  #
  .emDist <- sapply(seq_along(.curves), function(i) {
    .A <- cbind(.x, .curves[[i]]$dens)
    .B <- cbind(.x, .meanBeta)
    emd2d(.A, .B, max.iter = 500)
  })
  
  
  ## TODO: maybe faster to use emd(A, B) where A,B ~ [wt, coordinates]
  
  
  .emDistPctiles <- ecdf(.emDist)(.emDist)
  
  .cp <- hcl.colors(n = 50, palette = 'zissou1', rev = FALSE)
  .cr <- colorRamp(.cp, space = 'Lab', interpolate = 'spline')
  .cm <- col_numeric(palette = .cr, domain = range(.emDistPctiles), alpha = FALSE)
  
  .cols <- .cm(.emDistPctiles)
  .cols <- alpha(.cols, alpha = a)
  
  walk(seq_along(.curves), .f = function(i) {
    # lines(.x, .curves[[i]]$dens, col = alpha('white', alpha = a))
    lines(.x, .curves[[i]]$dens, col = .cols[i], ...)
  })
  
  
  lines(.x, .meanBeta, col = alpha('white', alpha = 0.75), lwd = 1.5, lty = 2)
  
}


par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white', mfrow = c(3, 3))

trash <- replicate(9, drawIt(a = 0.5, n = 60))


for(i in seq(0, 10, length.out = 9)) {
  drawIt(shape1 = c(i, 6), a = 0.5, n = 60)
}

for(i in seq(0, 8, length.out = 9)) {
  drawIt(shape2 = c(i, 3), a = 0.5, n = 60)
}







