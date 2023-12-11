

.gzip <- function(i) {
  length(memCompress(paste(as.character(i), collapse = ''), type = 'gzip'))
}

# https://rpubs.com/mstefan-rpubs/chaos

logEq <- function(x0 = 0.1, r = 2, n = 10){
  
  # x0: starting value
  # r: growth rate parameter
  # n: number of iterations
  
  x <- rep(NA,n)
  x[1] <- x0
  
  for(i in 2:n) x[i] <- r*x[i-1]*(1-x[i-1])
  return(x)
  
}


# based on:
# http://people.cryst.bbk.ac.uk/~fdosr01/Rfractals/index.html

# non linear, chaotic function
# r is growth rate and x is population size (from 0 to 1)
lg <- function(x, r) {
  r * x * (1 - x)
}



# https://en.wikipedia.org/wiki/Lyapunov_exponent
lyapunov <- function(x, r) sum(log2(abs(r*(1-2*x))))/length(x)



lgPlot <- function(type = c('points', 'lines'), x.i = 0.01, gen = 5, k = 16, div = 800, r.min = 1, r.max = 4, pt.cex = 0.25, colors  = hcl.colors(n = 100, 'zissou1', rev = FALSE), col.alpha = 125, include.legend = FALSE, ...) {
  
  type <- match.arg(type)
  
  # 
  # gen <- 5       # number of initial generations
  # r.min <- 0.1   # default: 1
  # r.max <- 4     # default: 4
  # div <- 1200     # plot resolution along the x axis
  # k <- 64         # generations to be plotted, increase to 64 to get a high
  #                # resolution plot
  # cl <- scales::alpha('forestgreen', 0.25)
  # 
  
  
  d <- list()
  r.s <- seq(r.min, r.max, length.out = div)
  for(r.i in seq_along(r.s)) {
    r <- r.s[r.i]
    # x.init <- runif(n = 1, min = 0.01, max = 0.1)
    x.init <- x.i
    
    for(i in seq(0, gen)) {
      x.next <- lg(x.init, r)
      x.init <- x.next
      # x.init <- x.next + pmax((rnorm(n = 1, mean = 0, sd = 0.01)), 0)
    }
    
    x <- vector(mode = 'numeric', length = k)
    for(i in seq(0, k)) {
      x.next <- lg(x.init, r)
      x.init <- x.next
      # x.init <- sqrt(x.next)
      x[i] <- x.next
      # points(r, x.next, pch = 16, col = colors[i+1], cex = pt.cex)
    }
    
    d[[r.i]] <- data.frame(
      r = r,
      x = x,
      gz = .gzip(x)
    )
    
  }
  
  if(type == 'points') {
    d <- do.call('rbind', d)
    e <- ecdf(d$gz)
    cr <- colorRamp(colors = colors, space = 'Lab', interpolate = 'spline')
    .cols <- rgb(cr(e(d$gz)), maxColorValue = 255, alpha = col.alpha)
    
    # full plot
    par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
    plot(
      x = 0, y = 0, 
      type = 'n', 
      xlim = c(r.min, r.max), 
      ylim = c(0, 1),
      xlab = '', ylab = '', 
      axes = FALSE
    )
    
    points(d$r, d$x, pch = 16, col = .cols, cex = pt.cex)
  }
  
  if(type == 'lines') {
    
    ## TODO: simplify this
    .d <- lapply(1:k, function(i) {
      t(sapply(d, function(j) j[i, 1:2]))
    })
    
    par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
    plot(
      x = 0, y = 0, 
      type = 'n', 
      xlim = c(r.min, r.max), 
      ylim = c(0, 1),
      xlab = '', ylab = '', 
      axes = FALSE
    )
    
    .gz <- sapply(.d, function(i) .gzip(i[, 2]))
    e <- ecdf(.gz)
    cr <- colorRamp(colors = colors, space = 'Lab', interpolate = 'spline')
    .cols <- rgb(cr(e(.gz)), maxColorValue = 255, alpha = col.alpha)
    
    sapply(1:k, function(i) {
      lines(.d[[i]], col = .cols[i])
    })
    
  }
  
  
  
  
  
  
  if(include.legend) {
    legend(
      'topleft',
      legend = quantile(d$gz),
      pch = 15,
      pt.cex = 2,
      bty = 'n', 
      xpd = NA,
      col = rgb(cr(e(quantile(d$gz))), maxColorValue = 255, alpha = 150)
    )
  }
}
