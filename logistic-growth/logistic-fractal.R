library(scales)
library(MetBrewer)


flexoki.cols.dark <- c('#A02F6F', '#AF3029', '#BC5215', '#AD8301', '#66800B', '#24837B', '#205EA6', '#5E409D')

flexoki.cols.light <- c('#CE5D97', '#D14D41', '#DA702C', '#D0A215', '#879A39', '#3AA99F', '#4385BE', '#8B7EC8')


# other ideas: 
# https://rpubs.com/mstefan-rpubs/fractals

# https://rpubs.com/mstefan-rpubs/chaos

logEq <- function(x0=.1, r=2, n=10){
  
  # x0: starting value
  # r: growth rate parameter
  # n: number of iterations
  
  x <- rep(NA,n)
  x[1] <- x0
  
  for(i in 2:n) x[i] <- r*x[i-1]*(1-x[i-1])
  return(x)
  
}


.gzip <- function(i) {
  length(memCompress(paste(as.character(i), collapse = ''), type = 'gzip'))
}

# plot the system's final values for different values of r
r <- seq(from = 1, to = 4, by = 0.015)
x <- sapply(r, logEq, x0 = 0.01, n = 200)

gz <- apply(x, 1, .gzip)
e <- ecdf(gz)


## TODO: try again, color using Lyapunov exponent
# https://en.wikipedia.org/wiki/Lyapunov_exponent
lyapunov <- function(x, r) sum(log2(abs(r*(1-2*x))))/length(x)

l <- rep(NA, length(r))
for(i in 1:length(l)) l[i] <- lyapunov(x = x[,i], r = r[i])
e.le <- ecdf(l)


.colors <- hcl.colors(n = 100, 'zissou1', rev = FALSE)
# .colors <- met.brewer('Hiroshige', n = 100, direction = -1)

cr <- colorRamp(.colors, space = 'Lab', interpolate = 'spline')
.cols <- rgb(cr(e(gz)), maxColorValue = 255, alpha = 200)

# .cols <- rgb(cr(e.le(l)), maxColorValue = 255, alpha = 200)

par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
plot(
  as.numeric(x) ~ rep(r, each = nrow(x)), 
  pch = 16,
  cex = 0.3,
  xlab = '', 
  ylab = '', 
  axes = FALSE,
  col = .cols
)




# based on:
# http://people.cryst.bbk.ac.uk/~fdosr01/Rfractals/index.html

# non linear, chaotic function
# r is growth rate and x is population size (from 0 to 1)
lg <- function(x, r) {
  r * x * (1 - x)
}

lgPlot <- function(x.i = 0.01, gen = 5, k = 16, div = 800, r.min = 1, r.max = 4, pt.cex = 0.25, colors  = hcl.colors(n = 100, 'zissou1', rev = FALSE), col.alpha = 125, include.legend = FALSE, ...) {
  
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
    
    # x <- sapply(seq(0, k), function(i) {
    #   x.next <- lg(x.init, r)
    #   x.init <- x.next
    #   return(x.init)
    # })
    # 
    # points(rep(r, times = length(x)), x, pch = 16, col = colors[1], cex = pt.cex)
    
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




lgPlot(gen = 1, k = 80, div = 100, x.i = 0.0001, pt.cex = 1, col.alpha = 50)
lgPlot(gen = 1, k = 80, div = 100, x.i = 0.0001, pt.cex = 1, col.alpha = 50, colors = met.brewer('Hiroshige', n = 100, direction = -1))

lgPlot(
  gen = 1, 
  k = 50, 
  div = 800, 
  x.i = 0.1, 
  pt.cex = 0.33, 
  col.alpha = 90, 
  colors = met.brewer('Hiroshige', n = 100, direction = -1)
)


ragg::agg_png(file = 'logistic-0002.png', width = 1500, height = 1000, scaling = 1.66)

lgPlot(gen = 5, k = 80, div = 100, x.i = 0.0001, pt.cex = 1)

dev.off()



lgPlot(gen = 5, k = 80, div = 100, x.i = 0.0001, pt.cex = 1, colors = hcl.colors(n = 10))
lgPlot(gen = 5, k = 80, div = 100, x.i = 0.0001, pt.cex = 1, colors = flexoki.cols.light)
lgPlot(gen = 5, k = 80, div = 100, x.i = 0.0001, pt.cex = 1, colors = 'royalblue', col.alpha = 100)

lgPlot(gen = 5, k = 16, div = 1000, x.i = 0.01, pt.cex = 0.25)

# neat, but slow
# lgPlot(gen = 5, k = 60, div = 10000, x.i = 0.99999, col.alpha = 25)

ragg::agg_png(file = 'logistic-0001.png', width = 1500, height = 1000, scaling = 1.66)

lgPlot(gen = 10, k = 60, div = 66, x.i = 0.1, pt.cex = 2.5, col.alpha = 25)

dev.off()

