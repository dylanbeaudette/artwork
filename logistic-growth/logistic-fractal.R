library(scales)
library(MetBrewer)

source('local-functions.R')


flexoki.cols.dark <- c('#A02F6F', '#AF3029', '#BC5215', '#AD8301', '#66800B', '#24837B', '#205EA6', '#5E409D')

flexoki.cols.light <- c('#CE5D97', '#D14D41', '#DA702C', '#D0A215', '#879A39', '#3AA99F', '#4385BE', '#8B7EC8')


# other ideas: 
# https://rpubs.com/mstefan-rpubs/fractals




# plot the system's final values for different values of r
r <- seq(from = 1, to = 4, by = 0.015)
x <- sapply(r, logEq, x0 = 0.01, n = 200)

gz <- apply(x, 1, .gzip)
e <- ecdf(gz)

## TODO: try again, color using Lyapunov exponent

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

plot(
  as.numeric(x) ~ rep(r, each = nrow(x)), 
  type = 'n',
  xlab = '', 
  ylab = '', 
  axes = FALSE,
  col = .cols
)

for(i in 1:nrow(x)) {
  lines(r, x[i, ], col = .cols[i], lwd = 1)
}




## TODO: re-factor, plot lines along r




lgPlot(gen = 1, k = 80, div = 100, x.i = 0.0001, pt.cex = 1, col.alpha = 50)

lgPlot(gen = 1, k = 25, div = 100, x.i = 0.0001, pt.cex = 1, col.alpha = 200, type = 'lines', r.max = 3.95, col = 'white')


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
lgPlot(gen = 5, k = 16, div = 1000, x.i = 0.01, pt.cex = 0.25, colors = 'white')

# neat, but slow
# lgPlot(gen = 5, k = 60, div = 10000, x.i = 0.99999, col.alpha = 25)

ragg::agg_png(file = 'logistic-0001.png', width = 1500, height = 1000, scaling = 1.66)

lgPlot(gen = 10, k = 60, div = 66, x.i = 0.1, pt.cex = 2.5, col.alpha = 25)

dev.off()



## axidraw
lgPlot(r.min = 1, r.max = 3.8, gen = 1, k = 15, div = 200, x.i = 0.0001, pt.cex = 0.5, colors = 3, col.alpha = 255)




