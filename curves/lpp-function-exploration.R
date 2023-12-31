library(aqp)
library(purrr)

# Amplitude of the peak is controlled by ("lpp.a + "lpp.b"), 
# depth of  the peak by "lpp.u"
# abruptness by "lpp.d" and "lpp.e".


n <- 100
ymax <- 300
y <- seq(1, ymax, length.out = n)

# test
x <- aqp:::.lpp(y, a = 10, b = 10, u = 10, d = 5, e = 2)

plot(x, y, type = 'n', ylim = c(ymax + 5, 0), las = 1)
lines(x, y)


# g <- expand.grid(y = y, a = c(2, 8), b = 20, u = 20:60, d = c(10, 20), e = c(8, 10))

g <- expand.grid(y = y, a = 8, b = c(20, 50, 80), u = seq(80, 150, by = 10), d = c(40, 80, 100), e = 1)

.m <- 3
g <- expand.grid(
  y = y, 
  a = rnorm(n = .m, mean = 8, sd = 0.1), 
  b = rnorm(n = .m, mean = 50, sd = 20), 
  u = runif(n = .m, min = 60, max = 90), 
  d = runif(n = .m, min = 5, max = 100), 
  e = 1
)

g$x <- with(g, aqp:::.lpp(y, a = a, b = b, u = u, d = d, e = e))


.ids <- rep(1:(nrow(g)/n), each = n)
stopifnot(nrow(g) == length(.ids))

.r <- range(g$x)

g <- split(g, .ids)

par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
# plot(x, y, type = 'n', xlim = .r, ylim = c(ymax + 5, 0), las = 1)
plot(y, x, type = 'n', ylim = .r, xlim = c(ymax + 5, 0), las = 1, axes = FALSE, xlab = '', ylab = '')

walk(g, .f = function(i) {
  lines(x ~ y, data = i, lwd = 0.25)
})



plotPanel <- function(n = 100, ymax = 500, a.n = 1, b.n = 1, u.n = 1, d.n = 1, e.n = 1) {
  
  y <- seq(1, ymax, length.out = n)
  
  g <- expand.grid(
    y = y, 
    a = rnorm(n = a.n, mean = 8, sd = 0.1), 
    b = rnorm(n = b.n, mean = 50, sd = 20), 
    u = runif(n = u.n, min = 10, max = ymax-10), 
    d = runif(n = d.n, min = 5, max = 80), 
    e = runif(n = e.n, min = 1, max = 5)
  )
  
  g$x <- with(g, aqp:::.lpp(y, a = a, b = b, u = u, d = d, e = e))
  
  
  .ids <- rep(1:(nrow(g)/n), each = n)
  stopifnot(nrow(g) == length(.ids))
  
  .r <- range(g$x)
  
  g <- split(g, .ids)
  
  plot(1, 1, type = 'n', ylim = .r, xlim = c(ymax + 1, 0), las = 1, axes = FALSE, xlab = '', ylab = '')
  
  walk(g, .f = function(i) {
    lines(x ~ y, data = i)
  })
  
  ## TODO: build off of previous randomness
  invisible(g)
}


par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white', lwd = 0.5, mfcol = c(4, 5))
for(i in 1:20) {
  plotPanel(n = 100, a.n = 5)
}

for(i in 1:20) {
  plotPanel(n = 100, b.n = 5)
}

for(i in 1:20) {
  plotPanel(n = 100, u.n = 5)
}

for(i in 1:20) {
  plotPanel(n = 100, u.n = 25)
}


for(i in 1:20) {
  plotPanel(n = 100, d.n = 5)
}

par(fg = 'black', bg = 'white')
for(i in 1:20) {
  plotPanel(n = 100, e.n = 5)
}


for(i in 1:20) {
  plotPanel(n = 100, e.n = 25, ymax = 500)
}



