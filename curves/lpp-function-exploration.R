library(aqp)

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

g$x <- with(g, aqp:::.lpp(y, a = a, b = b, u = u, d = d, e = e))

head(g, 100)
nrow(g)

.ids <- rep(1:(nrow(g)/n), each = n)
stopifnot(nrow(g) == length(.ids))

.r <- range(g$x)

g <- split(g, .ids)

par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
# plot(x, y, type = 'n', xlim = .r, ylim = c(ymax + 5, 0), las = 1)
plot(y, x, type = 'n', ylim = .r, xlim = c(ymax + 5, 0), las = 1, axes = FALSE, xlab = '', ylab = '')

purrr::walk(g, .f = function(i) {
  lines(x ~ y, data = i, lwd = 0.25)
})

