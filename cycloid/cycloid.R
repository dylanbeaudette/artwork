# https://twitter.com/S_Conradi/status/1777607465288225280/photo/1


library(cycloids)

op <- par(mar = c(0,0,0,0), bg = "black")
plot.new()
plot.window(asp = 1, xlim = c(-23, 23), ylim = c(-23, 23))
ll   <- seq(2, 0, -0.2)
ccol <- rep(c("lightblue", "lightgreen", "yellow", "yellow",
              "yellow"), 2)
for (i in c(1:length(ll))) {
  z <- zykloid(A = 15, a = 7, lambda = ll[i], hypo = TRUE)
  lines(y ~ x, data = z, col = ccol[i])
} # for i
par(op)



cycl <- zykloid(A = 17, a = 9, lambda = 0.9, hypo = TRUE)
plot(y ~ x, data = cycl, asp = 1, type = "l")


cycl <- zykloid(A = 17, a = 10, lambda = 0.9, hypo = TRUE)
plot(y ~ x, data = cycl, asp = 1, type = "l")



f <- function(.x, .y) {
  cycl <- zykloid(A = .x, a = .y, lambda = 0.5, hypo = TRUE)
  plot(y ~ x, data = cycl, asp = 1, type = "l")
  return(1)
}

.n <- 6
.x <- 5:9
.y <- 5:9
par(mfcol = c(5, 5), mar = c(0, 0, 0, 0))
outer(.x, .y, FUN = f)


p <- function(n, m, theta) {
  r <- cos(n/m * theta)
  r
}

.seq <- 0:360 * pi/180
p(m = 1, n = 1, theta = .seq)

d <- data.frame(
  theta <- .seq,
  r = p(n = 1, m = 2, theta = .seq)
)


plot(r ~ theta, data = d)


library(ggplot2)
ggplot(d, aes(r, theta)) + geom_line() + coord_polar()

library(plotrix)
polar.plot(p(n = 1, m = 2, theta = 0:360), 0:360, clockwise = TRUE, start = 90)


