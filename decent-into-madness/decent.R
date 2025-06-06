library(aqp)

library(MetBrewer)

# original
.npanel <- 4
.min <- 0.1
.max <- 1.5
.seed <- 10101

# subset
.npanel <- 3
.min <- 0.1
.max <- 0.9
.seed <- 10101


cols <- rev(met.brewer('Hiroshige', n = 10))
# cols <- hcl.colors(n = 9, palette = 'Zissou 1', rev = TRUE)


## PNG output
ragg::agg_png(file = 'decent-into-madness.png', width = 1600, height = 900, scaling = 1.8)
par(mar = c(0.1, 0.1, 0.1, 0.1), bg = 'black', fg = 'white', mfrow = c(.npanel, .npanel))

## PDF output
# pdf(file = 'decent-into-madness.pdf', width = 12, height = 8)
# par(mar = c(0.1, 0.1, 0.1, 0.1), mfrow = c(.npanel, .npanel))

## SVG output
svglite::svglite(filename = 'decent-into-madness.svg', width = 12, height = 8)
par(mar = c(0.1, 0.1, 0.1, 0.1), mfrow = c(.npanel, .npanel))


# results are visually interesting
# consider adjusting exponent and constant
set.seed(.seed)
x <- c(1, rep(25, times = 30), 50)
x <- abs(jitter(x, factor = 5))

# interesting effect to start from tightly clustered region
# x <- abs(jitter(x, factor = 1))

# uniform but out of order
# x <- sample(1:50)

## artistic effects more interesting without pre-sort
# (x <- sort(x))


## effect of q
for(i in seq(.min, .max, length.out = .npanel^2)) {
  
  .cols <- colorRampPalette(cols)(length(x))
  
  z <- fixOverlap(x, thresh = 2, q = i, chargeDecay = 0, QkA_GrowthRate = 0, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(
    rbind(x, z$states), 
    type = 'l', 
    lty = 1, 
    las = 1, 
    axes = FALSE, 
    col = .cols, 
    lwd = 1, 
    log = 'x'
  )
  
}

dev.off()

# PNG output
par(mar = c(0.1, 0.1, 0.1, 0.1), bg = 'black', fg = 'white', mfrow = c(.npanel, .npanel))

x <- z$x

## effect of q
for(i in seq(.min, .max, length.out = .npanel^2)) {
  
  .cols <- colorRampPalette(cols)(length(x))
  
  z <- fixOverlap(x, thresh = 2, q = i, chargeDecay = 0, QkA_GrowthRate = 0, method = 'E', maxIter = 100, trace = TRUE)
  .n <- nrow(z$states)
  
  matplot(rbind(x, z$states), type = 'l', lty = 1, las = 1, axes = FALSE, col = .cols, lwd = 1, log = 'x')
  
}


