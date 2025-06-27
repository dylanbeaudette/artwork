##
##
##

cols <- hcl.colors(4, alpha = 0.25, palette = 'Light Grays', rev = TRUE)

g <- expand.grid(year = 1981:(1981+80), week = 1:52)


this.year <- as.numeric(format(Sys.time(), "%Y"))
this.week <- as.numeric(format(Sys.time(), "%W"))

pdf(file = 'e:/temp/time-left.pdf', width = 11, height = 8.5, pointsize = 8)

par(mar = c(0.5, 2, 2.75, 0.5), las = 1)

plot(x = g$week, y = g$year, axes = FALSE, xlab = '', ylab = '', type = 'n', ylim = rev(range(g$year)), pch = 16, cex = 0.5)

wx <- 0.5
wy <- 0.5

g$col <- g$week %% 4
g$col[g$col == 0] <- 4

g$gone <- FALSE
g$gone[g$year <= this.year] <- TRUE
g$gone[g$year == this.year & g$week > this.week] <- FALSE

## TODO: convert to segments to avoid aliasing by over-printed rectangles
rect(xleft = g$week - wx, xright = g$week + wx, ytop = g$year + wy, ybottom = g$year - wy, col = cols[g$col], lwd = 0.5, border = grey(0.125))

text(x = g$week[g$gone], y = g$year[g$gone], labels = 'X', font = 2, cex = 0.5, col = 'black')

axis(side = 2, at = g$year, tick = FALSE, line = -2, cex.axis = 1, font = 2)
axis(side = 3, at = g$week, tick = FALSE, line = -2.8, cex.axis = 0.8, font = 2)
axis(side = 1, at = g$week, tick = FALSE, line = -2.8, cex.axis = 0.8, font = 2)

m <- seq(from = (52 / 12), to = 52 - (52 / 12), length.out = 12)
axis(side = 3, at = m, tick = FALSE, line = -1.8, cex.axis = 0.75, font = 3, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

title('Time Left', line = 1.5)

dev.off()
