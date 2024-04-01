
library(ape)

par(bg = 'black', fg = 'white', mfcol = c(1, 1))

r <- rtree(n = 10, tip.label = sprintf('A%s', 1:10))

plot(r, align.tip = TRUE, adj = 1, type = 'f')


##

tr <- rtree(n <- 30)
p <- 5
x <- matrix(sample(3, size = n*p, replace = TRUE), n, p)
dimnames(x) <- list(paste0("t", 1:n), LETTERS[1:p])
plot(tr, x.lim = 35, align.tip = TRUE, adj = 1)
phydataplot(x, tr, "m", 2)
## change the aspect:
plot(tr, x.lim = 35, align.tip = TRUE, adj = 1)
phydataplot(x, tr, "m", 2, width = 2, border = "white", lwd = 3, legend = "side")
## user-defined colour:
f <- function(n) c("yellow", "blue", "red")
phydataplot(x, tr, "m", 18, width = 2, border = "white", lwd = 3,
            legend = "side", funcol = f)


x[] <- 1:(n*p)
plot(tr, x.lim = 35, align.tip = TRUE, adj = 1, type = 'c')
phydataplot(x, tr, "m", 2, width = 1.5, continuous = TRUE, legend = "side",
            funcol = colorRampPalette(c("white", "darkgreen")))
phydataplot(x, tr, "m", 18, width = 1.5, continuous = 5, legend = "side",
            funcol = topo.colors)

##


ts <- rcoal(N <- 100)
X <- rTraitCont(ts) # names are set
dd <- dist(X)
op <- par(mar = rep(0, 4))
plot(ts, x.lim = 10, cex = 0.4, font = 1)
phydataplot(as.matrix(dd), ts, "i", offset = 0.2, col = hcl.colors(100, palette = 'mako'))


##

# use powers of 2
plot(stree(16, type = 's'), type = 'fan', show.tip.label = FALSE)

plot(stree(64, type = 'b'), type = 'fan', show.tip.label = FALSE)

plot(stree(64, type = 'b'), type = 'unrooted', show.tip.label = FALSE)


##

r1 <- rtree(n = 10, tip.label = sprintf('A%s', 1:10))
r2 <- rtree(n = 10, tip.label = sprintf('A%s', 1:10))

comparePhylo(r1, r2, plot = TRUE)

