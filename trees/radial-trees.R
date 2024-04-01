
# http://blog.phytools.org/2024/03/function-for-plotting-discrete-andor.html

library(phytools)
library(ape)

par(bg = 'black', fg = 'white', mfcol = c(1, 1))
r <- rtree(n = 10, tip.label = sprintf('A%s', 1:10))
plot(r, align.tip = TRUE, adj = 1, type = 'f')

##

r1 <- rtree(n = 10, tip.label = sprintf('A%s', 1:10))
r2 <- rtree(n = 10, tip.label = sprintf('B%s', 1:10))

r <- bind.tree(r1, r2, where = 'root')
r <- bind.tree(r1, r2, where = 10)
r <- r1 + r2

par(bg = 'black', fg = 'white', mfcol = c(1, 1))
plot(r1, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
nodelabels()
edgelabels()
tiplabels()

par(bg = 'black', fg = 'white', mfrow = c(3, 3))

plot(r1, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
plot(r2, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
plot(bind.tree(r1, r2), use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)

plot(r1, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
plot(r2, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
plot(r1 + r2, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)


plot(r1, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
plot(r2, use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)
plot(bind.tree(r1, r2, where = 10), use.edge.length = FALSE, type = 'phylogram', cex = 1, no.margin = TRUE)


##

par(bg = 'black', fg = 'white', mfcol = c(1, 1))

.n <- 50
tree <- rtree(n = .n)

for(i in 1:5) {
  tree <- bind.tree(tree, rtree(n = .n), where = sample(1:Ntip(tree), size = 1))
  # tree <- bind.tree(tree, rtree(n = .n), where = 1)
}

plot(tree, type = 'fan', no.margin = TRUE, show.tip.label = TRUE, align.tip.label = TRUE, tip.color = 1)

plot(tree, type = 'fan', no.margin = TRUE, show.tip.label = FALSE)

# tiplabels(pch = 16, col = 'white', cex = 0.25, offset = 0.1)
# tiplabels(pch = 16, col = 'white', cex = 0.25, offset = 0.3)
# tiplabels(pch = 16, col = 'white', cex = 0.25, offset = 1)
# tiplabels(pch = 16, col = 'white', cex = 0.25, offset = 3)

## TODO: give each sub-tree its own color



par(bg = 'black', fg = 'white', mfcol = c(3, 3))

for(i in 1:9) {
  tree <- rtree(n = 50)
  plot(tree, type = 'fan', no.margin = TRUE, show.tip.label = FALSE)
  tiplabels(pch = 21, bg = 'white', cex = 0.8, offset = 0.1)
  tiplabels(pch = 21, bg = 'white', cex = 0.6, offset = 0.5)
  tiplabels(pch = 21, bg = 'white', cex = 0.5, offset = 1)
}


for(i in 1:9) {
  tree <- rtree(n = 200)
  plot(tree, type = 'fan', no.margin = TRUE, show.tip.label = FALSE)
}


for(i in 1:9) {
  tree <- rtree(n = 90)
  plot(tree, type = 'cladogram', no.margin = TRUE, show.tip.label = TRUE, direction = 'down', align.tip.label = 3, tip.color = 'black')
}

for(i in 1:9) {
  tree <- rtree(n = 50)
  plot(tree, type = 'cladogram', no.margin = TRUE, show.tip.label = FALSE, direction = 'down')
}

for(i in 1:9) {
  tree <- rtree(n = 50)
  plot(tree, type = 'radial', no.margin = TRUE, show.tip.label = FALSE)
}


## TODO: find optimal rotation
for(i in 1:9) {
  tree <- rtree(n = 50)
  plot(tree, type = 'unrooted', no.margin = TRUE, show.tip.label = FALSE, rotate.tree = 90)
}


par(bg = 'black', fg = 'white', mfcol = c(3, 3))

for(i in 1:9) {
  tree <- rtree(n = 50)
  
  Q <- matrix(c(-1,1,0,1,-2,1,0,1,-1),nrow = 3,ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
  X <- sim.Mk(tree, Q, nsim = 10)
  
  plotFanTree.wTraits(tree, X, ftype = "off")
}

