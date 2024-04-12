# http://blog.phytools.org/2023/11/creating-custom-phylogeny-plotting.html

library(phytools)
library(ape)



slantedTree <- function(phy, h = 0.02, direction = c("rightwards", "upwards"), ...) {
  
  direction <- direction[1]
  phy <- reorder(phy, "cladewise") ## reorder
  plotTree(phy, plot = FALSE, direction = direction, ...)
  
  pp <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  
  h <- h * max(nodeHeights(phy))
  
  par(ljoin = 2, lend = 1)
  
  for(i in 1:nrow(phy$edge)){
    
    edge <- phy$edge[i,]
    elength <- phy$edge.length[i]
    
    if(elength < h) {
      lines(pp$xx[edge], pp$yy[edge])
    } else {
      if(direction == "rightwards"){
        lines(pp$xx[edge[c(1, 1, 2)]] + c(0, h, 0), pp$yy[edge[c(1, 2, 2)]])
      } else if(direction == "upwards"){
        lines(pp$xx[edge[c(1, 2, 2)]], pp$yy[edge[c(1, 1, 2)]] + c(0, h, 0))
      }
    }
  }
  
  # text(pp$xx[1:Ntip(phy)]+
  #        if(direction=="rightwards") 0.5*strwidth("o")*par()$cex else 0,
  #      pp$yy[1:Ntip(phy)]+
  #        if(direction=="upwards") 0.5*strheight("o")*par()$cex else 0,
  #      gsub("_"," ",phy$tip.label),pos=4,offset=0,
  #      srt=if(direction=="upwards") 90 else 0)
}



tr <- rtree(25)
tr <- pbtree(b = 5, d = 0.1, n = 25, scale = 1)

plotTree(tr)

sigmoidPhylogram(tr, lwd = 1, show.tip.labels = FALSE)

# splinePhylogram(tr, lwd = 1)

slantedTree(tr)



slantedTree(rtree(n = 25))
slantedTree(rtree(n = 25, br = 1))
slantedTree(rtree(n = 100, br = 1))

slantedTree(rtree(n = 25, br = runif))
slantedTree(rtree(n = 25, br = rnorm))

slantedTree(rtopology(n = 25))
slantedTree(rtopology(n = 25, br = 1))

slantedTree(rcoal(n = 25))
slantedTree(rcoal(n = 25, br = 1))

plot(rtree(n = 25, br = rnorm), type = 'fan', show.tip.label = FALSE)

plot(rcoal(n = 25, br = 1), type = 'fan', show.tip.label = FALSE)
plot(rcoal(n = 25, br = 1), type = 'radial', show.tip.label = FALSE)
plot(rcoal(n = 25, br = 1), type = 'unrooted', show.tip.label = FALSE)

slantedTree(pbtree(b = 10, d = 1, n = 25, scale = 1))


.n <- 25
tree <- rtree(n = .n)

tree <- pbtree(b = 5, d = 0.1, n = .n, scale = 1)

tree <- rcoal(n = .n, br = 1)

for(i in 1:5) {
  # tree <- bind.tree(tree, pbtree(b = 5, d = 0.1, n = .n, scale = 1), where = sample(1:Ntip(tree), size = 1))
  # tree <- bind.tree(tree, rtree(n = .n), where = 1)
  tree <- bind.tree(tree, rcoal(n = .n, br = 1), where = 1)
}

# remove tip labels
tree$tip.label[] <- ''

par(mfcol = c(1, 1), fg = 'white', bg = 'black')


plot(tree, type = 'fan', no.margin = TRUE, show.tip.label = TRUE, align.tip.label = TRUE, tip.color = 1)


slantedTree(tree, h = 0.01)

slantedTree(rtree(200, equiprob = TRUE), h = 0.03, use.edge.length = FALSE)
slantedTree(rtree(200, equiprob = TRUE), h = 0.03, use.edge.length = TRUE)

slantedTree(rtree(200, equiprob = FALSE), h = 0.03)

slantedTree(rtree(50, equiprob = FALSE), h = 0.03)

slantedTree(pbtree(n = 50, scale = 2), h = 0.02)


.n <- 36
.nsim <- 25
par(mfcol = c(sqrt(.n), sqrt(.n)), fg = 'white', bg = 'black')

for(i in 1:.n) {
  slantedTree(pbtree(n = .nsim, scale = 1, b = 10, d = 1), h = 0.03, direction = 'upwards')
}

for(i in 1:.n) {
  slantedTree(rtree(.nsim, equiprob = FALSE), h = 0.03, direction = 'upwards')
}

for(i in 1:.n) {
  slantedTree(rcoal(n = .nsim, br = 1), h = 0.03, direction = 'upwards')
}



