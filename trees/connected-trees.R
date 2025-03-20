# http://blog.phytools.org/2024/10/an-ntip-s3-method-for-cophylo-object.html


library(phytools)

data("wasp.trees")
data("wasp.data")


obj <- cophylo(wasp.trees[[1]], wasp.trees[[2]], wasp.data, print = FALSE)

plot(obj, link.type = "curved", type = "cladogram", lwd = 3, fsize = 0.7, part = 0.45)
