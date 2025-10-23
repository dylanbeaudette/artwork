library(aqp)
library(cluster)
library(ape)
library(farver)


m <- c('10YR 4/6', '5G 4/6', '2.5R 3/8', '5R 4/6', '10RP 4/8', '5G 2/4', '10YR 6/10', '7.5YR 4/8', '5Y 6/6', '10YR 4/6')

m <- as.data.frame(table(m))

m$cols <- parseMunsell(m$m)

# weights based on number of occurences


# TODO: compare all permutations when m2 is missing
# colorContrast(m)

lab <- parseMunsell(m$m, returnLAB = TRUE)

d <- compare_colour(lab, lab, from_space = 'lab', to_space = 'lab', method = 'cie2000', white_from = 'D65', white_to = 'D65')

dimnames(d) <- list(m$m, m$m)
d <- as.dist(d)

p <- as.phylo(as.hclust(diana(d)))

plot(p, label.offset = 3, font = 1, type = 'unrooted', no.margin = TRUE)
tiplabels(pch = 15, cex = 2 * m$Freq, col = m$cols)


plot(p, label.offset = 3, font = 1, type = 'unrooted', no.margin = TRUE, use.edge.length = FALSE)
tiplabels(pch = 15, cex = 4, col = cols)
