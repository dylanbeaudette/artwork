library(aqp)
library(cluster)
library(ape)
library(farver)

# add an extra, dummy column name
# remove non-row data at bottom
# removed previous data and bogus black color
m <- read.csv('fall-colors-2025.csv')

# subset
m <- m[, c('Time.Stamp', 'L', 'A', 'B')]

.rgb <- grDevices::convertColor(m[, c('L', 'A', 'B')], from = 'Lab', to = 'sRGB', from.ref.white = 'D65')

m$cols <- rgb(.rgb, maxColorValue = 1)

.munsell <- col2Munsell(m[, c('L', 'A', 'B')], space = 'CIELAB')
m$m <- sprintf("%s %s/%s", .munsell$hue, .munsell$value, .munsell$chroma)

table(m$m)


# TODO: compare all permutations when m2 is missing
# colorContrast(m)

d <- compare_colour(
  m[, c('L', 'A', 'B')], 
  m[, c('L', 'A', 'B')], 
  from_space = 'lab', 
  to_space = 'lab', 
  method = 'cie2000', 
  white_from = 'D65',
  white_to = 'D65'
)

dimnames(d) <- list(m$m, m$m)
d <- as.dist(d)

p <- as.phylo(as.hclust(diana(d)))

plot(p, label.offset = 3, font = 1, type = 'unrooted', no.margin = TRUE)
tiplabels(pch = 15, cex = 4, col = m$cols)

plot(p, label.offset = 3, font = 1, type = 'unrooted', no.margin = TRUE, use.edge.length = FALSE)
tiplabels(pch = 15, cex = 4, col = m$cols)

plot(p, label.offset = 3, font = 1, type = 'unrooted', no.margin = TRUE, use.edge.length = FALSE)
tiplabels(pch = 0, cex = 4, col = 'black')


par(mar = c(1, 1, 1, 1), xpd = NA)
plot(p, label.offset = 3, font = 1, type = 'fan', show.tip.label = FALSE)
tiplabels(pch = 15, cex = 4, col = m$cols, offset = 2)

plot(p, label.offset = 2, font = 1, type = 'cladogram', no.margin = TRUE, direction = 'downwards')
tiplabels(pch = 15, cex = 4, col = m$cols, offset = 1)

plot(p, label.offset = 2, font = 1, type = 'cladogram', no.margin = TRUE, direction = 'downwards')
tiplabels(pch = 0, cex = 4, col = 'black', offset = 1)


slantedTree(p)
tiplabels(pch = 15, cex = 4, col = m$cols, offset = 1)


