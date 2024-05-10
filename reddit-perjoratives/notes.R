library(reshape2)
library(FactoMineR)
library(corrplot)

# https://colinmorris.github.io/blog/compound-curse-words
# https://github.com/colinmorris/pejorative-compounds


x <- read.csv('https://raw.githubusercontent.com/colinmorris/pejorative-compounds/master/counts.csv')
x$pre <- factor(x$pre)
x$suff <- factor(x$suff)

m <- dcast(x, pre ~ suff, value.var = 'count')

str(m)

mm <- as.matrix(m[, -1])
dimnames(mm)[[1]] <- m$pre
dimnames(mm)[[2]] <- names(m)[-1]

# convert to percentiles
# maybe meaningless
e <- ecdf(mm)
mm.p <- mm
mm.p[] <- e(mm)


tab <- as.table(mm)


mosaicplot(tab, shade = TRUE, color = TRUE, cex.axis = 0.1)


ca <- CA(tab, graph = FALSE)

# experiment with transformations
ca <- CA(sqrt(tab), graph = FALSE)


plot(ca, autoLab = 'yes', title = 'Perjorative Compounds', cex=0.75, col.col='firebrick', col.row='royalblue')


par(bg = 'black', fg = 'white')

corrplot(
  log1p(mm), 
  col = hcl.colors(50, palette = 'zissou1'), 
  is.corr = FALSE, 
  col.lim = range(log1p(mm), na.rm = TRUE), 
  method = "color", 
  order = "original",
  diag = TRUE,
  tl.cex = 0.66,
  tl.col = 'white',
  mar = c(0.1, 0, 0, 0.8),
  addgrid = FALSE,
) 

# try re-ordering
col.idx <- order(colSums(mm), decreasing = TRUE)
row.idx <- order(rowSums(mm), decreasing = TRUE)


corrplot(
  log1p(mm[row.idx, col.idx]), 
  col = hcl.colors(50, palette = 'zissou1'), 
  is.corr = FALSE, 
  col.lim = range(log1p(mm), na.rm = TRUE), 
  method = "color", 
  order = "original",
  diag = TRUE,
  tl.cex = 0.66,
  tl.col = 'white',
  mar = c(0.1, 0, 0, 0.8),
  addgrid = FALSE,
) 


# replace 0 with NA
mm.no.zero <- mm
mm.no.zero[mm.no.zero == 0] <- NA

corrplot(
  log1p(mm.no.zero[row.idx, col.idx]), 
  col = hcl.colors(50, palette = 'zissou1'), 
  is.corr = FALSE, 
  col.lim = range(log1p(mm), na.rm = TRUE), 
  method = "color", 
  order = "original",
  diag = TRUE,
  tl.cex = 0.66,
  tl.col = 'white',
  mar = c(0.1, 0, 0, 0.8),
  addgrid = FALSE,
) 



# use CA axes
row.idx <- order(ca$row$coord[, 1])
col.idx <- order(ca$col$coord[, 1])

corrplot(
  log1p(mm[row.idx, col.idx]), 
  col = hcl.colors(50, palette = 'zissou1'), 
  is.corr = FALSE, 
  col.lim = range(log1p(mm), na.rm = TRUE), 
  method = "color", 
  order = "original",
  diag = TRUE,
  tl.cex = 0.8,
  tl.col = 'white',
  mar = c(0.1, 0, 0, 0.8),
  addgrid = FALSE,
) 

# how do you suppress text labels?



## does this make sense.. ?

library(igraph)

g <- graph_from_adjacency_matrix(mm, mode = 'directed', weighted = TRUE, diag = FALSE)

plot(g)

