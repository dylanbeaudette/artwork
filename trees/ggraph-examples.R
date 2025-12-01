
library(aqp)
library(soilDB)

library(SoilTaxonomy)
library(data.tree)
library(data.table)

library(ape)
library(cluster)

library(ggraph)
library(tidygraph)
library(purrr)
library(rlang)

library(igraph)


# https://ggraph.data-imaginist.com/articles/Nodes.html
# https://ggraph.data-imaginist.com/articles/Edges.html


set_graph_style(plot_margin = margin(1, 1, 1, 1), background = 'black')

tr <- create_tree(50, 5)

ggraph(tr, 'dendrogram') + 
  geom_edge_elbow(strength = 0.9, color = 'white')

ggraph(tr, 'dendrogram') + 
  geom_edge_diagonal(strength = 0.5, color = 'white')

ggraph(tr, 'dendrogram') + 
  geom_edge_bend(color = 'white')



graph <- tbl_graph(flare$vertices, flare$edges)
set.seed(1)

ggraph(graph, 'circlepack', weight = size) + 
  geom_node_circle(linewidth = 0.25, color = 'white') + 
  coord_fixed()


#
# define a vector of series
s.list <- c('amador', 'redding', 'pentz', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'drummer', 'musick', 'zook', 'argonaut', 'PALAU')

# get and SPC object with basic data on these series
s <- fetchOSD(s.list)

# extract horizon data from select OSDs in above example
h <- horizons(s)

# convert Munsell color notation to sRGB
# these are moist colors
rgb.data <- munsell2rgb(h$hue, h$value, h$chroma, return_triplets = TRUE)
lab.data <- munsell2rgb(h$hue, h$value, h$chroma, returnLAB = TRUE)

# retain unique colors
rgb.data <- unique(rgb.data)
lab.data <- unique(lab.data)

# create distance matrix from LAB coordinates
d <- daisy(lab.data, stand = FALSE)

# divisive hierarchical clustering
d.hclust <- as.hclust(diana(d))

g <- as_tbl_graph(d.hclust)


ggraph(g, 'circlepack') + 
  geom_node_circle(linewidth = 0.25, color = 'white') + 
  coord_fixed()


## TODO: use actual colors

# ggraph(g, 'circlepack') + 
#   geom_node_circle(aes(filter = leaf, fill = 'white'), linewidth = 0.25) + 
#   scale_fill_identity(h$soil_color) + 
#   coord_fixed()
# 
# 
# # ... something like this?
# ggplot(mt, aes(mpg, disp, color = literal_color)) +
#   geom_point() +
#   scale_color_identity()

###


# series stats
u <- 'https://github.com/ncss-tech/SoilWeb-data/raw/refs/heads/main/files/series_stats.csv.gz'
s <- fread(u)
s <- as.data.frame(s)

 
# SC database
u <- 'https://github.com/ncss-tech/SoilWeb-data/raw/refs/heads/main/files/SC-database.csv.gz'
sc <- fread(u)
sc <- as.data.frame(sc)

sc <- sc[which(sc$soilseriesstatus == 'established'), ]

x <- sc[which(sc$taxorder == 'alfisols'), ]

x <- merge(x, s, by.x = 'soilseriesname', by.y = 'series', all.x = TRUE, sort = FALSE)


x$pathString <- paste('ST', x$taxorder, x$taxsuborder, x$taxgrtgroup, x$taxsubgrp, x$taxclname, x$soilseriesname, sep = '/')

x <- x[, c('pathString', 'soilseriesname', 'ac')]

x$ac[which(is.na(x$ac))] <- 1

n <- as.Node(x)
# compute acreage for parent nodes
n$Do(function(node) node$ac <- Aggregate(node, attribute = "ac", aggFun = sum), traversal = "post-order")

print(n, 'ac')


g <- as_tbl_graph(n, vertexAttributes = c('soilseriesname', 'ac'))



set_graph_style(plot_margin = margin(1, 1, 1, 1), background = 'black')

ggraph(g, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(linewidth = 0.25, color = 'white') +
  coord_fixed()


ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
  geom_node_circle(linewidth = 0.25, color = 'white') +
  coord_fixed()


# what?
ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
  # geom_node_circle(linewidth = 0.25, color = 'white') +
  geom_edge_diagonal(linewidth = 0.25, color = 'white') +
  coord_fixed()

# hmm..
ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
  geom_node_circle(linewidth = 0.25, color = 'white') +
  # geom_edge_diagonal(linewidth = 0.25, color = 'white') +
  geom_node_point(aes(filter = leaf), color = 'white', size = 0.1) + 
  coord_fixed()


## doesn't work as expected

# ig <- as.igraph.Node(n)
# .el <- as_edgelist(g)
# 
# ggraph(g, layout = 'circlepack', circular = TRUE) +
#   geom_conn_bundle(data = get_con(from = .el[, 1], to = .el[, 2]),  alpha = 0, color = 'red')
# 


# subgroup acreages from SoilWeb / SSURGO
u <- 'https://github.com/ncss-tech/SoilWeb-data/raw/refs/heads/main/files/taxsubgrp-stats.txt.gz'
sg.ac <- fread(file = u, header = FALSE, sep = "|")
names(sg.ac) <- c('subgroup', 'ac', 'n_polygons')

# normalize names
sg.ac$subgroup <- tolower(sg.ac$subgroup)


data("ST")
ST <- ST[, 1:4]

# LEFT JOIN to acreage
ST <- merge(ST, sg.ac, by = 'subgroup', all.x = TRUE)

# set NA acreage to 0
ST$ac[which(is.na(ST$ac))] <- 1

# circle area proportional to square root of "acres mapped"
ST$ac <- sqrt(ST$ac)

ST$pathString <- paste('ST', ST$order, ST$suborder, ST$greatgroup, ST$subgroup, sep = '/')
n <- as.Node(ST)

alf <- n$alfisols$xeralfs
print(alf, 'ac')

# compute acreage for parent nodes
alf$Do(function(node) node$ac <- Aggregate(node, attribute = "ac", aggFun = sum), traversal = "post-order")


# g <- as.igraph.Node(alf, vertexAttributes = 'ac')

## TODO: simpler to manipulate as an igraph object vs. tidygraph API 

# see arguments to as.igraph.Node()
# preservation of node attributes
g <- as_tbl_graph(alf, vertexAttributes = 'ac')

set_graph_style(plot_margin = margin(1, 1, 1, 1), background = 'black')

ggraph(g, 'circlepack', weight = ac) +
  geom_node_circle(linewidth = 0.25, color = 'white') +
  coord_fixed()


ggraph(g, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(linewidth = 0.25, color = 'white') +
  coord_fixed()




# 
# compute acreage for parent nodes
n$Do(function(node) node$ac <- Aggregate(node, attribute = "ac", aggFun = sum), traversal = "post-order")


# see arguments to as.igraph.Node()
# preservation of node attributes
g <- as_tbl_graph(n, vertexAttributes = 'ac')

ggraph(g, 'circlepack', weight = ac) + 
  geom_node_circle(linewidth = 0.25, color = 'white') + 
  coord_fixed()



ggraph(g, 'treemap', weight = ac) + 
  geom_node_tile(linewidth = 0.25, color = 'white')


ggraph(g, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(linewidth = 0.25, color = 'white') +
  coord_fixed()


## 

gr <- as_tbl_graph(highschool)

ggraph(gr, layout = 'kk') + 
  geom_point(aes(x = x, y = y))


ggraph(gr, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() + 
  # geom_node_point(aes(filter = leaf)) + 
  coord_fixed()



flaregraph <- tbl_graph(flare$vertices, flare$edges)
from <- match(flare$imports$from, flare$vertices$name)
to <- match(flare$imports$to, flare$vertices$name)

ggraph(flaregraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha = 0.1) + 
  coord_fixed()


