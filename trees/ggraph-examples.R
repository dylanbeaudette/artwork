



library(ggraph)
library(tidygraph)
library(purrr)
library(rlang)


# https://ggraph.data-imaginist.com/articles/Nodes.html
# https://ggraph.data-imaginist.com/articles/Edges.html


tr <- create_tree(50, 5)

ggraph(tr, 'dendrogram') + 
  geom_edge_elbow(strength = 0.9)

ggraph(tr, 'dendrogram') + 
  geom_edge_diagonal(strength = 0.5)

ggraph(tr, 'dendrogram') + 
  geom_edge_bend()



graph <- tbl_graph(flare$vertices, flare$edges)
set.seed(1)

ggraph(graph, 'circlepack', weight = size) + 
  geom_node_circle(size = 0.25, n = 50) + 
  coord_fixed()


library(SoilTaxonomy)
library(data.tree)

# subgroup acreages from SoilWeb / SSURGO
sg.ac <- read.table(file = '../SoilWeb-data/files/taxsubgrp-stats.txt.gz', header = FALSE, sep="|")
names(sg.ac) <- c('subgroup', 'ac', 'n_polygons')

# normalize names
sg.ac$subgroup <- tolower(sg.ac$subgroup)


data("ST")
ST <- ST[, 1:4]

# LEFT JOIN to acreage
ST <- merge(ST, sg.ac, by = 'subgroup', all.x = TRUE)

# set NA acreage to 0
ST$ac[which(is.na(ST$ac))] <- 0


ST$pathString <- paste('ST', ST$order, ST$suborder, ST$greatgroup, ST$subgroup, sep = '/')
n <- as.Node(ST)

alf <- n$alfisols$xeralfs
print(alf, 'ac')

# compute acreage for parent nodes
alf$Do(function(node) node$ac <- Aggregate(node, attribute = "ac", aggFun = sum), traversal = "post-order")

# TODO: retain node attributes

g <- as_tbl_graph(alf)

set_graph_style(plot_margin = margin(1,1,1,1))

ggraph(g, 'circlepack') + 
  geom_node_circle(size = 0.25) + 
  coord_fixed()


# un weighted
g <- as_tbl_graph(n)

set_graph_style(plot_margin = margin(1,1,1,1))

ggraph(g, 'circlepack') + 
  geom_node_circle(size = 0.25) + 
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


