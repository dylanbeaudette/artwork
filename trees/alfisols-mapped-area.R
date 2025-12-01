
library(aqp)
library(soilDB)

library(SoilTaxonomy)
library(data.tree)
library(data.table)

library(ape)
library(cluster)
library(igraph)

library(ggraph)
library(tidygraph)
library(purrr)
library(rlang)




# https://ggraph.data-imaginist.com/articles/Nodes.html
# https://ggraph.data-imaginist.com/articles/Edges.html



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

x <- x[, c('pathString', 'soilseriesname', 'ac', 'taxsuborder')]

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


.pal <- hcl.colors(25, 'vik')

ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
  geom_node_circle(aes(filter = leaf, fill = ac), color = 'transparent', alpha = 1) +
  scale_fill_continuous(palette = .pal) + 
  geom_node_circle(aes(filter = !leaf), linewidth = 0.1, color = 'white') +
  guides(fill = 'none', color = 'none') + 
  coord_fixed()



## TODO: use pre-computed colors


# 
# 
# .cols <- colors(25)[sample(1:25, size = nrow(x), replace = TRUE)]
# x$cols <- .cols
# nn <- as.Node(x)
# nn <- nn$alfisols$xeralfs$durixeralfs
# 
# nn$Do(function(node) node$ac <- Aggregate(node, attribute = "ac", aggFun = sum), traversal = "post-order")
# 
# g <- as_tbl_graph(nn, vertexAttributes = c('soilseriesname', 'ac', 'cols'))
# 
# 
# set_graph_style(plot_margin = margin(1, 1, 1, 1), background = 'black')
# 
# ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
#   geom_node_circle(aes(filter = leaf, fill = cols, color = 'transparent'), linewidth = 0.25) +
#   geom_node_circle(linewidth = 0.25, color = 'white') +
#   guides(fill = 'none', color = 'none') + 
#   coord_fixed()
# 
# ## doesn't seem to work as expected
# # ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
# #   geom_node_circle(aes(filter = leaf, fill = cols, color = 'transparent'), linewidth = 0.25) +
# #   scale_color_identity(value = cols) + 
# #   geom_node_circle(linewidth = 0.25, color = 'white') +
# #   guides(fill = 'none', color = 'none') + 
# #   coord_fixed()
# 
# 
# ggraph(g, layout = 'circlepack', circular = TRUE, weight = ac) +
#   geom_node_circle(aes(filter = leaf, fill = ac, color = 'transparent'), linewidth = 0.25) +
#   scale_fill_continuous(transform = 'log') + 
#   geom_node_circle(linewidth = 0.25, color = 'white') +
#   guides(fill = 'none', color = 'none') + 
#   coord_fixed()
# 
# 
# 
