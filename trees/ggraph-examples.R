



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



set_graph_style(plot_margin = margin(1,1,1,1))
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


