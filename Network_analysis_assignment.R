install.packages("igraph")
library("igraph")
florence = as.matrix(read.csv("network.csv"))
florence
marriage = graph.adjacency(florence, mode="undirected", diag=FALSE)
set.seed(1)
plot(marriage,layout=layout.fruchterman.reingold, vertex.label=V(marriage)$name,vertex.color="red", vertex.label.color="black", vertex.frame.color=0, vertex.label.cex=1.5)
data.frame(V(marriage)$name,degree(marriage))

## Calculate and plot the shortest paths
V(marriage)$color = 8
E(marriage)$color = 8
PtoA = get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")
E(marriage, path=PtoA[[1]])$color = "magenta"
V(marriage)[PtoA[[1]] ]$color = "magenta"
GtoS = get.shortest.paths(marriage, from="Ginori", to="Strozzi")
E(marriage, path=GtoS[[1]])$color = "green"
V(marriage)[GtoS[[1]]]$color = "magenta"
V(marriage)["Medici"]$color = "cyan"

set.seed(1)
plot(marriage, layout=layout.fruchterman.reingold, vertex.lavel=V(marriage)$name, vertex.label.color="black", vertex.frame.color=0, vertex.label.cex=1.5)

data.frame(V(marriage)$name, betweenness(marriage))
