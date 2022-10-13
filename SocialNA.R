install.packages('igraph')
library(igraph)

data <- read.csv("socialnetworkdata.csv", header=T)
y <- data.frame(data$first,data$second)

net <- graph.data.frame(y, directed=T)
V(net)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

hist(V(net)$degree,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')



set.seed(222)
plot(net,
     vertex.color = 'green',
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.8)


plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout=layout.graphopt)

net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net,
     edge.arrow.size = 0.1,
     vertex.size = 10,
     vertex.label.cex = 0.8)
