
library(igraph)
library(Algorithmique)

#exemple graphe a partir de la librairie igraph
g <- as.matrix(read.table(text=
                            "C T1 V1 T2 V2 WS1 WS2 TC1 TC2 RRC
 C    0  8 10 12  0   0   0   0   0   0
 T1   8  0  2  0  0   0   0   0  12   0
 V1  10  2  0  0  2   0   0   0   0   0
 T2  12  0  0  0  0   5   0  10   0   0
 V2   0  0  2  0  0   1   7   0   0   0
 WS1  0  0  0  5  1   0   0   2   0   0
 WS2  0  0  0  0  7   0   0   2   2   0
 TC1  0  0  0 10  0   2   2   0   5   8
 TC2  0 12  0  0  0   0   2   5   0   3
 RRC  0  0  0  0  0   0   0   8   3   0", header=T))


g <- graph.adjacency(g, mode="undirected", weighted=TRUE,diag = FALSE)
str(g)
#plot(g)

plot(g,edge.label=E(g)$weight)

results <- dijkstra(g, start = "C")

Seq <- shortDPath(results)

#chemin optimal partant de "C" pour aller a "RRC"
Seq




library(Rcpp)

sourceCpp("~/cours/M2_algo/Algorithmique/src/path_Function.cpp")

#matrice d'adjacence representant le poid de chaque arete
edges <- matrix(data = c(0,4,8,0,0,4,0,2,5,0,8,2,0,5,9,0,5,5,0,4,0,0,9,4,0),5,5)
dijkstra_cpp(edges)


