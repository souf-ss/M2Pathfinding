


library(igraph)


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



#la fonction dijkstra donne la distance entre le noeud de départ et de tous les autres noeuds.

dijkstra <- function(g, start) {
  # Set all distances to infinity
  distance_g <- rep(Inf, length = vcount(g))
  names(distance_g) <- V(g)$name


  # Set distance to source vertex to zero
  distance_g[start] <- 0

  # Q, the queue that initially contains all the vertices
  # on crée un ensemble de sommet Q
  Q <- V(g)$name

  # Previous node in optimal path
  previous <- rep(NA, length = vcount(g))
  names(previous) <- V(g)$name

  # While queue is not empty
  while(length(Q) > 0) {

    # Select the element of Q with the minimum distance
    # Selectionne un element de Q avec le minimum de distance.
    min <- distance_g[Q[1]]

    if ( length(Q) > 1 ) {
      for (i in 2:length(Q)) {
        if (min > (distance_g[Q[i]])) {
          min <- distance_g[Q[i]]
        }
      }
    }
    u <- names(min)

    # Remove u from list of visited vertices
    Q <- Q[which(Q!=u)]

    # for every neighbour (v) of u
    for (v in neighbors(g,u)){
      alt <- distance_g[u] + g[u,v] # weight going from u to it's neighbour v

      # if shortest path is found (i.e. weight on neighbour is greater than going through vertex u)
      if (alt < distance_g[v]) {
        distance_g[v] <- alt
        previous[v] <- u
      }
    }
  }

  return( list(shortPath = distance_g, previous = previous, lastVertex = u))
}

results <- dijkstra(g, start = "C")

#Nous pouvons maintenant lire le chemin le plus court
#de la source à la cible par itération inverse:

shortDPath <- function(results) {
  Seq <- vector(mode = "character", length = 0L)
  u <- results$lastVertex

  while( !is.na(u) ) {
    Seq <- append(Seq, u)
    u <- results$previous[u]
  }

  names(Seq) <- NULL
  return(rev(Seq))
}

Seq <- shortDPath(results)

Seq


