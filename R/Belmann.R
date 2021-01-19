BellmanFord <- function(vertices, edges, vertex_source){
  
  # le vecteur vertices doit être de la forme
  # la fonction est fait en considérant la forme suivante des edges l'element "edges" :
  #       un élément "e" de "edges" est de la forme  : e = c(sommet de départ, sommet arrivée, poids de l'arrête entre les sommets)
  
  n <- length(vertices)                       # on récupère le nombre de sommet "n" dans notre graphe
  k <-1                                       # sert pour l'arret de l'algo
  distance <- vector("list", length = n)      # liste des distances de chaque sommet partant de sommet source = "vertex_source"
  predecessor <- vector("list", length = n)   # stocke les sommets précédents
  
  # Etape 1 : Initialisation du graphe
  
  
  for(v in 1:n){
    distance[v] <- Inf                        # Initialisation des distances à l'infinity
    predecessor[v] <-  NULL                   # Initialisation des predécesseurs
    distance[vertex_source] <- 0              # diastance du sommet initial
  }
  
  # Etape 2 : relaxation de l'algorithme en répétant l'actualisation des distances
  
  repeat {
    for(e in edges){
      if (as.numeric(distance[e[1]])+ (e[3]) < as.numeric(distance[e[2]]) ){     # si (distance départ + poids arrete < distances arrivée)
        distance[e[2]] <- as.numeric(distance[e[1]]) + (e[3])                    # on actualise la distance de l'arrivée
        predecessor[as.numeric(e[2])] <- e[1]                                    # on stocke ayant donnée l'actusalition
      }
    }
    k<- k+1
    if (k > n-1){                              # on arrète l'algo à n-1 itérations
      break
    }
  }
  
  # Etape 3: recherche de cycle de poids négatif
  
  for(e in edges){
    if (as.numeric(distance[e[1]]) + (e[3]) < as.numeric(distance[e[2]])){    # on vérifie si au-delà de n-1 itération on peut tjrs améliorer les distances
      #print("le graphe contient un cycle de poids négatif")
      message("le graphe contient un cycle de poids négatif, l'algo utilisé n'est pas adapté à votre problème")
    }
  }
  
  
  return (c("dist" = distance, "pred" = predecessor, "k" = k))
  
}

#example
#vertices <-list('1','2', '3','4')
#edges1 <- list(c(1,2,4),c(1,4,5), c(3,2,-10), c(4,3,3))
#edges2 <- list(c(1,2,4),c(1,4,5), c(2,4,5), c(3,2,-10), c(4,3,3))

#BellmanFord(vertices, edges1, 1)
#BellmanFord(vertices, edges2, 1)
