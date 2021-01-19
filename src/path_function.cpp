#include <Rcpp.h>
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]

// function qui determine le sommet ayant la plus petite distance
int find_distance_min(NumericVector distance ,LogicalVector visited, int n){

  int minVertex=-1;
  for (int i=0 ;i< n; i++){
    if (!visited[i] && (minVertex==-1 || distance[i]< distance[minVertex])){
      minVertex = i;
    }

  }
  return minVertex;
}



// Function to print shortest
// path from source to j
// using parent array
// [[Rcpp::export]]
void short_path(NumericVector parent, int j)
{

  // Base Case : If j is source
  if (parent[j] == - 1)
    return;

  short_path(parent, parent[j]);

  Rcout<< j;
}

// A utility function to print
// the constructed distance
// array
// [[Rcpp::export]]
void path(NumericVector dist, int n,NumericVector parent)
{
  int src = 0;
  Rcout<<"V  | Dist | Path";
  for (int i = 1; i < n; i++)
  {
    Rcout<<endl<<src<<"->"<<i<<"| "<<dist[i]<<" | "<<src;
    short_path(parent, i);
  }
}



// [[Rcpp::export]]

void dijkstra_cpp(NumericMatrix edges){
  int n;
  n=edges.ncol();
  NumericVector distance(n);
  NumericVector parent(n);
  LogicalVector visited(n);

  for (int i=0; i<n; i++){
    parent[0]=-1;
    distance[i]= INT_MAX;
    visited[i]= false;
  }
  distance[0]=0;

  for (int i=0;i<n-1;i++){

    int minVertex=find_distance_min(distance,visited,n);

    visited[minVertex]=true;

    for (int j=0;j<n;j++){

      if(edges(minVertex,j) !=0 && !visited[j]){
        int dist = distance[minVertex] + edges(minVertex,j);
        if (dist< distance[j]){
          parent[j]=minVertex;
          distance[j] = dist;
        }
      }
    }

  }


  Rcout<<endl<<"les chemins les plus courts partant du sommet d'origine "<<endl;
  path(distance, n, parent);


}

/*** R
library(Rcpp)

sourceCpp("~/cours/M2_algo/Algorithmique/src/path_Function.cpp")

#matrice d'adjacence representant le poid de chaque arete
edges <- matrix(data = c(0,4,8,0,0,4,0,2,5,0,8,2,0,5,9,0,5,5,0,4,0,0,9,4,0),5,5)
edges
dijkstra_cpp(edges)

*/
