#include <Rcpp.h>
#include<iostream>

using namespace Rcpp;
using namespace std;

#define MAX 10


// [[Rcpp::export]]
typedef struct edge
{
  int src;
  int dest;
  int wt;
}edge;


// [[Rcpp::export]]
void bellman_ford(int nv,edge e[],int src_graph,int ne)
{
  int u,v,weight,i,j=0;
  int dis[MAX];
  
  /* initializing array 'dis' with 999. 999 denotes infinite distance */
  for(i=0;i<nv;i++)
  {
    dis[i]=999;
  }
  
  /* distance of source vertex from source vertex is o */
  dis[src_graph]=0;
  
  /* relaxing all the edges nv - 1 times */
  for(i=0;i<nv-1;i++)
  {
    for(j=0;j<ne;j++)
    {
      u=e[j].src;
      v=e[j].dest;
      weight=e[j].wt;
      
      if(dis[u]!=999 && dis[u]+weight < dis[v])
      {
        dis[v]=dis[u]+weight;
      }
    }
    
  }
  
  /* checking if negative cycle is present */
  for(j=0;j<ne;j++)
  {
    u=e[j].src;
    v=e[j].dest;
    weight=e[j].wt;
    
    if(dis[u]+weight < dis[v])
    {
      cout<<"\n\nNEGATIVE CYCLE PRESENT..!!\n";
      return;
    }
  }
  
  cout<<"\nVertex"<<"  Distance from source";
  for(i=1;i<=nv;i++)
  {
    cout<<"\n"<<i<<"\t"<<dis[i];
  }
}

/*
 int main()
 {
 
 int nv,ne,vertex_source;
 edge e[MAX];
 nv = 4 ;
 ne = 4  ;
 vertex_source = 0;
 
 e[0].src =  0 ;
 e[0].dest = 1;
 e[0].wt = 4;
 
 e[1].src =  0;
 e[1].dest = 3;
 e[1].wt = 5;
 
 e[2].src =  2;
 e[2].dest = 1;
 e[2].wt = -10;
 
 e[3].src =  3 ;
 e[3].dest = 2;
 e[3].wt = 3;
 
 
 bellman_ford(nv,e,vertex_source,ne);
 
 return 0;
 }
 */
