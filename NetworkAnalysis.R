install.packages("igraph") 
library(igraph)
edges <- rbind(c("Dave","Jenny"),c("Peter","Jenny"),c("John","Jenny"),
               c("Dave","Peter"),c("Dave","John"),c("Peter","Sam"),
               c("Sam","Albert"),c("Peter","John"))


###### undirected graph 
ug<-graph.edgelist(edges,directed=FALSE)     
plot(ug,vertex.size=30,vertex.label.cex=0.6)  # plotting

E(ug) 
V(ug)

indeg<-degree(ug,mode="in")      
outdeg<-degree(ug,mode="out")    
totaldeg<-degree(ug,mode="all") 
indeg

x<-cbind(indeg,outdeg,totaldeg)
cor(x)                   #we get cor=1 becuase it is undirected so the three degree are just the same


####### directed graph
dg<-graph.edgelist(edges,directed=TRUE)   
plot(dg,vertex.size=30,vertex.label.cex=0.5
     ,edge.arrow.size=0.6)

indeg<-degree(dg,mode="in")
outdeg<-degree(dg,mode="out")
totaldeg<-degree(dg,mode="all")

x<-cbind(indeg,outdeg,totaldeg)
cor(x) 

V(dg)$indeg<-indeg*10

plot(dg,vertex.size=V(dg)$indeg,vertex.label.cex=0.5,edge.arrow.size=0.05,
     layout=layout.random)

plot(dg,vertex.size=V(dg)$indeg,vertex.label.cex=0.5,edge.arrow.size=0.05,
     layout=layout.circle)


######## Betweenness centrality

btw <- betweenness(dg,directed=TRUE)  
V(dg)$btw <- btw*10

plot(dg,vertex.size=V(dg)$btw,vertex.label.cex=0.8,edge.arrow.size=0.2)

x<-cbind(indeg,btw)
cor(x)


