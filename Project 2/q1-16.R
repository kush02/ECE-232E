library("igraph")
library(repr)
library(ggplot2)

setwd("C:/Users/Kushagra/Documents/UCLA-MS/EE 232E/Project 2")
g <- read.graph("facebook_combined.txt",format="edgelist",directed = FALSE)

#Q1
plot(g)
is.connected(g)

#Q2
diameter(g, directed = F)

#Q3
plot(degree.distribution(g),main="Degree distribution",xlab="Degree",ylab="Frequency", pch=16, cex=0.5)
mean(degree(g))

#Q4
plot(degree.distribution(g), log="xy", main="Degree distribution(log scale)",xlab="log Degree",ylab="log Frequency")
segments(15, 0.04, 300, 0.00025, col = "red", lwd=2)
#x1 <- seq(15,300,by=1)
#y1 <- x1^(-1.75) * (1)
#lines(x1, y1, col = 'blue')

#Q5
g1 = make_ego_graph(g, nodes=c('1'))[[1]]
plot(g1, edge.arrow.size=.1, vertex.size=5,vertex.label=NA)
print(vcount(g1))
print(ecount(g1))

#Q6
dm = diameter(g1)

#Q8
nodes = c(V(g))
core_nodes = c()
deg = c()
for(i in nodes){
  if(length(neighbors(g,i)) > 200){
    deg = c(deg, degree(g,i))
    core_nodes = c(core_nodes,i)
  } 
}
print(length(core_nodes))
print(mean(deg))

#Q9
id = c(108)
g1 = make_ego_graph(g, 1,nodes=V(g)[id])[[1]]

cfg <- cluster_fast_greedy(g1)
mod_cfg = modularity(cfg)
plot(g1,mark.groups=groups(cfg),vertex.color=cfg$membership,vertex.size=5,vertex.label=NA,
     main=paste("Fast-Greedy, ID: ",id,", Modularity: ",mod_cfg))

ceb <- cluster_edge_betweenness(g1)
mod_ceb= modularity(ceb)
plot(g1,mark.groups=groups(ceb),vertex.color=ceb$membership,vertex.size=5,vertex.label=NA,
     main=paste("Edge-Betweenness, ID: ",id,", Modularity: ",mod_ceb))

cmap <- cluster_infomap(g1)
mod_cmap = modularity(cmap)
plot(g1,mark.groups=groups(cmap),vertex.color=cmap$membership,vertex.size=5,vertex.label=NA,
     main=paste("Infomap, ID: ",id,", Modularity: ",mod_cmap))


#Q10
id = c(108)
g1 <- induced_subgraph(g, neighbors(g, id))

cfg <- cluster_fast_greedy(g1)
mod_cfg = modularity(cfg)
plot(g1,mark.groups=groups(cfg),vertex.color=cfg$membership,vertex.size=5,vertex.label=NA,
     main=paste("Fast-Greedy, ID: ",id,", Modularity: ",mod_cfg))

ceb <- cluster_edge_betweenness(g1)
mod_ceb= modularity(ceb)
plot(g1,mark.groups=groups(ceb),vertex.color=ceb$membership,vertex.size=5,vertex.label=NA,
     main=paste("Edge-Betweenness, ID: ",id,", Modularity: ",mod_ceb))

cmap <- cluster_infomap(g1)
mod_cmap = modularity(cmap)
plot(g1,mark.groups=groups(cmap),vertex.color=cmap$membership,vertex.size=5,vertex.label=NA,
     main=paste("Infomap, ID: ",id,", Modularity: ",mod_cmap))


#Q12
id <- c(108)

sub_nodes<-c(id,neighbors(g,id))
g1<-induced_subgraph(g,sub_nodes)
V(g1)$label<-sort(sub_nodes)
temp_id<-which(V(g1)$label==id)
neighs<-neighbors(g1,temp_id)
em<-c()
for(i in neighs){
  val<-intersection(neighbors(g1,temp_id),neighbors(g1,i))
  em<-c(em,length(val))
}
hist(em,xlab="Embeddedness", main=paste("Embeddedness for Node ID: ",id))


sub_nodes<-c(id,neighbors(g,id))
g1<-induced_subgraph(g,sub_nodes)
V(g1)$label<-sort(sub_nodes)
temp_id<-which(V(g1)$label==id)
neighs<-neighbors(g1,temp_id)
dis<-c()
for(i in neighs){
  print(i)
  og<-V(g1)$label
  tar<-V(g1)[i]$label
  core<-V(g1)[temp_id]$label
  labs<-c()
  for(j in og)
    if(j!=core & j!=tar)
      labs<-c(labs,j)
  g2<-delete_vertices(g1,c(i,temp_id))
  V(g2)$label<-sort(labs)
  mut_labs<-V(g1)[intersection(neighbors(g1,i),neighbors(g1,temp_id))]$label
  b<-c()
  for(j in V(g2))
    for(k in mut_labs)
      if(V(g2)[j]$label==k)
        b<-c(b,j)
  t<-distances(g2,v=b,to=b)
  t[t==Inf]<-NA
  dis<-c(dis,sum(t,na.rm = TRUE)/2)
}
hist(dis,xlab="Dispersion", main=paste("Dispersion for Node ID: ",id))

#Q13+Q14
id <- c(484)

sub_nodes<-c(id,neighbors(g,id))
g1<-induced_subgraph(g,sub_nodes)
V(g1)$label<-sort(sub_nodes)
temp_id<-which(V(g1)$label==id)
neighs<-neighbors(g1,temp_id)
max_em<-0
max_dis<-0
max_em_node<-0
max_dis_node<-0
max_ratio<-0
max_ratio_node<-0
for(i in neighs){
  print(i)
  
  em_val<-length(intersection(neighbors(g1,temp_id),neighbors(g1,i)))
  
  og<-V(g1)$label
  tar<-V(g1)[i]$label
  core<-V(g1)[temp_id]$label
  labs<-c()
  for(j in og)
    if(j!=core & j!=tar)
      labs<-c(labs,j)
  g2<-delete_vertices(g1,c(i,temp_id))
  V(g2)$label<-sort(labs)
  mut_labs<-V(g1)[intersection(neighbors(g1,i),neighbors(g1,temp_id))]$label
  b<-c()
  for(j in V(g2))
    for(k in mut_labs)
      if(V(g2)[j]$label==k)
        b<-c(b,j)
  t<-distances(g2,v=b,to=b)
  t[t==Inf]<-NA
  dis_val<-sum(t,na.rm = TRUE)/2
  
  if(em_val>max_em){
    max_em<-em_val
    max_em_node<-i
  }
  if(dis_val>max_dis){
    max_dis<-dis_val
    max_dis_node<-i
  }
  if(em_val>0){
    ratio<-dis_val/em_val
    if(ratio>max_ratio){
      max_ratio<-ratio
      max_ratio_node<-i
    }
  }
}

com<-cluster_fast_greedy(g1)

nc<-rep(com$membership+1, length(V(g1)))
ns<-rep(3, length(V(g1)))
ec<-rep("gray", length(E(g1)))
ew<-rep(0.5, length(E(g1)))
ec[which(get.edgelist(g1, name = FALSE)[,1] == max_dis_node |get.edgelist(g1, name = FALSE)[,2] == max_dis_node)] <- "blue";
ew[which(get.edgelist(g1, name = FALSE)[,1] == max_dis_node |get.edgelist(g1, name = FALSE)[,2] == max_dis_node)] <- 5;
ns[max_dis_node] <- 8
nc[max_dis_node] <- "red"
ns[which(V(g1)$label==id)] <- 8
nc[which(V(g1)$label==id)] <- "black"
plot(g1,vertex.size = ns, vertex.label = NA,edge.width = ew, edge.color = ec, vertex.color = nc,
     layout=layout.fruchterman.reingold,main = paste("Node ID: ",id,", Max Dispersion Node: ",max_dis_node))

nc<-rep(com$membership+1, length(V(g1)))
ns<-rep(3, length(V(g1)))
ec<-rep("gray", length(E(g1)))
ew<-rep(0.5, length(E(g1)))
ec[which(get.edgelist(g1, name = FALSE)[,1] == max_em_node |get.edgelist(g1, name = FALSE)[,2] == max_em_node)] <- "blue";
ew[which(get.edgelist(g1, name = FALSE)[,1] == max_em_node |get.edgelist(g1, name = FALSE)[,2] == max_em_node)] <- 5;
ns[max_em_node] <- 8
nc[max_em_node] <- "red"
ns[which(V(g1)$label==id)] <- 8
nc[which(V(g1)$label==id)] <- "black"
plot(g1,vertex.size = ns, vertex.label = NA,edge.width = ew, edge.color = ec, vertex.color = nc,
     layout=layout.fruchterman.reingold,main = paste("Node ID: ",id,", Max Embeddedness Node: ",max_em_node))

nc<-rep(com$membership+1, length(V(g1)))
ns<-rep(3, length(V(g1)))
ec<-rep("gray", length(E(g1)))
ew<-rep(0.5, length(E(g1)))
ec[which(get.edgelist(g1, name = FALSE)[,1] == max_ratio_node |get.edgelist(g1, name = FALSE)[,2] == max_ratio_node)] <- "blue";
ew[which(get.edgelist(g1, name = FALSE)[,1] == max_ratio_node |get.edgelist(g1, name = FALSE)[,2] == max_ratio_node)] <- 5;
ns[max_ratio_node] <- 8
nc[max_ratio_node] <- "red"
ns[which(V(g1)$label==id)] <- 8
nc[which(V(g1)$label==id)] <- "black"
plot(g1,vertex.size = ns, vertex.label = NA,edge.width = ew, edge.color = ec, vertex.color = nc,
     layout=layout.fruchterman.reingold,main = paste("Node ID: ",id,", Max Dispersion/Embeddedness Node: ",max_ratio_node))


#Q16
id <- c(415)
g1<-make_ego_graph(g,nodes=id)[[1]]
deg<-degree(g1)
Nr<-length(which(deg==24))
print(Nr)


