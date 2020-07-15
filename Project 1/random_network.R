library(igraph)
library('Matrix')
library('pracma')
library(repr)

#1.1.a
for(p in c( 0.003, 0.004, 0.01, 0.05, 0.1)){
  g <- erdos.renyi.game(1000, p)
  hist(degree(g))
  print(p)
  print(mean(degree(g)))
  print(var(degree(g)))
}

#1.1.b
for(p in c( 0.003, 0.004, 0.01, 0.05, 0.1))
{
  g<-erdos.renyi.game(1000, p, directed=F)
  if(!is.connected(g))
  {
    g.components <- clusters(g)
    ix <- which.max(g.components$csize)
    g.giant <- induced.subgraph(g, which(g.components$membership == ix))
    dm=diameter(g.giant, directed = FALSE, unconnected = TRUE, weights = NULL)
    print(dm)
    print(vcount(g.giant))
  }
}

#1.1.c
l<-list()
val<-list()
i<-1
for (p in seq(0.000, 0.01, by = 0.0001)){
  for(j in 1:101){
    g <- erdos.renyi.game(1000, p)
    g.components <- clusters(g)
    ix <- which.max(g.components$csize)
    g.giant <- induced.subgraph(g, which(g.components$membership == ix))
    l[[i]]<-p
    val[[i]]<-vcount(g.giant)/1000
    i<-i+1
  }
}
plot(x=l,y=val,col="blue",main="Normalized GCC vs p",xlab="p",ylab="Normalized GCC")

l<-list()
val<-list()
i<-1
for (p in seq(0.000, 0.01, by = 0.0001)){
  a<-0
  for(j in 1:101){
    g <- erdos.renyi.game(1000, p)
    g.components <- clusters(g)
    ix <- which.max(g.components$csize)
    g.giant <- induced.subgraph(g, which(g.components$membership == ix))
    a<-a+vcount(g.giant)/1000
  }
  l[[i]]<-p
  val[[i]]<-a/100
  i<-i+1
}
lines(x=l,y=val,col="blue",main="Average normalized GCC vs p",xlab="p",ylab="Average normalized GCC")

#1.1.d i
avg=0.5
l<-list()
l1<-list()
i<-1
for (n in seq(100, 10000, by = 100)){
  g <- erdos.renyi.game(n, avg/n)    
  g.components <- clusters(g)
  ix <- which.max(g.components$csize)
  g.giant <- induced.subgraph(g, which(g.components$membership == ix))   
  l[[i]]<-vcount(g.giant)
  l1[[i]]<-n
  i<-i+1
}
plot(x=l1,y=l,col="red",main="Average degree of node=0.5",xlab="Number of nodes",ylab="Size of GCC")

#1.1.d ii
avg=1
l<-list()
i<-1
for (n in seq(100, 10000, by = 100)){
  g <- erdos.renyi.game(n, avg/n)    
  g.components <- clusters(g)
  ix <- which.max(g.components$csize)
  g.giant <- induced.subgraph(g, which(g.components$membership == ix))   
  l[[i]]<-vcount(g.giant)
  i<-i+1
}
plot(y=l,x=seq(100, 10000, by = 100),col="red",main="Average degree of node=1",xlab="Number of nodes",ylab="Size of GCC")

#1.1.d iii
avg=1.1
l1<-list()
i<-1
for (n in seq(100, 10000, by = 100)){
  g <- erdos.renyi.game(n, avg/n)    
  g.components <- clusters(g)
  ix <- which.max(g.components$csize)
  g.giant <- induced.subgraph(g, which(g.components$membership == ix))   
  l1[[i]]<-vcount(g.giant)
  i<-i+1
}
avg=1.2
l2<-list()
i<-1
for (n in seq(100, 10000, by = 100)){
  g <- erdos.renyi.game(n, avg/n)    
  g.components <- clusters(g)
  ix <- which.max(g.components$csize)
  g.giant <- induced.subgraph(g, which(g.components$membership == ix))   
  l2[[i]]<-vcount(g.giant)
  i<-i+1
}
avg=1.3
l3<-list()
i<-1
for (n in seq(100, 10000, by = 100)){
  g <- erdos.renyi.game(n, avg/n)    
  g.components <- clusters(g)
  ix <- which.max(g.components$csize)
  g.giant <- induced.subgraph(g, which(g.components$membership == ix))   
  l3[[i]]<-vcount(g.giant)
  i<-i+1
}

plot(y=l1,x=seq(100, 10000, by = 100),col="red",main="Average degree of node=1.1,1.2 and 1.3",xlab="Number of nodes",ylab="Size of GCC")
points(y=l2,x=seq(100, 10000, by = 100),col="blue")
points(y=l3,x=seq(100, 10000, by = 100),col="green")
legend(1,2000,c("1.1","1.2","1.3"),col=c("red","blue","green"),lty=1, cex=0.8)

#1.2.a
for (i in seq(1:1000)){
  g<-barabasi.game(1000, m = 1,directed=FALSE)
  if(!is.connected(g)){print("Not Connected")}
  else {print("Connected")}
}

#1.2.b
g<-barabasi.game(1000, m = 1,directed=FALSE)
com <- cluster_fast_greedy(g)
mod <- modularity(com)
print(mod)
plot(com,g,vertex.size=5, vertex.label.cex=0.3)

#1.2.c
g<-barabasi.game(10000, m = 1,directed=FALSE)
com <- cluster_fast_greedy(g)
mod <- modularity(com)
print(mod)
plot(com,g,vertex.size=5, vertex.label.cex=0.3)

#1.2.d
g1<-barabasi.game(1000, m = 1,directed=FALSE)
g2<-barabasi.game(10000, m = 1,directed=FALSE)
plot(x=degree_distribution(g1),log="xy",type="l",col="red",main="Degree distribution on a log-log scale",xlab=" ",ylab="")
lines(x=degree_distribution(g2),log="xy",type="l",col="blue")
legend("topright",c("n=1000","n=10000"),col=c("red","blue"),lty=1, cex=0.8)

d<-degree(g1,mode="all")
fit1 <- fit_power_law(d)
print(fit1$alpha)

d<-degree(g2,mode="all")
fit1 <- fit_power_law(d)
print(fit1$alpha)

#1.2.e
degs<-list()
i<-1
for (n in sample(vcount(g1), 100)){
  if(length(neighbors(g1,n))==1){
    n1<-neighbors(g1,n)
  }
  else {
    n1<-sample(neighbors(g1,n),1)
  }
  degs[[i]]<-degree(g1,n1)
  i<-i+1
}
deg<-table(unlist(degs))
deg<-deg/sum(table(unlist(degs)))
plot(x=deg,type="l",col="red",log="x",main="Degree distribution on a log-log scale for n=10000",xlab="log Degree ",ylab="log P")

#1.2.f
g<-barabasi.game(1000, m = 1,directed=FALSE)
degs<-list()
i<-1
for(n in seq(1,1000,1)){
  degs[[i]]<-degree(g,n)
  i<-i+1
}
deg<-unlist(degs)
plot(x=seq(1000,1,-1),deg,type="l",col="red",main="Degree vs Age",xlab="Age ",ylab="Degree")

#1.2.h
g<-barabasi.game(1000, m = 1,directed=FALSE)
degs<-list()
i<-1
for(n in seq(1,1000,1)){
  degs[[i]]<-degree(g,n)
  i<-i+1
}
deg_seq<-unlist(degs)
g1 <- degree.sequence.game(deg_seq,method="simple.no.multiple")

com <- cluster_fast_greedy(g)
plot(com,g)
com1 <- cluster_fast_greedy(g1)
plot(com1,g1)
mod <- modularity(com)
print(mod)
mod1 <-modularity(com1)
print(mod1)

#1.3.a
g=sample_pa_age(1000,m=1,pa.exp=1,aging.exp=-1,directed=FALSE)
barplot(degree_distribution(g),col="blue",main="Degree Distribution for n=1000",xlab="Degree ",ylab="Frequency")
d<-degree(g,mode="all")
fit1 <- fit_power_law(d)
print(fit1$alpha)

#1.3.b
com <- cluster_fast_greedy(g)
plot(com,g)
mod = modularity(fg)
print(mod)

