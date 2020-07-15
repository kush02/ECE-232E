library('igraph')

setwd("~/UCLA-MS/EE 232E/Project 4")
companies <- read.csv("Name_sector.csv",header=TRUE,stringsAsFactors=FALSE)
files_to_read <- list.files("data", pattern="*.csv")
files_to_read <- paste("data", files_to_read, sep="/")

L <- c(); k<-1
mat <- matrix(0,length(files_to_read)-11,764)
for(i in c(1:length(files_to_read))){
  df <- read.csv(files_to_read[i],header=TRUE, stringsAsFactors=FALSE)
  L[i] <- dim(df)[1]
  if(L[i]==765){
    p <- df[,5]; q <- c(); r <- c()
    for(j in c(2:length(p))){
      q[j-1] <- (p[j]-p[j-1])/p[j-1]
    }
    r <- log(1+q); mat[k,] <- r; k <- k+1
  }
}

companies["length"] <- L
ind <- which(L!=765)
companies <- companies[-ind,]

getedgelist<- function(conn,mat,companies){
  numCom <- dim(mat)[1]
  cat("from","\t","to","\t","wt",file=conn)
  for(i in c(1:(numCom-1))){
    for(j in c((i+1):numCom)){
      ri <- mean(mat[i,]); rj <- mean(mat[j,])
      mult <- mean(mat[i,]*mat[j,])
      num <- mult-(ri*rj)
      ri2 <- mat[i,]^2; rj2 <- mat[j,]^2
      den <- sqrt((mean(ri2)-(ri^2))*(mean(rj2)-(rj^2)))
      rhoij <- num/den
      wij <- sqrt(2*(1-rhoij))
      cat('\n',companies[i,1],'\t',companies[j,1],'\t',wij,file=conn)
    }
  }
}


# 2
conn <- file("edgelist.txt", "w")
getedgelist(conn,mat,companies)
close(conn)
elist <-read.delim("edgelist.txt",header=TRUE)
g <- graph.data.frame(elist, directed = FALSE)
E(g)$weight <- elist[,"wt"]
hist(elist[,"wt"],col="blue",main="Un-normalized distribution of edge weights",xlab="Edge Weights",ylab="Frequency")

# 3
mst <- mst(g,algorithm="prim")
sectors <- unique(companies[,2])
colorcode<-function(allsect,sector){
  i <- which(allsect==sector)
  color <- switch(i,"red","green","blue","yellow","grey","black","pink","orange","yellowgreen","skyblue","peachpuff")
  return(color)
}
getcolorvector <- function(g,allsect,companies){
  colors <- c()
  for(v in c(1:vcount(g))){
    sector <- companies[v,2]
    colors[v] <- colorcode(allsect,sector)
  }
  return(colors)
}
colors <- getcolorvector(g,sectors,companies)
plot(mst,vertex.size=5, vertex.label=NA, vertex.color=colors)

# 4
counts <- c()
for(i in c(1:length(sectors))){
  counts[i] <- length(which(companies[,2]==sectors[i]))
}
p1 <- c(); p2 <- c()
for(v in c(1:vcount(mst))){
  neigh <- neighbors(mst,v)
  Ni <- length(neigh)
  Qi<-0
  for(i in neigh){
    if(companies[i,2]==companies[v,2])
      Qi<-Qi+1
  }
  p1[v] <- Qi/Ni
  p2[v] <- counts[which(sectors==companies[v,2])]/vcount(mst)
}
alpha1 <- sum(p1)/vcount(mst); alpha2 <- sum(p2)/vcount(mst)

# 5
df["Day"]<-weekdays(as.Date(df[,1]))
L<-c(); k<-1
matw <- matrix(0,length(files_to_read)-13,142)
for(i in c(1:length(files_to_read))){
  df <- read.csv(files_to_read[i],header=TRUE, stringsAsFactors=FALSE)
  df["Day"]<-weekdays(as.Date(df[,1])) 
  df <- subset(df, Day=='Monday')
  L[i] <- dim(df)[1]
  if(L[i]==143){
    p <- df[,5]; q <- c(); r <- c()
    for(j in c(2:length(p))){
      q[j-1] <- (p[j]-p[j-1])/p[j-1]
    }
    r <- log(1+q); matw[k,] <- r; k <- k+1
  }
}
companies_weekly <- read.csv("name_sector.csv",header=TRUE,stringsAsFactors=FALSE)
companies_weekly["length"] <- L
ind <- which(L!=143)
companies_weekly <- companies_weekly[-ind,]
conn <- file("edgelist-weekly.txt", "w")
getedgelist(conn,matw,companies_weekly)
close(conn)
elist_weekly <-read.delim("edgelist-weekly.txt",header=TRUE)
g_weekly <- graph.data.frame(elist_weekly, directed = FALSE)
E(g_weekly)$weight <- elist_weekly[,"wt"]
mst_weekly <- mst(g_weekly,algorithm="prim")
allsect_weekly <- unique(companies_weekly[,2])
colors_weekly <- getcolorvector(g_weekly,allsect_weekly,companies_weekly)
plot(mst_weekly,vertex.size=5, vertex.label=NA, vertex.color=colors_weekly)