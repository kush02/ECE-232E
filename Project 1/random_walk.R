library('igraph')
library('Matrix')
library('pracma')
library(lintr)
library(repr)


##################################################################################################
# Functions
##################################################################################################

create_transition_matrix = function (g){

    # WARNING: make sure your graph is connected (you might input GCC of your graph)

    n = vcount(g)
    adj = as_adjacency_matrix(g)
    adj[diag(rowSums(adj) == 0)] = 1  # handle if the user is using the function for networks with "isolated nodes" and "sinks" by creating self-edges
    z = matrix(rowSums(adj, , 1))     # vectorize the adjacency matrix into a column (# rows = length(adj), # cols = 1) ()

    transition_matrix = adj / repmat(z, 1, n)  # normalize to get probabilities (no broadcasting in r, so replicate column vector "n" times)

    return(transition_matrix)
}

random_walk_pagerank = function (g, num_steps, start_node, transition_matrix = NULL){
  # The input must be connected graph
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g)
  
  walker_path = numeric(length=num_steps)
  v = start_node
  for(i in 1:num_steps){
    PMF = transition_matrix[v, ]            # gets probability of each outgoing link of v
    v = sample(1:vcount(g), 1, prob = PMF)  # traverse to a new node from an outoging link of v and set it as the new v
    walker_path[i] = v                      # add the new v to the random walker's path
  }
  
  return(walker_path)
}

random_walk_with_teleport = function (g, num_steps, start_node, transition_matrix = NULL, probability = NULL){
  vcount_g = vcount(g)
  if(is.null(transition_matrix))
    transition_matrix = create_transition_matrix(g)
  if(is.null(probability))
    probability = rep(1/vcount_g, vcount_g)             # teleportation operation: teleport to any edge with probability 1/vcount_g
  
  row_nodes = numeric(length=num_steps)
  v = start_node
  for(i in 1:num_steps){
    PMF = transition_matrix[v, ]                        # gets probability of each outgoing link of v
    random = sample(1:100, 1)
    if(random <= 15 || degree(g, v, mode = "out") == 0) # perform teleportation with a probability of 15% or if the current node has no outgoing links
      v = sample(1:vcount_g, 1, prob = probability)     # perform teleportation with equal probability or with pagerank
    else
      v = sample(1:vcount_g, 1, prob = PMF)             # perform random walk
    row_nodes[i] = v
  }
  return(row_nodes)
}


##################################################################################################
# Q2.1.a
##################################################################################################

set.seed(0)
g <- erdos.renyi.game(1000, 0.01)
diameter(g)


##################################################################################################
# Q2.1.b
##################################################################################################

random_walk = function (g, num_steps, start_node, transition_matrix = NULL){
    if(is.null(transition_matrix))
        transition_matrix = create_transition_matrix(g)

    dist = c()
    v = start_node
    for(i in 1:num_steps){
        #fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
        PMF = transition_matrix[v, ]
        v = sample(1:vcount(g), 1, prob = PMF)
        dist[i] <- shortest.paths(g,start_node,v)
    }

    return(dist)
}


t<-100
dist_total<-seq(0,0,length.out=t)
total <- list()
t_arr<-seq(1,t,1)
for (i in seq(1,100,1)) {
    v_start = sample(vcount(g), 1)
    dist = random_walk(g, t, v_start)
    total[[i]] <- dist
    dist_total<-dist_total+dist
}
dist_total<-dist_total/100
var <- seq(0,0,length.out=t)
for(i in seq(1,100,1)) {
    var<-var+`^`(total[[i]]-dist_total,2)
}
var<-var/100
plot(t_arr,dist_total,type="l",main="<s(t)> vs t", xlab="t", ylab="<s(t)>")
plot(t_arr,var,type="l",main="Variance vs t", xlab="t", ylab="Variance")


##################################################################################################
# Q2.1.c
##################################################################################################

random_walk = function (g, num_steps, start_node, transition_matrix = NULL){
    if(is.null(transition_matrix))
        transition_matrix = create_transition_matrix(g)

    dist = c()
    v = start_node
    for(i in 1:num_steps){
        #fprintf('Step %d: %d\n', i, v)  # COMMENT THIS
        PMF = transition_matrix[v, ]
        v = sample(1:vcount(g), 1, prob = PMF)
    }

    return(degree(g,v))
}

deg <- c()
for (i in seq(1,100,1)) {
    v_start = sample(vcount(g), 1)
    deg[i] = random_walk(g, t, v_start)
}
dg <- table(deg)/100
hist(dg,main="Degree after random walk", xlab="Degree", ylab="Frequency")
hist(degree(g),main="Degree distribution of graph", xlab="Degree", ylab="Frequency")


##################################################################################################
# Q2.2.a
##################################################################################################

g3 <- barabasi.game(1000, m=1, directed=FALSE)


##################################################################################################
# Network generation for Q2.3 and Q2.4
##################################################################################################

# initializations
num_v = 1000
m = 4

# main directed random network generated using preferential attachment
primary_g <- barabasi.game(num_v, directed = T, m = m)

# second directed network used to prevent "black holes"
secondary_g <- barabasi.game(num_v, directed = T, m = m)

# shuffle/permute the nodes of the second network
secondary_g_permute <- permute(secondary_g, sample(num_v))

# extract the edges from the permuted graph (edges = column 1 ----> column 2)
e = as_edgelist(secondary_g_permute)

# transpose the edgelist so edges are now => row 1 ----> row 2
e = t(e)

# add these edges to the primary graph
combined_graph <- add.edges(primary_g, e)
primary_g <- combined_graph

# remove duplicate/multiple edges (ie two edges from node "i" to node "j")
primary_g <- simplify(primary_g, remove.multiple = T, remove.loops = T)     # for directed graph, remove.loops = T removes "self-loops"


##################################################################################################
# Q2.3.a and Q2.3.b
##################################################################################################

# initializations
steps = 1000                              # number of steps to take for each random walk
times = 1000                              # number of random walks taken
node_idx = 1:num_v                        # axes for plotting "y" vs "node number"
tm = create_transition_matrix(primary_g)  # create transition matrix from network generated
nodes_pagerank_3a = rep(0, num_v)         # keeps track of total visits per node for Q2.3.a
nodes_pagerank_3b = rep(0, num_v)         # keeps track of total visits per node for Q2.3.b

# take random walks without teleporation (Q2.3.a) and with teleportation (Q2.3.b)
for(i in 1:times){
  start = sample(1:vcount(primary_g), 1)                                                # random start

  row_nodes_3a = random_walk_pagerank(primary_g, steps, start, tm)                      # Q2.3.a random walk (no teleporation)
  nodes_pagerank_3a[row_nodes_3a[steps]] = nodes_pagerank_3a[row_nodes_3a[steps]] + 1   # increment visit count for last node

  row_nodes_3b = random_walk_with_teleport(primary_g, steps, start, tm)                 # Q2.3.b random walk (with teleporation)
  nodes_pagerank_3b[row_nodes_3b[steps]] = nodes_pagerank_3b[row_nodes_3b[steps]] + 1   # increment visit count for last node
}

# normalize to get probability of visits for each node for Q2.3.a and Q2.3.b
pagerank_3a = nodes_pagerank_3a / times
pagerank_3b = nodes_pagerank_3b / times

# get the "in-degrees" of each node
degreesVector <- degree(primary_g, mode = "in")

# plots for Q2.3.a
plot(x=node_idx, y=degreesVector, main = "Degree Distribution of Graph", xlab = "Node number", ylab = "In-degree")
plot(x=node_idx, y=pagerank_3a, main = "3a: Probability of walker visiting each node", xlab = "Node number", ylab = "Likelihood of visiting")
plot(x=degreesVector, y=pagerank_3a, main = "3a: Probability-Degree Relationship", xlab = "In-degree", ylab = "Likelihood of visiting")

# plots for Q2.3.b
plot(x=node_idx, y=pagerank_3b, main = "3b: Probability of walker visiting each node", xlab = "Node number", ylab = "Likelihood of visiting")
plot(x=degreesVector, y=pagerank_3b, main = "3b: Probability-Degree Relationship", xlab = "In-degree", ylab = "Likelihood of visiting")


##################################################################################################
# Q2.4.a, Q2.4.b, and Q2.4.c
##################################################################################################

# Q2.4.b: find  the two nodes with median pageranks
oredered_idx <- order(nodes_pagerank_3a)

median_node_1 <- oredered_idx[times / 2]
median_node_2 <- oredered_idx[times / 2 + 1]

# Q2.4.b: teleport to these 2 nodes each with probability 0.5 and the rest with probability 0
nodes_with_median_prob = rep(0, num_v)
nodes_with_median_prob[median_node_1] <- 1 / 2
nodes_with_median_prob[median_node_2] <- 1 / 2

# Q2.4.c: teleport to nodes with hybrid probability of normal pagerank and median pagerank
N = 1000                            # total number of pages
damping_factor = 0.85               # applies 85% importance to median pagerank, 15% importance to regular pagerank

normal_pagerank_term = matrix((1 - damping_factor) / N, N, 1)   # normal pagerank term
median_pagerank_term = damping_factor * nodes_with_median_prob  # median pagerank term
hybrid_pagerank = normal_pagerank_term + median_pagerank_term   # hybrid pagerank term

# take random walks with teleportation using personalized pagerank, median pagerank, and hybrid pagerank
nodes_pagerank_4a = rep(0, num_v)
nodes_pagerank_4b = rep(0, num_v)
nodes_pagerank_4c = rep(0, num_v)

for(i in 1:times){
  start = sample(1:num_v, 1)

  row_nodes_4a = random_walk_with_teleport(primary_g, steps, start, tm, pagerank_3a)            # Q2.4.a random walk (personalized teleporation)
  nodes_pagerank_4a[row_nodes_4a[steps]] = nodes_pagerank_4a[row_nodes_4a[steps]] + 1           # increment visit count for last node

  row_nodes_4b = random_walk_with_teleport(primary_g, steps, start, tm, nodes_with_median_prob) # Q2.4.b random walk (median teleporation)
  nodes_pagerank_4b[row_nodes_4b[steps]] = nodes_pagerank_4b[row_nodes_4b[steps]] + 1           # increment visit count for last node
  
  row_nodes_4c = random_walk_with_teleport(primary_g, steps, start, tm, hybrid_pagerank)        # Q2.4.c random walk (hybrid teleporation)
  nodes_pagerank_4c[row_nodes_4c[steps]] = nodes_pagerank_4c[row_nodes_4c[steps]] + 1           # increment visit count for last node
}

# normalize to get probability of visits for each node for Q2.3.a and Q2.3.b
pagerank_4a = nodes_pagerank_4a / times
pagerank_4b = nodes_pagerank_4b / times
pagerank_4c = nodes_pagerank_4c / times

# plots for Q2.4.a
plot(x=node_idx, y=pagerank_4a, main = "4a: Probability of walker visiting each node", xlab = "Node number", ylab = "Likelihood of visiting")
plot(x=degreesVector, y=pagerank_4a, main = "4a: Probability-Degree Relationship", xlab = "In-degree", ylab = "Likelihood of visiting")

# plots for Q2.4.b
plot(x=node_idx, y=pagerank_4b, main = "4b: Probability of walker visiting each node", xlab = "Node number", ylab = "Likelihood of visiting")
plot(x=degreesVector, y=pagerank_4b, main = "4b: Probability-Degree Relationship", xlab = "In-degree", ylab = "Likelihood of visiting")

# plots for Q2.4.c
plot(x=node_idx, y=pagerank_4c, main = "4c: Probability of walker visiting each node", xlab = "Node number", ylab = "Likelihood of visiting")
plot(x=degreesVector, y=pagerank_4c, main = "4c: Probability-Degree Relationship", xlab = "In-degree", ylab = "Likelihood of visiting")




