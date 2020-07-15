########################################################################################
# Import libraries
########################################################################################

library('igraph')
library('entropy')
library('infotheo')


########################################################################################
# Helper Function for Question 19
########################################################################################

plot_in_out_deg = function(network_id) {

  # disabling scientific notation
  options(scipen=999)

  # gather the files that contain the suffix ".circles"
  file <- paste(path_to_file, network_id, sep="")
  file <- paste(file, ".edges", sep = "")

  # generate the graph/network of node 1's edge file
  graph <- read_graph(file, directed=T, format="ncol")
  graph <- graph + vertex(network_id)

  num_v <- vcount(graph) - 1
  id <- num_v + 1

  # add edges to the graph
  edge_seq <- 1:num_v
  edge_list <- mapply(c, rep(id, num_v), edge_seq, SIMPLIFY=F)
  e <- unlist(edge_list)
  graph <- add.edges(graph, e)

  # plot the in and out degrees of first node
  in_deg = degree(graph, mode = "in")
  out_deg = degree(graph, mode = "out")

  hist(in_deg,breaks=100,freq=FALSE,col="white",xlab="In-degree", main=paste("In-Degree Distribution for network", network_id))
  hist(out_deg,breaks=100,freq=FALSE,col="white",xlab="Out-degree", main=paste("Out-Degree Distribution for network", network_id))

  return(graph)

}


########################################################################################
# Helper Function for Question 20
########################################################################################

plot_communities = function(graph, network_id) {

  # find the densely connected subgraphs (communities) using walktrap algorithm
  communities <- cluster_walktrap(graph)

  # get the color and modularity scores of the communities
  membership <- membership(communities)         # membership gives the node numbers and which community it belongs to
  num_communities <- length(sizes(communities)) # number of communities in network
  colors <- rainbow(num_communities)            # number of distinct colors

  # plot the communities, distinguished by color
  plot(communities, graph, vertex.colors=colors[membership], layout=layout.fruchterman.reingold,
       vertex.label=NA, vertex.size=1, width=0,edge.arrow.mode=0,edge.lty=0)
  
  cat("Modularity score for network ", network_id, ": ", modularity(communities), "\n")

  return(communities)

}


########################################################################################
# Helper Function for Question 22
########################################################################################

compute_homogeneity_and_completeness = function(network_id, communities) {

  # initializations
  circle_nodes <- c()

  # extract circles/classes of the network
  circle_file <- paste(path_to_file, network_id, ".circles", sep = "")
  circle_data <- readLines(circle_file)

  num_circles <- length(circle_data)
  num_communities <- length(communities)
  circle_data_temp <- vector(mode="list", length=num_circles)

  for (i in 1:num_circles) {

    lines <- strsplit(circle_data[i], "\t")     # split the lines while removing tabs from each line
    num_nodes <- length(lines[[1]])             # number of nodes in the ith class/circle for a network
    lines <- (lines[[1]])[2:num_nodes]          # filter out the label in front
    circle_data_temp[[i]] = lines

    circle_nodes <- c(circle_nodes, lines)      # gather all of the nodes of each class in the network into a single vector

  }
  
  circle_data <- circle_data_temp               # contains circle data: C = {C1, C2, ...}
  circle_nodes <- unique(circle_nodes)          # nodes that contain circle information
  
  # total number of people with circle information
  N <- length(circle_nodes)

  # number of people in each circle and community
  a <- lengths(circle_data)
  temp <- unname(sizes(communities))
  b <- unname(sizes(communities))
  
  for (i in 1:num_communities) {
     for (j in 1:temp[i]) {
       if (is.na(match(communities[[i]][j], circle_nodes)))
           b[i] <- b[i] - 1
     }
  }

  # populate C matrix, where C(j, i) is the number of people belonging to community j and circle i
  C <- matrix(0, num_communities, num_circles)

  for (j in 1:num_communities) {
    for (i in 1:num_circles) {
      common_nodes <- intersect(communities[[j]], circle_data[[i]])
      num_common_nodes <- length(common_nodes)
      if (num_common_nodes)
        C[j, i] = num_common_nodes
    }
  }

  # calcuate entropies
  circle_entropy <- 0
  community_entropy <- 0
  
  for (i in 1:num_circles) 
    if (a[i] != 0)
      circle_entropy <- circle_entropy - a[i] / N * log2(a[i] / N)
  
  for (i in 1:num_communities)
    if (b[i] != 0)
      community_entropy <- community_entropy - b[i] / N * log2(b[i] / N)
  
  # calculate conditional entropies
  circle_conditional_entropy <- 0
  community_conditional_entropy <- 0

  for (j in 1:num_communities)
    for (i in 1:num_circles)
      if (C[j, i]) {
        circle_conditional_entropy <- circle_conditional_entropy - C[j, i] / N * log2(C[j, i] / b[j])
        community_conditional_entropy <- community_conditional_entropy - C[j, i] / N * log2(C[j, i] / a[i])
      }

  # calculate homogeneity and completeness
  homogeneity <- 1 - circle_conditional_entropy / circle_entropy
  completeness <- 1 - community_conditional_entropy / community_entropy
  
  cat("Homogeneity of network ", network_id, ": ", homogeneity, "\n")
  cat("Completeness of network ", network_id, ": ", completeness, "\n\n")

}


########################################################################################
# Question 18
########################################################################################

# initialization
num_personal_networks <- 0

# WARNING: this command to get current source directory is strict to RStudio
path_to_file <- dirname(rstudioapi::getSourceEditorContext()$path)
path_to_file <- paste(path_to_file, "gplus/", sep="/")

# gather the files that contain the suffix ".circles"
all_files <- setdiff(list.files(path_to_file),list.dirs(path_to_file,recursive=F, full.names=F))
circle_files <- all_files[grep(".circles", all_files)]

# count number of personal networks
for (file_path in circle_files) {

  file_path <- paste(path_to_file, file_path, sep="")
  num_lines <- length(readLines(file_path))

  if (num_lines > 2)
    num_personal_networks <- num_personal_networks + 1

}

# print the number of personal networks
cat("Number of Personal Networks: ", num_personal_networks, "\n\n")


########################################################################################
# Question 19 - indegree and outdegree plot for three networks
########################################################################################

# store the networks
id_1 <- "109327480479767108490"
id_2 <- "115625564993990145546"
id_3 <- "101373961279443806744"

# plot the in and out degrees of the nodes
network_1 <- plot_in_out_deg(id_1)
network_2 <- plot_in_out_deg(id_2)
network_3 <- plot_in_out_deg(id_3)


########################################################################################
# Question 20 - Compare Modularity Scords & plot Communities
########################################################################################

communities_1 <- plot_communities(network_1, id_1)
communities_2 <- plot_communities(network_2, id_2)
communities_3 <- plot_communities(network_3, id_3)
cat("\n")


########################################################################################
# Question 22 (Question 21 is simply answering a question for the report)
########################################################################################

compute_homogeneity_and_completeness(id_1, communities_1)
compute_homogeneity_and_completeness(id_2, communities_2)
compute_homogeneity_and_completeness(id_3, communities_3)






