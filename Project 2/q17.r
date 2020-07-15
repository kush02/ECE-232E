########################################################################################
# Import libraries
########################################################################################

library('igraph')



########################################################################################
# Helper Functions
########################################################################################
generate_avg_acc = function(num_deg_w_24, network_415, method) {
  
  #Common Neighbors measure
  avg_acc = c()
  
  for(node_i in num_deg_w_24) {
    
    acc = c()
    for (i in 1:10) {
      
      R = c()
      tmp_network = network_415[[1]]
      
      neighbors_of_node_i = neighbors(tmp_network, node_i) 
      for (node in neighbors_of_node_i) {
        if (runif(1, 0, 1) <= 0.25) {
          
          tmp_network = delete_edges(tmp_network, edge(node, node_i))
          R = c(R, node)
          
        }
      }
      
      new_neighbors_of_node_i = setdiff(neighbors_of_node_i, R)
      new_neighbors_of_node_i = c(new_neighbors_of_node_i, node_i)
      dist_nodes_of_i = setdiff(V(tmp_network), new_neighbors_of_node_i)
      neighbors_count = c()
      
      for (node_j in dist_nodes_of_i) {
        
        neighbors_of_node_j = neighbors(tmp_network, node_j) 
        comm_neighbors = intersect(new_neighbors_of_node_i, neighbors_of_node_j)
        
        if (method == "Jaccard") {
          
          union_comm_neighbors = union(new_neighbors_of_node_i, neighbors_of_node_i)
          
        } else if (method == "Adamic Adar") {
          
          record = 0
          for (neigh in comm_neighbors) {
            neigh_size = length(neighbors(tmp_network, neigh))	
            record = record + 1/log2(neigh_size)
          }
        }
        
        neighbors_count = c(neighbors_count, length(comm_neighbors))
        
      }
      
      index = sort(neighbors_count,decreasing=T, index.return=T)$ix
      
      dist_n_i = dist_nodes_of_i[index[1:length(R)]]
      intersect_of_R_dist = intersect(R, dist_n_i)
      
      # calculate accuracy
      acc = c(acc, length(intersect_of_R_dist) / length(R))
      
    }
    
    avg_acc = c(avg_acc, mean(acc))
    
  }
  
  avg_acc = mean(avg_acc)
  cat(method, " method: ", avg_acc, "\n")
  
  return(mean(avg_acc))
  
}



########################################################################################
# Question 16
########################################################################################

# get path to facebook file
path_to_file <- dirname(rstudioapi::getSourceEditorContext()$path)
path_to_file <- paste(path_to_file, "/facebook_combined.txt", sep="/")

# get facebook data
network_data = read_graph(path_to_file, format='ncol', directed=F)
network_edgelist = as_edgelist(network_data)
facebook_edgelist_graph = graph_from_edgelist(network_edgelist, directed=F)
network_415 = make_ego_graph(facebook_edgelist_graph, nodes=c('414'))

# get the degree list and find the nodes with degree value of 24
deg_vec = degree(network_415[[1]])
num_deg_w_24 = which(deg_vec == 24)



########################################################################################
# Question 17 - Generate mean accuracies
########################################################################################

avg_acc_1 = generate_avg_acc(num_deg_w_24, network_415, "Common Neighbors")
avg_acc_2 = generate_avg_acc(num_deg_w_24, network_415, "Jaccard")
avg_acc_3 = generate_avg_acc(num_deg_w_24, network_415, "Adamic Adar")

