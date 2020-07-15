import json
import numpy as np
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt 


optimal_cost = {'djibouti':6656,'egypt':172387,'oman':86891,'tanzania':394718,'canada':1290319}
# increasing order of cities - djibouti (38), oman (1979), canada (4663), tanzania (6117), egypt (7146)
country = 'egypt'
data = pd.read_csv(country+'.txt',delimiter=' ',index_col=0,header=None,names=['a','b'])


n = len(data)
cost_matrix = np.zeros((n,n))
a, b = data['a'].values, data['b'].values
for i in range(n):
	x1, y1 = a[i], b[i]
	for j in range(n):
		x2, y2 = a[j], b[j]
		cost_matrix[i,j] = ((x1-x2)**2 + (y1-y2)**2)**0.5
		if i == j: cost_matrix[i,j] = np.inf



g = nx.from_numpy_array(cost_matrix)
gcc = g.subgraph(max(nx.connected_components(g), key=len))
mst = nx.minimum_spanning_tree(gcc)
multigraph = nx.MultiGraph()
mst_cost = 0
for i in mst.edges:
	w = mst.edges[i[0],i[1]]['weight']
	mst_cost += w
	multigraph.add_edge(i[0],i[1],weight=w)
	multigraph.add_edge(i[0],i[1],weight=w)

print("Start finding cycle")
tour = [u for u,v in nx.eulerian_circuit(multigraph)]
emb_tour, visited = [], set()
for i in tour:
	if i not in visited: emb_tour.append(i); visited.add(i) 
emb_tour.append(emb_tour[0])
print("Finish cycle")

multigraph_cost = 0
for i in range(len(emb_tour)-1):
	s,t = emb_tour[i], emb_tour[i+1]
	w = 0
	if mst.has_edge(s,t): w = mst.edges[s,t]['weight']
	else: w = cost_matrix[s,t]
	multigraph_cost += w
print(multigraph_cost)
print('rho = ',multigraph_cost/optimal_cost[country])

ratios = [1.3189172043127988,1.3702510997279176,1.389543336852672,1.3815820939776107,1.3521031284681255]
dj, eg, om, tanz, can = (6656*ratios[0]-6656)*100/6656, (172387*ratios[1]-172387)*100/172387, (86891*ratios[2]-86891)*100/86891, (394718*ratios[3]-394718)*100/394718, (1290139*ratios[4]-1290139)*100/1290139
num_city = [38,1979,4663,6117,7146]
print("Djibouti:",dj,", Egypt:", eg,", Oman:", om,", Tanzania:", tanz,", Canada:", can)
plt.plot(num_city,ratios)

