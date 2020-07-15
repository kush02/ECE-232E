import json
import numpy as np
import pandas as pd
import igraph as ig
import networkx as nx
import matplotlib.pyplot as plt 


data = json.load(open('los_angeles_censustracts.json'))

for i in range(len(data['features'])):
	j = data['features'][i]['geometry']['coordinates'][0]
	if len(j)==1:
		c = np.asarray(j[0]).mean(axis=0)
		print(i,c,'y')
	elif len(j)==2:
		c = np.asarray(j[0]).mean(axis=0)
		c1 = np.asarray(j[1]).mean(axis=0)
		print(i,c,'y')
		print(i,c1,'y')
	elif i==1931:
		c,c1,c2,c3,c4,c5 = np.asarray(j[0]).mean(axis=0),np.asarray(j[1]).mean(axis=0),np.asarray(j[2]).mean(axis=0),np.asarray(j[3]).mean(axis=0),np.asarray(j[4]).mean(axis=0),np.asarray(j[5]).mean(axis=0)
		print(i,c,'y')
		print(i,c1,'y')
		print(i,c2,'y')
		print(i,c3,'y')
		print(i,c4,'y')
		print(i,c5,'y')
	else: print(i,np.asarray(j).mean(axis=0))

df = pd.read_csv("los_angeles-censustracts-2019-4-All-MonthlyAggregate.csv")
df = df[df['month']==12][['sourceid','dstid','mean_travel_time']]
vals = df.values
temp = np.zeros_like(vals)
for i in range(len(vals)):
	if vals[i][0] > vals[i][1]:
		temp[i][0], temp[i][1], temp[i][2] = vals[i][1], vals[i][0], vals[i][2]  
	else: temp[i] = vals[i] 

new_df = pd.DataFrame(temp,columns=['sourceid','dstid','mean_travel_time'])
final_df = new_df.groupby(['sourceid','dstid']).mean().reset_index()
final_df_vals = final_df.values


outfile = open("graph_processed.txt", "w")
for i in range(len(final_df_vals)):
    line=str('%d'%int(final_df_vals[i][0]))+'\t'+str('%d'%int(final_df_vals[i][1]))+'\t'+str('%.3f'%final_df_vals[i][2]) + '\n'
    outfile.write(line)
outfile.close()


# 6 
g = ig.Graph.Read(f = 'graph_processed.txt', format = 'ncol', directed = False)
gcc = g.components().giant()
print(len(gcc.vs),len(gcc.es))


# 7
mst = gcc.spanning_tree(weights = gcc.es["weight"])
edges = [mst.es[5].vertex_tuple, mst.es[10].vertex_tuple, mst.es[15].vertex_tuple, mst.es[20].vertex_tuple, mst.es[25].vertex_tuple]

data = json.load(open('los_angeles_censustracts.json'))
coord1 = []
for i in edges:
	for j in range(len(data['features'])):
		c1, f1 = 0, False
		if data['features'][j]['properties']['MOVEMENT_ID']==i[0]['name']:
			c1 = np.asarray(data['features'][j]['geometry']['coordinates'][0]).mean(axis=0)
			f1 = True
		if f1: coord1.append(c1)

coord2 = []
for i in edges:
	for j in range(len(data['features'])):
		c2, f2 = 0, False
		if data['features'][j]['properties']['MOVEMENT_ID']==i[1]['name']:
			c2 = np.asarray(data['features'][j]['geometry']['coordinates'][0]).mean(axis=0)
			f2 = True
		if f2: coord2.append(c2)

pair_dists = []
for i in range(len(coord1)):
	x, y = (abs(coord1[i][0]-coord2[i][0]))*69, (abs(coord1[i][1]-coord2[i][1]))*69
	z = np.sqrt(x**2+y**2)
	pair_dists.append(z)
print(edges)
print(pair_dists)


# 8
triangles = []
while len(triangles)<1000:
	points = np.random.randint(1,high=len(gcc.vs),size=3)
	try:
		e1, e2, e3 = gcc.get_eid(points[0],points[1]), gcc.get_eid(points[1],points[2]), gcc.get_eid(points[2],points[0])
		weights = [gcc.es['weight'][e1],gcc.es['weight'][e2],gcc.es['weight'][e3]]
		triangles.append(weights)
	except: continue

c = 0
for i in triangles:
	w1, w2, w3 = i[0], i[1], i[2]
	if w1+w2>w3 and w1+w3>w2 and w3+w2>w1: c+=1
print(c/len(triangles))


# 9 & 10
df = pd.read_csv("los_angeles-censustracts-2019-4-All-MonthlyAggregate.csv")
df = df[df['month']==12]
df1 = df[['sourceid','dstid','mean_travel_time']]
arr = df1.values
for i in range(0,len(arr)):
    if(arr[i][0]>arr[i][1]):
        t = arr[i][0]
        arr[i][0] = arr[i][1]
        arr[i][1] = t
newdf = pd.DataFrame(arr)
arr1 = newdf.groupby([0,1]).mean().reset_index()
arr1 = arr1.rename(columns={0: "source", 1: "target", 2: "weight"})

g = nx.from_pandas_edgelist(arr1, 'source','target', ['weight'])
gcc = g.subgraph(max(nx.connected_components(g), key=len))
mst = nx.minimum_spanning_tree(gcc)
multigraph = nx.MultiGraph()
mst_cost = 0
for i in mst.edges:
	w = mst.edges[i[0],i[1]]['weight']
	mst_cost += w
	multigraph.add_edge(i[0],i[1],weight=w)
	multigraph.add_edge(i[0],i[1],weight=w)

#print(len(multigraph.nodes),len(multigraph.edges),nx.is_eulerian(multigraph))
nodes, count = [], 0
for i in multigraph.nodes:
	nodes.append(i)
	count += 1
	if count>60: break

costs, emb_tours = [], []
for node in nodes:
	tour = [u for u,v in nx.eulerian_circuit(multigraph,source=node)]
	emb_tour, visited = [], set()
	for i in tour:
		if i not in visited: emb_tour.append(i); visited.add(i) 
	emb_tour.append(emb_tour[0])
	emb_tours.append(emb_tour)

	multigraph_cost = 0
	for i in range(len(emb_tour)-1):
		s,t = emb_tour[i], emb_tour[i+1]
		w = 0
		if mst.has_edge(s,t): w = mst.edges[s,t]['weight']
		else: w = nx.dijkstra_path_length(gcc,s,t)
		multigraph_cost += w
	print(multigraph_cost)
	costs.append(multigraph_cost)

min_multigraph_cost = min(costs)
traj = emb_tours[np.argmin(costs)]
print(mst_cost,min_multigraph_cost,min_multigraph_cost/mst_cost)

data = json.load(open('los_angeles_censustracts.json'))
cds = []
for i in traj:
	for j in range(len(data['features'])):
		if data['features'][j]['properties']['MOVEMENT_ID']==str(int(i)):
			c2 = data['features'][j]['geometry']['coordinates'][0]
			if len(c2)==1:
				t = np.asarray(c2[0]).mean(axis=0)
				cds.append(t)
				print(i,t,'y1')
			elif len(c2)==2:
				t = np.asarray(c2[0]+c2[1]).mean(axis=0)
				cds.append(t)
				print(i,t,'y2')
			elif i==1932.0:
				t = np.asarray(c2[0]+c2[1]+c2[2]+c2[3]+c2[4]+c2[5]).mean(axis=0)
				cds.append(t)
				print(i,t,'y1932')
			else: t = np.asarray(c2).mean(axis=0); cds.append(t); print(i,t)
			
x,y = [i[0] for i in cds], [i[1] for i in cds]
plt.plot(x,y,color='red',marker='o',markersize=3,markerfacecolor='blue')
plt.show()


