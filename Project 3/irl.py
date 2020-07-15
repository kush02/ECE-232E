import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from cvxopt import matrix, solvers
import copy


################################################################################################################################### 
# Helper functions for IRL
###################################################################################################################################
def actions(w=0.1):

    # each action is a 10x10 that corresponds to each of the 100 states
    up      = np.zeros((100, 100))
    left    = np.zeros((100, 100))
    down    = np.zeros((100, 100))
    right   = np.zeros((100, 100))
    
    # define the transition probabilities
    for i in range(100):
        
        # left edge
        if i <= 9:

            # corner states
            if i == 0:

                # off grid moves
                up[i, i + 10], up[i, i + 1], up[i, i]       = w / 4, w / 4, 1 - w + w / 2
                left[i, i + 10], left[i, i + 1], left[i, i] = w / 4, w / 4, 1 - w + w / 2

                # in grid moves
                down[i, i + 10], down[i, i + 1], down[i, i]     = w / 4, 1 - w + w / 4, w / 2
                right[i, i + 10], right[i, i + 1], right[i, i]  = 1 - w + w / 4, w / 4, w / 2

            elif i == 9:

                # off grid moves
                down[i, i + 10], down[i, i - 1], down[i, i] = w / 4, w / 4, 1 - w + w / 2
                left[i, i + 10], left[i, i - 1], left[i, i] = w / 4, w / 4, 1 - w + w / 2

                # in grid moves
                up[i, i + 10], up[i, i - 1], up[i, i]           = w / 4, 1 - w + w / 4, w / 2
                right[i, i + 10], right[i, i - 1], right[i, i]  = 1 - w + w / 4, w / 4, w / 2

            # edge states
            else:
                
                # off grid moves
                left[i, i], left[i, i + 1], left[i, i - 1], left[i, i + 10] = 1 - w + w / 4, w / 4, w / 4, w / 4

                # in grid moves
                right[i, i], right[i, i + 1], right[i, i - 1], right[i, i + 10] = w / 4, w / 4, w / 4, 1 - w + w / 4
                down[i, i], down[i, i + 1], down[i, i - 1], down[i, i + 10]     = w / 4, 1 - w + w / 4, w / 4, w / 4
                up[i, i], up[i, i + 1], up[i, i - 1], up[i, i + 10]             = w / 4, w / 4, 1 - w + w / 4, w / 4

        # right edge states:
        elif i >= 90:

            # corner states
            if i == 90:
                
                # off grid moves
                right[i, i - 10], right[i, i + 1], right[i, i]  = w / 4, w / 4, 1 - w + w / 2
                up[i, i - 10], up[i, i + 1], up[i, i]           = w / 4, w / 4, 1 - w + w / 2

                # in grid moves
                down[i, i - 10], down[i, i + 1], down[i, i] = w / 4, 1 - w + w / 4, w / 2
                left[i, i - 10], left[i, i + 1], left[i, i] = 1 - w + w / 4, w / 4, w / 2

            elif i == 99:

                # off grid moves
                right[i, i - 10], right[i, i - 1], right[i, i]  = w / 4, w / 4, 1 - w + w / 2
                down[i, i - 10], down[i, i - 1], down[i, i]     = w / 4, w / 4, 1 - w + w / 2
                
                # in grid moves
                up[i, i - 10], up[i, i - 1], up[i, i]       = w / 4, 1 - w + w / 4, w / 2
                left[i, i - 10], left[i, i - 1], left[i, i] = 1 - w + w / 4, w / 4, w / 2

            # edge states
            else:
                
                # off grid moves
                right[i, i], right[i, i + 1], right[i, i - 1], left[i, i - 10] = 1 - w + w / 4, w / 4, w / 4, w / 4

                # in grid moves
                left[i, i], left[i, i + 1], left[i, i - 1], left[i, i - 10] = w / 4, w / 4, w / 4, 1 - w + w / 4
                down[i, i], down[i, i + 1], down[i, i - 1], down[i, i - 10] = w / 4, 1 - w + w / 4, w / 4, w / 4
                up[i, i], up[i, i + 1], up[i, i - 1], up[i, i - 10]         = w / 4, w / 4, 1 - w + w / 4, w / 4

        # upper edge states:
        elif i % 10 == 0:
            
            # off grid moves
            up[i, i], up[i, i + 1], up[i, i - 10], up[i, i + 10] = 1 - w + w / 4, w / 4, w / 4, w / 4

            # in grid moves
            right[i, i], right[i, i + 1], right[i, i - 10], right[i, i + 10] = w / 4, w / 4, w / 4, 1 - w + w / 4
            left[i, i], left[i, i + 1], left[i, i - 10], left[i, i + 10]     = w / 4, w / 4, 1 - w + w / 4, w / 4
            down[i, i], down[i, i + 1], down[i, i - 10], down[i, i + 10]     = w / 4, 1 - w + w / 4, w / 4, w / 4

        # lower edge states:
        elif i % 10 == 9:
            
            # off grid moves
            down[i, i], down[i, i - 1], down[i, i - 10], down[i, i + 10] = 1 - w + w / 4, w / 4, w / 4, w / 4

            # in grid moves
            right[i, i], right[i, i - 1], right[i, i - 10], right[i, i + 10] = w / 4, w / 4, w / 4, 1 - w + w / 4
            left[i, i], left[i, i - 1], left[i, i - 10], left[i, i + 10]     = w / 4, w / 4, 1 - w + w / 4, w / 4
            up[i, i], up[i, i - 1], up[i, i - 10], up[i, i + 10]             = w / 4, 1 - w + w / 4, w / 4, w / 4

        # inner edges
        else:
            down[i, i + 1], down[i, i - 1], down[i, i - 10], down[i, i + 10]     = 1 - w + w / 4, w / 4, w / 4, w / 4
            up[i, i + 1], up[i, i - 1], up[i, i - 10], up[i, i + 10]             = w / 4, 1 - w + w / 4, w / 4, w / 4
            left[i, i + 1], left[i, i - 1], left[i, i - 10], left[i, i + 10]     = w / 4, w / 4, 1 - w + w / 4, w / 4
            right[i, i + 1], right[i, i - 1], right[i, i - 10], right[i, i + 10] = w / 4, w / 4, w / 4, 1 - w + w / 4

    return up, down, left, right


def get_optimal_value(up, down, right, left, reward_func, gamma=0.8, snapshots=False):

    # initialization
    v = np.zeros(100)
    reward = reward_func.transpose().flatten()
    delta = np.inf
    iterations = 0
    snaps = []

    # policy evaluation
    while(delta>0.01):
        delta = 0
        v_copy = np.copy(v)
        for i in range(0,100):
            t = v_copy[i]
            u,d,r,l = np.sum(up[i]*(reward+gamma*v_copy)), np.sum(down[i]*(reward+gamma*v_copy)), np.sum(right[i]*(reward+gamma*v_copy)), np.sum(left[i]*(reward+gamma*v_copy))
            v[i] = max(u,d,r,l)
            delta = max(delta,abs(t-v[i]))
        iterations += 1
        snaps.append(v_copy.reshape(10,10))
    if snapshots:
    	steps = np.round(np.linspace(1,iterations,5))
    	for i in steps:
    		t = np.round(snaps[int(i-1)],decimals=3)
    		plt.table(cellText=t,loc=(0,0),cellLoc='center'); plt.title('Optimal State Value for Reward Function 1, Step Number = %i' %i); plt.show()
    
    return v.reshape(10,10).T


def get_value_and_opt_policy(up, down, right, left, reward_func, gamma=0.8):

    # intialization
	v, state_values, optimal_policies = np.zeros(100), np.zeros(100), np.zeros(100)
	reward = reward_func.transpose().flatten()
	delta = np.inf

    # policy evaluation
	while(delta>0.01):
	    delta = 0
	    v_copy = np.copy(v)

	    for i in range(100):
	        t = v_copy[i]
	        u,d,r,l = np.sum(up[i]*(reward+gamma*v_copy)), np.sum(down[i]*(reward+gamma*v_copy)), np.sum(right[i]*(reward+gamma*v_copy)), np.sum(left[i]*(reward+gamma*v_copy))
	        v[i] = max(u,d,r,l)
	        delta = max(delta,abs(t-v[i]))

    # policy improvement
	for i in range(100):
		u,d,r,l = np.sum(up[i]*(reward+gamma*v)), np.sum(down[i]*(reward+gamma*v)), np.sum(right[i]*(reward+gamma*v)), np.sum(left[i]*(reward+gamma*v))
		arr = [r,l,u,d]
		state_values[i] = np.amax(arr)
		optimal_policies[i] = arr.index(np.amax(arr))

	return state_values, optimal_policies


def plot_arrows(arrows, title=''):
	arr = np.chararray((10,10),unicode=True)
	for i in range(10):
		for j in range(10):
			if arrows[i,j]==0: arr[i,j] = '\u2192' # right
			elif arrows[i,j]==1: arr[i,j] = '\u2190' # left
			elif arrows[i,j]==2: arr[i,j] = '\u2191' # up
			elif arrows[i,j]==3: arr[i,j] = '\u2193' # down
	plt.table(cellText=arr,loc=(0,0),cellLoc='center')
	plt.title(title)

	return


################################################################################################################################### 
# Q11: finding the optimal penalty coefficient lambda (plot accuracy vs lambda) with reward function 1 as reference
###################################################################################################################################
solvers.options['show_progress'] = False
num_states = 100
num_lambdas = 500
gamma = 0.8
lambda_vec = np.linspace(0, 5, num_lambdas)
len_lambda = len(lambda_vec)
R_max = [1.0, 10.0]

# define the optimal transitional probabilities of each action from the "expert" and list them in order
up, down, left, right = actions()
action_prob_list = [right, left, up, down]

# define the reward functions
reward_func_1 = np.zeros((10,10))
reward_func_1[-1,-1] = 1
reward_func_1[2:4,5:7], reward_func_1[4:6,1:3], reward_func_1[8:,2:4] = -10, -10, -10

reward_func_2  = np.zeros((10,10))
reward_func_2[-1,-1] = 10
reward_func_2[8,6],reward_func_2[1:7,4],reward_func_2[1,4:7],reward_func_2[1:4,6],reward_func_2[3,6:9],reward_func_2[3:8,8],reward_func_2[7,6:9] = -100,-100,-100,-100,-100,-100,-100

# get the optimal values of each state and optimal policies using reward functions 1 and 2
state_values_1, optimal_policies_1 = get_value_and_opt_policy(up, down, right, left, reward_func_1, gamma)		# for reward function 1
state_values_2, optimal_policies_2 = get_value_and_opt_policy(up, down, right, left, reward_func_2, gamma)		# for reward function 2
optimal_policies_1_2 = [optimal_policies_1, optimal_policies_2]

# initialize matrices for the LP
ones_100x1 = matrix(np.ones(num_states))
zeros_100x1 = matrix(np.zeros(num_states))
zeros_400x1 = matrix(np.zeros(4 * num_states))
zeros_100x100 = matrix(np.zeros((num_states, num_states)))
zeros_200x100 = matrix(np.zeros((2 * num_states, num_states)))
zeros_700x100 = matrix(np.zeros((7 * num_states, num_states)))
identity_100x100 = matrix(np.identity(num_states))

# declare variables to store the estimated reward functions, policies, and accuracies
reward_func_list = [[], []]
policy_list = [[], []]
accuracy_list = [[], []]
max_lambda = []
max_lambda_idx = []

# compute the variables c, b, and D
c = matrix([[ones_100x1, ones_100x1 * -lambda_vec[i], zeros_100x1] for i in range(len_lambda)])    # each column is a corresponding lambda
b = [[], []]
D = [[], []]

# iterate to find the third column of 
for r_idx, r in enumerate(R_max):

    b[r_idx] = matrix([zeros_400x1, zeros_400x1, R_max[r_idx] * ones_100x1, R_max[r_idx] * ones_100x1])
    D[r_idx] = matrix([[identity_100x100, identity_100x100, identity_100x100, zeros_700x100], \
        [zeros_200x100, zeros_200x100, zeros_200x100, -identity_100x100, -identity_100x100, zeros_200x100]])

    # compute the third column of D
    idx = 0															# used to ignore the increment if the optimal policy is the same as the action of interest
    third_col = np.zeros((300, 100))

    for s in range(num_states):
        p_a1 = action_prob_list[int(optimal_policies_1_2[r_idx][s])]		# for action 1
        for a in range(4):
            if(int(optimal_policies_1_2[r_idx][s]) == a):
                continue
            p_ax = action_prob_list[a]								# for all other actions
            third_col[idx, :] = -np.dot(p_a1[s] - p_ax[s], np.linalg.inv(np.array(identity_100x100) - gamma * p_a1))
            idx += 1

    third_col = matrix(third_col)
    D[r_idx] = matrix([[D[r_idx]], [third_col, third_col, -identity_100x100, identity_100x100, identity_100x100, -identity_100x100]])

    # extract the reward function and policy from each lambda. get the accuracy of each policy compared with the optimal policy
    for i in range(num_lambdas):
        sol = solvers.lp(c[:, i], D[r_idx], b[r_idx])
        reward_func_list[r_idx].append(np.array(sol['x'])[200:]) #reshape(10, 10))

        v, p = get_value_and_opt_policy(up, down, right, left, reward_func_list[r_idx][i], gamma)
        policy_list[r_idx].append(p)

        accuracy_list[r_idx].append(np.sum(p == optimal_policies_1_2[r_idx]) / 100.0)

    max_lambda_idx.append(np.argmax(accuracy_list[0]))
    max_lambda.append(lambda_vec[max_lambda_idx[r_idx]])

plt.figure(1)
plt.xlabel("\u03BB")
plt.ylabel("accuracy")
plt.title("Accuracy over \u03BB for reward function 1")
plt.plot(lambda_vec, accuracy_list[0])

plt.figure(2)
plt.xlabel("\u03BB")
plt.ylabel("accuracy")
plt.title("Accuracy over \u03BB for reward function 2")
plt.plot(lambda_vec, accuracy_list[1])


################################################################################################################################### 
# Q13, Q20: compare ground truth and max lambda for extracted reward function 1 and 2
###################################################################################################################################
plt.figure(3)
plt.title("Heatmap for Ground Truth Reward Function 1")
sns.heatmap(reward_func_1)

plt.figure(4)
plt.title("Heatmap for Extracted Reward Function 1")
sns.heatmap(reward_func_list[0][int(max_lambda_idx[0])].reshape(10, 10).T)

plt.figure(5)
plt.title("Heatmap for Ground Truth Reward Function 2")
sns.heatmap(reward_func_2)

plt.figure(6)
plt.title("Heatmap for Extracted Reward Function 2")
sns.heatmap(reward_func_list[1][int(max_lambda_idx[1])].reshape(10, 10).T)


################################################################################################################################### 
# Q14, Q21: plot state values for extracted reward 1 and 2
###################################################################################################################################
v1 = get_optimal_value(up, down, right, left, reward_func_list[0][max_lambda_idx[0]])
v2 = get_optimal_value(up, down, right, left, reward_func_list[1][max_lambda_idx[1]])

plt.figure(7)
plt.title("State Values Heat Map for Extracted Reward Function 1")
sns.heatmap(v1)

plt.figure(8)
plt.title("State Values Heat Map for Extracted Reward Function 2")
sns.heatmap(v2)


################################################################################################################################### 
# Q16, Q23: plot optimal policies for extracted reward 1 and 2
###################################################################################################################################
state_values_1, optimal_policies_1 = get_value_and_opt_policy(up, down, right, left, reward_func_list[0][max_lambda_idx[0]], gamma)
state_values_2, optimal_policies_2 = get_value_and_opt_policy(up, down, right, left, reward_func_list[1][max_lambda_idx[1]], gamma)

state_values_1, optimal_policies_1 = state_values_1.reshape(10, 10).T, optimal_policies_1.reshape(10, 10).T
state_values_2, optimal_policies_2 = state_values_2.reshape(10, 10).T, optimal_policies_2.reshape(10, 10).T

plt.figure(9)
plot_arrows(optimal_policies_1, "Optimal Policy for Extracted Reward Function 1")

plt.figure(10)
plot_arrows(optimal_policies_2, "Optimal Policy for Extracted Reward Function 2")


################################################################################################################################### 
# Q25: improving IRL
###################################################################################################################################


plt.show()