import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns



# Q1
reward_func_1 = np.zeros((10,10))
reward_func_1[-1,-1] = 1
reward_func_1[2:4,5:7], reward_func_1[4:6,1:3], reward_func_1[8:,2:4] = -10, -10, -10
sns.heatmap(reward_func_1); plt.title('Reward Function 1'); plt.show()

reward_func_2  = np.zeros((10,10))
reward_func_2[-1,-1] = 10
reward_func_2[8,6],reward_func_2[1:7,4],reward_func_2[1,4:7],reward_func_2[1:4,6],reward_func_2[3,6:9],reward_func_2[3:8,8],reward_func_2[7,6:9] = -100,-100,-100,-100,-100,-100,-100
sns.heatmap(reward_func_2); plt.title('Reward Function 2'); plt.show()

# Q2
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


def optimal_value(up, down, right, left, reward_func,gamma=0.8,snapshots=False):
    v = np.zeros(100)
    reward = reward_func.T.flatten()
    delta = np.inf
    iterations = 0
    snaps = []
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

up, down, right, left = actions()
v = optimal_value(up, down, right, left, reward_func_1,snapshots=False)
v = np.round(v,decimals=3)
plt.table(cellText=v,loc=(0,0),cellLoc='center'); plt.title('Optimal State Value for Reward Function 1'); plt.show()


# Q3
sns.heatmap(v); plt.title('Reward Function 1'); plt.show()


# Q5
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


def plot_arrows(arrows,title=''):
	arr = np.chararray((10,10),unicode=True)
	for i in range(10):
		for j in range(10):
			if arrows[i,j]==0: arr[i,j] = '\u2192' #left #'\u2190' # left/up
			elif arrows[i,j]==1: arr[i,j] = '\u2190' #right #'\u2192' # right
			elif arrows[i,j]==2: arr[i,j] = '\u2191' #up #'\u2193' # down
			elif arrows[i,j]==3: arr[i,j] = '\u2193' #down #'\u2191' # left/up
	plt.table(cellText=arr,loc=(0,0),cellLoc='center')
	plt.title(title)
	plt.show()

	return

up, down, left, right = actions()
opt_policy, arrows =  get_value_and_opt_policy(up, down, right, left, reward_func_1)#optimal_policy(up, down, right, left, reward_func_1)
opt_policy, arrows = opt_policy.reshape(10,10).T, arrows.reshape(10,10).T
plot_arrows(arrows,title='Optimal Policy for Reward Function 1')

# Q6
up, down, right, left = actions()
v = optimal_value(up, down, right, left, reward_func_2)
v = np.round(v,decimals=3)
plt.table(cellText=v,loc=(0,0),cellLoc='center'); plt.title('Optimal State Value for Reward Function 2'); plt.show()

# Q7
sns.heatmap(v); plt.title('Reward Function 2'); plt.show()

# Q8
up, down, right, left = actions()
opt_policy, arrows = get_value_and_opt_policy(up, down, right, left, reward_func_2)#optimal_policy(up, down, right, left, reward_func_2)
opt_policy, arrows = opt_policy.reshape(10,10).T, arrows.reshape(10,10).T
plot_arrows(arrows,title='Optimal Policy for Reward Function 2')

# Q9
up, down, right, left = actions(w=0.6)

opt_policy, arrows = get_value_and_opt_policy(up, down, right, left, reward_func_1)#optimal_policy(up, down, right, left, reward_func_1)
opt_policy, arrows = opt_policy.reshape(10,10).T, arrows.reshape(10,10).T
plot_arrows(arrows,title='Optimal Policy for Reward Function 1 with w = 0.6')

opt_policy, arrows = get_value_and_opt_policy(up, down, right, left, reward_func_2)#optimal_policy(up, down, right, left, reward_func_2)
opt_policy, arrows = opt_policy.reshape(10,10).T, arrows.reshape(10,10).T
plot_arrows(arrows,title='Optimal Policy for Reward Function 2 with w = 0.6')


