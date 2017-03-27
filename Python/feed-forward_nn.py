import numpy as np
import math
import matplotlib.pyplot as plt
import random


learn_rate = 0.1
N_hidden = 128 # 32, 64, 128, 256
N_output = 10

print("Reading train_x... ")
train_x = np.loadtxt('TrainDigitX.csv',delimiter=',')
print("Reading train_y... ")
train_y = np.loadtxt('TrainDigitY.csv')

print("Reading test_x... ")
test_x = np.loadtxt('TestDigitX.csv',delimiter=',')
print("Reading test_y... ")
test_y = np.loadtxt('TestDigitY.csv')
print("Reading test_x2... ")
test_x2 = np.loadtxt('TestDigitX2.csv',delimiter=',')


def sigmoid(u):
    return 1 / (1 + np.exp(-u))
def sigmoid_prime(u):
    return (np.exp(u)) / (np.power((1 + np.exp(u)),2)) 

#### Training

# Initialize weights as random
np.random.seed(100)
hidden_weights = np.random.rand(N_hidden,785) * 2 - 1 # weight of first hidden neuron is in hidden_weights[0]
output_weights = np.random.rand(N_output,N_hidden) * 2 - 1 # weight of first output neuron is in output_weights[0]
hidden_val = np.zeros(N_hidden)
output_val = np.zeros(N_output)

train_size = 50000
epochs = 4
# pred = np.zeros(train_size * epochs)

# Test Parameters
test_size = 10000
test_size2 = 5000
test_pred = np.zeros(test_size)
test_pred2 = np.zeros(test_size2)

for e in range(epochs):
	# np.random.shuffle(train_x) # Shuffle the training data, temporarily commented out for reproducablility of results
	print("Running epoch " + str(e+1))
	for k in range(train_size):
		current = np.append(train_x[k],1) # appending the bias input neuron
		for i in range(N_hidden):
			hidden_val[i] = sigmoid(np.dot(hidden_weights[i], current))
		for j in range(N_output):
			output_val[j] = sigmoid(np.dot(output_weights[j], hidden_val))
		# pred[train_size * e + k] = np.argmax(output_val)
		
		actual = np.zeros(N_output)
		actual[int(train_y[k])] = 1

		output_delta = np.ndarray(shape=(N_output,1))
		
		old_weights = np.copy(output_weights)
		
		# Update last layer's weights
		for j in range(N_output):
			#                         (x_t   -  y_i)      * phi'(a_t)
			output_delta[j] = (output_val[j] - actual[j]) * sigmoid_prime(np.dot(output_weights[j], hidden_val))
			output_weights[j] = output_weights[j] - learn_rate * output_delta[j] * hidden_val
		

		
		# Update hidden layer's weights
		ut = np.asscalar(sum(np.dot(old_weights.T,output_delta)))  # should it be output instead of old?
		for i in range(N_hidden):
			#       phi'(a_t)                                         * 
			delta = sigmoid_prime(np.dot(hidden_weights[i], current)) * ut
			hidden_weights[i] = hidden_weights[i] - learn_rate * delta * current
	
	## Test immediately after
	print("Testing for epoch " + str(e+1))
	for k in range(test_size):
		current = np.append(test_x[k],1) # appending the bias input neuron
		for i in range(N_hidden):
			hidden_val[i] = sigmoid(np.dot(hidden_weights[i], current))
		for j in range(N_output):
			output_val[j] = sigmoid(np.dot(output_weights[j], hidden_val))
		test_pred[k] = np.argmax(output_val)

	correct = sum(np.equal(test_pred,test_y))
	
	print("Correctness (Out of 10,000): " + str(correct))


def make_prediction():
	for k in range(test_size):
		current = np.append(test_x[k],1) # appending the bias input neuron
		for i in range(N_hidden):
			hidden_val[i] = sigmoid(np.dot(hidden_weights[i], current))
		for j in range(N_output):
			output_val[j] = sigmoid(np.dot(output_weights[j], hidden_val))
		test_pred[k] = np.argmax(output_val)

	for k in range(test_size2):
		current = np.append(test_x2[k],1) # appending the bias input neuron
		for i in range(N_hidden):
			hidden_val[i] = sigmoid(np.dot(hidden_weights[i], current))
		for j in range(N_output):
			output_val[j] = sigmoid(np.dot(output_weights[j], hidden_val))
		test_pred2[k] = np.argmax(output_val)


def save_output():
	target = open("TestDigitX_Prediction", 'w')
	for p in range(test_size):
		target.write(str(int(test_pred[p])))
		target.write("\n")

	target2 = open("TestDigitX2_Prediction", 'w')
	for p2 in range(test_size2):
		target2.write(str(int(test_pred2[p2])))
		target2.write("\n")

make_prediction()
save_output()

##### Prediction Results with varying learn_rate, N_hidden, and epochs

### Without Shuffle, learn_rate = 0.03, N = 32
# epochs = 1, 6835/10000
# epochs = 2, 7847/10000
# epochs = 3, 7284/10000
# epochs = 4, 7106/10000

### Without Shuffle, learn_rate = 0.03, N = 64
# epochs = 1, 6653/10000
# epochs = 2, 6743/10000
# epochs = 3, 7021/10000
# epochs = 4, 6706/10000

### Without Shuffle, learn_rate = 0.03, N = 128
# epochs = 1, 6448/10000
# epochs = 2, 7560/10000
# epochs = 3, 7732/10000
# epochs = 4, 7796/10000
# epochs = 5, 7795/10000
# epochs = 6, 7686/10000

### Without Shuffle, learn_rate = 0.03, N = 256
# epochs = 1, 4157/10000
# epochs = 2, 5677/10000
# epochs = 3, 7759/10000
# epochs = 4, 7928/10000
# epochs = 5, 8530/10000
# epochs = 6, 8650/10000
# epochs = 7, 8675/10000
# epochs = 8, 8841/10000
# epochs = 9, 8853/10000
# epochs = 10, 8853/10000