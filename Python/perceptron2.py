import numpy as np
import math
import matplotlib.pyplot as plt
import random

from operator import add, sub, mul, truediv

## Data Processing

n = 2000 #  Number of images to take in

inputfile = open('train35.digits')
labelsfile = open('train35.labels')

def data_parser(text,num):
	i = 0
	data = np.zeros(shape=(num,784))
	for line in text:
		temp = [int(z) for z in line.split()]
		data[i,:] = temp
		i += 1
		if (i >= num):
			break
	return data
	
def label_parser(labelsfile, num):
	i = 0
	labels = np.zeros(shape=num)
	for line in labelsfile:
		labels[i] = int(line)
		i += 1
		if (i >= n):
			break
	return labels

train_data = data_parser(inputfile,n)
labels = label_parser(labelsfile,n)

# print(data)
# print(labels)

# w = np.zeros(shape=(1,784))
# print(data[1,:])
# print(w)
# print(np.dot(data[1,:],w))
# print(np.add(data[1,:],w))

# Number of times to train the perceptron algorithm

# After trying differe M value through cross-validation and plotting their number of mistakes,
# it was suggested that M = 5 should be chosen.

M = 5

def perceptron(data,w, num, M):
	t = 0
	mistakes = 0
	tm = np.zeros(shape=10000)
	while(t < M):
		for i in range(0,num):
			pred = 0
			if (np.dot(w,data[i,:]) >= 0):
				pred = 1 # label 3
			else: 
				pred = -1 # label 5
			
			if (pred == -1 and labels[i] == 1):
				w = np.add(w,data[i,:])
				mistakes += 1
			elif (pred == 1 and labels[i] == -1):	
				w = np.subtract(w,data[i,:])
				mistakes += 1
			tm[t*2000 + i] = mistakes
		t += 1
	return w, mistakes, tm

empty = np.zeros(shape=(1,784)) # Initial Weight
final_w, train_mistakes, tm = perceptron(train_data, empty, n, M)

# print("Number of samples seen: " + str(n * M))
# print("Final w: " + str(final_w))

print("Training Mistakes: "+ str(train_mistakes))

# Cumulative Number of Mistakes vs Number of Sample Seen
plt.plot(range(0,10000),tm)
plt.title('Identifying Images with Perceptron')
plt.ylabel('Cumulative Number of Mistakes')
plt.xlabel('Number of Samples Seen')
plt.show()
plt.savefig("mistakes_vs_samples.png")




# Testing with new data

test_file = open('test35.digits')
test_data = data_parser(test_file, 200)

target = open("test35.predictions", 'w')

def batch_perceptron(data,w, num):
	t = 0
	mistakes = 0
	for i in range(0,num):
		pred = 0
		if (np.dot(w,data[i,:]) >= 0):
			pred = 1 # label 3
		else: 
			pred = -1 # label 5		
		target.write(str(pred))
		target.write("\n")

batch_perceptron(test_data,final_w, 200)
