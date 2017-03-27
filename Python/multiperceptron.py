import numpy as np
import math
import matplotlib.pyplot as plt
import random

from operator import add, sub, mul, truediv

## Data Processing

n = 5000 # 5000 #  Number of images to take in

inputfile = open('train01234.digits')
labelsfile = open('train01234.labels')

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

# print(train_data[0])
# print(labels)

#### Training the Perceptron

def perceptron(data,w, num, M):
	t = 0
	mistakes = 0
	tm = np.zeros(shape=(num * M))	

	while(t < M):
		j = list(range(0,5000))
		random.shuffle(j)
		for i in range(0,num):			
			x = data[j[i],:]
			## Prediction
			pred = 0
			val = np.zeros(shape=5)
			for k in range(0,5):
				val[k] = np.dot(w[k,:], x)
			pred = np.argmax(val)

			
			## Update
			li = int(labels[j[i]])
			if (pred != li):
				w[pred] = np.subtract(w[pred],x/2)
				# w[labels[i]] = np.add(w[labels[i]],data[i,:]/2)
				w[li] = np.add(w[li],x/2)
				mistakes += 1

			tm[t*num + i] = mistakes
		t += 1
	return w, mistakes, tm

empty = np.zeros(shape=(5,784)) # Initial Weight
final_w, train_mistakes, tm = perceptron(train_data, empty, n, 5)

# print(final_w[0])
print("Training Mistakes: "+ str(train_mistakes))

# Cumulative Number of Mistakes vs Number of Sample Seen
plt.plot(range(0,25000),tm)
plt.title('Identifying Images with Perceptron, 5 class')
plt.ylabel('Cumulative Number of Mistakes')
plt.xlabel('Number of Samples Seen')
plt.show()
plt.savefig("mistakes_vs_samples_5_class.png")

## Applying it on test data

test_file = open('test01234.digits')
test_data = data_parser(test_file, 500)

target = open("test01234.predictions", 'w')

def batch_perceptron(data,w):
	t = 0
	mistakes = 0
	for i in range(0,500):
		
		pred = 0
		val = np.zeros(shape=5)
		for j in range(0,5):
			val[j] = np.dot(w[j,:], data[i,:])
		pred = np.argmax(val)

		li = int(labels[i])

		target.write(str(pred))
		target.write("\n")

batch_perceptron(test_data,final_w)
