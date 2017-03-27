import numpy as np
import math
import matplotlib.pyplot as plt
import random

from operator import add, sub, mul, truediv

## Data Processing

inputfile = open('3Ddata.txt')


data_3d = np.zeros(shape=(500,3))
data_3d_color = np.zeros(shape=(500,4))
sum_3d = np.zeros(shape=(1,3))

def data_parser(text):
	i = 0
	for line in text:
		global sum_3d, data_3d, data_3d_color

		a,b,c,d = [float(z) for z in line.split()]
		
		sum_3d = np.add(sum_3d, np.array([a,b,c]))
		data_3d[i,:] = [a,b,c]
		data_3d_color[i,:] = [a,b,c,d]

		i += 1

data_parser(inputfile)

# print(data_3d)

##### Implementing PCA

# # Find center
# center = np.divide(sum_3d,500.0)

# # print("Center: ")
# # print(center)
# # print(type(center))


# # Center the matrix

# def center_matrix(m, center):
# 	for i in range(0,500):
# 		m[i,:] = np.subtract(m[i,:], center)
# 	return m

# data_3d = center_matrix(data_3d, center)

# # print("Centered Data: ")
# # print(data_3d)

# # Find Sample Covariance Matrix

# def find_scm(data):
# 	scm = np.zeros(shape=(3,3))
# 	for i in range(0,500):
# 		scm = np.add(scm, np.transpose([data[i,:]]) * data[i,:])
# 	scm = np.divide(scm,500)
# 	return scm

# scm = find_scm(data_3d)
# # print(scm) 

# w, v = np.linalg.eig(scm)
# eval_max = max(w)
# max_index = np.where(w == eval_max)[0][0]
# evec_max = np.squeeze(v[:,max_index])

# # print(w)
# # print(v)

# # print(eval_max)
# # print(evec_max)
# # print(v)
# # print(max_index)


# A = v[:,0:2]
# # print(A)
# # print(data_3d)
# # print(np.dot(data_3d , A))
# data_2d = np.dot(data_3d,A)
# data_2d2 = np.zeros(shape=(500,3))
# # data_2d2[:,0] = data_2d[:,0]
# # data_2d2[:,1] = data_2d[:,1]
# # data_2d2[:,2] = data_3d_color[:,3]
# labels = [int(x - 1) for x in data_3d_color[:,3]]
# # print(labels)
# # print(type(labels))
# plt.scatter(data_2d[:,0],data_2d[:,1],c=labels)
# plt.title('Dimensionality Reduction by PCA')
# plt.ylabel('PC1')
# plt.xlabel('PC2')
# plt.show()
# # plt.scatter(data_2d[0],data_2d[1],c=np.ndarray.tolist(data_3d_color[:,3]))
# plt.savefig("3d_2d.png")


######
# Isomap


# Finding distances between two points

data_parser(inputfile)

def dist(x,y):
	return math.sqrt((x[0] - y[0])**2 + (x[1] - y[1])**2 + (x[2] - y[2])**2)

# Find distance between any two points and save into matrix
# Each row is a point, and column is the distance from the point to point_i
# in the same order as input

all_dist = np.zeros(shape=(500,500))

def find_all_dist(data):
	for c in range(0,500):
		for r in range(0,500):
			all_dist[c][r] = dist(data_3d[c],data_3d[r]) 

find_all_dist(data_3d)
# print(all_dist)

graph = np.zeros(shape=(500,10))

def kNN10(all_dist):
	indices = np.argsort(all_dist)
	graph = indices[:,1:11]
	return graph

graph = kNN10(all_dist)
# print(graph)

new_graph = np.zeros(shape=(500,500))

def ng(graph):
	mat = [[float("inf") for x in range(500)] for x in range(500)]
	for i in range(500):
		for j in range(10):
			index = int(graph[i][j])
			# print(graph[i][j])
			mat[i][index] = dist(data_3d[i],data_3d[index])
	return mat

new_graph = ng(graph)
# print(new_graph)

# Floyd-Warshall 

# min_dist = all_dist

def fw(A):
	for k in range(500):
		for i in range(500):
			for j in range(500):
				if (A[i][j] > A[i][k] + A[k][j]):
					A[i][j] = A[i][k] + A[k][j]
	return A

### Saving min_dist to prevent recalculation

def save_min_dist(A):
	target = open("min_dist.txt", 'w')
	for i in range(500):
		for j in range(500):
			target.write(str(A[i][j]))
			target.write(" ")
		target.write("\n")

# min_dist = fw(new_graph)
# print(min_dist)
# save_min_dist(min_dist)

min_dist_input = open("min_dist.txt")

def min_dist_parse(input):
	i = 0
	data = np.zeros(shape=(500,500))
	for line in input:
		temp = [float(z) for z in line.split()]
		data[i,:] = temp
		i += 1
	return data

min_dist = min_dist_parse(min_dist_input)


def sym(min_dist):
	for i in range(500):
		for j in range(500):
			min_dist[i][j] = min(min_dist[i][j],min_dist[j][i])
	return min_dist

min_dist = sym(min_dist)
# print(min_dist[1])

# for i in range()

# # print((min_dist.T == min_dist).all())
# # print("Min Dist Matrix")
# # print(min_dist)

# # Getting the Gram Matrix

P = np.identity(500) - np.ones(shape=(500,500)) / 500
# min_dist = np.power(min_dist,2)
G = -1/2 * np.matmul(np.matmul(P,min_dist),P)

# print(np.shape(G))


w, Q = np.linalg.eig(G)
idx = w.argsort()[::-1]
w = w[idx]
Q = Q[:, idx]
# Q = abs(Q)

a = np.diag(w)
# a = np.abs(a)
# print(np.shape(a))
# print(np.shape(Q))
a = np.sqrt(a)
# # G_2 = Q * a * Q.T
# # print(np.sqrt(a))

y = np.dot(a,Q)

plt.scatter(y[:,0],y[:,1], c = np.ndarray.tolist(data_3d_color[:,3]))
plt.xlim(-0.05,0.05)
plt.show()
plt.savefig("final.png")