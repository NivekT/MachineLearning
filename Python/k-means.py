import numpy as np
import math
import matplotlib.pyplot as plt
import random

## Data Processing

inputfile = open('toydata.txt')

data = numpy.zeros(shape=(500,2))
i = 0
def data_parser(text):
	i = 0
	for line in text:
		x,y = [float(z) for z in line.split()]
		data[i,:] = [x,y]
		i += 1
data_parser(inputfile)

# print(data)

## k-means

# centroids(r x c) contain a centroid per row, with r centroids

def cluster_reassignment(data, centroids):
	nd = data.shape[0]
	nc = centroids.shape[0]

	# initial assigment does not matter
	assignment = numpy.zeros(shape=(500,1)) # 500 x 1, stores the index of the centroid for each data point

	for i in range(nd):
		min_distance = float("inf")
		for j in range(nc):
			difference = data[i,:] - centroids[j,:]
			distance = difference * difference.transpose()
			if distance < min_distance:
				min_distance = distance
				assignment[i] = j
	return assignment

def update_centroids(data, assignment, centroids):
	nc = centroids.shape[0]

	for j in range(nc):
		indices = [i for i, x in enumerate(assignment) if x == j] # Find indices of data point that is assigned to j
		sum = numpy.zeros(shape=(1,2))
		l = len(indices)
		for i in range(l):
			sum += data[i,:]
		#print(centroids[j])	
		if l: 
			centroids[j] = sum/l

		#print(centroids[j])
	return centroids


# test_centroids = numpy.matrix([[-0.5,-0.5],[-0.5,0.5],[0.5,-0.5],[0.5,0.5]])
test_centroids = numpy.matrix([[-0.5,0.5],[0.5,-0.5],[0.5,0.5]])
def kmeans(data,centroids):
	nc = centroids.shape[0]

	centroids = numpy.matrix([[random.uniform(-1,1),random.uniform(-1,1)],
							  [random.uniform(-1,1),random.uniform(-1,1)],
							  [random.uniform(-1,1),random.uniform(-1,1)]])

	old_assignment = numpy.zeros(shape=(500,1))
	assignment = numpy.zeros(shape=(500,1))
	old_centroids = centroids
	x = 0

	
	dist_array = numpy.zeros(shape=(15))
	#while(numpy.array_equal(old_assignment,assignment) and numpy.array_equal(old_centroids,centroids)):
	while (x < 15):
		distortion = 0
		old_centroids = centroids
		old_assignment = assignment

		assignment = cluster_reassignment(data,centroids)
		centroids = update_centroids(data,assignment,centroids)

		for j in range(0,nc):
			indices = [i for i, x in enumerate(assignment) if x == j] # Find indices of data point that is assigned to j
			for i in indices:
				difference = data[i,:] - centroids[j,:]
				distance = difference * difference.transpose()
				distortion += distance
		dist_array[x] = distortion
		x += 1


	xx = data[:,0]
	y = data[:,1]
	z = assignment
	plt.scatter(xx,y,c=z)
	plt.savefig('kmeans.png')

	
	


	#return (assignment, centroids)
	return dist_array

#a,c = kmeans(data,test_centroids)
#  daa = numpy.zeros(shape=(20,15))
# for i in range(0,20):
# 	da = kmeans(data,test_centroids)
# 	daa[i] = da

# print(daa)

# t = range(0,15)
# plt.plot(t,daa[0],t,daa[1],t,daa[2],t,daa[3],t,daa[4],t,daa[5],t,daa[6],t,daa[7],t,daa[8],t,daa[9],t,daa[10])
# plt.plot(t,daa[11],t,daa[12],t,daa[13],t,daa[14],t,daa[15],t,daa[16],t,daa[17],t,daa[18],t,daa[19])
# plt.savefig('distortion2.png')

def initialize(data,nc):
	for k in range(0,3):
		

def kmeans_pp(data):
	nd = data.shape[0]
	nc = centroids.shape[0]

	centroids = initialize(data,nc)

	old_assignment = numpy.zeros(shape=(500,1))
	assignment = numpy.zeros(shape=(500,1))
	old_centroids = centroids
	x = 0

	
	dist_array = numpy.zeros(shape=(15))
	#while(numpy.array_equal(old_assignment,assignment) and numpy.array_equal(old_centroids,centroids)):
	while (x < 15):
		distortion = 0
		old_centroids = centroids
		old_assignment = assignment

		assignment = cluster_reassignment(data,centroids)
		centroids = update_centroids(data,assignment,centroids)

		for j in range(0,nc):
			indices = [i for i, x in enumerate(assignment) if x == j] # Find indices of data point that is assigned to j
			for i in indices:
				difference = data[i,:] - centroids[j,:]
				distance = difference * difference.transpose()
				distortion += distance
		dist_array[x] = distortion
		x += 1


	xx = data[:,0]
	y = data[:,1]
	z = assignment
	plt.scatter(xx,y,c=z)
	plt.savefig('kmeans_pp.png')


daa = numpy.zeros(shape=(20,15))
for i in range(0,20):
	da = kmeans_pp(data,test_centroids)
	daa[i] = da

print(daa)

t = range(0,15)
plt.plot(t,daa[0],t,daa[1],t,daa[2],t,daa[3],t,daa[4],t,daa[5],t,daa[6],t,daa[7],t,daa[8],t,daa[9],t,daa[10])
plt.plot(t,daa[11],t,daa[12],t,daa[13],t,daa[14],t,daa[15],t,daa[16],t,daa[17],t,daa[18],t,daa[19])
plt.savefig('distortion2.png')





