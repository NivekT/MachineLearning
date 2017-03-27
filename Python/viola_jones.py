import numpy as np
import math
import matplotlib.pyplot as plt
import skimage.io as io 
from skimage.color import rgb2gray
# from skimage.transform import integral_image
import os 

import matplotlib.pyplot as plt
import matplotlib.patches as patches

from operator import add, sub, mul, truediv

n_images = 4000 # 4000 Number of Images to process # 


##### Reading the images 
dir_path = os.path.dirname(os.path.realpath(__file__))
face_path = os.path.join(dir_path, 'faces')
bg_path = os.path.join(dir_path, 'background')
# file_path = os.path.join(face_path, 'face0.jpg')

def read_images(path, type):
	print("Reading the images...")

	if (type == 0): # face
		all_paths = [os.path.join(path, 'face' + str(n) + '.jpg') for n in range(2000)]	
	else: # bg
		all_paths = [os.path.join(path, str(n) + '.jpg') for n in range(2000)]	
	# collections = io.imread_collection(os.path.join(path, '*.jpg'))
	return np.array([io.imread(jpg, as_grey = True) for jpg in all_paths])
		
faces = read_images(face_path, 0) # 2000 x 64 x 64 np.ndarray
bgs = read_images(bg_path, 1) 

## N_image x 64^2 array 
# integral_image takes in image and change value of each pixel to be the sum of all its left and above values

##### Computing the integral image
def int_image(img):
	l = len(img)
	for c in range(1,l):
		img[0][c] += img[0][c-1]  
	for r in range(1,l):
		s = np.ndarray(shape=64)
		c = 0
		s[0] = img[r][c]
		img[r][c] += img[r-1][c] 
		for c in range(1,l):
			s[c] = img[r][c] + s[c-1]
			img[r][c] = img[r-1][c] + s[c]
	return img

print("Computing iimages...")
iimages_f = np.array([int_image(jpg) for jpg in faces])
iimages_bg = np.array([int_image(jpg) for jpg in bgs])

def merge_ii():
	rv = []
	for i in range(0,2000):
		rv.append(iimages_f[i])
		rv.append(iimages_bg[i])
	return rv

iimages = merge_ii()
# Faces only locate in even indices
# Backgrounds only locate in odd indices
labels = [0] * int(n_images)
labels[0::2] = [1] * int(n_images/2)
labels[1::2] = [-1] * int(n_images/2)

######
## n_features x 8 + 1, with last element indicating shape
# featuretbl = np.ndarray(shape=(n_features, 8))

##### Generating Features
# skip steps in terms of origins
# d - displacement in length/width of feature
def generate_features(steps,d):
	print("Generating features...")
	features = []
	for x in range(5,64,steps): # Changed from 0
		for y in range(5,64, steps):
			for dx in range(2,64,d):
				for dy in range(2,64,d):
					if (x+2*dx <= 63 and y+dy <= 63): # dx is longer
						features.append([x,y,x+dx,y,x+dx,y+dy,x+2*dx,y+dy,1])
					if (y+2*dy <= 63 and x+dx <= 63): # dy is longer
						features.append([x,y,x,y+dy,x+dx,y+dy,x+dx,y+2*dy,0])
	return features
featuretbl = generate_features(3,3)
print("Number of features: " + str(len(featuretbl)))

# print(featuretbl)
# print(len(featuretbl))
n_features = len(featuretbl)

##### Computing Features
# Given integral image and feature, compute intensity
def compute_feature(i,j,imgs):
	f = featuretbl[i] # featuretbl is globally updated
	img = imgs[j] # Updated elsewhere then pass to here

	if (f[8] == 1): # dx is longer
	 return ((img[f[6],f[7]] + img[f[2],f[3]] - img[f[4],f[5]] - img[f[6],f[1]]) - (img[f[4],f[5]] + img[f[0],f[1]] - img[f[2],f[3]] - img[f[0],f[7]]))
	elif (f[8] == 0): # dy is longer
	 return ((img[f[6],f[7]] + img[f[2],f[3]] - img[f[4],f[5]] - img[f[0],f[7]]) - (img[f[4],f[5]] + img[f[0],f[1]] - img[f[2],f[3]] - img[f[6],f[1]]))


##### Sigma must be recomputed as features and 
# Store the sigma ordering for each feature

# n_images for sigma table will reduce as we remove iimages from cascade
# sigmatbl = np.ndarray(shape=(n_features,n_images))

# Takes in n_remain (initially n_images) number of integral images

def compute_sigma(n, imgs):
	print("Computing Sigma...")
	global sigmatbl 
	sigmatbl = np.ndarray(shape=(n_features,n))
	
	for i in range(n_features):
		intensity = np.ndarray(n)
		for j in range(n):
			intensity[j] = compute_feature(i,j,imgs)
		sigmatbl[i] = np.argsort(intensity) # Return indices of sorted intensity
	return 

# Initializing sigma table, done after removal in ada_boost

# print(sigmatbl)

n_remain = n_images # number of images remaining after removal, updated globally
remove_count = 0 # number of images removed
# bg_access = range(1,n_images,2) # These are indices of the bg images that have not been removed
bg_no_access = [] # These are indices of the bg images that have been removed in layers
# bg_no_access[0] returns the indices of the ones that needed to be deleted in first cascade run
# is not directly mapped to iimages

##### Learning

# Given weight vector (n_images) from adaboost,
def best_learner(weight, labs, imgs):

	# neither weight, labs, imgs contains value of removed images

	print("Running Best Learner...")

	S_plus = np.ndarray(shape=(n_remain)); S_minus = np.ndarray(shape=(n_remain));

	T_plus = sum([weight[idx] if (labs[idx] == 1) else 0 for idx in range(0,n_remain)])
	T_minus = sum([weight[idx] if (labs[idx] == -1) else 0 for idx in range(0,n_remain)])

	# print(T_plus)
	# print(T_minus)

	# return

	error = np.ndarray(shape=(n_remain))

	best_feature = 0 # indices of the best feature
	best_error = 999999; best_polarity = 0; best_theta = 0

	for i in range(0,n_features):
		
		idx = int(sigmatbl[i][0]) # for feature i, idx of image that produced lowest sigma

		if (labs[idx] == 1): # check image's label
			S_plus[0] = weight[idx]
			S_minus[0] = 0 
		else: # == -1
			S_plus[0] = 0
			S_minus[0] = weight[idx]
		error[0] = min((S_plus[0] + (T_minus - S_minus[0])),(S_minus[0] + (T_plus - S_plus[0])))

		for j in range(1,n_remain):
			idx = int(sigmatbl[i][j])
			if (labs[idx] == 1):
				S_plus[j] = S_plus[j-1] + weight[idx] 
				S_minus[j] = S_minus[j-1]
			else: # == -1
				S_plus[j] = S_plus[j-1]
				S_minus[j] = S_minus[j-1] + weight[idx] 
			error[j] = min((S_plus[j] + (T_minus - S_minus[j])),(S_minus[j] + (T_plus - S_plus[j])))

		min_j = np.argmin(error) # index of the image of smallest error

		# Updating if found new lowest error
		if (error[min_j] < best_error):
			best_error = error[min_j]
			best_feature = i

			# Computing polarity
			if ((S_plus[min_j] + (T_minus - S_minus[min_j])) < (S_minus[min_j] + (T_plus - S_plus[min_j]))):
				best_polarity = 1 # double check, initial I put 1 here
			else:
				best_polarity = -1

			# Computing theta
			# if (min_j == 0):
			# 	best_theta = compute_feature(i, min_j,imgs)
			# el
			if (min_j == (n_remain - 1)):
				best_theta = compute_feature(i, min_j,imgs)
			else:
				best_theta = (compute_feature(i, min_j,imgs) + compute_feature(i, min_j + 1,imgs)) /2

	return best_feature, best_error, best_polarity, best_theta

def strong_prediction(hs, j, imgs, bt):
	nwc = len(hs) # number of weak classifier
	p = 0
	for i in range(0,nwc):
		p += hs[i][3] * np.sign(hs[i][1] * (compute_feature(hs[i][0], j, imgs) - hs[i][2]))
	return 1 if np.sign(p - bt) >= 0 else -1



def ada_boost():
	global remove_count, n_remain, bg_access, bg_no_access
	global n_features, featuretbl # should this be global?

	weight = np.ndarray(shape=n_images)	
	

	# # Save locally so that we can remove unwanted ones
	# imgs = iimages
	# labs = labels

	cascade_classifier = []
	fp = [] # keep track of false positive rate for each cascade classifier

	for c in range(0,4):

		strong_hypothesis = [] # a list of weak learners and alpha, Num_weak_classifier x 4 (f,p,t,a)
		false_positive = 1

		bgs_correct = [] # store the no

		# Save locally so that we can remove unwanted ones
		imgs = iimages
		labs = labels

		# Weights 

		weight[0::2] = [1/(n_images/2)] * int(n_images / 2) # Initialize weight relative to number of faces, e.g. if 60 faces = 1/60 for the position
		weight[1::2] = [1/(n_images/2 - remove_count)] * int(n_images / 2) # initialize weight for faces 


		## Remove unwanted ones
		for k in range(0,c):
			for i in sorted(bg_no_access[k], reverse=True):
				del weight[i]
				del imgs[i]
				del labs[i]

		print("After removal, n_remain: " + str(n_remain))
		print("After removal, len(imgs): " + str(len(imgs)))

		compute_sigma(n_remain, imgs)

		# Normalize after removal
		s = sum(weight)
		weight = [x/s for x in weight]

		global strong_pred
		strong_pred = []

		while(false_positive > 0.3):

			# call best learner, returns index of best weak classifier, p, theta, its error		
			best_feature, best_error, polarity, theta = best_learner(weight,labs,imgs)

			# compute alpha and beta
			beta = best_error / (1 - best_error)
			alpha = np.log(1/beta)

			print("Weak classifier: " + str(best_feature)); print("WC error: " + str(best_error)); 
			print("Polarity: " + str(polarity)); print("Theta: " + str(theta)); print("Alpha: " + str(alpha))
			print("[" + str(best_feature) + "," + str(polarity) + "," + str(theta) + "," + str(alpha) + "]")


			pred = [np.sign(polarity * (compute_feature(best_feature, j, imgs) - theta)) for j in range(0,n_remain)]
			pred = [1 if x >= 0.0 else -1 for x in pred]
		

			# Updating weight
			for j in range(0,n_remain):
				if (int(labs[j]) == pred[j]):
					weight[j] *= beta # Updating weight						
				# else keep it the same

			# Normalize weight
			s = sum(weight)
			weight = [x/s for x in weight]	

			# add to strong_hypothesis
			strong_hypothesis.append([best_feature,polarity,theta,alpha])

			hs = strong_hypothesis
			nwc = len(hs)

			# apply sh on faces only, find min of n_images / 2 (or 2000)
			print("Finding big Theta...")
			bt = [0] * int(n_images / 2)
			for j in range(0, n_images, 2): # only taking even to skip over backgrounds
				for i in range(0,nwc):
					# alpha * sig(p * (h(f) - theta)) # Use iimages because we always want all faces
					bt[int(j/2)] += hs[i][3] * np.sign(hs[i][1] * (compute_feature(hs[i][0], j, iimages) - hs[i][2]))

			big_theta = min(bt) # should be -10 < big_theta < 0
			print("Big theta: " + str(big_theta))

			# should be all 1's for the first time with big thetas
			strong_pred = [strong_prediction(strong_hypothesis, j, imgs, big_theta) for j in range(0,n_remain)]
			# print(strong_pred)

			false_negative = sum([1 if strong_pred[j] != int(labs[j]) and int(labs[j]) == 1 else 0 for j in range(0,n_remain)]) / (n_images /2)
			print("False Negative Rate: " + str(false_negative))

			# divided by number of background images = 
			# false_positive = sum([1 if strong_pred[j] != int(labs[j]) else 0 for j in range(0,n_remain)]) / (n_images / 2 - remove_count)
			false_positive = sum([1 if strong_pred[j] != int(labs[j]) else 0 for j in range(0,n_remain)]) / (n_images / 2 - remove_count)
			print("False Positive Rate: " + str(false_positive) + "\n")			

			#### break if wrong
			if false_positive > 1:
				break

			###
		### end of best learner loop	

		print("Cumulative False Positive Rate: " + str(np.prod(fp)))
		print("Adding a new cascade_classifier...")
		print(strong_hypothesis)
		cascade_classifier.append(strong_hypothesis)

		# Want a list of indices of background images that I have classified correctly with this strong 
		print("Removing correctly classified background images...")
		for i in range(0,n_remain):
			# In theory, assume no false_negative, (strong_pred[i] == -1) will do 
			if (labels[i] == -1) and (labels[i] == strong_pred[i]):
				bgs_correct.append(i)

		rm_len = len(bgs_correct)

		print(bgs_correct)
		print("Total len of removal: " + str(rm_len))

		
		fp.append(false_positive)
		bg_no_access.append(bgs_correct)

		n_remain -= rm_len
		remove_count += rm_len

		###
	### end of cascade loop

	return cascade_classifier

cascade_classifier = ada_boost()
print(cascade_classifier)

def extract_grid(x,y):
	img = np.ndarray(shape=(64,64))
	img = sample[y:y+64,x:x+64]
	return img

def apply_classifier(cc, img):
	for k in range(0, len(cc)):
		sc = cc[k]
		nwc = len(sc); p = 0
		for i in range(0,nwc):
			# alpha * sig(p * (f - theta)) 
			p += sc[i][3] * np.sign(sc[i][1] * (compute_feature(sc[i][0], 0, [img]) - sc[i][2]))
		if (np.sign(p) >= 0):
			return True
	return False

# input is a list of coordinates sorted by x, then by y
def filtering_faces(ls):
	x = ls[0][0]
	y = ls[0][1]
	for i in range(1,len(ls)):
		if ((ls[i][0] < (x + 64) and ls[i][0] > (x - 64)) and 
		    (ls[i][1] < (y + 64) and ls[i][1] > (y - 64))):
			del ls[i]
			i -= 1
	return [ls[0]].append(filtering_faces(ls[1:]))
			

sample = io.imread(os.path.join(dir_path, 'class.jpg'), as_grey = True)
def detect_image(cc):
	global sample
	# rgb2gray()
	
	# print(sample)
	lx = int(sample.shape[1])
	ly = int(sample.shape[0])

	known_faces = []

	for x in range(0,lx - 64,2):
		for y in range(0,ly - 64,2):
			if (apply_classifier(cc,extract_grid(x,y))): # If true, we have detected a face
				known_faces.append([x,y]) 

	return known_faces

def draw_rec(faces):
	global sample
	
	# Create figure and axes
	fig,ax = plt.subplots(1)
	# Display the image
	io.imshow(sample)
	for i in range(len(faces)):
		# Create a Rectangle patch
		x = faces[i][0]
		y = faces[i][1]
		rect = patches.Rectangle((y,x),64,64,linewidth=1,edgecolor='r',facecolor='none')
		ax.add_patch(rect)
	
	plt.show()
	plt.savefig('foo.png')


