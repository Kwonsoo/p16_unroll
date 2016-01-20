#
# python run.py train.txt test.txt
#
# The train.txt and test.txt must be of the following format:
# 0 1 0 0 ... 0 0 1 0 : 0

import sys
import glob
import os
import re
import time

import numpy as np

from sklearn.metrics import roc_curve, auc
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.lda import LDA
from sklearn.qda import QDA
from sklearn.linear_model import SGDRegressor
from sklearn import linear_model
from sklearn.multiclass import OneVsRestClassifier

def train (f_train):
	features = []
	solutions = []
	traindata = []

	num_dups = 0
	num_data = 0
	num_cflct = 0

	with open(f_train) as f:
		for line in f:
			data = re.split(r':',line)
			proved = float(data[1].strip())
			wv = data[0]
			wv = re.split(r' |\t|\n', wv)
			wv = filter(lambda x:x.strip() != '', wv)
			wv = map(lambda x:int(x),wv)
			traindata.append((wv,proved))
			#if (wv,proved) not in traindata:
			#  traindata.append((wv,proved))
			#else:
			#  num_dups = num_dups + 1
			num_data = num_data + 1

	print "Data without duplicates:"
	print ""

	for i in range(len(traindata)):
	 (f,s) = traindata[i]
	#  print s, f
	 features.append(f)
	 solutions.append(s)

	print ""
	print "#data       : %d" % num_data
	print "#duplucates : %d" % num_dups
	print "#final-training-data : %d" % (len(traindata))

	return (features, solutions)

def read_test_data (f_test):
	testFeatures = []
	testSolutions = []
	with open(f_test) as f:
		for line in f:
			data = re.split(r':',line)
			proved = float(data[1].strip())
			wv = data[0]
			wv = re.split(r' |\t|\n', wv)
			wv = filter(lambda x:x.strip() != '', wv)
			wv = map(lambda x:int(x),wv)
			testFeatures.append (wv)
			testSolutions.append(proved)

	return (testFeatures, testSolutions)

def read_fbvector(file_fbvector):
	with open(file_fbvector) as f:
		fbvector = f.readline ()
		fbvector = re.split(r' |\t|\n', fbvector)
		fbvector = filter(lambda x:x.strip() != '', fbvector)
		fbvector = map(lambda x:int(x), fbvector)
		return fbvector

def predict_new_fbvector (fbvector, classifier_kind, features, solutions):
	C = 1.0
	classifiers = [('Logistic Regression (l1)', LogisticRegression(C=C, penalty='l1')),
								('Linear SVM', SVC(kernel='linear',C=C,probability=True,random_state=0))]
	name = ''
	clf = None
	if classifier_kind == 'LR':
		name = classifiers[0][0]
		clf = classifiers[0][1]
	elif classifier_kind == 'LSVM':
		name = classifiers[1][0]
		clf = classifiers[1][1]
	
	start_time = time.time()
	
	mclf = clf
	mclf.fit (features, solutions)

	print name
	print mclf

	pred = mclf.predict(fbvector)[0]
	if pred == 1:
		print "fbvector-predict: YES"
		sys.exit(10)	# 10 means it is worth precision.
	else:
		print "fbvector-predict: NO"
		sys.exit(11)	# 11 means it is not.

def predict_test (features, solutions, testFeatures, testSolutions):
	C = 1.0
	classifiers = [('Nearest Neighbors', KNeighborsClassifier(5)),
								('Logistic Regression (l1)', LogisticRegression(C=C, penalty='l1')),
               ('Logistic Regression (l2)', LogisticRegression(C=C, penalty='l2')),
               ('Linear SVM', SVC(kernel='linear',C=C,probability=True,random_state=0)),
               ('RBF SVM', SVC(gamma=2,C=C,probability=True,random_state=0)),
               ('Decision Tree', DecisionTreeClassifier(max_depth=10)),
               ('Random Forest', RandomForestClassifier(max_depth=10,n_estimators=10,max_features=1)),
               ('AdaBoost', AdaBoostClassifier()),
               ('Naive Bayes', GaussianNB())
             ]

	index = 0;

	for (name, clf) in classifiers:
  
		start_time = time.time()

		index = index + 1
		mclf = clf #OneVsRestClassifier(clf)
		mclf.fit (features,solutions)

		print ""
		print "%d" % index, 
		print ".", name
		print mclf

		correct = 0
		incorrect = 0

		select = 0
		target = 0

		for i in range(len(testFeatures)):
			pred = mclf.predict(testFeatures[i])[0]	
			solution = testSolutions[i]

			proba = mclf.predict_proba (np.array (testFeatures, np.int32))
			testProbs = []
			for i in range(len(testSolutions)):
			 testProbs.append (proba[i][1])

			false_positive_rate, true_positive_rate, thresholds = roc_curve(testSolutions, testProbs)
			roc_auc = auc(false_positive_rate, true_positive_rate)
 
			if solution == 1:
				target = target + 1 

			if pred == 1:
				select = select + 1

			if solution == 1 and pred == solution:
				correct = correct + 1

		print "#queries that require high precision  : %d" % (target)
		print "#queries selected by the classifier   : %d" % (select)

		if select > 0:
			print "Precision : %d%% (%d / %d)" % (correct*100/select, correct, select)
		if target > 0:
			print "Recall    : %d%% (%d / %d)" % (correct*100/target, correct, target)

		print "AUC : ",
		print roc_auc
 
		end_time = time.time()

		print "%.2f" % (end_time - start_time),
		print "sec"

if __name__ == "__main__":
	if len(sys.argv) == 4 and sys.argv[1] == "test":
			f_train = sys.argv[2]
			f_test = sys.argv[3]
			(features, solutions) = train(f_train)
			(test_features, test_solutions) = read_test_data(f_test)
			predict_test(features, solutions, test_features, test_solutions)
	elif len(sys.argv) == 5 and sys.argv[1] == "fbvector_predict":
		classifier_kind = sys.argv[2]
		if not (classifier_kind == "LR" or classifier_kind == "LSVM"):
			print "kind_of_classifier: LR or LSVM only"
			quit()
		f_train = sys.argv[3]
		f_fbvector = sys.argv[4]
		(features, solutions) = train(f_train)
		fbvector = read_fbvector(f_fbvector)
		predict_new_fbvector(fbvector, classifier_kind, features, solutions)
	else:
		print "Usage1: python learning.py test <train-data> <test-data>"
		print "Usage2: python learning.py fbvector_predict kind_of_classifier <train-data> <one-fvector-file>"
		quit()
