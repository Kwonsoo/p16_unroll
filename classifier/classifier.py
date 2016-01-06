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

#
# Checking command line arguments
#

f_train = ""
f_test  = ""

if len(sys.argv) == 3:
  f_train = sys.argv[1]
  f_test = sys.argv[2]
else:
  print "Usage: python learning.py <train-data> <test-data>"
  quit()

#
# Training Phase
#

features  = []
solutions = []
traindata = []

num_dups = 0
num_data = 0
num_cflct = 0

# read without duplications
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


#
# Read test data
#

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

#
# prediction
#

C = 1.0
classifiers = [#('Nearest Neighbors', KNeighborsClassifier(5)),
               ('Logistic Regression (l1)', LogisticRegression(C=C, penalty='l1')),
#               ('Logistic Regression (l2)', LogisticRegression(C=C, penalty='l2')),
               ('Linear SVM', SVC(kernel='linear',C=C,probability=True,random_state=0)),
#               ('RBF SVM', SVC(gamma=2,C=C,probability=True,random_state=0)),
#               ('Decision Tree', DecisionTreeClassifier(max_depth=10)),
#               ('Random Forest', RandomForestClassifier(max_depth=10,n_estimators=10,max_features=1)),
#               ('AdaBoost', AdaBoostClassifier()),
#               ('Naive Bayes', GaussianNB())
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
