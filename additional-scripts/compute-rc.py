import math
import sys
import h5py
import numpy
import random
import scipy
from scipy.spatial.distance import cdist

f = h5py.File(sys.argv[1])

# default: relate average distance to distance of 10-th NN
k = 10

if len(sys.argv) > 2:
    k = int(sys.argv[2])

samples = 3000
m = len(f['test'])

distances = numpy.array(f['distances'])

#query_indices = [random.choice(range(len(f['test']))) for _ in range(m)]

queries = numpy.array(f['test'])

dataset = numpy.array(f['train'])

estimates = numpy.zeros(m, dtype=numpy.float)

random_matrix = numpy.array([random.choice(f['train']) for _ in range(samples)])

#for i, v in enumerate(queries):
if 'euclidean' in sys.argv[1]:
    avg = numpy.mean(numpy.transpose(cdist(random_matrix, queries, 'euclidean')), axis=1)
if 'angular' in sys.argv[1]:
    avg = numpy.median(numpy.transpose(cdist(random_matrix, queries, 'cosine')), axis=1)

assert len(avg) == len(queries)

for i in range(len(queries)):
    for j in range(k - 1, 100):
        if distances[i][j] > 1e-6:
            dist = distances[i][j]
            break

    estimates[i] = (avg[i] / dist)

for i,e in enumerate(estimates):
    print(i, e)
# print(sys.argv[1], numpy.median(estimates))




