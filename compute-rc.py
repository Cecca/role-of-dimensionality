import math
import sys
import h5py
import numpy
import random
import scipy
from scipy.spatial.distance import cdist

f = h5py.File(sys.argv[1])

samples = 3000
m = 3000

distances = numpy.array(f['distances'])

query_indices = [random.choice(range(len(f['test']))) for _ in range(m)]

queries = numpy.array([f['train'][i] for i in query_indices])

dataset = numpy.array(f['train'])

estimates = numpy.zeros(m, dtype=numpy.float)

random_matrix = numpy.array([random.choice(f['train']) for _ in range(samples)])

for i, v in enumerate(queries):
    print(i)
    if 'euclidean' in sys.argv[1]:
        avg = numpy.mean(cdist(random_matrix, [v], 'euclidean'))
    #avg = numpy.mean((random_matrix - v, axis = 1))
    if 'angular' in sys.argv[1]:
        avg = numpy.mean(cdist(random_matrix, [v], 'cosine'))
    d_closest = f['distances'][query_indices[i]][1]

    estimates[i] = (avg / d_closest)

for i,e in enumerate(estimates):
    print(i, e)
print(sys.argv[1], numpy.median(estimates))




