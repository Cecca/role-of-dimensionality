# Computes the expansion around at the 10th nearest neighbour

import math
import sys
import h5py

k = 10

f = h5py.File(sys.argv[1])

i = 0
for vec in f['distances']:
    vec.sort()
    expansion = vec[2*k] / vec[k]
    print(i, expansion)
    i += 1




