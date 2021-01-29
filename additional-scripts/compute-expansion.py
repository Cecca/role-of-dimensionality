# Computes the expansion around at the k-th nearest neighbor

import math
import sys
import h5py


f = h5py.File(sys.argv[1])

# default: contrast 10-th and 20-th NN
k = 10
kk = 20

if len(sys.argv) > 2:
    k = int(sys.argv[2])
    kk = int(sys.argv[3])

i = 0
for vec in f['distances']:
    vec.sort()
    expansion = vec[kk - 1] / vec[k - 1]
    print(i, expansion)
    i += 1




