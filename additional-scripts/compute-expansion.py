# Computes the expansion around at the 10th nearest neighbour

import math
import sys
import h5py

k = 10

f = h5py.File(sys.argv[1])

c = 2

if len(sys.argv) > 2:
    c = int(sys.argv[2])

i = 0
for vec in f['distances']:
    vec.sort()
    expansion = vec[c*k - 1] / vec[k - 1]
    print(i, expansion)
    i += 1




