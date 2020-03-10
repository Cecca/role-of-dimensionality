import math
import sys
import h5py
import numpy

f = h5py.File(sys.argv[1])

distances = numpy.array(f['distances'])

estimates = []

for i, vec in enumerate(distances):
    if i % 10000 == 0:
        print(i)
    vec.sort()
    w = vec[-1]
    half_w = 0.5 * w
    s = 0.0
    valid = 0
    for v in vec:
        if v > 0.:
            if v < half_w:
                s += math.log(v / w)
            else:
                s += numpy.log1p((v - w) / w)
            valid += 1
    estimates.append(-valid / s)

for i,e in enumerate(estimates):
    print(i, e)
#print(estimates)
avg_estimate = sum(estimates) / len(estimates)
print(sys.argv[1], avg_estimate)




