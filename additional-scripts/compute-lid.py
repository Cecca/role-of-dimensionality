import math
import sys
import h5py
import numpy

# LID computation taken from
# https://github.com/elki-project/elki/blob/master/elki-core-math/src/main/java/elki/math/statistics/intrinsicdimensionality/HillEstimator.java.

f = h5py.File(sys.argv[1])
k = 100
if len(sys.argv) == 3:
    k = int(sys.argv[2])

print("Using k=", k, file=sys.stderr)
distances = numpy.array(f['distances'])

estimates = []

for i, vec in enumerate(distances):
    vec.sort()
    w = vec[min(len(vec) - 1, k)]
    half_w = 0.5 * w
    s = 0.0
    valid = 0
    for v in vec[:k+1]:
        if v > 1e-5:
            if v < half_w:
                s += math.log(v / w)
            else:
                s += numpy.log1p((v - w) / w)
            valid += 1
    estimates.append(-valid / s)

for i,e in enumerate(estimates):
    print(i, e)
#print(estimates)
# avg_estimate = sum(estimates) / len(estimates)
# print(sys.argv[1], avg_estimate)




