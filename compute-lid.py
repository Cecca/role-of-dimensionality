import math
import sys
import h5py

f = h5py.File(sys.argv[1])

distances = []

for dist in f['distances']:
    distances.append(dist)

estimates = []

for vec in distances:
    vec.sort()
    w = vec[-1]
    lid = 0.0
    for v in vec:
        if v > 0.0001:
            lid += math.log(v / w)
    if lid > -0.1 and lid < 0.1:
        continue
    lid /= len(vec)
    lid = -1 / lid
    estimates.append(lid)

for i,e in enumerate(estimates):
    print(i, e)
#print(estimates)
avg_estimate = sum(estimates) / len(estimates)
print(sys.argv[1], avg_estimate)




