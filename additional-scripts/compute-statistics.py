import numpy as np
import sys

fn = sys.argv[1]

with open(fn) as f:
    arr = np.array([float(line.split()[-1]) for line in f])

print(fn, "Mean", round(np.mean(arr), 3), "Median", round(np.median(arr), 3))
