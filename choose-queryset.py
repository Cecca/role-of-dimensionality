import sys
import random

random.seed(12341)

fn = sys.argv[1]

estimates = []

with open(fn) as f:
    for line in f:
        try:
            i, lid = line.strip().split()
            estimates.append((int(i), float(lid)))
        except:
            pass


estimates.sort(key=lambda x: x[-1])
easy = estimates[:10000]
middle = estimates[len(estimates) // 2 - 5000:len(estimates) // 2 + 5000]
hard = estimates[-10000:]

print("Easiest with avg lid: %f" % (sum(map(lambda x: x[-1], easy)) / 10000))
print(list(map(lambda x: x[0], easy)))
print("Average with avg lid: %f" % (sum(map(lambda x: x[-1], middle)) / 10000))
print(list(map(lambda x: x[0], middle)))
print("Most difficult with avg lid: %f" % (sum(map(lambda x: x[-1], hard)) / 10000))
print(list(map(lambda x: x[0], hard)))

lids = {}

for i, lid in estimates:
    lids.setdefault(int(lid), [])
    lids[int(lid)].append(i)

dataset = []

keys = list(lids.keys())
#print(keys)

for i in range(5000):
    b = random.choice(keys)
    dataset.append(random.choice(lids[b]))

print("Diverse: %f" % (sum(map(lambda x: estimates[x][-1], dataset)) / 5000))
print(dataset)
