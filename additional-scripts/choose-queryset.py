import sys
import random
import argparse

random.seed(12341)
parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('difficulty', metavar='FILE',
                    help='the file containing the difficulty of queries, either LID, expansion, or LRC')
parser.add_argument('--expansion', action='store_true',
                    help='the file contains expansion instead of LID')
parser.add_argument('--contrast', action='store_true',
                    help='the file contains LRC instead of LID')

#fn = sys.argv[1]
args = parser.parse_args()
fn = args.difficulty
use_expansion = args.expansion
use_rc = args.contrast
#print(fn, use_expansion)

estimates = []

with open(fn) as f:
    for line in f:
        try:
            i, diff = line.strip().split()
            estimates.append((int(i), float(diff)))
        except:
            pass

estimates.sort(key=lambda x: x[-1], reverse=use_expansion or use_rc)
easy = estimates[:10000]
middle = estimates[len(estimates) // 2 - 5000:len(estimates) // 2 + 5000]
hard = estimates[-10000:]

print("Easiest with avg difficulty: %f" % (sum(map(lambda x: x[-1], easy)) / 10000))
print(list(map(lambda x: x[0], easy)))
print("Average with avg difficulty: %f" % (sum(map(lambda x: x[-1], middle)) / 10000))
print(list(map(lambda x: x[0], middle)))
print("Most difficult with avg difficulty: %f" % (sum(map(lambda x: x[-1], hard)) / 10000))
print(list(map(lambda x: x[0], hard)))

def choose_lid_diverse(estimates):
    diffs = {}

    for i, diff in estimates:
        diffs.setdefault(int(diff), [])
        diffs[int(diff)].append(i)

    dataset = []

    keys = list(diffs.keys())
#print(keys)

    for i in range(5000):
        b = random.choice(keys)
        dataset.append(random.choice(diffs[b]))

    print("Diverse: %f" % (sum(map(lambda x: estimates[x][-1], dataset)) / 5000))
    print(dataset)

def choose_expansion_diverse(estimates):
    mid = estimates[1500:-1500]
    dataset = [x[0] for x in estimates[:1500] + estimates[-1500:] + [random.choice(mid) for _ in range(2000)]]
    print("Diverse: %f" % (sum(map(lambda x: estimates[x][-1], dataset)) / 5000))
    print(dataset)

if use_expansion or use_rc:
    choose_expansion_diverse(estimates)
else:
    choose_lid_diverse(estimates)
