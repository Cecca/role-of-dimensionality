import h5py
import numpy
import sys
import argparse
parser = argparse.ArgumentParser(description='Process some integers.')
parser.add_argument('datafile', metavar='FILE',
                    help='the HDF5 file containing the dataset')
parser.add_argument('queryfile', metavar='FILE',
                    help='the text file containing queries')
parser.add_argument('--expansion', action='store_true',
                    help='the queries are selected by their expansion')
parser.add_argument('--contrast', action='store_true',
                    help='the file contains LRC instead of LID')
args = parser.parse_args()

fn = args.datafile # sys.argv[1]
gn = args.queryfile # sys.argv[2]
use_expansion = args.expansion
use_rc = args.contrast

# read h5py file completely
f = h5py.File(fn)

attrs = f.attrs
train = f['train']
test = f['test']
nn = f['neighbors']
dd = f['distances']

# choose querysets

with open(gn) as g:
    lines = g.readlines()

easy = list(map(int, lines[1].strip()[1:-1].split(",")))
middle = list(map(int, lines[3].strip()[1:-1].split(",")))
hard = list(map(int, lines[5].strip()[1:-1].split(",")))
diverse = list(map(int, lines[7].strip()[1:-1].split(",")))


# make four different versions containing the different querysets

def create_dataset(f, train, nn, dd, l, name, difficulty_type):
    g = h5py.File(fn.replace('.hdf5','') + '-{}-{}.hdf5'.format(name, difficulty_type), 'w')

    g.attrs['distance'] = f.attrs['distance']
    g.attrs['point_type'] = f.attrs['point_type']
    g.create_dataset('train', (len(train), len(train[0])), dtype=train.dtype)[:] = train

    queries = []
    distances = []
    neighbors = []

    for i in l:
        queries.append(train[i])
        neighbors.append(nn[i])
        distances.append(dd[i])

    g.create_dataset('test', (len(queries), len(queries[0])), dtype=train.dtype)[:] = queries
    g.create_dataset('neighbors', (len(neighbors), len(neighbors[0])), dtype='i')[:] = neighbors
    g.create_dataset('distances', (len(distances), len(distances[0])), dtype='f')[:] = distances

    g.close()

if use_expansion:
    difficulty_type = "expansion"
elif use_rc:
    difficulty_type = "lrc"
else:
    difficulty_type = "lid"

create_dataset(f, train, nn, dd, easy, 'easy', difficulty_type)
create_dataset(f, train, nn, dd, middle, 'middle', difficulty_type)
create_dataset(f, train, nn, dd, hard, 'hard', difficulty_type)
create_dataset(f, train, nn, dd, diverse, 'diverse', difficulty_type)

