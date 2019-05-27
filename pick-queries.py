import h5py
import numpy
import sys

fn = sys.argv[1]
gn = sys.argv[2]

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

def create_dataset(f, train, nn, dd, l, name):
    g = h5py.File(fn.replace('.hdf5','') + '-%s.hdf5' % name, 'w')

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


create_dataset(f, train, nn, dd, easy, 'easy')
create_dataset(f, train, nn, dd, middle, 'middle')
create_dataset(f, train, nn, dd, hard, 'hard')
create_dataset(f, train, nn, dd, diverse, 'diverse')

