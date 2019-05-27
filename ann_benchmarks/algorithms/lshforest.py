from __future__ import absolute_import
from ann_benchmarks.algorithms.base import BaseANN
from sklearn.neighbors import LSHForest
import sys

sys.setrecursionlimit(int(10**6))


class LSHForest(BaseANN):
    def __init__(self, n_trees):
        self._n_trees = n_trees

    def fit(self, X):
        self._index = LSHForest(n_trees = self._n_trees)
        self._index.fit(X)

    def query(self, v, n):
        return self._index.kneighbors([v], n_neighbors = n)[1]

    def __str__(self):
        return 'LSHForest(n_trees=%d)' % (self._n_trees)
