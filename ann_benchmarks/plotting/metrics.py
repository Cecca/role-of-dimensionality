from __future__ import absolute_import
import numpy as np


def knn(dataset_distances, run_distances, count, metrics, epsilon=1e-10):
    if 'knn' not in metrics:
        print('Computing knn metrics')
        knn_metrics = metrics.create_group('knn')
        total = len(run_distances) * count
        recalls = np.zeros(len(run_distances))
        for i in range(len(run_distances)):
            t = dataset_distances[i][count - 1] + epsilon
            actual = 0
            for j in range(count):
                if run_distances[i][j] <= t:
                    actual += 1
                else:
                    break
            recalls[i] = actual
        knn_metrics.attrs['mean'] = np.mean(recalls) / float(count)
        knn_metrics.attrs['std'] = np.std(recalls) / float(count)
        percentiles = [5,25,50,75,95]
        percentile_values = np.percentile(recalls/float(count), percentiles)
        for p, v in zip(percentiles, percentile_values):
            knn_metrics.attrs['perc-' + str(p)] = v
        knn_metrics['recalls'] = recalls
    else:
        print("Found cached result")
    return metrics['knn']


def epsilon(dataset_distances, run_distances, count, metrics, epsilon=0.01):
    s = 'eps' + str(epsilon)
    if s not in metrics:
        print('Computing epsilon metrics')
        epsilon_metrics = metrics.create_group(s)
        total = len(run_distances) * count
        recalls = np.zeros(len(run_distances))
        for i in range(len(run_distances)):
            t = dataset_distances[i][count - 1] * (1 + epsilon)
            actual = 0
            for j in range(count):
                if run_distances[i][j] <= t:
                    actual += 1
                else:
                    break
            recalls[i] = actual
        epsilon_metrics.attrs['mean'] = np.mean(recalls) / float(count)
        epsilon_metrics.attrs['std'] = np.std(recalls) / float(count)
        percentiles = [5,25,50,75,95]
        percentile_values = np.percentile(recalls/float(count), percentiles)
        for p, v in zip(percentiles, percentile_values):
            epsilon_metrics.attrs['perc-' + str(p)] = v
        epsilon_metrics['recalls'] = recalls
    else:
        print("Found cached result")
    return metrics[s]

def rel(dataset_distances, run_distances, metrics):
    if 'rel' not in metrics.attrs:
        print('Computing rel metrics')
        total_closest_distance = 0.0
        total_candidate_distance = 0.0
        for true_distances, found_distances in zip(dataset_distances, run_distances):
            for rdist, cdist in zip(true_distances, found_distances):
                total_closest_distance += rdist
                total_candidate_distance += cdist
        if total_closest_distance < 0.01:
            metrics.attrs['rel'] = float("inf")
        else:
            metrics.attrs['rel'] = total_candidate_distance / total_closest_distance
    else:
        print("Found cached result")
    return metrics.attrs['rel']

def queries_per_second(query_times, metrics):
    if 'qps' not in metrics:
        print('Computing qps metrics')
        qps_values = 1.0/np.array(query_times)
        qps_metrics = metrics.create_group('qps')
        qps_metrics.attrs['mean'] = 1.0/np.mean(query_times)
        percentiles = [5,25,50,75,95]
        percentile_values = np.percentile(qps_values, percentiles)
        for p, v in zip(percentiles, percentile_values):
            qps_metrics.attrs['perc-' + str(p)] = v
        qps_metrics['query_times'] = query_times
    else:
        print("Found cached result")
    return metrics['qps']

def index_size(queries, attrs):
    # TODO(erikbern): should replace this with peak memory usage or something
    return attrs.get("index_size", 0)

def build_time(queries, attrs):
    return attrs["build_time"]

def candidates(queries, attrs):
    return attrs["candidates"]

def dist_comps(queries, attrs):
    return attrs.get("dist_comps", 0) / len(queries)

all_metrics = {
    "k-nn": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['mean'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "k-nn-std": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['std'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "k-nn-median": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['perc-50'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "k-nn-perc-5": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['perc-5'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "k-nn-perc-95": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['perc-95'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "k-nn-perc-25": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['perc-25'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "k-nn-perc-75": {
        "description": "Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: knn(true_distances, run_distances, run_attrs["count"], metrics).attrs['perc-75'],
        "worst": float("-inf"),
        "lim": [0.0, 1.03]
    },
    "epsilon": {
        "description": "Epsilon 0.01 Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: epsilon(true_distances, run_distances, run_attrs["count"], metrics).attrs['mean'],
        "worst": float("-inf")
    },
    "largeepsilon": {
        "description": "Epsilon 0.1 Recall",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: epsilon(true_distances, run_distances, run_attrs["count"], metrics, 0.1).attrs['mean'],
        "worst": float("-inf")
    },
    "rel": {
        "description": "Relative Error",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: rel(true_distances, run_distances, metrics),
        "worst": float("inf")
    },
    "qps": {
        "description": "Queries per second (1/s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: queries_per_second(query_times, metrics).attrs['mean'],
        "worst": float("-inf")
    },
    "qps-median": {
        "description": "Queries per second (1/s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: queries_per_second(query_times, metrics).attrs['perc-50'],
        "worst": float("-inf")
    },
    "qps-perc-5": {
        "description": "Queries per second (1/s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: queries_per_second(query_times, metrics).attrs['perc-5'],
        "worst": float("-inf")
    },
    "qps-perc-95": {
        "description": "Queries per second (1/s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: queries_per_second(query_times, metrics).attrs['perc-95'],
        "worst": float("-inf")
    },
    "qps-perc-25": {
        "description": "Queries per second (1/s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: queries_per_second(query_times, metrics).attrs['perc-25'],
        "worst": float("-inf")
    },
    "qps-perc-75": {
        "description": "Queries per second (1/s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: queries_per_second(query_times, metrics).attrs['perc-75'],
        "worst": float("-inf")
    },
    "distcomps" : {
        "description": "Distance computations",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: dist_comps(true_distances, run_attrs),
        "worst": float("inf")
    },
    "build": {
        "description": "Build time (s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: build_time(true_distances, run_attrs),
        "worst": float("inf")
    },
    "candidates" : {
        "description": "Candidates generated",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: candidates(true_distances, run_attrs),
        "worst": float("inf")
    },
    "indexsize" : {
        "description": "Index size (kB)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: index_size(true_distances, run_attrs),
        "worst": float("inf")
    },
    "queriessize" : {
        "description": "Index size (kB)/Queries per second (s)",
        "function": lambda true_distances, run_distances, query_times, metrics, run_attrs: index_size(true_distances, run_attrs) / queries_per_second(true_distances, metrics).attrs['mean'],
        "worst": float("inf")
    }
}
