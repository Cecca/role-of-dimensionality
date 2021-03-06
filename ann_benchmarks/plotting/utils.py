from __future__ import absolute_import

import random
import numpy as np
import pandas as pd
from pandas.api.types import CategoricalDtype
import os, itertools, json, numpy, pickle
from ann_benchmarks.plotting.metrics import all_metrics as metrics
from ann_benchmarks.plotting.metrics import compute_knn, compute_rel
import matplotlib.pyplot as plt
import re
from scipy.spatial.distance import cdist
import math

def compute_expansion(all_distances, k):
    def compute_single(distances):
        distances.sort()
        return distances[2*k] / distances[k]
    print("Computing the Expansion")
    return numpy.array([
        compute_single(ds) for ds in all_distances
    ])

def compute_lid(all_distances):
    def compute_single(distances):
        distances.sort()
        w = distances[-1]
        half_w = 0.5 * w
        s = 0.0
        valid = 0
        for v in distances:
            if v > 1e-5:
                if v < half_w:
                    s += numpy.log(v / w)
                else:
                    s += numpy.log1p((v - w) / w)
                valid += 1
        return -valid / s

    print("Computing the Local Intrinsic Dimensionality")
    return numpy.array([
        compute_single(ds) for ds in all_distances
    ])

def compute_lid_10(all_distances):
    def compute_single(distances):
        distances.sort()
        w = distances[9]
        half_w = 0.5 * w
        s = 0.0
        valid = 0
        for v in distances[:10]:
            if v > 1e-5:
                if v < half_w:
                    s += numpy.log(v / w)
                else:
                    s += numpy.log1p((v - w) / w)
                valid += 1
        return -valid / s

    print("Computing the Local Intrinsic Dimensionality (k=10)")
    return numpy.array([
        compute_single(ds) for ds in all_distances
    ])

def compute_rc(queries, dataset, distances, k, distance_type, n_samples=3000):
    print("Computing the Local Relative Contrast")
    samples = numpy.array([random.choice(dataset) 
                           for _ in range(n_samples)])
    
    if 'euclidean' in distance_type:
        avg = numpy.mean(numpy.transpose(
            cdist(samples, queries, 'euclidean')), axis=1)
    if 'angular' in distance_type:
        avg = numpy.median(numpy.transpose(
            cdist(samples, queries, 'cosine')), axis=1)

    estimates = []
    for i in range(len(queries)):
        for j in range(10, 100):
            if distances[i][j] > 1e-6:
                dist = distances[i][j]
                break
        estimates.append(avg[i] / dist)
    return numpy.array(estimates)

def get_dimensionality_measures(dataset, distance_type):
    if 'dimensionality_measures' not in dataset:
        group = dataset.create_group('dimensionality_measures')
        # measures = {
        #     'lrc' : compute_rc(dataset['test'], 
        #                        dataset['train'],
        #                        dataset['distances'],
        #                        10,
        #                        distance_type),
        #     'lid': compute_lid(dataset['distances']),
        #     'lid10': compute_lid_10(dataset['distances']),
        #     'expansion': compute_expansion(dataset['distances'], 10)
        # }
    if 'lrc' not in dataset['dimensionality_measures']:
        dataset['dimensionality_measures']['lrc'] = compute_rc(dataset['test'], 
                               dataset['train'],
                               dataset['distances'],
                               10,
                               distance_type)
    if 'lid' not in dataset['dimensionality_measures']:
        dataset['dimensionality_measures']['lid'] = compute_lid(dataset['distances'])
    if 'lid10' not in dataset['dimensionality_measures']:
        dataset['dimensionality_measures']['lid10'] = compute_lid_10(dataset['distances'])
    if 'expansion' not in dataset['dimensionality_measures']:
        dataset['dimensionality_measures']['expansion'] = compute_expansion(dataset['distances'], 10)

    return dataset['dimensionality_measures']


def get_or_create_metrics(run):
    if 'metrics' not in run:
        run.create_group('metrics')
    return run['metrics']


def create_pointset(data, xn, yn):
    xm, ym = (metrics[xn], metrics[yn])
    rev = ym["worst"] < 0
    data.sort(key=lambda t: t[-1], reverse=rev) # sort by y coordinate

    axs, ays, als = [], [], []
    # Generate Pareto frontier
    xs, ys, ls = [], [], []
    last_x = xm["worst"]
    comparator = \
      (lambda xv, lx: xv > lx) if last_x < 0 else (lambda xv, lx: xv < lx)
    for algo, algo_name, xv, yv in data:
        if not xv or not yv:
            continue
        axs.append(xv)
        ays.append(yv)
        als.append(algo_name)
        if comparator(xv, last_x):
            last_x = xv
            xs.append(xv)
            ys.append(yv)
            ls.append(algo_name)
    return xs, ys, ls, axs, ays, als

def compute_metrics(dataset, res, metric_1, metric_2, recompute=False):
    true_nn_distances = numpy.array(dataset['distances'])
    all_results = {}
    for i, (properties, run) in enumerate(res):
        algo = properties['algo']
        algo_name = properties['name']
        # cache to avoid access to hdf5 file
        run_distances = numpy.array(run['distances'])
        query_times = numpy.array(run['times'])
        if recompute and 'metrics' in run:
            del run['metrics']
        metrics_cache = get_or_create_metrics(run)

        metric_1_value = metrics[metric_1]['function'](true_nn_distances, run_distances, query_times, metrics_cache, run.attrs)
        metric_2_value = metrics[metric_2]['function'](true_nn_distances, run_distances, query_times, metrics_cache, run.attrs)

        print('%3d: %80s %12.3f %12.3f' % (i, algo_name, metric_1_value, metric_2_value))

        all_results.setdefault(algo, []).append((algo, algo_name, metric_1_value, metric_2_value))

    return all_results

difficulty_cat = pd.CategoricalDtype(['hard', 'middle', 'easy', 'diverse'])
difficulty_type_cat = pd.CategoricalDtype(['lid', 'expansion', 'lrc'])
algo_map = {
    'annoy' : 'Annoy',
    'faiss-ivf' : 'IVF',
    'hnsw(faiss)' : 'HNSW',
    'NGT-onng' : 'ONNG',
    'puffinn' : "PUFFINN"
}
algo_cat = pd.CategoricalDtype(algo_map.values())
dataset_map = {
    "glove-2m-300-angular": "GLOVE-2M" ,
    "gnews-300-angular": "GNEWS" ,
    "glove-100-angular": "GLOVE" ,
    "sift-128-euclidean": "SIFT" ,
    "fashion-mnist-784-euclidean": "Fashion-MNIST" ,
    "mnist-784-euclidean": "MNIST" 
}
dataset_cat = pd.CategoricalDtype(dataset_map.values())

def compute_metrics_all_runs(dataset, res, recompute=False):
    true_nn_distances=list(dataset['distances'])
    for i, (properties, run) in enumerate(res):
        algo = properties['algo']
        algo_name = properties['name']
        # cache distances to avoid access to hdf5 file
        # print('Load distances and times')
        run_distances = np.array(run['distances'])
        query_times = np.array(run['times'])
        # print('... done')
        if recompute and 'metrics' in run:
            print('Recomputing metrics, clearing cache')
            del run['metrics']
        metrics_cache = get_or_create_metrics(run)
        
        dataset = properties['dataset']
        if 'expansion' in dataset:
            difficulty_type = 'expansion'
        elif 'lrc' in dataset:
            difficulty_type = "lrc"
        else:
            difficulty_type = "lid"
        difficulty = re.findall("hard|middle|easy|diverse", dataset)[0]
        dataset = re.sub("-(hard|middle|easy|diverse)", "", dataset)
        dataset = re.sub("-(expansion|lrc|lid)", "", dataset)

        run_result = {
            'algorithm': algo_map[algo],
            'parameters': algo_name,
            'dataset': dataset_map[dataset],
            'difficulty': difficulty,
            'difficulty_type': difficulty_type,
            'count': properties['count']
        }
        for name, metric in metrics.items():
            v = metric["function"](true_nn_distances, run_distances, query_times, metrics_cache, properties)
            run_result[name] = v
        yield run_result


def runs_to_sqlite(dataset, res, conn):
    for (properties, run) in res:
        print(".", end="")
        run_to_sqlite(dataset, run, properties, conn)
    print()


def run_to_sqlite(data, run, properties, conn):
    true_nn_distances = numpy.array(data['distances'])
    k = len(run['distances'][0])
    count = int(properties['count'])
    assert(count == k)
    algo = properties["algo"]
    algo_name = properties["name"]
    dataset = properties['dataset']
    # cache to avoid access to hdf5 file
    run_distances = numpy.array(run["distances"])
    query_times = numpy.array(run['times'])

    recall = compute_knn(true_nn_distances, run_distances, k) / k
    rel = compute_rel(true_nn_distances, run_distances)

    avg_recall = float(np.mean(recall))
    avg_epsilon_recall = float(np.mean(compute_knn(true_nn_distances, run_distances, k, epsilon=0.01)) / k)
    avg_largeepsilon_recall = float(np.mean(compute_knn(true_nn_distances, run_distances, k, epsilon=0.1)) / k)
    avg_rel = float(np.mean(rel))
    qps = float(1/np.mean(query_times))


    # Data frame to store the query stats data
    df = pd.DataFrame({
        'recall': recall,
        'query_time': query_times,
        'rel': rel
    })
    if 'expansion' in dataset:
        difficulty_type = 'expansion'
    elif 'lrc' in dataset:
        difficulty_type = "lrc"
    else:
        difficulty_type = "lid"

    difficulty = re.findall("hard|middle|easy|diverse", dataset)[0]
    distance_type = re.findall("angular|euclidean", dataset)[0]
    dimensionality_measures = get_dimensionality_measures(
        data, distance_type)

    dataset = re.sub("-(hard|middle|easy|diverse)", "", dataset)
    dataset = re.sub("-(expansion|lrc|lid)", "", dataset)

    df['lrc'] = dimensionality_measures['lrc']
    df['lid'] = dimensionality_measures['lid']
    df['expansion'] = dimensionality_measures['expansion']

    dataset = dataset_map[dataset]
    algorithm = algo_map[algo]
    parameters = algo_name
    difficulty_type = difficulty_type
    difficulty = difficulty

    distcomps = int(metrics['distcomps']['function'](true_nn_distances, run_distances, query_times, None, run.attrs))
    build_time = float(run.attrs['build_time'])
    index_size = int(run.attrs.get("index_size", 0))

    # Do the insertion in the two tables in a transaction
    with conn:
        # Null is for the auto increment key
        # TODO: add the averaged values of the metrics
        cursor = conn.execute(
            """
            INSERT INTO main VALUES (
                NULL,
                :k,
                :dataset,
                :algorithm,
                :parameters,
                :difficulty_type,
                :difficulty,
                :qps,
                :avg_recall,
                :avg_epsilon_recall,
                :avg_largeepsilon_recall,
                :avg_rel,
                :distcomps,
                :build_time,
                :index_size,
                :queriessize
            )
            """,
            {
                "k": count,
                "dataset": dataset,
                "algorithm": algorithm,
                "parameters": parameters,
                "difficulty_type": difficulty_type,
                "difficulty": difficulty,
                "qps": qps,
                "avg_recall": avg_recall,
                "avg_epsilon_recall": avg_epsilon_recall,
                "avg_largeepsilon_recall": avg_largeepsilon_recall,
                "avg_rel": avg_rel,
                "distcomps": distcomps,
                "build_time": build_time,
                "index_size": index_size,
                "queriessize": index_size / qps
            }
        )
        # cursor = conn.execute(
        #     "INSERT INTO main VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        #     (dataset, algorithm, parameters, difficulty_type, difficulty, qps, avg_recall, avg_rel, distcomps))
        idx = cursor.lastrowid

        df['id'] = idx
        df.to_sql('query_stats', conn, if_exists='append', index=False)

    return df


def compute_all_metrics(true_nn_distances, run, properties, recompute=False):
    algo = properties["algo"]
    algo_name = properties["name"]
    print('--')
    print(algo_name)
    results = {}
    # cache to avoid access to hdf5 file
    run_distances = numpy.array(run["distances"])
    query_times = numpy.array(run['times'])
    if recompute and 'metrics' in run:
        del run['metrics']
    metrics_cache = get_or_create_metrics(run)

    for name, metric in metrics.items():
        v = metric["function"](true_nn_distances, run_distances, query_times, metrics_cache, run.attrs)
        results[name] = v
        if v:
            print('%s: %g' % (name, v))
    return (algo, algo_name, results)

def generate_n_colors(n):
    vs = numpy.linspace(0.4, 1.0, 7)
    colors = [(.9, .4, .4, 1.)]
    def euclidean(a, b):
        return sum((x-y)**2 for x, y in zip(a, b))
    while len(colors) < n:
        new_color = max(itertools.product(vs, vs, vs), key=lambda a: min(euclidean(a, b) for b in colors))
        colors.append(new_color + (1.,))
    return colors

def create_linestyles(unique_algorithms):
    colors = dict(zip(unique_algorithms, generate_n_colors(len(unique_algorithms))))
    linestyles = dict((algo, ['--', '-.', '-', ':'][i%4]) for i, algo in enumerate(unique_algorithms))
    markerstyles = dict((algo, ['+', '<', 'o', '*', 'x'][i%5]) for i, algo in enumerate(unique_algorithms))
    faded = dict((algo, (r, g, b, 0.3)) for algo, (r, g, b, a) in colors.items())
    return dict((algo, (colors[algo], faded[algo], linestyles[algo], markerstyles[algo])) for algo in unique_algorithms)

def get_up_down(metric):
    if metric["worst"] == float("inf"):
        return "down"
    return "up"

def get_left_right(metric):
    if metric["worst"] == float("inf"):
        return "left"
    return "right"

def get_plot_label(xm, ym):
    return "%(xlabel)s-%(ylabel)s tradeoff - %(updown)s and to the %(leftright)s is better" % {
            "xlabel" : xm["description"], "ylabel" : ym["description"], "updown" : get_up_down(ym), "leftright" : get_left_right(xm) }

