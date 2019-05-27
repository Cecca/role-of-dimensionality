from __future__ import absolute_import

import numpy as np
import pandas as pd
import os, itertools, json, numpy, pickle
from ann_benchmarks.plotting.metrics import all_metrics as metrics
import matplotlib.pyplot as plt


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


def compute_metrics_all_runs(true_nn_distances, res, recompute=False):
    for i, (properties, run) in enumerate(res):
        algo = properties['algo']
        algo_name = properties['name']
        # cache distances to avoid access to hdf5 file
        print('Load distances and times')
        run_distances = np.array(run['distances'])
        query_times = np.array(run['times'])
        print('... done')
        if recompute and 'metrics' in run:
            print('Recomputing metrics, clearing cache')
            del run['metrics']
        metrics_cache = get_or_create_metrics(run)
        run_result = {
            'algorithm': algo,
            'parameters': algo_name,
            'dataset': properties['dataset'],
            'count': properties['count']
        }
        for name, metric in metrics.items():
            v = metric["function"](true_nn_distances, run_distances, query_times, metrics_cache, properties)
            run_result[name] = v
        yield run_result


def run_to_dataframe(true_nn_distances, run, properties, recompute=False):
    algo = properties["algo"]
    algo_name = properties["name"]
    dataset = properties['dataset']
    # cache to avoid access to hdf5 file
    run_distances = numpy.array(run["distances"])
    query_times = numpy.array(run['times'])
    if recompute and 'metrics' in run:
        print('deleting cached metrics')
        del run['metrics']
    metrics_cache = get_or_create_metrics(run)
    # cache the knn recall (if needed)
    metrics['k-nn']['function'](true_nn_distances, run_distances, query_times, metrics_cache, run.attrs)
    recalls = metrics_cache['knn']['recalls']
    df = pd.DataFrame({
        'recall': recalls,
        'query_time': query_times,
    })
    df['queries_per_second'] = 1.0/df['query_time']
    df['dataset'] = dataset
    df['algo'] = algo
    df['params'] = algo_name
    return df


def runs_to_dataframe(true_nn_distances, res, recompute=False):
    dfs = []
    for i, (properties, run) in enumerate(res):
        dfs.append(run_to_dataframe(true_nn_distances, run, properties, recompute))
    return pd.concat(dfs)


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

