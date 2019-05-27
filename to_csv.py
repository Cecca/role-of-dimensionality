import pandas as pd
import sys
import os
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import argparse
import bz2

from ann_benchmarks.datasets import get_dataset
from ann_benchmarks.algorithms.definitions import get_definitions
from ann_benchmarks.plotting.metrics import all_metrics as metrics
from ann_benchmarks.plotting.utils  import get_plot_label, compute_metrics_all_runs, compute_metrics, create_linestyles, create_pointset, runs_to_dataframe
from ann_benchmarks.results import store_results, load_all_results, get_unique_algorithms, get_algorithm_name

datasets = [
    'fashion-mnist-784-euclidean-diverse',
    'mnist-784-euclidean-diverse',
    'glove-100-angular-diverse',
    'glove-2m-300-angular-diverse',
    'gnews-300-angular-diverse',
    'mnist-784-euclidean-diverse',
    'sift-128-euclidean-diverse',
#    'sift-128-euclidean-diverse-2',
    'fashion-mnist-784-euclidean-easy',
    'fashion-mnist-784-euclidean-hard',
    'fashion-mnist-784-euclidean-middle',
    'glove-2m-300-angular-easy',
    'glove-2m-300-angular-middle',
    'glove-2m-300-angular-hard',
    'glove-100-angular-easy',
    'glove-100-angular-hard',
    'glove-100-angular-middle',
    #'glove-100-angular1',
    # 'glove-100-angular2',
    # 'glove-100-angular3',
    # 'glove-100-angular4',
    # 'glove-100-angular5',
    # 'glove-100-angular6',
    # 'glove-100-angular7',
    # 'glove-100-angular8',
    # 'glove-100-angular9',
    # 'glove-100-angular10',
    'mnist-784-euclidean-easy',
    'mnist-784-euclidean-hard',
    'mnist-784-euclidean-middle',
    # 'random-xs-20-euclidean',
    'sift-128-euclidean-diverse',
    'sift-128-euclidean-diverse-2',
    'sift-128-euclidean-easy',
    'sift-128-euclidean-hard',
    'sift-128-euclidean-middle',
]

# datasets = [p[:-len('.hdf5')] for p in os.listdir('data') if p.endswith('.hdf5')]

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--count',
        default=10)
    parser.add_argument(
        '--definitions',
        metavar='FILE',
        help='load algorithm definitions from FILE',
        default='algos.yaml')
    parser.add_argument(
        '--limit',
        default=-1)
    parser.add_argument(
        '--batch',
        help='Plot runs in batch mode',
        action='store_true')
    parser.add_argument(
        '--output',
        help='Path to the output csv file')
    parser.add_argument(
        '--recompute',
        action='store_true',
        help='Path to the output csv file')
    parser.add_argument(
        '--detail',
        action='store_true',
        help='Output the detailed information about each single query')
    args = parser.parse_args()

    count = int(args.count)
    dfs = []
    for dataset_name in datasets:
        print("Looking at dataset", dataset_name)
        dataset = get_dataset(dataset_name)
        unique_algorithms = get_unique_algorithms()
        results = load_all_results(dataset_name, count, True, args.batch)
        if args.detail:
            df = runs_to_dataframe(list(dataset["distances"]), results, args.recompute)
            dfs.append(df)
        else:
            results = compute_metrics_all_runs(list(dataset["distances"]), results, args.recompute)
            dfs.append(pd.DataFrame(results))
    data = pd.concat(dfs)
    data.to_csv(args.output, index=False, compression='bz2')

