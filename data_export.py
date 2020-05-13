import pandas as pd
import sys
import os
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import argparse
import bz2
import pyarrow as pa
import pyarrow.parquet as pq

from ann_benchmarks.datasets import get_dataset
from ann_benchmarks.algorithms.definitions import get_definitions
from ann_benchmarks.plotting.metrics import all_metrics as metrics
from ann_benchmarks.plotting.utils  import get_plot_label, compute_metrics_all_runs, compute_metrics, create_linestyles, create_pointset, runs_to_dataframe
from ann_benchmarks.results import store_results, load_all_results, get_unique_algorithms, get_algorithm_name

datasets = [
    # LRC
    'fashion-mnist-784-euclidean-diverse-lrc',
    'fashion-mnist-784-euclidean-easy-lrc',
    'fashion-mnist-784-euclidean-hard-lrc',
    'fashion-mnist-784-euclidean-middle-lrc',
    'glove-100-angular-diverse-lrc',
    'glove-100-angular-easy-lrc',
    'glove-100-angular-hard-lrc',
    'glove-100-angular-middle-lrc',
    'glove-2m-300-angular-diverse-lrc',
    'glove-2m-300-angular-easy-lrc',
    'glove-2m-300-angular-hard-lrc',
    'glove-2m-300-angular-middle-lrc',
    'gnews-300-angular-diverse-lrc',
    'gnews-300-angular-easy-lrc',
    'gnews-300-angular-hard-lrc',
    'gnews-300-angular-middle-lrc',
    'mnist-784-euclidean-diverse-lrc',
    'mnist-784-euclidean-easy-lrc',
    'mnist-784-euclidean-hard-lrc',
    'mnist-784-euclidean-middle-lrc',
    'sift-128-euclidean-diverse-lrc',
    'sift-128-euclidean-easy-lrc',
    'sift-128-euclidean-hard-lrc',
    'sift-128-euclidean-middle-lrc',
    # Expansion
    'fashion-mnist-784-euclidean-diverse-expansion',
    'fashion-mnist-784-euclidean-easy-expansion',
    'fashion-mnist-784-euclidean-hard-expansion',
    'fashion-mnist-784-euclidean-middle-expansion',
    'glove-100-angular-diverse-expansion',
    'glove-100-angular-easy-expansion',
    'glove-100-angular-hard-expansion',
    'glove-100-angular-middle-expansion',
    'glove-2m-300-angular-diverse-expansion',
    'glove-2m-300-angular-easy-expansion',
    'glove-2m-300-angular-hard-expansion',
    'glove-2m-300-angular-middle-expansion',
    'gnews-300-angular-diverse-expansion',
    'gnews-300-angular-easy-expansion',
    'gnews-300-angular-hard-expansion',
    'gnews-300-angular-middle-expansion',
    'mnist-784-euclidean-diverse-expansion',
    'mnist-784-euclidean-easy-expansion',
    'mnist-784-euclidean-hard-expansion',
    'mnist-784-euclidean-middle-expansion',
    'sift-128-euclidean-diverse-expansion',
    'sift-128-euclidean-easy-expansion',
    'sift-128-euclidean-hard-expansion',
    'sift-128-euclidean-middle-expansion',
    # LID
    'fashion-mnist-784-euclidean-diverse-lid',
    'fashion-mnist-784-euclidean-easy-lid',
    'fashion-mnist-784-euclidean-hard-lid',
    'fashion-mnist-784-euclidean-middle-lid',
    'glove-100-angular-diverse-lid',
    'glove-100-angular-easy-lid',
    'glove-100-angular-hard-lid',
    'glove-100-angular-middle-lid',
    'glove-2m-300-angular-diverse-lid',
    'glove-2m-300-angular-easy-lid',
    'glove-2m-300-angular-hard-lid',
    'glove-2m-300-angular-middle-lid',
    'gnews-300-angular-diverse-lid',
    'gnews-300-angular-easy-lid',
    'gnews-300-angular-hard-lid',
    'gnews-300-angular-middle-lid',
    'mnist-784-euclidean-diverse-lid',
    'mnist-784-euclidean-easy-lid',
    'mnist-784-euclidean-hard-lid',
    'mnist-784-euclidean-middle-lid',
    'sift-128-euclidean-diverse-lid',
    'sift-128-euclidean-easy-lid',
    'sift-128-euclidean-hard-lid',
    'sift-128-euclidean-middle-lid'
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

    is_first = True
    pqwriter = None
    for dataset_name in datasets:
        print("Looking at dataset", dataset_name)
        dataset = get_dataset(dataset_name)
        unique_algorithms = get_unique_algorithms()
        results = load_all_results(dataset_name, count, True, args.batch)
        if args.detail:
            # df = runs_to_dataframe(list(dataset["distances"]), results, args.recompute)
            df = runs_to_dataframe(dataset, results, args.recompute)
            if df is not None:
                table = pa.Table.from_pandas(df)
                if is_first:
                    pqwriter = pq.ParquetWriter(args.output, table.schema)
                    is_first = False
                pqwriter.write_table(table)
            else:
                print("WARNING: problems getting results for", dataset_name)
            ## Directly write the data frame, appending to the file
            #df.to_csv(args.output, mode="w" if is_first else "a",
            #          header=is_first, index=False
            #          #, compression='bz2'
            #          )
        else:
            results = compute_metrics_all_runs(dataset, results, args.recompute)
            dfs.append(pd.DataFrame(results))
    if len(dfs) > 0:
        data = pd.concat(dfs)
        data.to_csv(args.output, index=False, compression='bz2')
    if pqwriter is not None:
        pqwriter.close()

