Benchmarking nearest neighbors
==============================

This is the version of http://github.com/erikbern/ann-benchmarks/ accompanying our SISAP 2019 paper _Benchmarking Nearest Neighbor Search: Influence of Local Intrinsic Dimensionality_. 
See the main repository for the benchmarking tool intended for use for a general audience. 


Install
=======

The only prerequisite is Python (tested with 3.6) and Docker.

1. Clone the repo.
2. Run `pip install -r requirements.txt`.
3. Run `python install.py` to build all the libraries inside Docker containers (this can take a while, like 10-30 minutes).

Running
=======

1. Run `python run.py` (this can take an extremely long time, potentially days)
2. Run `python plot.py` or `python create_website.py` to plot results.

You can customize the algorithms and datasets if you want to:

* Check that `algos.yaml` contains the parameter settings that you want to test
* To run experiments on SIFT, invoke `python run.py --dataset glove-100-angular`. See `python run.py --help` for more information on possible settings. Note that experiments can take a long time. 

Result processing
=================

First, you have to export the results: 
* `python data_export.py --output summarised.csv.bz2`
* `python data_export.py --output detail.parquet --detail`

Then you have to setup your `R` installation. Open an `R` shell and type `packrat::restore()`.
At this point you can run the analysis and plotting pipeline by just typing `make`

SISAP 2019 Changes
=====

See http://ann-benchmarks.com/sisap19/ for the evaluation including plots, preprocessed datasets, and raw results.

Generating the datasets described in the paper works as follows. (We use `glove-100-angular` as an example.)

- Run `python3 create_dataset.py --dataset glove-100-angular`. (This takes a long time, since it takes the whole data set as the query set.)
- Run `python3 compute-lid.py data/glove-100-angular.hdf5 > glove-100-angular-lid.txt` to compute estimates for the LID of every single query based on its 100-NN stored in `data/glove-100-angular.hdf5`. 
- Run `python3 choose-queryset.py glove-100-angular-lid.txt > glove-100-angular-queries.txt` to pick the queries to use for the easy, middle, hard, and diverse query set.
- Run `python3 pick-queries.py data/glove-100-angular.hdf5 glove-100-angular-queries.txt` to prepare hdf5 versions of these 4 datasets. 
- Run `python3 run.py --algorithm faiss-ivf --dataset glove-100-angular-diverse` (exchange algorithm and dataset accordingly) to run the experiments.
- Run `python3 plot.py --dataset glove-100-angular-diverse` to create a basic recall/QPS plot. (If your docker service runs as root, you might need to execute this script as root as well since it will write to the result files in `results/glove-100-angular-diverse`. Alternatively: Change owner of files in `results` to your local user.) 
- Or use `python3 data_export.py --output results.csv --detail` to generate a CSV file with all metrics that can be used for visualization through Python/pandas or R. (Again: Might need to run as root.)














Related Publication
==================

The following publication details design principles behind the benchmarking framework: 

- M. Aumüller, E. Bernhardsson, A. Faithfull:
[ANN-Benchmarks: A Benchmarking Tool for Approximate Nearest Neighbor Algorithms](http://www.itu.dk/people/maau/additional/sisap2017-preprint.pdf). SISAP 2017: 34-49
