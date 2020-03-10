mkdir -p workloads

for ds in mnist-784-euclidean; do
    python3 create_dataset.py --dataset $ds
    python3 additional-scripts/compute-lid.py data/${ds}.hdf5 > workloads/${ds}-lid.txt
    python3 additional-scripts/compute-rc.py data/${ds}.hdf5 > workloads/${ds}-lrc.txt
    python3 additional-scripts/compute-expansion.py data/${ds}.hdf5 > workloads/${ds}-expansion.txt
    python3 additional-scripts/choose-queryset.py workloads/${ds}-lid.txt > workloads/${ds}-lid-queries.txt
    python3 additional-scripts/choose-queryset.py --expansion workloads/${ds}-expansion.txt > workloads/${ds}-expansion-queries.txt
    python3 additional-scripts/choose-queryset.py --contrast workloads/${ds}-lrc.txt > workloads/${ds}-lrc-queries.txt
    python3 additional-scripts/pick-queries.py --contrast data/${ds}.hdf5 workloads/${ds}-lrc-queries.txt 
    python3 additional-scripts/pick-queries.py --expansion data/${ds}.hdf5 workloads/${ds}-expansion-queries.txt 
    python3 additional-scripts/pick-queries.py data/${ds}.hdf5 workloads/${ds}-lid-queries.txt 
done



