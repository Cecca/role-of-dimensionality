mkdir -p workloads

for ds in mnist-784-euclidean; do
    python3 create_dataset.py --dataset $ds
    for t in lid rc expansion; do
        python3 additional-scripts/compute-${t}.py data/${ds}.hdf5 > workloads/${ds}-${t}.txt
        python3 additional-scripts/choose-queryset.py --${t} workloads/${ds}-${t}.txt > workloads/${ds}-${t}-queries.txt
        python3 additional-scripts/pick-queries.py --${t} data/${ds}.hdf5 workloads/${ds}-${t}-queries.txt 
    done
done



