mkdir -p workloads

for ds in mnist-784-euclidean fashion-mnist-784-euclidean glove-100-angular glove-2m-300-angular gnews-300-angular sift-128-euclidean; do
    python3 create_dataset.py --dataset $ds
    for t in lid rc expansion; do
        echo Computing $t on $ds
        python3 additional-scripts/compute-${t}.py data/${ds}.hdf5 > workloads/${ds}-${t}.txt
        echo Picking query sets for $t on $ds
        python3 additional-scripts/choose-queryset.py --${t} workloads/${ds}-${t}.txt > workloads/${ds}-${t}-queries.txt
        python3 additional-scripts/pick-queries.py --${t} data/${ds}.hdf5 workloads/${ds}-${t}-queries.txt 
    done
done



