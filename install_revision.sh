mkdir -p workloads

for ds in fashion-mnist-784-euclidean mnist-784-euclidean gnews-300-angular sift-128-euclidean glove-100-angular glove-2m-300-angular; do
    python3 create_dataset.py --dataset $ds
    for t in lid rc; do
        for k in 5 10 100; do
            echo Computing $t on $ds with k=$k
            python3 additional-scripts/compute-${t}.py data/${ds}.hdf5 $k > workloads/${ds}-${t}-${k}.txt
            echo Picking query sets for $t on $ds
            python3 additional-scripts/choose-queryset.py --${t} workloads/${ds}-${t}-${k}.txt > workloads/${ds}-${t}-${k}-queries.txt
            python3 additional-scripts/pick-queries.py --${t} data/${ds}.hdf5 workloads/${ds}-${t}-${k}-queries.txt
        done
    done
    for k in 5 10 50; do
        for kk in $(( $k * 2)) 100; do
            echo Computing expansion on $ds with k=$k and kk=$kk
            python3 additional-scripts/compute-expansion.py data/${ds}.hdf5 $k $kk > workloads/${ds}-expansion-${k}_${kk}.txt
            echo Picking query sets for expansion on $ds
            python3 additional-scripts/choose-queryset.py --expansion workloads/${ds}-expansion-${k}_${kk}.txt > workloads/${ds}-expansion-${k}_${kk}-queries.txt
            python3 additional-scripts/pick-queries.py --expansion data/${ds}.hdf5 workloads/${ds}-expansion-${k}_${kk}-queries.txt
        done
    done
done


