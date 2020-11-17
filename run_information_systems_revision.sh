for algo in annoy; do #hnsw\(faiss\) NGT-onng faiss-ivf puffinn; do
    for ds in data/*; do
        ds=$(basename $ds .hdf5)
        c=`awk -F"-" '{print NF-1}' <<< $ds`
        if [[ $c -gt 3 ]]; then
            python3 run.py --algo $algo --dataset $ds --runs 1 --force &
        fi
    done
done
