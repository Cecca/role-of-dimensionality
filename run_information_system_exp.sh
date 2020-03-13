for algo in annoy hnsw\(faiss\) NGT-onng faiss-ivf puffinn; do
    for ds in mnist-784-euclidean fashion-mnist-784-euclidean gnews-300-angular glove-100-angular glove-2m-300-angular sift-128-euclidean; do
        for d in easy middle hard diverse; do
            for t in lid lrc expansion; do
                python3 run.py --algo $algo --dataset ${ds}-${d}-${t} --runs 1 --force
            done
        done
    done
done
