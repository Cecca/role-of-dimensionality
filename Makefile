plots:
	Rscript make.R

copy: plots
	cp imgs/*.{tex,pdf} ${HOME}/Work/ann-benchmarks-sisap/imgs/
	cp imgs/GLOVE-scores.png ${HOME}/Work/ann-benchmarks-sisap/imgs/


