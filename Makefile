plots: R/plan.R R/functions.R
	LANGUAGE=en Rscript make.R
	
report.Rmd: report.template R/render_template.R
	Rscript R/render_template.R

copy: plots
	cp imgs/*.{tex,pdf} ${HOME}/Work/ann-benchmarks-sisap/imgs/
	cp imgs/GLOVE-scores.png ${HOME}/Work/ann-benchmarks-sisap/imgs/

install: data/mnist-784-euclidean.hdf5 data/fashion-mnist-784-euclidean.hdf5 data/glove-100-angular.hdf5 data/glove-2m-300-angular.hdf5 data/gnews-300-angular.hdf5 data/sift-128-euclidean.hdf5
	bash install.sh

run:
	bash run_information_system_exp.sh

