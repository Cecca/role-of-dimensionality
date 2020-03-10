plots: report.Rmd R/plan.R R/functions.R
	LANGUAGE=en Rscript make.R
	
report.Rmd: report.template R/render_template.R
	Rscript R/render_template.R

copy: plots
	cp imgs/*.{tex,pdf} ${HOME}/Work/ann-benchmarks-sisap/imgs/
	cp imgs/GLOVE-scores.png ${HOME}/Work/ann-benchmarks-sisap/imgs/

install: 
	bash install.sh

