source("R/packages.R")
library(whisker)

template <- readLines("report.template")

data <- list(
  datasets = list(
    list(dataset="GLOVE", dataset_esc="GLOVE"), 
    list(dataset="GLOVE-2M", dataset_esc="GLOVE.2M"),
    list(dataset="GNEWS", dataset_esc="GNEWS"),
    list(dataset="SIFT", dataset_esc="SIFT"),
    list(dataset="MNIST", dataset_esc="MNIST"),
    list(dataset="Fashion-MNIST", dataset_esc="Fashion.MNIST")
  ),
  algorithms = list(
    list(algorithm="PUFFINN", algorithm_esc="PUFFINN"),
    list(algorithm="ONNG", algorithm_esc="ONNG"),
    list(algorithm="HNSW", algorithm_esc="HNSW"),
    list(algorithm="FAI-IVF", algorithm_esc="FAI.IVF"),
    list(algorithm="Annoy", algorithm_esc="Annoy")
  ),
  difficulties = list(
    list(difficulty="easy"),
    list(difficulty="middle"),
    list(difficulty="hard"),
    list(difficulty="diverse")
  ),
  types = list(
    list(type="lid"),
    list(type="expansion")
  )
)

rendered <- whisker.render(template, data)
writeLines(rendered, "report.Rmd")


