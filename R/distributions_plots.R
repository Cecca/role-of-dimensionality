#!/usr/bin/env Rscript

args <- commandArgs(T)
data_file <- args[1]
output_directory <- args[2]
if (is.na(data_file)) {
  print("USAGE: ./distribution_plots.R <data.csv> <output_directory>")
  stop("A data file is needed")
}
if (is.na(output_directory)) {
  print("USAGE: ./distribution_plots.R <data.csv> <output_directory>")
  stop("The output directory is needed")
}
if (!dir.exists(output_directory)) {
  print("Creating output directory")
  dir.create(output_directory)
}

library(tidyverse)
library(ggridges)
library(stringi)
library(tikzDevice)
library(gridExtra)

data <- readr::read_csv('results-detailed-all.csv.bz2') %>%
  rename(algorithm = algo, parameters = params) %>%
  mutate(algorithm = recode_factor(algorithm,
    'annoy' = 'Annoy',
    'faiss-ivf' = 'FAI-IVF',
    'hnsw(faiss)' = 'HNSW',
    'NGT-onng' = 'ONNG'
  )) %>%
  mutate(recall = recall / 10)

# Get the tibbles for the single datasets, extracting the correct parameters
faiss <- data %>% filter(algorithm == "FAI-IVF") %>%
  tidyr::extract(parameters, c("n_lists", "n_probe"), "FaissIVF\\(n_list=(\\d+), n_probe=(\\d+)\\)", remove=F)

hnsw <- data %>% filter(algorithm == "HNSW") %>%
  tidyr::extract(parameters, c("efConstruction", "M"), 
                 "faiss \\(\\{u'efConstruction': (\\d+), u'M': (\\d+)\\}\\)",
                 remove=F)

annoy <- data %>% filter(algorithm == "Annoy") %>%
  tidyr::extract(parameters, c("n_trees", "search_k"), "n_trees=(\\d+), search_k=(\\d+)", remove=F)

onng <- data %>% filter(algorithm == "ONNG") %>%
  tidyr::extract(parameters, c("param"), 
                 "ONNG-NGT\\(\\d+, \\d+, \\d+, (-?\\d+), \\d+.\\d+\\)", remove=F)


thm <- theme(
  #text = element_text(size = rel(1)),
  axis.title = element_text(size = rel(0.65)),
  axis.text = element_text(size = rel(0.5)),
  strip.text = element_text(size = rel(0.65)),
  strip.background.y = element_rect(size=1)
  )

plot_ridges <- function(partial_data) {
  print(paste("Plotting", file_name))
  averages <- partial_data %>%
    group_by(algorithm, dataset, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time)
  
  plot_data <- partial_data %>%
    inner_join(averages,
               by=c("algorithm" = "algorithm", "parameters" = "parameters",
                    "dataset" = "dataset"))
  p1 <- ggplot(plot_data, aes(x=recall, y=qps, group=parameters)) +
    geom_density_ridges(alpha=0.4, scale=15, size=0.1, show.legend = F) +
    geom_point(data=averages, 
               mapping=aes(x=avg_recall), 
               size=0.3,
               show.legend = F) +
    scale_y_log10() + 
    ylab("QPS (1/s)") +
    xlab("recall") +
    ggtitle("Recall")
    #theme_bw() +
    theme(legend.position = "top")
  p2 <- ggplot(plot_data, aes(x=queries_per_second, y=avg_recall, group=parameters)) +
    geom_density_ridges(alpha=0.4, scale=15, size=0.1, show.legend = F) +
    geom_point(data=averages, 
               mapping=aes(x=qps), 
               size=0.3,
               show.legend = F) +
    scale_x_log10() + 
    xlab("1 / query time") +
    ylab("recall") +
    ggtitle("Queries per second") +
    #theme_bw() +
    theme(legend.position = "top")
  grid.arrange(p1, p2, ncol=2)
}

datasets <- annoy %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- annoy %>% filter(dataset == d)
  for (trees in to_plot %>% select(n_trees) %>% distinct() %>% pull()) {
    file_name <- paste("Annoy", d, "trees", trees, sep="-")
    file_name <- paste0(file_name, ".png")
    single_plot_data <- to_plot %>% filter(n_trees == trees) 
    p <- plot_ridges(single_plot_data)
    ggsave(p, filename=file.path(output_directory, file_name), 
           dpi=300, device="png",
           width=6, height=3)
  }
}

datasets <- hnsw %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- hnsw %>% filter(dataset == d)
  for (ec in to_plot %>% select(efConstruction) %>% distinct() %>% pull()) {
    file_name <- paste("HNSW", d, "efConstruction", ec, sep="-")
    file_name <- paste0(file_name, ".png")
    single_plot_data <- to_plot %>% filter(efConstruction == ec) 
    p <- plot_ridges(single_plot_data)
    ggsave(p, filename=file.path(output_directory, file_name), 
           dpi=300, device="png",
           width=6, height=3)
  }
}

datasets <- faiss %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- faiss %>% filter(dataset == d)
  for (nl in to_plot %>% select(n_lists) %>% distinct() %>% pull()) {
    file_name <- paste("FAI-IVF", d, "n_lists", nl, sep="-")
    file_name <- paste0(file_name, ".png")
    single_plot_data <- to_plot %>% filter(n_lists == nl) 
    p <- plot_ridges(single_plot_data)
    ggsave(p, filename=file.path(output_directory, file_name), 
           dpi=300, device="png",
           width=6, height=3)
  }
}

datasets <- onng %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- onng %>% filter(dataset == d)
  for (p in to_plot %>% select(param) %>% distinct() %>% pull()) {
    file_name <- paste("ONNG", d, "param", p, sep="-")
    file_name <- paste0(file_name, ".png")
    single_plot_data <- to_plot %>% filter(param == p) 
    print(nrow(single_plot_data))
    p <- plot_ridges(single_plot_data)
    ggsave(p, filename=file.path(output_directory, file_name), 
           dpi=300, device="png",
           width=6, height=3)
  }
}
