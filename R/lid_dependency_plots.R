#!/usr/bin/env Rscript

args <- commandArgs(T)
data_file <- args[1]
output_directory <- args[2]
if (is.na(data_file)) {
  print("USAGE: ./lid_dependency_plots.R <data.csv> <output_directory>")
  stop("A data file is needed")
}
if (is.na(output_directory)) {
  print("USAGE: ./lid_dependency_plots.R <data.csv> <output_directory>")
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

data <- readr::read_csv('lid-dependency/new-diverse.csv') %>%
  rename(algorithm = algo, parameters = params) %>%
  mutate(recall = recall / 10.0) %>%
  mutate(algorithm = recode_factor(algorithm,
    'annoy' = 'Annoy',
    'faiss-ivf' = 'FAI-IVF',
    'hnsw(faiss)' = 'HNSW',
    'NGT-onng' = 'ONNG'
  )) %>%
  filter(str_detect(dataset, "diverse")) %>%
  filter(!str_detect(dataset, "sift")) %>%
  mutate(recall = recall / 10)

# Extract the tibbles relative to each dataset, with the relevant parameters 

faiss <- data %>% filter(algorithm == "FAI-IVF") %>%
  tidyr::extract(parameters, c("n_lists", "n_probe"), "FaissIVF\\(n_list=(\\d+), n_probe=(\\d+)\\)", remove=F)

hnsw <- data %>% filter(algorithm == "HNSW") %>%
  tidyr::extract(parameters, c("efConstruction", "M"), 
                 "faiss \\(\\{u'efConstruction': (\\d+), u'M': (\\d+)\\}\\)",
                 remove=F)

annoy <- data %>% filter(algorithm == "Annoy") %>%
  tidyr::extract(parameters, c("n_trees", "search_k"), "n_trees=(\\d+), search_k=(\\d+)", remove=F)

onng <- data %>% filter(algorithm == "ONNG") %>%
  tidyr::extract(parameters, 
                 c("edge", "outdegree", "indegree", "edge_size_for_search", "epsilon"), 
                 "ONNG-NGT\\((\\d+), (\\d+), (\\d+), (-?\\d+), (\\d+.\\d+)\\)", 
                 remove=F)


get_queryset <- function(dataset_name) {
  # TODO: The last row should not be part of the tibble
  lids <- read_delim(paste0("lid-dependency/",
                            str_replace(dataset_name, "-diverse(-2)?", ""),
                            "-lid.txt"), 
                     delim = " ", col_names = c("identifier","lid"))
  con <- file(paste0("lid-dependency/", dataset_name, ".txt"))
  diverse <- jsonlite::fromJSON(readLines(con)[2])
  close(con)
  # Offset by 1 the indices because the file contains 0-based indices, the tibbles have 1 based indices
  result_lids <- lids %>% slice(diverse + 1)
  result_lids
}

get_plot_dataset <- function(single_conf_data, dataset_name) {
  dataset <- single_conf_data %>% select(dataset) %>% pull()
  dataset_name <- dataset[1]
  print(paste(dataset_name, single_conf_data %>% select(parameters) %>% distinct() %>% pull()))
  queryset <- get_queryset(dataset_name)
  if (nrow(single_conf_data) > nrow(queryset)) {
    print("WARNING: Cutting after the first 5000 rows")
  }
  single_conf_data %>%
    slice(1:nrow(queryset)) %>%
    bind_cols(queryset)
}

joint_plot <- function(plot_data) {
  empty <- ggplot()+geom_point(aes(1,1), colour="white")+
         theme(axis.ticks=element_blank(), 
               panel.background=element_blank(), 
               axis.text.x=element_blank(), axis.text.y=element_blank(),           
               axis.title.x=element_blank(), axis.title.y=element_blank())
  margin_grid_fixes <- theme(
    axis.ticks = element_blank(), 
    axis.line = element_blank(),
    panel.background=element_blank(), 
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    plot.margin = unit(c(0,0,0,0), 'cm')
  )
  bw <- plot_data %>% select(lid) %>% max() / 10.0
  central <- ggplot(plot_data, aes(y=recall, x=lid)) +
    geom_bin2d(binwidth=c(bw, 0.1), show.legend = F) + 
    coord_cartesian(ylim = c(0,1)) +
    scale_fill_gradient(low='white', high='black') +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.margin = unit(c(0,0,0,0), 'cm'),
          panel.border = element_blank(),
          axis.line = element_line(color='black'),
          panel.grid = element_blank())
  lid_dist <- ggplot(plot_data, aes(x=lid)) +
    geom_density(aes(x=lid, y=..ndensity..),
                 fill='gray',
                 alpha=0.3,
                 color='black') +
    xlab("") +
    coord_cartesian(xlim = c(0, plot_data %>% select(lid) %>% max())) +
    theme_bw() +
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_text(colour = "white"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_text(colour = "white")
    ) + margin_grid_fixes
  recall_dist <- ggplot(plot_data, aes(x=recall)) +
    geom_density(aes(x=recall, y=..ndensity..),
                 fill='gray', 
                 alpha=0.3,
                 color='black') +
    xlab("") +
    coord_flip(xlim = c(0,1)) +
    theme_bw() +
    theme(
      axis.text.x=element_text(colour = "white"),
      axis.text.y=element_blank(), 
      axis.title.x=element_text(colour = "white"), 
      axis.title.y=element_blank()
    ) + margin_grid_fixes
  grid.arrange(lid_dist, empty, central, recall_dist, 
               top = title,
               ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
}

# Annoy
datasets <- annoy %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- annoy %>% filter(dataset == d)
  for (trees in to_plot %>% select(n_trees) %>% distinct() %>% pull()) {
    inner <- to_plot %>% filter(n_trees == trees)
    for (k in inner %>% select(search_k) %>% distinct() %>% pull()) {
      file_name <- paste("Annoy", d, "trees", trees, "k", k, sep="-")
      file_name <- paste0(file_name, ".png")
      single_plot_data <- inner %>% filter(search_k == k) 
      png(filename=file.path(output_directory, file_name), 
          res=300, units = 'cm',
          width=12, height=12)
      p <- single_plot_data %>% get_plot_dataset() %>% joint_plot()
      print(p)
      dev.off()
    }
  }
}

# HNSW
datasets <- hnsw %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- hnsw %>% filter(dataset == d)
  for (ec in to_plot %>% select(efConstruction) %>% distinct() %>% pull()) {
    inner <- to_plot %>% filter(efConstruction == ec) 
    for (m_value in inner %>% select(M) %>% distinct() %>% pull()) {
      file_name <- paste("HNSW", d, "efConstruction", ec, sep="-")
      file_name <- paste0(file_name, ".png")
      single_plot_data <- to_plot %>% filter(M == m_value) 
      png(filename=file.path(output_directory, file_name), 
          res=300, units = 'cm',
          width=12, height=12)
      p <- single_plot_data %>% get_plot_dataset() %>% joint_plot()
      print(p)
      dev.off()
    }
  }
}

# FAISS
datasets <- faiss %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- faiss %>% filter(dataset == d)
  for (nl in to_plot %>% select(n_lists) %>% distinct() %>% pull()) {
    for (np in to_plot %>% select(n_probe) %>% distinct() %>% pull()) {
      file_name <- paste("FAI-IVF", d, "n_lists", nl, "n_probe", np, sep="-")
      file_name <- paste0(file_name, ".png")
      print(paste("FAISS", d, "n_lists", nl, "n_probe", np))
      single_plot_data <- to_plot %>% filter(n_lists == nl) %>% filter(n_probe == np)
      if (nrow(single_plot_data) > 0) {
        png(filename=file.path(output_directory, file_name), 
            res=300, units = 'cm',
            width=12, height=12)
        p <- single_plot_data %>% get_plot_dataset() %>% joint_plot()
        print(p)
        dev.off()
      }
    }
  }
}

# ONNG
datasets <- onng %>% select(dataset) %>% distinct()
for (d in datasets %>% pull()) {
  to_plot <- onng %>% filter(dataset == d)
  for (en in to_plot %>% select(edge) %>% distinct() %>% pull()) {
    for (id in to_plot %>% select(indegree) %>% distinct() %>% pull()) {
      for (od in to_plot %>% select(outdegree) %>% distinct() %>% pull()) {
        for (esfs in to_plot %>% select(edge_size_for_search) %>% distinct() %>% pull()) {
          for (eps in to_plot %>% select(epsilon) %>% distinct() %>% pull()) {
            file_name <- paste("ONNG", d, 
                               "edge", en,
                               "outdegree", od,
                               "indegree", id,
                               "esfs", esfs,
                               "epsilon", eps,
                               p, sep="-")
            file_name <- paste0(file_name, ".png")
            single_plot_data <- to_plot %>% 
              filter(edge == en)  %>%
              filter(indegree == id)  %>%
              filter(outdegree == od)  %>%
              filter(edge_size_for_search == esfs)  %>%
              filter(epsilon == eps)
            png(filename=file.path(output_directory, file_name), 
                res=300, units = 'cm',
                width=12, height=12)
            p <- single_plot_data %>% get_plot_dataset() %>% joint_plot()
            print(p)
            dev.off()
          }
        }
      }
    }
  }
}

## Paper plots
# side <- 2.25
# tikz(file="onng-glove-lid-recall.tex", width=side, height=side)
# p <- onng %>%
#   filter(dataset == "glove-100-angular-diverse") %>%
#   filter(parameters == "ONNG-NGT(100, 10, 120, -2, 1.020)") %>%
#   get_plot_dataset() %>%
#   joint_plot(title = 'ONNG')
# print(p)
# dev.off()
# 
# tikz(file="ivf-glove-lid-recall.tex", width=side, height=side)
# faiss %>%
#   filter(dataset == "glove-100-angular-diverse") %>%
#   filter(parameters == "FaissIVF(n_list=4096, n_probe=5)") %>%
#   get_plot_dataset() %>%
#   joint_plot(title = 'IVF') %>%
#   print()
# dev.off()
