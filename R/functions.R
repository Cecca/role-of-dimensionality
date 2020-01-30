load_results <- function(csv_file) {
  read_csv(csv_file)
}

recode_datasets <- function(data) {
  data %>%
    mutate(dataset = dataset %>% str_remove_all("-lid") %>% str_remove_all("-expansion") %>% str_remove_all(".txt"))
}

add_difficulty <- function(detail_data) {
  pattern_get <- "(diverse|easy|middle|hard)"
  pattern_rm <- "-(diverse|easy|middle|hard)(-expansion)?"
  detail_data %>% 
    mutate(difficulty = str_extract(dataset, pattern_get),
           difficulty_type = if_else(str_ends(dataset, "expansion"),
                                       "expansion",
                                       "lid"),
           dataset = str_remove(dataset, pattern_rm))
}

recode_algos <- function(data) {
  data %>% 
    mutate(algorithm = recode_factor(algorithm,
      'annoy' = 'Annoy',
      'faiss-ivf' = 'FAI-IVF',
      'hnsw(faiss)' = 'HNSW',
      'NGT-onng' = 'ONNG',
      'puffinn' = "PUFFINN"
    ))
}

my_theme <- function() {
  theme(
    #text = element_text(size = rel(1)),
    axis.title = element_text(size = rel(0.65)),
    axis.text = element_text(size = rel(0.5)),
    strip.text = element_text(size = rel(0.65)),
    strip.background.y = element_rect(size=1)
  )
}

do_plot_ridges <- function(partial_data) {
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

do_plot_recall_vs_qps <- function(data) {
  dataset <- data %>% select(dataset) %>% distinct() %>% pull()
  frontier <- data %>% 
    group_by(dataset, algorithm) %>% 
    psel(high(qps) * high(`k-nn`))
  frontier %>% 
    ggplot(aes(x=`k-nn`, y=qps, color=algorithm)) +
    geom_point() +
    geom_line() +
    scale_y_log10() +
    labs(title=dataset) +
    #facet_wrap(vars(dataset)) +
    theme_bw()
}

do_scatter_distribution <- function(data) {
  dataset <- data %>% head(1) %>% select(dataset) %>% pull()
  correlation <- data %>% summarise(correlation = cor(lid, expansion)) %>% round(digits=3)
  scatter <- data %>% 
    sample_n(10000) %>% 
    ggplot(aes(x=lid, y=expansion)) +
    #stat_binhex() +
    geom_point(alpha=0.5) +
    stat_smooth() +
    labs(title=dataset,
         subtitle=str_c("Correlation: ", correlation),
         caption="The scatter plot shows only a sample of 10k points") +
    theme_bw() +
    theme(legend.position = 'bottom')
  exp_kde <- ggplot(data, aes(x=expansion)) +
    stat_density(alpha=0.5) +
    coord_flip() +
    theme_void()
  lid_kde <- ggplot(data, aes(x=lid)) +
    stat_density(alpha=0.5) +
    theme_void()
  p1 <- insert_xaxis_grob(scatter,
                          lid_kde,
                          position="top")
  p2 <- insert_yaxis_grob(p1,
                          exp_kde,
                          position="right")
  ggdraw(p2)
}

# Invoke with only one parameter varying and the others fixed, on a single dataset
interactive_distribution_plot <- function(algo_data) {
  algo_data <- algo_data %>% mutate(parameters = str_replace_all(parameters, "u?'", ""))
  algorithm <- algo_data %>% distinct(algorithm) %>% pull()
  dataset <- algo_data %>% distinct(dataset) %>% pull()
  difficulty <- algo_data %>% distinct(difficulty) %>% pull()
  
  averages <- algo_data %>%
    group_by(algorithm, dataset, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall)) %>% 
    psel(high(qps) * high(avg_recall))
  
  plot_data <- algo_data %>%
    inner_join(averages,
               by=c("algorithm" = "algorithm", "parameters" = "parameters",
                    "dataset" = "dataset")) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall))
  
  #sample_points <- sample_n(group_by(plot_data, parameters), 1000)
  #print(paste("Taken the sample points: ", nrow(sample_points)))
  
  ymin <- 1/max(algo_data$query_time)
  ymax <- 1/min(algo_data$query_time)
  
  binning_factor <- (ymax - ymin) / 20
  binning_factor <- 100
  sized_points <- plot_data %>% 
    sample_n(1000) %>% 
    mutate(binned_qps = floor((1/query_time)/binning_factor) * binning_factor) %>% 
    group_by(parameters, recall, binned_qps) %>% 
    mutate(bin_count = n()) %>% 
    ungroup()
  
  scatter <- ggplot(averages, aes(avg_recall, qps, data_id=parameters)) +
    geom_point_interactive(aes(tooltip=parameters)) +
    geom_point_interactive(data=sized_points,
                           mapping=aes(recall, binned_qps, data_id=parameters, size=bin_count),
                           alpha=0.0) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    guides(size=FALSE)+
    labs(title=str_interp("${dataset} ${difficulty} - ${algorithm}"),
         caption="Each point represents the average performance of a parameter configuration.
Hover on the points to see the distribution of the recall and queries per second for that parameter configuration.",
         x="recall",
         y="queries per second") +
    theme_bw()
  
  # compute density distribution only of those configurations whose average performance is on the
  # Pareto frontier
  density_data <- semi_join(algo_data, averages,
                            by=c("parameters")) %>% 
    group_by(parameters) %>% 
    sample_n(pmin(10000, n())) %>% 
    ungroup()
  
  kde_recall <- ggplot(density_data, aes(recall, group=parameters, data_id=parameters)) +
    geom_density_interactive(aes(y=..scaled..),
                             color=NA,
                             alpha=0) +
    scale_x_continuous(limits = c(0,1)) +
    theme_void()
  
  kde_qps <- ggplot(density_data, aes(1/query_time, group=parameters, data_id=parameters)) +
    geom_density_interactive(aes(y=..scaled..),
                             color=NA,
                             alpha=0) +
    coord_flip() +
    theme_void()
  
  p1 <- insert_xaxis_grob(scatter,
                        kde_recall,
                        position="top")
  p2 <- insert_yaxis_grob(p1,
                          kde_qps,
                          position="right")
  
  combined <- ggdraw(p2)
  girafe(ggobj = combined,
         options = list(
           opts_hover("fill:orange; opacity:0.5")
         ),
         width_svg = 8,
         height_svg = 4)
  
}
