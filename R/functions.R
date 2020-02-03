load_results <- function(csv_file) {
  read_csv(csv_file)
}

recode_datasets <- function(data) {
  data %>%
    mutate(dataset = dataset %>% str_remove_all("-lid") %>% str_remove_all("-expansion") %>% str_remove_all(".txt")) %>% 
    mutate(dataset = fct_recode(dataset,
      "GLOVE-2M" = "glove-2m-300-angular",
      "GNEWS" = "gnews-300-angular",
      "GLOVE" = "glove-100-angular",
      "SIFT" = "sift-128-euclidean",
      "Fashion-MNIST" = "fashion-mnist-784-euclidean",
      "MNIST" = "mnist-784-euclidean"
    ))
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
    labs(x="Local Intrinsic Dimensionality",
         y="Expansion") +
    #labs(title=dataset,
    #     subtitle=str_c("Correlation: ", correlation),
    #     caption="The scatter plot shows only a sample of 10k points") +
    theme_bw() +
    theme(legend.position = 'bottom')
  #exp_kde <- ggplot(data, aes(x=expansion)) +
  #  stat_density(alpha=0.5) +
  #  scale_x_log10() +
  #  coord_flip() +
  #  theme_void()
  #lid_kde <- ggplot(data, aes(x=lid)) +
  #  stat_density(alpha=0.5) +
  #  theme_void()
  #p1 <- insert_xaxis_grob(scatter,
  #                        lid_kde,
  #                        position="top")
  #p2 <- insert_yaxis_grob(p1,
  #                        exp_kde,
  #                        position="right")
  ##ggdraw(p2)
  scatter
}

static_ridges_plot_recall <- function(algo_data) {
  averages <- algo_data %>%
    group_by(algorithm, dataset, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall)) %>% 
    psel(high(qps) * high(avg_recall))
  
  all_points <- inner_join(algo_data, averages)
  
  ggplot(all_points, aes(x=recall, y=qps, group=parameters)) +
    geom_density_ridges(alpha=0.4, scale=15, size=0.1, show.legend = F) +
    geom_point(data=averages, 
               mapping=aes(x=avg_recall), 
               size=0.3,
               show.legend = F) +
    scale_y_log10() +
    facet_wrap(vars(algorithm), ncol=5) +
    labs(y="QPS (1/s)", 
         x="recall") +
    theme_bw()
}
  
static_ridges_plot_qps <- function(algo_data) {
  averages <- algo_data %>%
    group_by(algorithm, dataset, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall)) %>% 
    psel(high(qps) * high(avg_recall))
  
  all_points <- inner_join(algo_data, averages)
  
  ggplot(all_points, aes(x=1/avg_time, y=avg_recall, group=parameters)) +
    geom_density_ridges(alpha=0.4, scale=15, size=0.1, show.legend = F) +
    geom_point(data=averages, 
               mapping=aes(x=qps), 
               size=0.3,
               show.legend = F) +
    scale_x_log10() + 
    facet_wrap(vars(algorithm), ncol=5, scales="free_x") +
    labs(x="QPS (1/s)", 
         y="recall") +
    theme_bw()
  
}

static_distribution_plot <- function(algo_data, xlab=TRUE, ylab=TRUE) {
  algorithm <- algo_data %>% distinct(algorithm) %>% pull()
  averages <- algo_data %>%
    group_by(algorithm, dataset, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall)) %>% 
    psel(high(qps) * high(avg_recall))
  
  ymin <- 1/max(algo_data$query_time)
  ymax <- 1/min(algo_data$query_time)
  
  display_config <- bind_rows(
    averages %>% slice(which.max(qps)),
    averages %>% filter(avg_recall >= 0.6) %>% slice(which.max(qps)),
    averages %>% filter(avg_recall >= 0.9) %>% slice(which.max(qps))
  ) %>% 
    ungroup() %>% 
    mutate(parameters_id = factor(rank(qps)))
  
  params_ids <- display_config %>% 
    select(parameters, parameters_id)
  print(params_ids)
  
  scale_colors <- function() {
    colors <- RColorBrewer::brewer.pal(3, "Dark2")
    names(colors) <- c(1,2,3)
    scale_fill_manual(values = colors,
                      aesthetics = c("fill", "color"))
  }
  
  # Select only the three parameter configurations we are going to display
  all_points <- semi_join(algo_data, display_config)
  
  binning_factor <- 100
  sized_points <- all_points %>% 
    group_by(parameters) %>% 
    sample_n(500) %>% 
    mutate(binned_qps = floor((1/query_time)/binning_factor) * binning_factor) %>% 
    group_by(parameters, recall, binned_qps) %>% 
    summarise(bin_count = n()) %>% 
    ungroup() %>% 
    inner_join(params_ids)
  print(count(sized_points, parameters))
  
  density_data <- semi_join(all_points, averages,
                            by=c("parameters")) %>% 
    #group_by(parameters) %>% 
    #sample_n(pmin(10000, n())) %>% 
    #ungroup() %>% 
    inner_join(params_ids)
  
  kde_recall <- ggplot(density_data, aes(recall, fill=parameters_id)) +
    geom_density(aes(y=..count..), alpha=1) +
    scale_x_continuous(limits = c(0,1)) +
    scale_colors() +
    theme_void()
  
  kde_qps <- ggplot(density_data, aes(1/query_time, fill=parameters_id)) +
    geom_density(aes(y=..count..), alpha=1) +
    scale_colors() +
    coord_flip() +
    theme_void()
  
  scatter <- ggplot(display_config, aes(avg_recall, qps, color=parameters_id)) +
    geom_point(data=sized_points,
               mapping=aes(x=recall, 
                           y=binned_qps,
                           size=bin_count),
               alpha=0.8,
               position=position_jitter(height=0,width = 0)) +
    geom_point(shape="x", color="black", size=6) +
    scale_x_continuous(limits = c(0,1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    scale_colors() +
    guides(size=FALSE,
           color=FALSE,
           fill=FALSE)+
    labs(x="recall",
         y="queries per second",
         title=algorithm) +
    theme_bw()
  
  if (!xlab) {
    scatter <- scatter + theme(axis.title.x = element_blank())
  }
  if (!ylab) {
    scatter <- scatter + theme(axis.title.y = element_blank())
  }
  
  p1 <- insert_xaxis_grob(scatter,
                          kde_recall,
                          position="top")
  p2 <- insert_yaxis_grob(p1,
                          kde_qps,
                          position="right")
  
  ggdraw(p2)
  
}

composed_performance_distribution <- function(multi_algo_data) {
  puffinn <- multi_algo_data %>% filter(algorithm == "PUFFINN") %>% static_distribution_plot(xlab=F)
  onng <- multi_algo_data %>% filter(algorithm == "ONNG") %>% static_distribution_plot(xlab=F,ylab=F)
  hnsw <- multi_algo_data %>% filter(algorithm == "HNSW") %>% static_distribution_plot(ylab=F)
  faiivf <- multi_algo_data %>% filter(algorithm == "FAI-IVF") %>% static_distribution_plot()
  annoy <- multi_algo_data %>% filter(algorithm == "Annoy") %>% static_distribution_plot(ylab=F)
  
  plot_grid(
    puffinn, onng, hnsw,
    faiivf, annoy, NULL,
    ncol = 3
  )
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

ranking_qps <- function(averages) {
  averages <- averages %>% 
    ungroup() %>% 
    mutate(dataset = str_remove(dataset, "-(angular|euclidean)"))
  config_75 <- averages %>% 
    group_by(dataset, algorithm) %>% 
    filter(recall >= 0.75) %>% 
    slice(which.max(qps)) %>% 
    mutate(label="Recall > 0.75") %>% 
    ungroup()
  config_9 <- averages %>% 
    group_by(dataset, algorithm) %>% 
    filter(recall >= 0.9) %>% 
    slice(which.max(qps)) %>% 
    mutate(label="Recall > 0.9") %>% 
    ungroup()
  ranked <- bind_rows(config_75, config_9) %>% 
    mutate(algorithm = fct_reorder(algorithm, qps)) %>% 
    group_by(label, dataset) %>% 
    mutate(ratio_to_best = qps / max(qps))
    
  ggplot(ranked, aes(x=dataset, y=ratio_to_best, color=algorithm)) +
    geom_point() +
    geom_line(aes(group=algorithm)) +
    geom_label_repel(data=ranked %>% filter(dataset == "glove-100",
                                            algorithm %in% c("HNSW", "ONNG", "PUFFINN")),
                     mapping=aes(label=algorithm),
                     nudge_x=-0.5,
                     hjust="right") +
    geom_label_repel(data=ranked %>% filter(dataset == "sift-128",
                                            algorithm %in% c("FAI-IVF", "Annoy")),
                     mapping=aes(label=algorithm),
                     nudge_x=0.5,
                     hjust="left") +
    #geom_dl(aes(label=algorithm),
    #        method="first.points",
    #        cex=-0.8) +
    facet_wrap(vars(label), ncol=1) +
    theme_bw() +
    theme(panel.grid.major.x = element_line(color="lightgray"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          legend.position='none',
          axis.text.y = element_blank(),
          axis.text.x = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank()
          )
}
  
ranking_distcomps <- function(averages) {
  averages <- averages %>% 
    ungroup() %>% 
    filter(!(algorithm %in% c("ONNG", "PUFFINN"))) %>% 
    mutate(dataset = str_remove(dataset, "-(angular|euclidean)"))
  config_75 <- averages %>% 
    group_by(dataset, algorithm) %>% 
    filter(`k-nn` >= 0.75) %>% 
    slice(which.min(distcomps)) %>% 
    mutate(label="Recall > 0.75") %>% 
    ungroup()
  config_9 <- averages %>% 
    group_by(dataset, algorithm) %>% 
    filter(`k-nn` >= 0.9) %>% 
    slice(which.min(distcomps)) %>% 
    mutate(label="Recall > 0.9") %>% 
    ungroup()
  ranked <- bind_rows(config_75, config_9) %>% 
    drop_na() %>% 
    mutate(algorithm = fct_reorder(algorithm, qps)) %>% 
    group_by(label, dataset) %>% 
    mutate(ratio_to_best = min(distcomps) / (distcomps))
    
  ggplot(ranked, aes(x=dataset, y=ratio_to_best, color=algorithm)) +
    geom_point() +
    geom_line(aes(group=algorithm)) +
    geom_label_repel(data=ranked %>% filter(dataset == "glove-100",
                                            algorithm %in% c("HNSW", "ONNG", "PUFFINN")),
                     mapping=aes(label=algorithm),
                     nudge_x=-0.5,
                     hjust="right") +
    geom_label_repel(data=ranked %>% filter(dataset == "sift-128",
                                            algorithm %in% c("FAI-IVF", "Annoy")),
                     mapping=aes(label=algorithm),
                     nudge_x=0.5,
                     hjust="left") +
    #geom_dl(aes(label=algorithm),
    #        method="first.points",
    #        cex=-0.8) +
    facet_wrap(vars(label), ncol=1) +
    theme_bw() +
    theme(panel.grid.major.x = element_line(color="lightgray"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          legend.position='none',
          axis.text.y = element_blank(),
          axis.text.x = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank()
          )
  
}
  
do_plot_recall_vs_expansion_single <- function(detail_with_difficulty) {
  params <- detail_with_difficulty %>% distinct(parameters) %>% pull()
  expansion_range <- detail_with_difficulty %>% select(expansion) %>% range()
  print(expansion_range)
  bw <- (expansion_range[2] - expansion_range[1]) / 20.0
  print(bw)
  center <- detail_with_difficulty %>% 
    ggplot(aes(expansion, recall)) +
    geom_bin2d(binwidth=c(bw, 0.1), show.legend = F) + 
    scale_fill_gradient(low='white', high='black') +
    coord_cartesian(ylim = c(0,1)) +
    labs(caption=params) +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.margin = unit(c(0,0,0,0), 'cm'),
          panel.border = element_blank(),
          axis.line = element_line(color='black'),
          panel.grid = element_blank())
  expansion_dist <- ggplot(detail_with_difficulty, aes(x=expansion)) +
    geom_density(aes(x=expansion, y=..ndensity..),
                 fill='gray',
                 alpha=0.3,
                 color='black') +
    coord_cartesian(xlim = expansion_range) +
    theme_void()
  recall_dist <- ggplot(detail_with_difficulty, aes(x=recall)) +
    geom_density(aes(x=recall, y=..ndensity..),
                 fill='gray', 
                 alpha=0.3,
                 color='black') +
    coord_flip(xlim = c(0,1)) +
    theme_void()
    
  p1 <- insert_xaxis_grob(center,
                          expansion_dist,
                          position="top")
  p2 <- insert_yaxis_grob(p1, recall_dist,
                          position="right")
  ggdraw(p2)
}


do_plot_recall_vs_lid_single <- function(detail_with_difficulty) {
  params <- detail_with_difficulty %>% distinct(parameters) %>% pull()
  bw <- detail_with_difficulty %>% select(lid) %>% max() / 10.0
  center <- detail_with_difficulty %>% 
    ggplot(aes(lid, recall)) +
    geom_bin2d(binwidth=c(bw, 0.1), show.legend = F) + 
    scale_fill_gradient(low='white', high='black') +
    coord_cartesian(ylim = c(0,1)) +
    labs(caption=params) +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.margin = unit(c(0,0,0,0), 'cm'),
          panel.border = element_blank(),
          axis.line = element_line(color='black'),
          panel.grid = element_blank())
  lid_dist <- ggplot(detail_with_difficulty, aes(x=lid)) +
    geom_density(aes(x=lid, y=..ndensity..),
                 fill='gray',
                 alpha=0.3,
                 color='black') +
    coord_cartesian(xlim = c(0, detail_with_difficulty %>% select(lid) %>% max())) +
    theme_void()
  recall_dist <- ggplot(detail_with_difficulty, aes(x=recall)) +
    geom_density(aes(x=recall, y=..ndensity..),
                 fill='gray', 
                 alpha=0.3,
                 color='black') +
    coord_flip(xlim = c(0,1)) +
    theme_void()
    
  p1 <- insert_xaxis_grob(center,
                          lid_dist,
                          position="top")
  p2 <- insert_yaxis_grob(p1, recall_dist,
                          position="right")
  ggdraw(p2)
}

do_plot_qps_vs_lid_single <- function(detail_with_difficulty) {
  detail_with_difficulty <- detail_with_difficulty %>% 
    mutate(qps = 1/query_time)
  params <- detail_with_difficulty %>% distinct(parameters) %>% pull()
  bw <- detail_with_difficulty %>% select(lid) %>% max() / 40.0
  range_qps <- detail_with_difficulty %>% select(qps) %>% range()
  bw_qps <- (range_qps[2] - range_qps[1]) / 40
  center <- detail_with_difficulty %>% 
    ggplot(aes(lid, 1/query_time)) +
    geom_bin2d(binwidth=c(bw, bw_qps), show.legend = F) + 
    scale_fill_gradient(low='white', high='black') +
    coord_cartesian(ylim = range_qps) +
    labs(caption=params,
         y="QPS") +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.margin = unit(c(0,0,0,0), 'cm'),
          panel.border = element_blank(),
          axis.line = element_line(color='black'),
          panel.grid = element_blank())
  lid_dist <- ggplot(detail_with_difficulty, aes(x=lid)) +
    geom_density(aes(x=lid, y=..ndensity..),
                 fill='gray',
                 alpha=0.3,
                 color='black') +
    coord_cartesian(xlim = c(0, detail_with_difficulty %>% select(lid) %>% max())) +
    theme_void()
  qps_dist <- ggplot(detail_with_difficulty, aes(x=qps)) +
    geom_density(aes(x=qps, y=..ndensity..),
                 fill='gray', 
                 alpha=0.3,
                 color='black') +
    coord_flip(xlim = range_qps) +
    theme_void()
    
  p1 <- insert_xaxis_grob(center,
                          lid_dist,
                          position="top")
  p2 <- insert_yaxis_grob(p1, qps_dist,
                          position="right")
  ggdraw(p2)
}

do_plot_qps_vs_expansion_single <- function(detail_with_difficulty) {
  detail_with_difficulty <- detail_with_difficulty %>% 
    mutate(qps = 1/query_time)
  params <- detail_with_difficulty %>% distinct(parameters) %>% pull()
  range_exp <- detail_with_difficulty %>% select(expansion) %>% range()
  bw_exp <- (range_exp[2] - range_exp[1]) / 40
  range_qps <- detail_with_difficulty %>% select(qps) %>% range()
  bw_qps <- (range_qps[2] - range_qps[1]) / 40
  center <- detail_with_difficulty %>% 
    ggplot(aes(expansion, 1/query_time)) +
    geom_bin2d(binwidth=c(bw_exp, bw_qps), show.legend = F) + 
    scale_fill_gradient(low='white', high='black') +
    coord_cartesian(ylim = range_qps) +
    labs(caption=params,
         y="QPS") +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.margin = unit(c(0,0,0,0), 'cm'),
          panel.border = element_blank(),
          axis.line = element_line(color='black'),
          panel.grid = element_blank())
  expansion_dist <- ggplot(detail_with_difficulty, aes(x=expansion)) +
    geom_density(aes(x=expansion, y=..ndensity..),
                 fill='gray',
                 alpha=0.3,
                 color='black') +
    coord_cartesian(xlim = range_exp) +
    theme_void()
  qps_dist <- ggplot(detail_with_difficulty, aes(x=qps)) +
    geom_density(aes(x=qps, y=..ndensity..),
                 fill='gray', 
                 alpha=0.3,
                 color='black') +
    coord_flip(xlim = range_qps) +
    theme_void()
    
  p1 <- insert_xaxis_grob(center,
                          expansion_dist,
                          position="top")
  p2 <- insert_yaxis_grob(p1, qps_dist,
                          position="right")
  ggdraw(p2)
}


build_file_name <- function(base, dataset, difficulty, algorithm, parameters) {
  param_string <- parameters %>% 
    str_replace_all(" ", "_") %>% 
    str_replace_all("'", "") %>% 
    str_replace_all("=","") %>% 
    str_remove_all(":") %>% 
    str_remove_all("\\(") %>% 
    str_remove_all("\\)") %>% 
    str_remove_all("\\{") %>% 
    str_remove_all("\\}")
  str_c(str_c(base, dataset, difficulty, algorithm, param_string, sep="-"), ".png")
}
  
do_plot_recall_vs_lid <- function(detail_with_difficulty, queries_difficulty) {
  params <- detail_with_difficulty %>% ungroup() %>% 
    distinct(parameters) %>% 
    pull()
  for (p_config in params) {
    plot_data <- detail_with_difficulty %>% 
      filter(parameters == p_config)
    plot_data <- bind_cols(plot_data, queries_difficulty %>% slice(1:nrow(plot_data)))
    p <- plot_data %>% do_plot_recall_vs_lid_single()
    algorithm <- plot_data %>% distinct(algorithm) %>% pull()
    dataset <- plot_data %>% distinct(dataset) %>% pull()
    difficulty <- plot_data %>% distinct(difficulty) %>% pull()
    ggsave(filename = here("imgs", build_file_name("recall-vs-lid-", dataset, difficulty,
                                                   algorithm, p_config)),
           plot = p,
           width = 12, height = 12,
           units = "cm")
  }
}
  

get_queryset_lid <- function(dataset_name) {
  # TODO: The last row should not be part of the tibble
  lids <- read_delim(paste0(str_replace(dataset_name, "-diverse(-2)?", ""),
                            "-lid.txt"), 
                     delim = " ", col_names = c("identifier","lid"))
  con <- file(paste0(dataset_name, "-queries.txt"))
  diverse <- jsonlite::fromJSON(readLines(con)[8])
  close(con)
  # Offset by 1 the indices because the file contains 0-based indices, the tibbles have 1 based indices
  result_lids <- lids %>% slice(diverse + 1)
  result_lids %>% 
    mutate(dataset = dataset_name)
}

get_queryset_expansion <- function(dataset_name) {
  # TODO: The last row should not be part of the tibble
  fname <- paste0(str_replace(dataset_name, "-diverse(-2)?", ""),
                              "-expansion.txt")
  print(paste("Opening", fname))
  expansions <- read_delim(fname, 
                           delim = " ", col_names = c("identifier","expansion"))
  fname <- paste0(dataset_name, "-expansion-workload.txt")
  print(paste("Opening", fname))
  con <- file(fname)
  diverse <- jsonlite::fromJSON(readLines(con)[8])
  close(con)
  # Offset by 1 the indices because the file contains 0-based indices, the tibbles have 1 based indices
  result_expansions <- expansions %>% slice(diverse + 1)
  result_expansions %>% 
    mutate(dataset = dataset_name)
}


  
  
  
  
