load_results <- function(csv_file) {
  read_csv(csv_file)
}

scale_color_algo <- function() {
  colors <- brewer_pal(type="qual", palette = "Dark2")(5)
  names(colors) <- c(
    "ONNG",
    "HNSW",
    "Annoy",
    "IVF",
    "PUFFINN"
  )
  scale_color_manual(values=colors)
}

recode_datasets <- function(data) {
  data %>%
    mutate(dataset = dataset %>% 
             str_remove_all("-lid") %>% 
             str_remove_all("-rc") %>% 
             str_remove_all("-new") %>% 
             str_remove_all("-expansion") %>% 
             str_remove_all(".txt")) %>% 
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
  pattern_rm <- "-(diverse|easy|middle|hard)(-expansion|-lrc)?"
  detail_data %>% 
    mutate(difficulty = str_extract(dataset, pattern_get),
           difficulty_type = if_else(str_ends(dataset, "expansion"),
                                     "expansion",
                                     if_else(str_ends(dataset, "lrc"),
                                             "lrc",
                                             "lid")),
           dataset = str_remove(dataset, pattern_rm))
}

recode_algos <- function(data) {
  data %>% 
    mutate(algorithm = recode_factor(algorithm,
      'annoy' = 'Annoy',
      'faiss-ivf' = 'IVF',
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
  dataset <- data %>% select(dataset) %>% distinct() %>% pull() %>% as.character()
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ggplot(aes(x=recall, y=qps, color=algorithm, group=interaction(algorithm, difficulty_type))) +
    geom_point() +
    geom_line(aes(linetype=difficulty_type)) +
    scale_x_continuous(trans=scales::exp_trans()) +
    scale_y_log10() +
    labs(title=dataset) +
    facet_wrap(vars(difficulty)) +
    theme_bw()  +
    theme(legend.position = "bottom")
}

do_plot_recall_vs_distcomps_paper <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(1/distcomps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ggplot(aes(x=recall, y=distcomps, color=algorithm, group=algorithm)) +
    geom_point(size=0.4) +
    geom_line(alpha=0.8) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .25, .5, .75, 1),
                       labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_y_log10() +
    #scale_linetype_manual(values=c("expansion" = "dashed", 
    #                               "lid" = "solid",
    #                               "lrc" = "dotted")) +
    scale_color_algo() +
    labs(#linetype = "Difficulty selection",
         color = "Algorithm",
         y="Distance computations") +
    facet_grid(vars(dataset), vars(difficulty)) +
    theme_bw()  +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}

do_plot_recall_vs_qps_lid_paper <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ggplot(aes(x=recall, y=qps, color=algorithm, group=algorithm)) +
    geom_point(size=0.4) +
    geom_line(alpha=0.8) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .25, .5, .75, 1),
                       labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_y_log10() +
    scale_color_algo() +
    labs(color = "Algorithm") +
    facet_grid(vars(dataset), vars(difficulty)) +
    theme_bw()  +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          #legend.background = element_rect(color="black"),
          #legend.text = element_text(margin(t=0.1, b=0.1)),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}


do_plot_recall_vs_qps_one_algo_paper <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ggplot(aes(x=recall, y=qps, color=difficulty_type, group=difficulty_type)) +
    geom_point(size=0.4) +
    geom_line(alpha=0.8) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .25, .5, .75, 1),
                       labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_y_log10() +
    scale_linetype_manual(values=c("expansion" = "dashed", 
                                   "lid" = "solid",
                                   "lrc" = "dotted")) +
    labs(color = "Dimensionality measure") +
    facet_grid(vars(dataset), vars(difficulty)) +
    theme_bw()  +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          #legend.background = element_rect(color="black"),
          #legend.text = element_text(margin(t=0.1, b=0.1)),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}

do_plot_sift_glove_paper <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("middle", "diverse", "hard"),
                               ordered = TRUE))
  
  frontier %>% 
    ggplot(aes(x=recall, y=qps, 
               color=dataset, 
               linetype=difficulty,
               group=interaction(dataset, difficulty))) +
    geom_point(size=0.2) +
    geom_line(alpha=1) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .5, .75, 1),
                       labels = c("0", "0.5", "0.75", "1")) +
    scale_y_log10() +
    scale_linetype_manual(values=c(
      "hard" = "dashed",
      "middle" = "dotted",
      "diverse" = "solid"
    )) +
    #scale_color_algo() +
    scale_color_brewer(type="qual",
                       palette = "Dark2") +
    guides(linetype=FALSE)  +
    labs(color = "Dataset",
         linetype = "Difficulty") +
    theme_bw()  +
    theme(#legend.position = c(0.83, 0.2),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}

do_plot_mnist_paper <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("middle", "diverse", "hard"),
                               ordered = TRUE))
  
  colors <- brewer_pal(type="qual", palette = "Dark2")(5)[3:4]
  names(colors) <- c(
    "Fashion-MNIST",
    "MNIST"
  )
  
  frontier %>% 
    ggplot(aes(x=recall, y=qps, 
               color=dataset, 
               linetype=difficulty,
               group=interaction(dataset, difficulty))) +
    geom_point(size=0.2) +
    geom_line(alpha=1) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .5, .75, 1),
                       labels = c("0", "0.5", "0.75", "1")) +
    scale_y_log10() +
    scale_linetype_manual(values=c(
      "hard" = "dashed",
      "middle" = "dotted",
      "diverse" = "solid"
    )) +
    #scale_color_algo() +
    scale_color_manual(values=colors) +
    guides(linetype=FALSE)  +
    labs(color = "Dataset",
         linetype = "Difficulty") +
    theme_bw()  +
    theme(#legend.position = c(0.83, 0.2),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}


do_plot_recall_vs_distcomps_one_algo_paper <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(low(distcomps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, levels=c("easy", "middle", "hard"),
                               ordered = T)) %>% 
    ggplot(aes(x=recall, y=distcomps, color=difficulty_type, group=difficulty_type)) +
    geom_point(size=0.4) +
    geom_line(alpha=0.8) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .25, .5, .75, 1),
                       labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_y_log10() +
    scale_linetype_manual(values=c("expansion" = "dashed", 
                                   "lid" = "solid",
                                   "lrc" = "dotted")) +
    labs(color = "Dimensionality measure") +
    facet_grid(vars(dataset), vars(difficulty)) +
    theme_bw()  +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          #legend.background = element_rect(color="black"),
          #legend.text = element_text(margin(t=0.1, b=0.1)),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}


do_plot_recall_vs_qps_all_datasets <- function(data) {
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("middle", "diverse", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ggplot(aes(x=recall, y=qps, 
               color=dataset, 
               linetype=difficulty,
               group=interaction(difficulty, dataset))) +
    #geom_point(size=0.2) +
    geom_line(alpha=1) +
    scale_x_continuous(trans=scales::exp_trans(base = 10),
                       breaks = c(0, .5, .75, 1),
                       labels = c("0", "0.5", "0.75", "1")) +
    scale_y_log10() +
    scale_color_brewer(type="qual",
                       palette = "Dark2") +
    scale_linetype_manual(values=c(
      "hard" = "dashed",
      "middle" = "dotted",
      "diverse" = "solid"
    )) +
    guides(linetype=FALSE) +
    labs(#linetype = "Difficulty selection",
         color = "Dataset") +
    #facet_grid(vars(difficulty), vars(algorithm)) +
    facet_wrap(vars(algorithm), ncol = 5) +
    theme_bw()  +
    theme(#legend.position = c(0.83, 0.2),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}

do_plot_distcomps_vs_qps <- function(data) {
  # We are insterested in a different layout of the points of the
  # Recall/QPS plot, hence we select the same frontier
  frontier <- data %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    psel(high(qps) * high(recall)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard"),
                               ordered = TRUE))
  frontier %>% 
    ggplot(aes(x=qps, y=distcomps, color=algorithm, group=algorithm)) +
    geom_point(size=0.4) +
    geom_line(alpha=0.8) +
    #scale_x_continuous(trans=scales::exp_trans(base = 10),
    #                   breaks = c(0, .25, .5, .75, 1),
    #                   labels = c("0", "0.25", "0.5", "0.75", "1")) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_algo() +
    labs(color = "Algorithm") +
    facet_grid(vars(dataset), vars(difficulty)) +
    theme_bw()  +
    theme(#legend.position = c(0.83, 0.2),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}


do_scatter_distribution_lid_exp <- function(data) {
  dataset <- data %>% head(1) %>% select(dataset) %>% pull()
  #range_expansion <- data %>% select(expansion) %>% range()
  #range_lid <- data %>% select(lid) %>% range()
  
  scatter <- data %>% 
    ggplot(aes(x=lid, y=expansion)) +
    scale_x_continuous(trans="log",
                       labels=scales::number_format()) +
    scale_y_continuous(trans=log_trans(1.01),
                       labels=scales::number_format(accuracy=0.01))+
                       #breaks=c(1,2,3,4,5,6,7)) +
    geom_point(alpha=0.1) +
    scale_size_area() +
    #stat_smooth() +
    labs(x="Local Intrinsic Dimensionality",
         y="Expansion") +
    theme_bw() +
    theme(legend.position = 'bottom')
  scatter
}

do_scatter_distribution_lid_lrc <- function(data) {
  dataset <- data %>% head(1) %>% select(dataset) %>% pull()
  
  scatter <- data %>% 
    ggplot(aes(x=lid, y=rc)) +
    scale_x_continuous(trans="log",
                       labels=scales::number_format()) +
    scale_y_continuous(trans="log",
                       labels=scales::number_format(accuracy = 0.01)) +
    geom_point(alpha=0.1) +
    labs(x="Local Intrinsic Dimensionality",
         y="Local Relative Contrast") +
    theme_bw() +
    theme(legend.position = 'bottom')
  scatter
}

do_scatter_distribution_lrc_exp <- function(data) {
  dataset <- data %>% head(1) %>% select(dataset) %>% pull()
  
  scatter <- data %>% 
    ggplot(aes(x=expansion, y=rc)) +
    geom_point(alpha=0.1) +
    scale_y_continuous(trans="log",
                       labels=scales::number_format(accuracy = 0.01)) +
    scale_x_continuous(trans=log_trans(1.01),
                       labels=scales::number_format(accuracy=0.01))+
                       #breaks=c(1,2,3,4,5,6,7)) +
    scale_size_area() +
    labs(x="Expansion",
         y="Local Relative Contrast") +
    theme_bw() +
    theme(legend.position = 'bottom')
  scatter
}


static_ridges_plot_recall <- function(algo_data) {
  averages <- algo_data %>%
    group_by(algorithm, dataset, difficulty, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall)) %>% 
    psel(high(qps) * high(avg_recall))
  
  all_points <- inner_join(algo_data %>% select(-qps, -avg_recall), averages)
  
  ggplot(all_points, aes(x=recall, y=qps, group=parameters)) +
    geom_density_ridges(alpha=0.4, scale=15,
                        size=0.1, show.legend = F) +
    geom_point(data=averages, 
               mapping=aes(x=avg_recall), 
               size=0.3,
               show.legend = F) +
    scale_y_log10() +
    facet_grid(vars(difficulty), vars(algorithm)) +
    labs(y="QPS (1/s)", 
         x="recall") +
    theme_bw()
}
  
static_ridges_plot_qps <- function(algo_data) {
  dataset <- algo_data %>% distinct(dataset) %>% pull()
  difficulty <- algo_data %>% distinct(difficulty) %>% pull()
  difficulty_type <- algo_data %>% distinct(difficulty_type) %>% pull()
  averages <- algo_data %>%
    group_by(algorithm, dataset, difficulty, parameters) %>%
    summarise(avg_time = mean(query_time), avg_recall=mean(recall)) %>%
    mutate(qps = 1/avg_time) %>% 
    mutate(parameters = fct_reorder(parameters, avg_recall)) %>% 
    psel(high(qps) * high(avg_recall))
  
  all_points <- inner_join(algo_data %>% select(-qps, -avg_recall), averages)
  print(count(all_points, algorithm, difficulty, avg_recall) %>% 
          filter(algorithm == "IVF") %>% 
          arrange(desc(avg_recall)))
  
  ggplot(all_points, aes(x=1/query_time, 
                         y=avg_recall,
                         group=parameters)) +
    geom_density_ridges(alpha=0.4, scale=15,
                        size=0.1, show.legend = F) +
    geom_point(data=averages, 
               mapping=aes(x=qps), 
               size=0.3,
               show.legend = F) +
    scale_x_log10() + 
    scale_y_continuous(labels=c("0", "0.3", "0.6", "0.9", "")) +
    facet_grid(vars(difficulty), vars(algorithm), scales="free_x") +
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
  print(averages)
  
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
    scale_x_continuous(limits = c(0,1), trans="exp") +
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
    mutate(ratio_to_best = qps / max(qps)) %>% 
    ungroup() %>% 
    mutate(dataset = factor(dataset,
                            levels=c("GLOVE", "GLOVE-2M", "GNEWS", "SIFT", "MNIST"),
                            ordered = T))
  
  labels <- tribble(
    ~algorithm, ~label_y,
    "ONNG", 0.98,
    "HNSW", 0.55,
    "IVF", 0.25,
    "Annoy", 0.15,
    "PUFFINN", 0.03
  ) %>% 
    mutate(algorithm = factor(algorithm)) %>% 
    mutate(label = "Recall > 0.9") %>% 
    inner_join(filter(ranked,dataset == "GLOVE"))
    
  ggplot(ranked, aes(x=dataset, y=ratio_to_best, color=algorithm)) +
    geom_point() +
    geom_line(aes(group=algorithm)) +
    geom_text(data=labels,
               mapping=aes(label=algorithm, y=label_y),
               x = 0.35,
               hjust="center",
               size=2) +
    scale_y_continuous(position = "left",
                       trans= scales::log_trans(base = 2),
                       breaks =c(1, 0.5, 0.25, 0.1, 0.01),
                       labels = c("best", "1/2 qps", "1/4 qps", "1/10 qps", "1/100 qps")) +
    scale_color_algo() +
    facet_wrap(vars(label), ncol=2) +
    coord_cartesian(clip="off") +
    labs(title="By queries per second") +
    theme_bw() +
    theme(panel.grid.major.x = element_line(color="lightgray"),
          panel.grid.major.y = element_line(color="lightgray",
                                            linetype = "dotted"),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(hjust=0),
          plot.title = element_text(size=rel(1)),
          legend.position='none',
          text = element_text(size = 8),
          axis.text.y.left = element_text(),
          axis.text.x = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y.right = element_line(),
          axis.text.y.right = element_text()
          )
}
  
ranking_distcomps <- function(averages) {
  averages <- averages %>% 
    ungroup() %>% 
    mutate(dataset = str_remove(dataset, "-(angular|euclidean)")) %>% 
    mutate(dataset = factor(
      dataset,
      levels = c("GLOVE", "GLOVE-2M", "GNEWS", "SIFT", "MNIST"),
      ordered = TRUE
    ))
  invalid <- averages %>%
    group_by(algorithm) %>% 
    summarise(min_distcomps = min(distcomps)) %>% 
    filter(min_distcomps == 0)
  averages <- anti_join(averages, invalid) %>% 
    drop_na()
  config_75 <- averages %>% 
    group_by(dataset, algorithm) %>% 
    filter(recall >= 0.75) %>% 
    slice(which.min(distcomps)) %>% 
    mutate(label="Recall > 0.75") %>% 
    ungroup()
  config_9 <- averages %>% 
    group_by(dataset, algorithm) %>% 
    filter(recall >= 0.9) %>% 
    slice(which.min(distcomps)) %>% 
    mutate(label="Recall > 0.9") %>% 
    ungroup()
  ranked <- bind_rows(config_75, config_9) %>% 
    drop_na() %>% 
    mutate(algorithm = fct_reorder(algorithm, qps)) %>% 
    group_by(label, dataset) %>% 
    mutate(ratio_to_best = min(distcomps) / (distcomps)) %>% 
    ungroup() %>% 
    mutate(dataset = factor(dataset,
                            levels=c("GLOVE", "GLOVE-2M", "GNEWS", "SIFT", "MNIST"),
                            ordered = T))
  
  labels <- tribble(
    ~algorithm, ~label_y,
    "ONNG", 0.97,
    "HNSW", 0.55,
    "Annoy", 0.15,
    "IVF", 0.05,
    "PUFFINN", 0.25
  ) %>% 
    mutate(algorithm = factor(algorithm)) %>% 
    mutate(label = "Recall > 0.9") %>% 
    inner_join(filter(ranked,dataset == "GLOVE"))
    
  ggplot(ranked, aes(x=dataset, y=ratio_to_best, color=algorithm)) +
    geom_point() +
    geom_line(aes(group=algorithm)) +
    #geom_segment(data=labels,
    #             mapping=(aes(x=0.9, xend="GLOVE",
    #                          y=label_y, yend=ratio_to_best)),
    #             linetype="dashed",
    #             size=0.4)+
    geom_text(data=labels,
               mapping=aes(label=algorithm, y=label_y),
               x=0.35,
               hjust="center",
               size=2) +
    scale_y_continuous(position = "left",
                       trans= scales::log_trans(base = 2),
                       breaks =c(1, 0.5, 0.25, 0.1, 0.01),
                       labels = c("best", "2x dists", "4x dists", "10x dists", "100x dists")) +
    scale_color_algo() +
    facet_wrap(vars(label), ncol=2) +
    coord_cartesian(clip="off") +
    labs(title="By distance computations") +
    theme_bw() +
    theme(panel.grid.major.x = element_line(color="lightgray"),
          panel.grid.major.y = element_line(color="lightgray",
                                            linetype = "dotted"),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size=rel(1)),
          strip.background = element_blank(),
          strip.text = element_text(hjust=0),
          text = element_text(size = 8),
          legend.position='none',
          axis.text.y.left = element_text(),
          axis.text.x = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y.right = element_line(),
          axis.text.y.right = element_text()
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
    #geom_bin2d(binwidth=c(bw, 0.1), show.legend = F) + 
    stat_bin2d(#binwidth=c(bw, 0.1), 
               bins = c(20, 10),
               geom = "tile",
               show.legend = F) +
    scale_fill_gradient(low='white', high='black') +
    scale_x_continuous(trans="identity",
                       limits = expansion_range) +
    scale_size_area() +
    coord_cartesian(ylim = c(0,1)) +
    labs(x="1/log(expansion)") +
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

do_plot_recall_vs_lrc_single <- function(detail_with_difficulty) {
  # detail_with_difficulty <- filter(detail_with_difficulty, lrc <= 5)
  params <- detail_with_difficulty %>% distinct(parameters) %>% pull()
  bw <- detail_with_difficulty %>% select(lrc) %>% max() / 20.0
  lrc_range <- detail_with_difficulty %>% select(lrc) %>% range()
  center <- detail_with_difficulty %>% 
    ggplot(aes(lrc, recall)) +
    #geom_bin2d(binwidth=c(bw, 0.1), show.legend = F) + 
    stat_bin2d(binwidth=c(bw, 0.1), 
               geom = "tile",
               show.legend = F) +
    scale_fill_gradient(low='white', high='black') +
    scale_size_area() +
    #scale_x_log10() +
    coord_cartesian(ylim = c(0,1)) +
    labs(x="1/log(lrc)") +
    theme_bw() +
    theme(legend.position = 'bottom',
          plot.margin = unit(c(0,0,0,0), 'cm'),
          panel.border = element_blank(),
          axis.line = element_line(color='black'),
          panel.grid = element_blank())
  rc_dist <- ggplot(detail_with_difficulty, aes(x=lrc)) +
    geom_density(aes(x=lrc, y=..ndensity..),
                 fill='gray',
                 alpha=0.3,
                 color='black') +
    coord_cartesian(xlim = lrc_range) +
    theme_void()
  recall_dist <- ggplot(detail_with_difficulty, aes(x=recall)) +
    geom_density(aes(x=recall, y=..ndensity..),
                 fill='gray', 
                 alpha=0.3,
                 color='black') +
    coord_flip(xlim = c(0,1)) +
    theme_void()
    
  p1 <- insert_xaxis_grob(center,
                          rc_dist,
                          position="top")
  p2 <- insert_yaxis_grob(p1, recall_dist,
                          position="right")
  ggdraw(p2)
}


do_plot_recall_vs_lid_single <- function(detail_with_difficulty) {
  params <- detail_with_difficulty %>% distinct(parameters) %>% pull()
  bw <- detail_with_difficulty %>% select(lid) %>% max() / 20.0
  center <- detail_with_difficulty %>% 
    ggplot(aes(lid, recall)) +
    #geom_bin2d(binwidth=c(bw, 0.1), show.legend = F) + 
    stat_bin2d(binwidth=c(bw, 0.1), 
               geom = "tile",
               show.legend = F) +
    scale_fill_gradient(low='white', high='black') +
    scale_size_area() +
    coord_cartesian(ylim = c(0,1)) +
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
    labs(#caption=params,
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
  

get_queryset <- function(dataset_name, difficulty_type, difficulty) {
  lids <- read_delim(paste0(str_replace(dataset_name, "-diverse(-2)?", ""),
                            "-", difficulty_type, ".txt"), 
                     delim = " ", col_names = c("identifier", difficulty_type))
  suffix <- c(
    "lid" = "-queries.txt",
    "expansion" = "-expansion.txt"
  )
  row_number <- c(
    "easy" = 2,
    "middle" = 4,
    "hard" = 6,
    "diverse" = 8
  )
  con <- file(paste0(dataset_name, suffix[difficulty_type]))
  diverse <- jsonlite::fromJSON(readLines(con)[row_number[difficulty]])
  close(con)
  # Offset by 1 the indices because the file contains 0-based indices, the tibbles have 1 based indices
  result_lids <- lids %>% slice(diverse + 1)
  result_lids
}

attach_score_difficulty <- function(data) {
  datasets <- data %>% distinct(dataset) %>% pull()
  difficulty_types <- distinct(data, difficulty_type) %>% pull()
  difficulties <- distinct(data, difficulty) %>% pull()
  
  tibbles <- list()
  
  for (dataset_name in datasets) {
    for (difficulty_type_name in difficulty_types) {
      for (difficulty_name in difficulties) {
        local_data <- filter(data,
                             dataset == dataset_name,
                             difficulty_type == difficulty_type_name,
                             difficulty == difficulty_name
                             )
        params <- local_data %>% distinct(parameters) %>% pull()
        for (params_name in params) {
          param_data <- filter(local_data, parameters == params_name)
          print(count(param_data, dataset, difficulty_type, difficulty))
          queries <- get_queryset(dataset_name, difficulty_type_name, difficulty_name)
          extended <- bind_cols(param_data, queries)
          tibbles <- c(tibbles, list(extended))
        }
      }
    }
  }
  
  bind_rows(tibbles)
}

get_queryset_lid <- function(dataset_name) {
  lids <- read_delim(paste0(str_replace(dataset_name, "-diverse(-2)?", ""),
                            "-new-lid.txt"), 
                     delim = " ", col_names = c("identifier","lid"))
  con <- file(paste0(dataset_name, "-queries.txt"))
  diverse <- jsonlite::fromJSON(readLines(con)[8])
  close(con)
  # Offset by 1 the indices because the file contains 0-based indices, the tibbles have 1 based indices
  result_lids <- lids %>% slice(diverse + 1)
  print(head(result_lids))
  result_lids %>% 
    mutate(dataset = dataset_name)
}

get_queryset_lrc <- function(dataset_name) {
  lids <- read_delim(paste0(str_replace(dataset_name, "-diverse(-2)?", ""),
                            "-rc.txt"), 
                     delim = " ", col_names = c("identifier","rc"))
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

arrow_plot <- function(data) {
  data <- data %>% 
    arrange(dataset, algorithm, difficulty) %>% 
    #recode_datasets() %>% 
    #mutate(difficulty_type = factor(difficulty_type, levels=c("LID", "RC", "Expansion"), ordered = T)) %>% 
    mutate(difficulty_type = factor(difficulty_type, levels=c("lid", "lrc", "expansion"), ordered = T)) %>% 
    mutate(dataset = factor(
      dataset,
      levels = c("GLOVE", "GLOVE-2M", "GNEWS", "SIFT", "MNIST", "Fashion-MNIST"),
      ordered = TRUE
    )) %>% 
    mutate(difficulty_type = recode_factor(difficulty_type,
                                           "lid" = "LID",
                                           "lrc" = "RC",
                                           "expansion" = "Expansion"))
  speed_drop <- data %>% 
    filter(difficulty %in% c("easy", "hard")) %>% 
    select(dataset, difficulty_type, difficulty, algorithm, qps) %>% 
    spread(difficulty, qps) %>% 
    mutate(speed_ratio = easy / hard,
           x = "hard")
  
  data %>% 
    ggplot(aes(x=difficulty, y=qps, color=algorithm, group=algorithm)) +
    geom_line() +
    geom_text_repel(aes(x=x, y=hard, label=scales::number(speed_ratio,
                                                          big.mark = "",
                                                          accuracy=1,
                                                          suffix="x")),
                    data=speed_drop,
                    nudge_x      = 1,
                    direction    = "y",
                    hjust        = 0,
                    segment.alpha = .5,
                    segment.color = "black",
                    size=2,
                    show.legend = F) +
    geom_point(size=.5) +
    scale_y_log10() +
    scale_x_discrete(limits=c("easy", "middle", "hard", "")) +
    #scale_linetype_manual(values=c("expansion" = "dotted", "lid" = "solid")) +
    labs(linetype = "Difficulty selection",
         color = "Algorithm") +
    facet_grid(vars(difficulty_type), vars(dataset)) +
    theme_bw()  +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          legend.box.margin = margin(0,0,0,0),
          legend.box.spacing = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, "mm"),
          axis.text.x = element_text(angle=45),
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
}

# What we want to show is the _conditional_ probability of a vector getting
# a rank with k=10 given that it has a rank in some range with k=100
plot_heatmap <- function(data, rank_accurate, rank_less_accurate) {
  binned <- data %>%
    mutate(g_acc = cut({{rank_accurate}}, 30, right=FALSE), 
           g_nonacc = cut({{rank_less_accurate}}, 30, right=FALSE)) %>%
    group_by(dataset, g_acc, g_nonacc) %>%
    summarise(cnt = n(), .groups="drop_last") %>%
    mutate(prob = cnt / sum(cnt))

  ggplot(binned, aes(x=desc(g_acc), y=desc(g_nonacc), fill=prob)) + 
    geom_tile() +
    facet_wrap(vars(dataset), ncol=3, scales="free") + 
    scale_fill_viridis_c(option="inferno", trans=scales::exp_trans(10)) + 
    scale_x_discrete(breaks=c()) +
    scale_y_discrete(breaks=c()) +
    labs(fill="Conditional probability",
         x="", y="") +
    theme_classic() +
    theme(legend.position="top",
          strip.background = element_blank(),
          strip.text=element_text(hjust=0.05))
}

# This plot answers the question: given a range of ranks build with a low accuracy
# estimator, were those points ranked very far away in the accurate ranking??
plot_displacements2 <- function(data, rank_accurate, rank_less_accurate) {
  grays <- rev(sequential_hcl(5, "Grays")[1:4])

  data %>%
    group_by(dataset) %>%
    mutate(group=cut( {{rank_less_accurate}}, breaks=50, labels=F)) %>%
    group_by(dataset, group) %>%
    summarise(
      acc_min = min({{ rank_accurate }}),
      acc_max = max({{ rank_accurate }}),
      inacc_min = min({{rank_less_accurate}}),
      inacc_max = max({{rank_less_accurate}}),
      inacc995 = quantile({{ rank_accurate }}, 0.995),
      inacc90 = quantile({{ rank_accurate }}, 0.9),
      inacc75 = quantile({{ rank_accurate }}, 0.75),
      inacc50 = quantile({{ rank_accurate }}, 0.5),
      inacc25 = quantile({{ rank_accurate }}, 0.25),
      inacc10 = quantile({{ rank_accurate }}, 0.1),
      inacc05 = quantile({{ rank_accurate }}, 0.005)
    ) %>%

  ggplot(aes(xmin=inacc_min, xmax=inacc_max, ymin=acc_min, ymax=acc_max)) +
    geom_rect(fill=grays[1], color=NA) +
    geom_rect(aes(ymin=inacc05, ymax=inacc995), fill=grays[2], color=NA) +
    geom_rect(aes(ymin=inacc10, ymax=inacc90), fill=grays[3], color=NA) +
    geom_rect(aes(ymin=inacc25, ymax=inacc75), fill=grays[4], color=NA) +
    scale_x_continuous(breaks=c(0), trans="reverse") +
    scale_y_continuous(breaks=c(0), trans="reverse") +
    facet_wrap(vars(dataset),
              scales="free",
              ncol=3) +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text=element_text(hjust=0.05))

}

plot_displacements <- function(dataset, x, y, n_hard=10000) {
    pd <- dataset %>%
      mutate(group = cut({{ x }}, 10, labels = F) / 10 - 0.5/10) %>%
      group_by(dataset, group) %>%
      summarise(
        ymax = max({{ y }}),
        ymin = min({{ y }}),
        y95 = quantile({{ y }}, 0.95),
        y75 = quantile({{ y }}, 0.75),
        y50 = quantile({{ y }}, 0.5),
        y25 = quantile({{ y }}, 0.25),
        y5 = quantile({{ y }}, 0.05),
        hard_mark = 1 - 10000/n(),
      )
    ggplot(pd, aes(group)) +
      geom_abline(intercept=0, slope=1) +
      geom_linerange(aes(ymin=ymin, ymax=ymax), color="black") +
      geom_linerange(aes(ymin=y5, ymax=y95), size=1.5, color="black") +
      geom_linerange(aes(ymin=y25, ymax=y75), size=2.5, color="black") +
      geom_point(aes(y=y50), size=2.5, shape=15, color="white") +
      # geom_vline(aes(xintercept=hard_mark), color="red") +
      geom_hline(aes(yintercept=hard_mark), color="red") +
      labs(x="rank with k=10",
           y="rank with k=100") +
      facet_wrap(vars(dataset),
                scales="free",
                ncol=1)
    # ggsave(plot=p,
    #        filename=filename,
    #        width=5,
    #        height=10,
    #        dpi=300)
}  
  
plot_score_distribution <- function(distribution, score, param, param_low, param_high, xlab, xmax=NA, reverse=F) {
  maxval <- distribution %>% summarise(max({{score}})) %>% pull()
  if (!is.na(xmax)) {
    maxval <- min(xmax, maxval)
  }

  ordering <- if (reverse) {
    function(d) {d}
  } else {
    function(d) {desc(d)}
  }

  medians <- distribution %>%
    filter({{param}} == param_low) %>%
    group_by(dataset) %>%
    summarise(median_score = median({{score}})) %>%
    ungroup()

  datasets <- distribution %>%
    filter({{param}} == param_low) %>%
    arrange(dataset, ordering({{score}})) %>%
    group_by(dataset) %>%
    slice(10000) %>%
    ungroup() %>%
    inner_join(medians) %>%
    mutate(
      hard_threshold = {{score}},
      dataset_id = row_number(ordering(desc(median_score)))
    ) %>%
    select(dataset, dataset_id, hard_threshold)

  distribution <- inner_join(distribution, datasets)
  plot_data_high <- distribution %>% 
    as_tibble() %>%
    filter({{ param }} == param_high)
  plot_data_low <- distribution %>% 
    as_tibble() %>%
    filter({{ param }} == param_low)

  p <- ggplot(plot_data_high, aes(x={{ score }}, y=dataset_id, group=dataset)) +
    geom_density_ridges(scale=.95,
                        rel_min_height = 0.0001,
                        size=0.3,
                        color="black",
                        quantile_lines=T) +
    geom_density_ridges(scale=.95,
                        data=plot_data_low,
                        color="red",
                        fill="red",
                        size=0.3,
                        alpha=0.3,
                        rel_min_height = 0.0001,
                        quantile_lines=T) +
    geom_segment(aes(y=dataset_id, yend = dataset_id+.8, x=hard_threshold, xend=hard_threshold),
                  data=datasets,
                  color="steelblue",
                  size=0.3) +
    geom_text(aes(y=dataset_id + 0.3, label=dataset, x=maxval),
              data=datasets,
              hjust="right",
              size = 2) +
    scale_x_continuous(limits=c(NA, xmax)) +
    labs(y="",
         x=xlab) +
    coord_cartesian(clip = "on") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size=8))

    p
}  

save_figure <- function(plot, basename, tex_width, tex_height, png_width, png_height) {
  tikz(paste0(basename, ".tex"),
       width = tex_width, height = tex_height)
  print(plot)
  dev.off()

  ggsave(plot=plot, filename=paste0(basename, ".png"),
         width=png_width, height=png_height, dpi=300)
}

# Compute the Pearson correlation between the two given columns without loading
# into a dataframe all the data, letting the database handle most of the operations
sqlite_cor <- function(data, X, Y) {
  data %>%
    mutate(
      diffX = {{X}} - mean({{X}}, na.rm=T),
      diffY = {{Y}} - mean({{Y}}, na.rm=T)
    ) %>%
    summarise(
      mX = mean({{X}}, na.rm=T),
      mX2 = mean({{X}} * {{X}}, na.rm=T),
      mY = mean({{Y}}, na.rm=T),
      mY2 = mean({{Y}} * {{Y}}, na.rm=T),
      mDiffXY = mean(diffX*diffY, na.rm=T)
    ) %>%
    collect() %>%
    transmute(
      corr = mDiffXY / (sqrt(mX2 - mX^2) * sqrt(mY2 - mY^2))
    )
}
