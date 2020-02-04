expansion_files <- list(
  "Fashion-mnist" = "fashion-mnist-784-euclidean-expansion.txt",
  "Glove-100" = "glove-100-angular-expansion.txt",
  "Glove-2m" = "glove-2m-300-angular-expansion.txt",
  "GNews" = "gnews-300-angular-expansion.txt",
  "Mnist" = "mnist-784-euclidean-expansion.txt",
  "Sift" = "sift-128-euclidean-expansion.txt"
)

expansion_50_files <- list(
  "fashion-mnist-784-euclidean-expansion-50.txt",
  "glove-100-angular-expansion-50.txt",
  "glove-2m-300-angular-expansion-50.txt",
  "gnews-300-angular-expansion-50.txt",
  "mnist-784-euclidean-expansion-50.txt",
  "sift-128-euclidean-expansion-50.txt"
)

lid_files <- list(
  "Fashion-mnist" = "fashion-mnist-784-euclidean-lid.txt",
  "Glove-100" = "glove-100-angular-lid.txt",
  "Glove-2m" = "glove-2m-300-angular-lid.txt",
  "GNews" = "gnews-300-angular-lid.txt",
  "Mnist" = "mnist-784-euclidean-lid.txt",
  "Sift" = "sift-128-euclidean-lid.txt"
)

datasets <- list(
  "fashion-mnist-784-euclidean",
  "glove-100-angular",
  "glove-2m-300-angular",
  "gnews-300-angular",
  "mnist-784-euclidean",
  "sift-128-euclidean"
)

datasets <- list(
  "GLOVE-2M",
  "GNEWS",
  "GLOVE",
  "SIFT",
  "Fashion-MNIST",
  "MNIST"
)

difficulties <- list(
  "diverse",
  "easy",
  "middle",
  "hard"
)

algorithms <- list(
  "PUFFINN",
  "ONNG",
  "HNSW",
  "FAI-IVF",
  "Annoy"
)

plan <- drake_plan(
  # ------- Performance plots ----------
  data_expansion = load_results("res-with-expansion.csv.bz2") %>% 
    add_difficulty() %>% 
    recode_algos() %>% 
    recode_datasets(),
  
  averages_recoded = averages %>% ungroup() %>% recode_datasets() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard", "diverse"),
                               ordered = TRUE)),
  
  plot_recall_vs_qps = target(
    averages_recoded %>% 
      filter(dataset == dataset_name, algorithm != "bruteforce-blas") %>% 
      do_plot_recall_vs_qps(),
    transform = cross(
      dataset_name = !!datasets
      #difficulty_name = !!difficulties
    )
  ),
  
  plot_recall_vs_qps_paper = target(
    averages_recoded %>% 
      filter(dataset %in% c("GLOVE", "SIFT"), 
             algorithm != "bruteforce-blas",
             difficulty != "diverse") %>% 
      do_plot_recall_vs_qps_paper(),
  ),
  
  figure_recall_vs_qps_paper = {
    tikz(file = here("imgs", "recall-vs-qps.tex"),
         width = 5.5, height = 4)
    print(plot_recall_vs_qps_paper)
    dev.off()
  },
  
  plot_all_datasets_paper = averages_recoded %>% 
    filter(algorithm %in% c("ONNG", "Annoy"),
           dataset %in% c("GLOVE", "SIFT", "GLOVE-2M")) %>% 
    do_plot_recall_vs_qps_all_datasets(),
    
  figure_all_datasets_paper = {
    tikz(file = here("imgs", "datasets-comparison.tex"),
         width = 5.5, height = 4)
    print(plot_all_datasets_paper)
    dev.off()
  },
  
  #figure_recall_vs_qps = target(
  #  ggsave(
  #    filename = here("imgs", str_c(dataset_name, "-", difficulty_name, ".png")),
  #    plot = plot_recall_vs_qps
  #  ),
  #  transform = map(plot_recall_vs_qps)
  #),
  
  # ------- Distribution plots ----------
  expansion_scores_part = target(
    read_delim(expansion_file,
               col_types = "id",
               col_names = c("id", "expansion"),
               delim = " ") %>% 
      mutate(dataset = expansion_file),
    transform = map(expansion_file = !!expansion_files)
  ),
  expansion_scores = target(
    bind_rows(expansion_scores_part) %>% recode_datasets(),
    transform = combine(expansion_scores_part)
  ),
  
  expansion_50_scores_part = target(
    read_delim(expansion_50_file,
               col_types = "id",
               col_names = c("id", "expansion_50"),
               delim = " ") %>% 
      mutate(dataset = expansion_50_file %>% str_remove("-50")),
    transform = map(expansion_50_file = !!expansion_50_files)
  ),
  expansion_50_scores = target(
    bind_rows(expansion_50_scores_part) %>% recode_datasets(),
    transform = combine(expansion_50_scores_part)
  ),
  
  lid_scores_part = target(
    read_delim(lid_file,
               col_types = "id",
               col_names = c("id", "lid"),
               delim = " ") %>% 
      mutate(dataset = lid_file),
    transform = map(lid_file = !!lid_files)
  ),
  lid_scores = target(
    bind_rows(lid_scores_part) %>% recode_datasets(),
    transform = combine(lid_scores_part)
  ),
  
  difficulty_scores = inner_join(lid_scores, expansion_scores) %>% 
    inner_join(expansion_50_scores),
  
  scores_correlation = difficulty_scores %>% 
    group_by(dataset) %>% 
    summarise(correlation = cor(lid, expansion)),
  
  plot_scores = target(
    difficulty_scores %>% 
      filter(dataset == dat) %>% 
      do_scatter_distribution(),
    transform = map(dat = !!datasets)
  ),
  
  save_scores = target({
      #tikz(here("imgs", str_c(dat, "-scores.tex")))
      #print(plot_scores)
      #dev.off()
      ggsave(filename = here("imgs", str_c(dat, "-scores.png")),
             plot = plot_scores,
             width=12, height=12,
             units = "cm")
    },
    transform = map(plot_scores)
  ),
  
  difficult_threshold_lid = difficulty_scores %>% 
    group_by(dataset) %>% 
    arrange(lid) %>% 
    slice(n() - 10000) %>% 
    transmute(hard_threshold = lid) %>% 
    ungroup() %>% 
    mutate(dataset = fct_reorder(dataset, hard_threshold)) %>% 
    arrange(dataset) %>% 
    mutate(dataset_id = row_number()),
  
  difficult_threshold_expansion = difficulty_scores %>% 
    group_by(dataset) %>% 
    arrange(expansion) %>% 
    slice(10000) %>% 
    transmute(hard_threshold = expansion) %>% 
    ungroup() %>% 
    inner_join(difficult_threshold_lid %>% rename(thresh_lid = hard_threshold)) %>% 
    mutate(dataset = fct_reorder(dataset, thresh_lid)) %>% 
    arrange(dataset) %>% 
    mutate(dataset_id = row_number()),
  
  
  density_lid_plot = {
    plot_data <- difficulty_scores %>% 
      group_by(dataset) %>% 
      ungroup() %>% 
      inner_join(difficult_threshold_lid) %>% 
      mutate(dataset = fct_reorder(dataset, hard_threshold))
    ggplot(plot_data, aes(x=lid, y=dataset_id, group=dataset)) +
      geom_density_ridges(scale=.95,
                          rel_min_height = 0.001,
                          quantile_lines=T) +
      geom_segment(aes(y=dataset_id, yend = dataset_id+.8, x=hard_threshold, xend=hard_threshold),
                   data=difficult_threshold_lid,
                   color="red",
                   size=1) +
      geom_label(aes(y=dataset_id + 0.3, label=dataset, x=120),
                 data=difficult_threshold_lid,
                 hjust="right",
                 size = 3) +
      labs(y="",
           x="Local Intrinsic Dimensionality") +
      coord_cartesian(clip = "on") +
      theme_bw() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size=8))
  },
  
  density_plot_lid_tex = {
    tikz(here("imgs", "density-lid.tex"),
         width = 3, height = 2.25)
    print(density_lid_plot)
    dev.off()
  },
  
  density_expansion_plot = {
    plot_data <- difficulty_scores %>% 
      group_by(dataset) %>% 
      arrange(expansion) %>% 
      ungroup() %>% 
      inner_join(difficult_threshold_expansion) %>% 
      mutate(dataset = fct_reorder(dataset, hard_threshold)) %>% 
      filter(expansion <= 1.5)
    label_x <- plot_data %>% ungroup() %>% summarise(max(expansion)) %>% pull()
    ggplot(plot_data, aes(x=expansion, y=dataset_id, group=dataset)) +
      geom_density_ridges(scale=.95,
                          rel_min_height = 0.001,
                          quantile_lines=T) +
      geom_segment(aes(y=dataset_id, yend = dataset_id+.8, x=hard_threshold, xend=hard_threshold),
                   data=difficult_threshold_expansion,
                   color="red",
                   size=1) +
      geom_label(aes(y=dataset_id + 0.3, label=dataset, x=label_x),
                 data=difficult_threshold_expansion,
                 hjust="right",
                 size=3) +
      scale_x_continuous(trans=log_trans(1.1), breaks=c(1,1.1,1.3,1.5)) +
      labs(y="",
           x="Expansion") +
      coord_cartesian(clip = "on") +
      theme_bw() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(size=8))
  },
  
  density_plot_expansion_tex = {
    tikz(here("imgs", "density-expansion.tex"),
         width = 3, height = 2.25)
    print(density_expansion_plot)
    dev.off()
  },
  
  # ------- Performance distribution scores ------------
  detail = target(
    load_results("detail.csv.bz2") %>% 
      rename(algorithm = algo, parameters = params) %>% 
      recode_algos() %>% 
      add_difficulty() %>% 
      mutate(recall = recall / 10) %>% 
      select(dataset, difficulty, algorithm, parameters, recall, query_time, difficulty_type),
    format = "fst"
  ),
  averages = detail %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm, parameters) %>% 
    summarise(qps = 1/mean(query_time), recall = mean(recall)),
  
  #figure_distribution = target({
  #  plot_data <- detail %>%
  #    filter(algorithm == algorithm_name,
  #           dataset == dataset_name,
  #           difficulty == difficulty_name,
  #           difficulty_type == difficulty_type_name)
  #  if (nrow(plot_data) > 0) {
  #    widget <- interactive_distribution_plot(plot_data)
  #    htmlwidgets::saveWidget(widget,
  #                            here("imgs", str_c("perf-distribution-",
  #                                               algorithm_name, "-",
  #                                               dataset_name, "-",
  #                                               difficulty_name, "-",
  #                                               difficulty_type_name,
  #                                               ".html")))
  #  } else {
  #    print(paste("Skipping dataset", dataset_name, difficulty_name, algorithm_name))
  #  }
  #  },
  #  transform = cross(
  #    algorithm_name = !!algorithms,
  #    dataset_name = !!datasets,
  #    difficulty_name = !!difficulties,
  #    difficulty_type_name = c("expansion", "lid")
  #  )
  #),
  
  data_performance_distribution_paper = detail %>% 
    filter(dataset == "glove-2m-300-angular",
           difficulty == "middle",
           difficulty_type == "lid"),
  
  plot_performance_distribution_recall = {
    #tikz(file = file_out("imgs/distribution_recall.tex"), width = 8, height = 4)
    p <- data_performance_distribution_paper %>%
      filter(algorithm %in% algorithms) %>% 
      static_ridges_plot_recall()
    ggsave("imgs/distribution_recall.pdf", plot=p,
           width = 8, height=3)
    #dev.off()
  },
  
  plot_performance_distribution_qps = {
    #tikz(file = file_out("imgs/distribution_qps.tex"), width = 8, height = 4)
    p <- data_performance_distribution_paper %>% 
      filter(algorithm %in% algorithms) %>% 
      static_ridges_plot_qps()
    ggsave("imgs/distribution_qps.pdf", plot=p,
           width = 8, height=3)
    #dev.off()
  },
  
  figure_performance_distribution_paper = target(
    {
      #data_performance_distribution_paper %>% 
      #  composed_performance_distribution()
    }
  ),
  
  # ------------------ Ranking ------------------------
  plot_ranking_qps = averages %>% 
    filter(difficulty == "middle") %>% 
    filter(algorithm %in% algorithms) %>% 
    filter(dataset != "fashion-mnist-784-euclidean") %>% 
    filter(difficulty_type == "lid") %>% 
    ungroup() %>% 
    recode_datasets() %>% 
    ranking_qps(),
  
  plot_ranking_qps_expansion = averages %>% 
    filter(difficulty == "middle") %>% 
    filter(algorithm %in% algorithms) %>% 
    filter(dataset != "fashion-mnist-784-euclidean") %>% 
    filter(difficulty_type == "expansion") %>% 
    ungroup() %>% 
    recode_datasets() %>% 
    ranking_qps(),
  
  ranking_qps_tex = {
    tikz(here("imgs", "ranking-qps.tex"),
         width=5.5, height=1.5)
    print(plot_ranking_qps)
    dev.off()
  },
  
  plot_ranking_distcomps = data_expansion %>% 
    filter(difficulty == "middle") %>% 
    filter(algorithm %in% algorithms) %>% 
    filter(dataset != "fashion-mnist-784-euclidean") %>% 
    filter(difficulty_type == "lid") %>% 
    ranking_distcomps(),
  
  ranking_distcomps_tex = {
    tikz(here("imgs", "ranking-distcomps.tex"),
         width=5.5, height=1.5)
    print(plot_ranking_distcomps)
    dev.off()
  },
  
  # ------------------ How is the difficulty? --------------------
  
  avg_lid = target(
    detail %>% 
      filter(difficulty_type == "lid") %>% 
      group_by(dataset, difficulty, algorithm, difficulty_type, parameters) %>% 
      summarise(
        recall = mean(recall),
        qps = 1/mean(query_time)
      ) %>% ungroup()
  ),
  avg_expansion = detail %>% 
    filter(difficulty_type == "expansion") %>% 
    group_by(dataset, difficulty, algorithm, difficulty_type, parameters) %>% 
    summarise(
      recall = mean(recall),
      qps = 1/mean(query_time)
    ) %>% ungroup(),
  
  fast_lid = avg_lid %>% 
    group_by(dataset, difficulty, algorithm) %>% 
    filter(recall >= 0.9) %>% 
    slice(which.max(qps)),
  
  fast_expansion = avg_expansion %>% 
    group_by(dataset, difficulty, algorithm) %>% 
    filter(recall >= 0.9) %>% 
    slice(which.max(qps)),
  
  difficulty_comparison = inner_join(fast_lid, 
                                     fast_expansion, 
                                     by=c("dataset", "difficulty", "algorithm"),
                                     suffix=c("_lid", "_expansion")) %>% 
    mutate(delta_qps = qps_lid - qps_expansion,
           ratio_qps = qps_expansion / qps_lid) %>% 
    ungroup(),
    
  plot_difficulty_comparison_data = difficulty_comparison %>% 
    mutate(x = fct_reorder(interaction(factor(dataset), factor(algorithm)),
                           ratio_qps),
           sign = if_else(ratio_qps < 1, "LID", "Expansion")),
  
  plot_difficulty_comparison = plot_difficulty_comparison_data %>% 
    ggplot(aes(algorithm, ratio_qps, fill=sign)) +
    geom_col_interactive(aes(tooltip=str_c("lid: ", qps_lid, " exp: ", qps_expansion,
                                           " param_lid: ", parameters_lid,
                                           " param_exp: ", parameters_expansion))) +
    geom_hline(yintercept = 1) +
    scale_fill_manual(values=c("LID" = "red", "Expansion" = "blue")) +
    labs(fill="On which dataset algorithms are faster?",
         y="qps_expansion / qps_lid") +
    coord_flip() +
    facet_grid(vars(dataset), vars(difficulty), scales="free_y") +
    theme_bw() +
    theme(legend.position = "bottom"),
 
  figure_difficulty_comparison = ggsave(
    filename = "difficulty-comparison.png",
    plot = plot_difficulty_comparison,
    width = 10,
    height = 5,
  ),
  
  figure_difficulty_comparison_html = girafe(ggobj = plot_difficulty_comparison) %>% 
    htmlwidgets::saveWidget("difficulty-comparison.html"),
  
  plot_difficulty_comparison_slope = difficulty_comparison %>% 
    mutate(
      dataset = str_remove(dataset, "-(angular|euclidean)"),
      slope = qps_expansion / qps_lid,
      slopesign = if_else(slope <= 1,
                          "LID",
                          "Expansion")
    ) %>% 
    ggplot(aes(group=algorithm,
               tooltip=str_c(algorithm, "\n",
                             "performance ratio: ", slope),
               data_id=algorithm)) +
    geom_point_interactive(aes(y=qps_lid, x="LID", color=slopesign)) +
    geom_point_interactive(aes(y=qps_expansion, x="Expansion", color=slopesign)) +
    geom_segment_interactive(aes(x="Expansion", xend="LID",
                             y=qps_expansion, yend=qps_lid,
                             color=slopesign)) +
    #scale_y_log10() +
    facet_grid(vars(difficulty), vars(dataset), scales='free') +
    labs(x="",
         y="Queries per second",
         title="Comparison of performance",
         subtitle="On datasets generated wrt LID or Expansion, at 0.9 recall",
         caption="How to read: each line represents the best configuration of the algorithm with >0.9 recall.
A line with a positive slope denotes that the algorithm is faster on the LID dataset.",
         color="Which dataset\nis easier?",
         shape="Algorithm") +
    theme_bw(),
 
  figure_difficulty_comparison_slope_html = girafe(ggobj = plot_difficulty_comparison_slope,
                                                   width_svg = 10,
                                                   height_svg = 8,
                                                   options = list(
                                                     opts_hover("stroke-width:3")
                                                   )) %>% 
    htmlwidgets::saveWidget("difficulty-comparison-slope.html"),
 
  # ----- Alternative difficulty comparison ------
  #
  # Performed by taking, for algorithms that attain at least .9 recall, the fastest configuration
  # on average on both datasets. For those not getting to .9, we get the configuration offering the
  # highest average recall between the two configurations
  data_alternative_difficulty_comparison = inner_join(avg_lid, avg_expansion,
                                                      by=c("dataset", "difficulty", "algorithm", "parameters"),
                                                      suffix = c("_lid", "_exp")) %>% 
    mutate(combined_recall = pmin(recall_lid, recall_exp),
           combined_qps = pmin(qps_lid, qps_exp)) %>% 
    mutate(larger_than_9 = combined_recall >= 0.9) %>% 
    group_by(dataset, difficulty, algorithm, larger_than_9) %>% 
    slice(if_else(larger_than_9,
                  which.max(combined_qps),
                  which.max(combined_recall))) %>% 
    group_by(dataset, difficulty, algorithm) %>% 
    slice(which.max(combined_qps)),
 
  plot_difficulty_comparison_alternative = data_alternative_difficulty_comparison %>% 
    mutate(delta_qps = qps_lid - qps_exp,
           ratio_qps = qps_exp / qps_lid,
           sign = if_else(ratio_qps < 1, "LID", "Expansion")) %>% 
    ungroup() %>% 
    ggplot(aes(algorithm, ratio_qps, fill=sign)) +
    geom_col_interactive(aes(tooltip=str_c("qps_lid: ", number(qps_lid), " qps_exp: ", number(qps_exp),
                                           "\nrecall_lid: ", number(recall_lid, accuracy=0.01), " recall_exp: ", number(recall_exp, accuracy = 0.01),
                                           "\nparams: ", parameters))) +
    geom_hline(yintercept = 1) +
    scale_fill_manual(values=c("LID" = "red", "Expansion" = "blue")) +
    scale_y_continuous(trans="log2") +
    labs(fill="On which dataset algorithms are faster?",
         y="log2(qps_expansion / qps_lid)",
         caption = "The configuration of each algorithm is the same on both datasets.
         For algorithms with parameter configurations achieveing at least 0.9 recall on both datasets, 
         the fastest such configuration is selected. For the others, the configuration
         achieving the highest minimum recall is selected.") +
    coord_flip() +
    facet_grid(vars(dataset), vars(difficulty), scales="free_y") +
    theme_bw() +
    theme(legend.position = "top"),
 
  figure_difficulty_comparison_alternative_html = girafe(ggobj = plot_difficulty_comparison_alternative) %>% 
    htmlwidgets::saveWidget("difficulty-comparison-alternative.html"),
  
  # ----------- Recall vs. difficulty ------------
  
  queries_glove_100_diverse_lid = get_queryset_lid("glove-100-angular"),
  queries_glove_100_diverse_expansion = get_queryset_expansion("glove-100-angular"),
  
  detail_to_plot = detail %>% 
    filter(dataset == "glove-100-angular",
           difficulty == "diverse"),
  
  detail_to_attach = detail %>% 
    filter(dataset == "glove-100-angular",
           difficulty_type == "lid",
           parameters == "ONNG-NGT(100, 10, 120, -2, 1.000)"),
  
  performance_with_score = attach_score_difficulty(detail_to_attach),
    
  recall_vs_lid_paper_1 = {
    tikz(file = here("imgs", "onng-recall-vs-lid.tex"),
         width = 2.25, height = 2.25)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "lid",
             parameters == "ONNG-NGT(100, 10, 120, -2, 1.000)") %>% 
      bind_cols(queries_glove_100_diverse_lid) %>% 
      do_plot_recall_vs_lid_single()
    print(p)
    dev.off()
  },
  
  recall_vs_expansion_paper_1 = {
    tikz(file = here("imgs", "onng-recall-vs-expansion.tex"),
         width = 2.25, height = 2.25)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "expansion",
             parameters == "ONNG-NGT(100, 10, 120, -2, 1.000)") %>% 
      bind_cols(queries_glove_100_diverse_expansion) %>% 
      filter(expansion <= 1.1) %>% 
      do_plot_recall_vs_expansion_single()
    print(p)
    dev.off()
  },
  
  qps_vs_lid_paper_1 = {
    tikz(file = here("imgs", "onng-qps-vs-lid.tex"),
         width = 2.25, height = 2.25)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "lid",
             parameters == "ONNG-NGT(100, 10, 120, -2, 1.000)") %>% 
      bind_cols(queries_glove_100_diverse_lid) %>% 
      do_plot_qps_vs_lid_single()
    print(p)
    dev.off()
  },
  
  qps_vs_expansion_paper_1 = {
    tikz(file = here("imgs", "onng-qps-vs-expansion.tex"),
         width = 2.25, height = 2.25)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "expansion",
             parameters == "ONNG-NGT(100, 10, 120, -2, 1.000)") %>% 
      bind_cols(queries_glove_100_diverse_expansion) %>% 
      filter(expansion <= 1.1) %>% 
      do_plot_qps_vs_expansion_single()
    print(p)
    dev.off()
  },
  
  
)







