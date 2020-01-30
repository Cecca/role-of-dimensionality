expansion_files <- list(
  "Fashion-mnist" = "fashion-mnist-784-euclidean-expansion.txt",
  "Glove-100" = "glove-100-angular-expansion.txt",
  "Glove-2m" = "glove-2m-300-angular-expansion.txt",
  "GNews" = "gnews-300-angular-expansion.txt",
  "Mnist" = "mnist-784-euclidean-expansion.txt",
  "Sift" = "sift-128-euclidean-expansion.txt"
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
  #"fashion-mnist-784-euclidean",
  #"glove-100-angular",
  #"glove-2m-300-angular",
  #"gnews-300-angular",
  #"mnist-784-euclidean",
  "sift-128-euclidean"
)

difficulties <- list(
  #"diverse",
  #"easy",
  #"middle",
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
    recode_algos(),
  
  plot_recall_vs_qps = target(
    data_expansion %>% 
      filter(dataset == str_c(dataset_name, difficulty_name, sep="-")) %>% 
      do_plot_recall_vs_qps(),
    transform = cross(
      dataset_name = !!datasets,
      difficulty_name = !!difficulties
    )
  ),
  
  figure_recall_vs_qps = target(
    ggsave(
      filename = here("imgs", str_c(dataset_name, "-", difficulty_name, ".png")),
      plot = plot_recall_vs_qps
    ),
    transform = map(plot_recall_vs_qps)
  ),
  
  # ------- Distribution plots ----------
  expansion_scores_part = target(
    read_delim(expansion_file,
               col_types = "id",
               col_names = c("id", "expansion"),
               delim = " ") %>% 
      mutate(dataset = expansion_file) %>% 
      recode_datasets(),
    transform = map(expansion_file = !!expansion_files)
  ),
  expansion_scores = target(
    bind_rows(expansion_scores_part),
    transform = combine(expansion_scores_part)
  ),
  
  lid_scores_part = target(
    read_delim(lid_file,
               col_types = "id",
               col_names = c("id", "lid"),
               delim = " ") %>% 
      mutate(dataset = lid_file) %>% 
      recode_datasets(),
    transform = map(lid_file = !!lid_files)
  ),
  lid_scores = target(
    bind_rows(lid_scores_part),
    transform = combine(lid_scores_part)
  ),
  
  difficulty_scores = inner_join(lid_scores, expansion_scores),
  
  scores_correlation = difficulty_scores %>% 
    group_by(dataset) %>% 
    summarise(correlation = cor(lid, expansion)),
  
  plot_scores = target(
    difficulty_scores %>% 
      filter(dataset == dat) %>% 
      do_scatter_distribution(),
    transform = map(dat = !!datasets)
  ),
  
  save_scores = target(
    plot_scores %>% ggsave(filename = str_c(dat, "-scores.png")),
    transform = map(plot_scores)
  ),
  
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
  
  figure_distribution = target({
    plot_data <- detail %>%
      filter(algorithm == algorithm_name,
             dataset == dataset_name,
             difficulty == difficulty_name,
             difficulty_type == difficulty_type_name)
    if (nrow(plot_data) > 0) {
      widget <- interactive_distribution_plot(plot_data)
      htmlwidgets::saveWidget(widget,
                              here("imgs", str_c("perf-distribution-",
                                                 algorithm_name, "-",
                                                 dataset_name, "-",
                                                 difficulty_name, "-",
                                                 difficulty_type_name,
                                                 ".html")))
    }
    },
    transform = cross(
      algorithm_name = !!algorithms,
      dataset_name = !!datasets,
      difficulty_name = !!difficulties,
      difficulty_type_name = c("lid", "expansion")
    )
  ),
  
  #detail_puffinn = detail %>% filter(algorithm == "PUFFINN") %>% 
  #  tidyr::extract(parameters, 
  #                 c("space", "recall_param", "hf", "hashsource"), 
  #                 "PUFFINN\\(space=(\\d+), recall=(\\d+.\\d+), hf=(.+), hashsource=(.+)\\)", remove=F),
 # 
 # figure_distribution_puffinn = target(
 #   detail_puffinn %>%
 #     filter(dataset == dataset_name,
 #            difficulty == difficulty_name,
 #            difficulty_type == difficulty_type_name) %>% 
 #     interactive_distribution_plot() %>% 
 #     htmlwidgets::saveWidget(here("imgs", str_c("perf-distribution-puffinn-",
 #                                                dataset_name, "-",
 #                                                difficulty_name, "-",
 #                                                difficulty_type_name,
 #                                                ".html")),),
 #   transform = cross(
 #     dataset_name = !!datasets,
 #     difficulty_name = !!difficulties,
 #     difficulty_type_name = c("lid")
 #   )
 # ),
  
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
    
)







