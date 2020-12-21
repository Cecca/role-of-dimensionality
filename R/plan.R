# expansion_files <- list(
#   "fashion-mnist-784-euclidean-expansion.txt",
#   "glove-100-angular-expansion.txt",
#   "glove-2m-300-angular-expansion.txt",
#   "gnews-300-angular-expansion.txt",
#   "mnist-784-euclidean-expansion.txt",
#   "sift-128-euclidean-expansion.txt"
# )

# rc_files <- list(
#   "fashion-mnist-784-euclidean-rc.txt",
#   "glove-100-angular-rc.txt",
#   "glove-2m-300-angular-rc.txt",
#   "gnews-300-angular-rc.txt",
#   "mnist-784-euclidean-rc.txt",
#   "sift-128-euclidean-rc.txt"
# )

# lid_files <- list(
#   "fashion-mnist-784-euclidean-lid.txt",
#   "glove-100-angular-lid.txt",
#   "glove-2m-300-angular-lid.txt",
#   "gnews-300-angular-lid.txt",
#   "mnist-784-euclidean-lid.txt",
#   "sift-128-euclidean-lid.txt"
# )

# Check that files are all there
# for (f in lid_files) {
#   if (!file.exists(here(f))) {
#     stop("Missing LID file")
#   }
# }
# for (f in expansion_files) {
#   if (!file.exists(here(f))) {
#     stop("Missing Expansion file")
#   }
# }
# for (f in lrc_files) {
#   if (!file.exists(here(f))) {
#     stop("Missing RC file")
#   }
# }
if (!file.exists("export.db")) {
  stop("Missing export.db file")
}

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
  "IVF",
  "Annoy"
)

conn <- DBI::dbConnect(RSQLite::SQLite(), "export.db")

detail <- function() {
  inner_join(
    tbl(conn, "main"),
    tbl(conn, "query_stats"),
    by="id"
  )
}

plan <- drake_plan(
  # ------- Data --------
  # summarized = load_results("summarised.csv.bz2"),
  summarized = tbl(conn, "main") %>% 
    collect() %>%
    rename(recall = avg_recall, rel = avg_rel),
  
  # detail = target(
  #   read_parquet("detail.parquet"),
  #   format="fst"
  # ),
  
  # averages = detail %>% 
  #   group_by(dataset, difficulty, difficulty_type, algorithm, parameters) %>% 
  #   summarise(qps = 1/mean(query_time), recall = mean(recall)),
  averages = summarized,
  
  distcomps = summarized %>% 
    select(dataset, difficulty, difficulty_type, algorithm, parameters, distcomps),
  
  averages_recoded = averages %>% ungroup() %>% recode_datasets() %>% 
    mutate(difficulty = factor(difficulty, 
                               levels = c("easy", "middle", "hard", "diverse"),
                               ordered = TRUE)) %>% 
    inner_join(distcomps),
  
  # ------- Performance plots ----------
  
  plot_recall_vs_qps = target(
    averages_recoded %>% 
      filter(dataset == dataset_name, algorithm != "bruteforce-blas") %>% 
      do_plot_recall_vs_qps(),
    transform = cross(
      dataset_name = !!datasets
    )
  ),
  
  plot_recall_vs_qps_lid_paper = target(
    averages_recoded %>% 
      filter(dataset %in% c("GLOVE", "GLOVE-2M", "SIFT"), 
             algorithm != "bruteforce-blas",
             difficulty_type == "lid",
             difficulty != "diverse") %>% 
      do_plot_recall_vs_qps_lid_paper(),
  ),
  
  plot_recall_vs_qps_one_algo_paper = target(
    averages_recoded %>% 
      filter(dataset %in% c("GLOVE", "GLOVE-2M", "SIFT"), 
             algorithm == "Annoy",
             difficulty != "diverse") %>% 
      do_plot_recall_vs_qps_one_algo_paper(),
  ),
  
  plot_recall_vs_distcomps_one_algo_paper = target(
    averages_recoded %>% 
      filter(dataset %in% c("GLOVE", "GLOVE-2M", "SIFT"), 
             algorithm == "Annoy",
             difficulty != "diverse") %>% 
      do_plot_recall_vs_distcomps_one_algo_paper(),
  ),
  
  plot_recall_vs_distcomps_paper = target(
    averages_recoded %>% 
      filter(dataset %in% c("GLOVE", "GLOVE-2M", "SIFT"), 
             algorithm != "bruteforce-blas",
             difficulty_type == "lid",
             difficulty != "diverse") %>% 
      do_plot_recall_vs_distcomps_paper(),
  ),
  
  plot_time_vs_distcomps_paper = averages_recoded %>% 
    filter(dataset %in% c("GLOVE", "GLOVE-2M", "SIFT"), 
           algorithm != "bruteforce-blas",
           difficulty_type == "lid",
           difficulty != "diverse") %>% 
    do_plot_distcomps_vs_qps(),

  plot_recall_vs_rel_paper = averages_recoded %>%
    filter(difficulty_type == "lid") %>%
    ggplot(aes(recall, rel, color=difficulty)) +
    geom_point(size=.5) +
    facet_wrap(vars(algorithm), scales="free", ncol=5) +
    scale_y_continuous(trans="identity") +
    scale_x_continuous(breaks=c(0.5,1)) + 
    theme_bw()  +
    theme(#legend.position = c(0.83, 0.2),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "vertical",
          text = element_text(size = 8),
          plot.margin = unit(c(0,0,0,0), "cm"))
  ,
    
  figure_recall_vs_rel_paper = {
    tikz(file = here("imgs", "recall-vs-rel.tex"),
         width = 5.5, height = 2)
    print(plot_recall_vs_rel_paper)
    dev.off()
  },

  figure_recall_vs_qps_lid_paper = {
    tikz(file = here("imgs", "recall-vs-qps-lid.tex"),
         width = 5.5, height = 3)
    print(plot_recall_vs_qps_lid_paper)
    dev.off()
  },
  
  figure_recall_vs_qps_one_algo_paper = {
    tikz(file = here("imgs", "recall-vs-qps-one-algo.tex"),
         width = 5.5, height = 3)
    print(plot_recall_vs_qps_one_algo_paper)
    dev.off()
  },
  
  figure_recall_vs_distcomps_one_algo_paper = {
    tikz(file = here("imgs", "recall-vs-distcomps-one-algo.tex"),
         width = 5.5, height = 3)
    print(plot_recall_vs_distcomps_one_algo_paper)
    dev.off()
  },
  
  figure_recall_vs_distcomps_paper = {
    tikz(file = here("imgs", "recall-vs-distcomps.tex"),
         width = 5.5, height = 3)
    print(plot_recall_vs_distcomps_paper)
    dev.off()
  },
  
  plot_all_datasets_paper = averages_recoded %>% 
    filter(algorithm != "bruteforce-blas",
           difficulty %in% c("middle","diverse", "hard"),
           difficulty_type == "lid"
           ) %>% 
    mutate(difficulty_type = factor(difficulty_type,
                                    levels = c("lid", "lrc", "expansion"),
                                    ordered = T)) %>% 
    do_plot_recall_vs_qps_all_datasets(),
    
  figure_all_datasets_paper = {
    tikz(file = here("imgs", "datasets-comparison.tex"),
         width = 5.5, height = 3)
    print(plot_all_datasets_paper)
    dev.off()
  },
  
  plot_sift_glove_paper = averages_recoded %>% 
    filter(algorithm %in% c("Annoy"),
           difficulty %in% c("middle","diverse", "hard"),
           difficulty_type == "lid",
           dataset %in% c("SIFT", "GLOVE")
           ) %>% 
    do_plot_sift_glove_paper(),
  
  plot_mnist_paper = averages_recoded %>% 
    filter(algorithm %in% c("Annoy"),
           difficulty %in% c("middle","diverse", "hard"),
           difficulty_type == "lid",
           dataset %in% c("MNIST", "Fashion-MNIST")
           ) %>% 
    do_plot_mnist_paper(),
  
  figure_sift_glove_paper = {
    tikz(file = here("imgs", "sift-glove.tex"),
         width = 2.5, height = 2)
    print(plot_sift_glove_paper)
    dev.off()
  },
  
  figure_mnist_paper = {
    tikz(file = here("imgs", "fashion-mnist.tex"),
         width = 2.5, height = 2)
    print(plot_mnist_paper)
    dev.off()
  },
  
  # ------- Distribution plots ----------
  # expansion_scores_part = target(
  #   read_delim(expansion_file,
  #              col_types = "id",
  #              col_names = c("id", "expansion"),
  #              delim = " ") %>% 
  #     mutate(dataset = expansion_file),
  #   transform = map(expansion_file = !!expansion_files)
  # ),
  # expansion_scores = target(
  #   bind_rows(expansion_scores_part) %>% recode_datasets(),
  #   transform = combine(expansion_scores_part)
  # ),
  
  # rc_scores_part = target(
  #   read_delim(rc_file,
  #              col_types = "id",
  #              col_names = c("id", "rc"),
  #              delim = " ") %>% 
  #     mutate(dataset = rc_file %>% str_remove("-rc")),
  #   transform = map(rc_file = !!rc_files)
  # ),
  # rc_scores = target(
  #   bind_rows(rc_scores_part) %>% recode_datasets(),
  #   transform = combine(rc_scores_part)
  # ),
  
  # lid_scores_part = target(
  #   read_delim(lid_file,
  #              col_types = "id",
  #              col_names = c("id", "lid"),
  #              delim = " ") %>% 
  #     mutate(dataset = lid_file %>% str_remove("-new")),
  #   transform = map(lid_file = !!lid_files)
  # ),
  # lid_scores = target(
  #   bind_rows(lid_scores_part) %>% recode_datasets(),
  #   transform = combine(lid_scores_part)
  # ),
  
  # difficulty_scores = inner_join(lid_scores, expansion_scores) %>% 
  #   inner_join(rc_scores),
  
  # difficulties_to_plot = {
  #   difficulty_scores %>% 
  #     group_by(dataset, ntile(expansion, 100)) %>% 
  #     sample_n(min(100, n())) %>% 
  #     ungroup()
  # },
  
  # scores_correlation = difficulty_scores %>% 
  #   group_by(dataset) %>% 
  #   summarise(corr_lid_exp = cor(lid, expansion),
  #             corr_lid_lrc = cor(lid, rc),
  #             corr_exp_lrc = cor(expansion, rc)),
  
  # scores_correlation_pearson = {
  #   lid_exp <- difficulty_scores %>% 
  #     group_by(dataset) %>% 
  #     summarise(
  #       corr = list(cor.test(lid, expansion, method="pearson") %>% broom::tidy())
  #     ) %>% 
  #     unnest(corr) %>% 
  #     select(dataset, corr_lid_exp=estimate, p_lid_exp=p.value)
  #   lid_lrc <- difficulty_scores %>% 
  #     group_by(dataset) %>% 
  #     summarise(
  #       corr = list(cor.test(lid, rc, method="pearson") %>% broom::tidy())
  #     ) %>% 
  #     unnest(corr) %>% 
  #     select(dataset, corr_lid_lrc=estimate, p_lid_lrc=p.value)
  #   exp_lrc <- difficulty_scores %>% 
  #     group_by(dataset) %>% 
  #     summarise(
  #       corr = list(cor.test(expansion, rc, method="pearson") %>% broom::tidy())
  #     ) %>% 
  #     unnest(corr) %>% 
  #     select(dataset, corr_exp_lrc=estimate, p_exp_lrc=p.value)
  #   inner_join(lid_exp, lid_lrc) %>% 
  #     inner_join(exp_lrc)
  # },
  
  # plot_scores_correlation = scores_correlation %>% 
  #   tidyr::gather(corr_lid_exp, corr_lid_lrc, corr_exp_lrc, 
  #                 key="pair", value="correlation") %>% 
  #   ggplot(aes(dataset, abs(correlation), color=pair)) + 
  #   geom_point() + 
  #   coord_cartesian(ylim=c(0,1)),
  
  # plot_scores_lid_exp = target(
  #   difficulties_to_plot %>% 
  #     filter(dataset == dat) %>% 
  #     do_scatter_distribution_lid_exp(),
  #   transform = map(dat = !!datasets)
  # ),
  # plot_scores_lid_lrc = target(
  #   difficulties_to_plot %>% 
  #     filter(dataset == dat) %>% 
  #     do_scatter_distribution_lid_lrc(),
  #   transform = map(dat = !!datasets)
  # ),
  # plot_scores_lrc_exp = target(
  #   difficulties_to_plot %>% 
  #     filter(dataset == dat) %>% 
  #     do_scatter_distribution_lrc_exp(),
  #   transform = map(dat = !!datasets)
  # ),
  
  # save_scores_lid_exp = target({
  #     ggsave(filename = here("imgs", str_c(dat, "-scores-lid-exp.png")),
  #            plot = plot_scores_lid_exp,
  #            width=8, height=8,
  #            units = "cm")
  #   },
  #   transform = map(plot_scores_lid_exp)
  # ),
  # save_score_lid_lrc = target({
  #     ggsave(filename = here("imgs", str_c(dat, "-scores-lid-lrc.png")),
  #            plot = plot_scores_lid_lrc,
  #            width=8, height=8,
  #            units = "cm")
  #   },
  #   transform = map(plot_scores_lid_lrc)
  # ),
  # save_scores_lrc_exp = target({
  #     ggsave(filename = here("imgs", str_c(dat, "-scores-lrc-exp.png")),
  #            plot = plot_scores_lrc_exp,
  #            width=8, height=8,
  #            units = "cm")
  #   },
  #   transform = map(plot_scores_lrc_exp)
  # ),
  
  
  # difficult_threshold_lid = difficulty_scores %>% 
  #   group_by(dataset) %>% 
  #   arrange(lid) %>% 
  #   slice(n() - 10000) %>% 
  #   transmute(hard_threshold = lid) %>% 
  #   ungroup() %>% 
  #   mutate(dataset = fct_reorder(dataset, hard_threshold)) %>% 
  #   arrange(dataset) %>% 
  #   mutate(dataset_id = row_number()),
  
  # difficult_threshold_expansion = difficulty_scores %>% 
  #   group_by(dataset) %>% 
  #   arrange(expansion) %>% 
  #   slice(10000) %>% 
  #   transmute(hard_threshold = expansion) %>% 
  #   ungroup() %>% 
  #   inner_join(difficult_threshold_lid %>% rename(thresh_lid = hard_threshold)) %>% 
  #   mutate(dataset = fct_reorder(dataset, thresh_lid)) %>% 
  #   arrange(dataset) %>% 
  #   mutate(dataset_id = row_number()),
  
  # difficult_threshold_rc = difficulty_scores %>% 
  #   group_by(dataset) %>% 
  #   arrange(rc) %>% 
  #   slice(10000) %>% 
  #   transmute(hard_threshold = rc) %>% 
  #   ungroup() %>% 
  #   mutate(dataset = fct_reorder(dataset, hard_threshold)) %>% 
  #   arrange(desc(dataset)) %>% 
  #   mutate(dataset_id = row_number()),
  
  # density_lid_plot = {
  #   plot_data <- difficulty_scores %>% 
  #     group_by(dataset) %>% 
  #     ungroup() %>% 
  #     inner_join(difficult_threshold_lid) %>% 
  #     mutate(dataset = fct_reorder(dataset, hard_threshold))
  #   iqrs <- plot_data %>% group_by(dataset) %>% summarise(iqr = IQR(lid))
  #   outliers <- inner_join(iqrs, plot_data) %>% 
  #     filter(lid > 3*iqr) %>% 
  #     group_by(dataset) %>% 
  #     arrange(desc(expansion)) %>% 
  #     slice(which(row_number() %% 100 == 1))
  #   message("Number of outliers", nrow(outliers))
  #   ggplot(plot_data, aes(x=lid, y=dataset_id, group=dataset)) +
  #     geom_density_ridges(scale=.95,
  #                         rel_min_height = 0.0001,
  #                         quantile_lines=T) +
  #     geom_point(data=outliers,
  #                shape=46,
  #                size=.1) +
  #     geom_segment(aes(y=dataset_id, yend = dataset_id+.8, x=hard_threshold, xend=hard_threshold),
  #                  data=difficult_threshold_lid,
  #                  color="red",
  #                  size=1) +
  #     geom_text(aes(y=dataset_id + 0.3, label=dataset, x=120),
  #                data=difficult_threshold_lid,
  #                hjust="right",
  #                size = 2) +
  #     labs(y="",
  #          x="Local Intrinsic Dimensionality") +
  #     coord_cartesian(clip = "on") +
  #     theme_bw() +
  #     theme(axis.text.y = element_blank(),
  #           axis.ticks.y = element_blank(),
  #           text = element_text(size=8))
  # },
  
  # density_plot_lid_tex = {
  #   tikz(here("imgs", "density-lid.tex"),
  #        width = 2.8, height = 2.25)
  #   print(density_lid_plot)
  #   dev.off()
  # },
  
  # density_lid_compare_plot = {
  #   plot_data <- difficulty_scores %>% 
  #     inner_join(lid10_scores) %>% 
  #     group_by(dataset) %>% 
  #     ungroup() %>% 
  #     inner_join(difficult_threshold_lid) %>% 
  #     mutate(dataset = fct_reorder(dataset, hard_threshold))
  #   iqrs <- plot_data %>% group_by(dataset) %>% summarise(iqr = IQR(lid))
  #   outliers <- inner_join(iqrs, plot_data) %>% 
  #     filter(lid > 3*iqr) %>% 
  #     group_by(dataset) %>% 
  #     arrange(desc(expansion)) %>% 
  #     slice(which(row_number() %% 100 == 1))
  #   message("Number of outliers", nrow(outliers))
  #   ggplot(plot_data, aes(x=lid, y=dataset_id, group=dataset)) +
  #     geom_density_ridges(scale=.95,
  #                         rel_min_height = 0.0001,
  #                         quantile_lines=T) +
  #     geom_density_ridges(mapping=aes(x=lid10),
  #                         alpha=0.2,
  #                         color="red",
  #                         scale=.95,
  #                         rel_min_height = 0.0001,
  #                         quantile_lines=T) +
  #     geom_text(aes(y=dataset_id + 0.3, label=dataset, x=120),
  #                data=difficult_threshold_lid,
  #                hjust="right",
  #                size = 2) +
  #     labs(y="",
  #          x="Local Intrinsic Dimensionality") +
  #     coord_cartesian(clip = "on") +
  #     theme_bw() +
  #     theme(axis.text.y = element_blank(),
  #           axis.ticks.y = element_blank(),
  #           text = element_text(size=8))
  # },
  
  # density_expansion_plot = {
  #   plot_data <- difficulty_scores %>% 
  #     group_by(dataset) %>% 
  #     arrange(expansion) %>% 
  #     ungroup() %>% 
  #     inner_join(difficult_threshold_expansion) %>% 
  #     mutate(dataset = fct_reorder(dataset, hard_threshold)) %>% 
  #     filter(expansion <= 1.5)
  #   iqrs <- plot_data %>% group_by(dataset) %>% summarise(iqr = IQR(expansion))
  #   outliers <- inner_join(iqrs, plot_data) %>% 
  #     filter(rc > 50*iqr) %>% 
  #     group_by(dataset) %>% 
  #     arrange(desc(expansion)) %>% 
  #     slice(which(row_number() %% 100 == 1))
  #   message("Number of outliers", nrow(outliers))
  #   label_x <- plot_data %>% ungroup() %>% summarise(max(expansion)) %>% pull()
  #   ggplot(plot_data, aes(x=expansion, y=dataset_id, group=dataset)) +
  #     geom_density_ridges(scale=.95,
  #                         rel_min_height = 0.001,
  #                         quantile_lines=T) +
  #     geom_segment(aes(y=dataset_id, yend = dataset_id+.8, x=hard_threshold, xend=hard_threshold),
  #                  data=difficult_threshold_expansion,
  #                  color="red",
  #                  size=1) +
  #     geom_text(aes(y=dataset_id + 0.3, label=dataset, x=label_x),
  #                data=difficult_threshold_expansion,
  #                hjust="right",
  #                size=2) +
  #     scale_x_continuous(trans=log_trans(1.1), breaks=c(1,1.1,1.3,1.5)) +
  #     labs(y="",
  #          x="Expansion") +
  #     coord_cartesian(clip = "on") +
  #     theme_bw() +
  #     theme(axis.text.y = element_blank(),
  #           axis.ticks.y = element_blank(),
  #           text = element_text(size=8))
  # },
  
  # density_plot_expansion_tex = {
  #   tikz(here("imgs", "density-expansion.tex"),
  #        width = 2.8, height = 2.25)
  #   print(density_expansion_plot)
  #   dev.off()
  # },
  
  # density_rc_plot = {
  #   plot_data <- difficulty_scores %>% 
  #     inner_join(difficult_threshold_rc) %>% 
  #     mutate(dataset = fct_reorder(dataset, hard_threshold))
  #   iqrs <- plot_data %>% group_by(dataset) %>% summarise(iqr = IQR(rc))
  #   outliers <- inner_join(iqrs, plot_data) %>% 
  #     filter(rc > 10*iqr) %>% 
  #     group_by(dataset) %>% 
  #     arrange(desc(rc)) %>% 
  #     slice(which(row_number() %% 100 == 1))
  #   message("Number of outliers", nrow(outliers))
  #   label_x <- plot_data %>% ungroup() %>% summarise(max(rc)) %>% pull()
  #   ggplot(plot_data, aes(x=rc, y=dataset_id, group=dataset)) +
  #     geom_density_ridges(scale=.95,
  #                         rel_min_height = 0.001,
  #                         quantile_lines=T) +
  #     geom_point(data=outliers,
  #                shape=46,
  #                size=.1) +
  #     geom_segment(aes(y=dataset_id, yend = dataset_id+.8, x=hard_threshold, xend=hard_threshold),
  #                  data=difficult_threshold_rc,
  #                  color="red",
  #                  size=1) +
  #     geom_text(aes(y=dataset_id + 0.3, label=dataset, x=label_x),
  #                data=difficult_threshold_rc,
  #                hjust="right",
  #                size = 2) +
  #     scale_x_log10() +
  #     labs(y="",
  #          x="Relative contrast") +
  #     coord_cartesian(clip = "on") +
  #     theme_bw() +
  #     theme(axis.text.y = element_blank(),
  #           axis.ticks.y = element_blank(),
  #           text = element_text(size=8))
  # },
  
  # density_plot_rc_tex = {
  #   tikz(here("imgs", "density-rc.tex"),
  #        width = 2.8, height = 2.25)
  #   print(density_rc_plot)
  #   dev.off()
  # },
  
  
  # ------- Performance distribution scores ------------

  # perf_distribution_json = {
  # detail() %>% 
  #     distinct(dataset, difficulty, difficulty_type, algorithm, parameters) %>%

  # },
  
  # TODO: this would be better replaced with a plot built with React/D3
  # plot_distribution = target({
  #   plot_data <- detail() %>%
  #     filter(algorithm == algorithm_name,
  #            dataset == dataset_name,
  #            difficulty == difficulty_name,
  #            difficulty_type == difficulty_type_name) %>% 
  #     collect()
  #   interactive_distribution_plot(plot_data)
  #   },
  #   transform = cross(
  #     algorithm_name = !!algorithms,
  #     dataset_name = !!datasets,
  #     difficulty_name = !!difficulties,
  #     difficulty_type_name = c("expansion", "lid")
  #   )
  # ),
  #
  # figure_distribution = target(
  #   htmlwidgets::saveWidget(plot_distribution,
  #                           here("imgs", str_c("perf-distribution-",
  #                                              dataset_name, "-",
  #                                              difficulty_name, "-",
  #                                              difficulty_type_name, "-",
  #                                              algorithm_name,
  #                                              ".html"))),
  #   transform = map(plot_distribution)
  # ),
  
  data_performance_distribution_paper = detail() %>% 
    filter(dataset == "GLOVE-2M",
           difficulty %in% c("middle", "diverse"),
           difficulty_type == "lid") %>%
    collect(),
  
  plot_performance_distribution_recall = {
    p <- data_performance_distribution_paper %>%
      filter(algorithm %in% algorithms) %>% 
      static_ridges_plot_recall()
    ggsave("imgs/distribution_recall.pdf", plot=p,
           width = 8, height=4)
  },
  
  plot_performance_distribution_qps = data_performance_distribution_paper %>% 
      filter(algorithm %in% algorithms) %>% 
      static_ridges_plot_qps(),
  
  figure_performance_distribution_qps = {
    ggsave("imgs/distribution_qps.pdf", plot=plot_performance_distribution_qps,
           width = 8, height=4)
  },
  
  # ------------------ Ranking ------------------------
  plot_ranking_qps = averages %>% 
    filter(difficulty == "middle") %>% 
    filter(algorithm %in% algorithms) %>% 
    filter(dataset != "Fashion-MNIST") %>% 
    filter(difficulty_type == "lid") %>% 
    ungroup() %>% 
    recode_datasets() %>% 
    ranking_qps(),
  
  plot_ranking_qps_expansion = averages %>% 
    filter(difficulty == "middle") %>% 
    filter(algorithm %in% algorithms) %>% 
    filter(dataset != "Fashion-MNIST") %>% 
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
  
  plot_ranking_distcomps = averages_recoded %>% 
    filter(difficulty == "middle") %>% 
    filter(algorithm %in% algorithms) %>% 
    filter(dataset != "Fashion-MNIST") %>% 
    filter(difficulty_type == "expansion") %>% 
    ranking_distcomps(),
  
  ranking_distcomps_tex = {
    tikz(here("imgs", "ranking-distcomps.tex"),
         width=5.5, height=1.5)
    print(plot_ranking_distcomps)
    dev.off()
  },
  
  # ------------------ How is the difficulty? --------------------
  
  # TODO remove
  # avg_lid = target(
  #   detail %>% 
  #     filter(difficulty_type == "lid") %>% 
  #     group_by(dataset, difficulty, algorithm, difficulty_type, parameters) %>% 
  #     summarise(
  #       recall = mean(recall),
  #       qps = 1/mean(query_time)
  #     ) %>% ungroup()
  # ),
  # avg_expansion = detail %>% 
  #   filter(difficulty_type == "expansion") %>% 
  #   group_by(dataset, difficulty, algorithm, difficulty_type, parameters) %>% 
  #   summarise(
  #     recall = mean(recall),
  #     qps = 1/mean(query_time)
  #   ) %>% ungroup(),
  
  fast_accurate = averages %>% 
    filter(algorithm != "bruteforce-blas") %>% 
    filter(difficulty != "diverse") %>% 
    group_by(dataset, difficulty, difficulty_type, algorithm) %>% 
    filter(recall >= 0.9) %>% 
    slice(which.max(qps)) %>% 
    ungroup() %>% 
    mutate(difficulty = factor(difficulty, levels=c("easy", "middle", "hard"), ordered = T)),
  
  moving_configurations = averages %>% 
    semi_join(filter(fast_accurate, difficulty == "easy"), 
              by=c("dataset", "difficulty_type", "algorithm", "parameters")) %>% 
    ungroup() %>% 
    filter(difficulty != "diverse") %>% 
    mutate(difficulty = factor(difficulty, levels=c("easy", "middle", "hard"), ordered = T)),
  
  fast_accurate_arrow_plot = fast_accurate %>% 
    arrow_plot(),
  
  fast_accurate_plot_paper = {
    tikz(here("imgs", "fast-accurate.tex"),
         width = 5.5, height = 3)
    print(fast_accurate_arrow_plot)
    dev.off()
  },
  
  # ----------- Recall vs. difficulty ------------
  
  correlation_plot_data = bind_rows(
    detail() %>%
      filter(difficulty=="diverse", difficulty_type == "lid") %>% 
      group_by(dataset, algorithm, parameters, difficulty_type, difficulty) %>% 
      sqlite_cor(recall, lid),
    detail() %>%
      filter(difficulty=="diverse", difficulty_type == "lrc") %>% 
      group_by(dataset, algorithm, parameters, difficulty_type, difficulty) %>% 
      mutate(lrc = 1/log(lrc)) %>%
      sqlite_cor(recall, lrc),
    detail() %>%
      filter(difficulty=="diverse", difficulty_type == "expansion") %>% 
      group_by(dataset, algorithm, parameters, difficulty_type, difficulty) %>% 
      mutate(expansion = 1/log(expansion)) %>%
      sqlite_cor(recall, expansion)
  ) %>%
  group_by(dataset, algorithm, difficulty_type) %>% 
  summarise(corr = mean(corr, na.rm=T)),

  # correlation_lid = detail() %>% 
  #   filter(difficulty_type == "lid") %>% 
  #   collect() %>%
  #   group_by(dataset, algorithm, parameters, difficulty_type, difficulty) %>% 
  #   summarise(correlation_recall = cor(recall, lid),
  #             correlation_time = cor(query_time, lid)),
  # correlation_lrc = detail() %>% 
  #   filter(difficulty_type == "lrc") %>% 
  #   collect() %>%
  #   group_by(dataset, algorithm, parameters, difficulty_type, difficulty) %>% 
  #   summarise(correlation_recall = cor(recall, lrc),
  #             correlation_time = cor(query_time, lrc)),
  # correlation_expansion = detail() %>% 
  #   filter(difficulty_type == "expansion") %>% 
  #   collect() %>%
  #   group_by(dataset, algorithm, parameters, difficulty_type, difficulty) %>% 
  #   summarise(correlation_recall = cor(recall, expansion),
  #             correlation_time = cor(query_time, expansion)),
  # correlation = bind_rows(correlation_lid, correlation_lrc, correlation_expansion),
  # correlation_plot_data = correlation %>% 
  #   group_by(dataset, algorithm, difficulty, difficulty_type) %>% 
  #   summarise(corr = mean(correlation_recall, na.rm=T)) %>% 
  #   filter(difficulty=="diverse"),
 
  correlation_plot = correlation_plot_data %>% 
    mutate(color=if_else(corr < -0.7, "white", "black")) %>% 
    ungroup() %>% 
    mutate(difficulty_type = recode_factor(difficulty_type,
                                           "lid" = "LID",
                                           "expansion" = "1/log(Expansion)",
                                           "lrc" = "1/log(RC)")) %>% 
    ggplot(aes(dataset, algorithm, fill=corr)) +
    geom_tile() + 
    geom_text(aes(label=scales::number(corr,accuracy=.01),
                  color=color),
              size=2) + 
    facet_wrap(vars(difficulty_type)) + 
    scale_fill_continuous_diverging() + 
    scale_color_identity()+
    theme_classic() + 
    theme(axis.text.x.bottom = element_text(angle=90)),
 
  correlation_plot_paper = {
    ggsave(plot=correlation_plot,
           filename = here("imgs", "correlation-plot.png"),
           width = 16,
           height = 8,
           units = "cm")
  },
 
  recall_vs_x_plot_size = 1.7,
  detail_to_plot = detail() %>% 
    filter(dataset == "GLOVE-2M",
           parameters == "Annoy(n_trees=100, search_k=200000)",
           difficulty == "diverse"
           ) %>%
    collect(),

    
  recall_vs_lid_paper_1 = {
    tikz(file = here("imgs", "onng-recall-vs-lid.tex"),
         width = recall_vs_x_plot_size, height = recall_vs_x_plot_size)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "lid") %>% 
      # (function(d){print(head(d)); d}) %>% 
      do_plot_recall_vs_lid_single()
    print(p)
    dev.off()
  },
  
  recall_vs_expansion_paper_1 = {
    tikz(file = here("imgs", "onng-recall-vs-expansion.tex"),
         width = recall_vs_x_plot_size, height = recall_vs_x_plot_size)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "expansion") %>% 
      # filter(expansion <= 1.1) %>% 
      mutate(expansion = 1/log(expansion)) %>%
      do_plot_recall_vs_expansion_single()
    print(p)
    dev.off()
  },
 
  recall_vs_lrc_paper_1 = {
    tikz(file = here("imgs", "onng-recall-vs-rc.tex"),
         width = recall_vs_x_plot_size, height = recall_vs_x_plot_size)
    p <- detail_to_plot %>% 
      filter(difficulty_type == "lrc") %>% 
      mutate(lrc = 1/log(lrc)) %>%
      do_plot_recall_vs_lrc_single()
    print(p)
    dev.off()
  },
  
  
)
