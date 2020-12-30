# Draw distribution plots for scores in the workloads directory
source("R/packages.R")
source("R/functions.R")

theme_set(theme_cowplot())

if (!dir.exists(here("imgs"))) {
  dir.create(here("imgs"))
}

lid_files <- Sys.glob(here("workloads/*-lid-*"))
expansion_files <- Sys.glob(here("workloads/*-expansion-*"))
rc_files <- Sys.glob(here("workloads/*-rc-*"))

scores_plan <- drake_plan(
  lid_scores = target(
    {
      lids <- c()
      for (f in lid_files) {
        if (is.na(str_match(f, "queries"))) {
          k <- as.integer(str_match(f, "lid-(\\d+)")[2])
          if (k %in% c(10, 100)) {
            cat(paste("parsing ", f, " \n"))
            dataset <- basename(str_match(f, "(.*)-lid")[2])
            part <- read_delim(f, col_names=c("id", "lid"), col_types="id", delim=" ") %>%
              mutate(k = k, dataset=dataset) %>%
              drop_na()
            lids <- c(lids, list(part))
          }
        }
      }
      bind_rows(lids) %>% 
        recode_datasets() %>%
        as.data.table()
    },
    format = "fst_dt"
  ),

  rc_scores = target(
    {
      rcs <- c()
      for (f in rc_files) {
        if (is.na(str_match(f, "queries"))) {
          k <- str_match(f, "rc-(\\d+)")[2]
          if (k %in% c(10, 100)) {
            cat(paste("parsing ", f, "\n"))
            dataset <- basename(str_match(f, "(.*)-rc")[2])
            part <- read_delim(f, col_names=c("id", "rc"), col_types="id", delim=" ") %>%
              mutate(k = k, dataset=dataset) %>%
              drop_na()
            rcs <- c(rcs, list(part))
          }
        }
      }
      bind_rows(rcs) %>% 
        group_by(dataset, k) %>%
        mutate(
          rescaling = case_when(
            k == 10 ~ log(n()/20),
            k == 100 ~ log(n()/200)
          ),
          logrc = rescaling/log(rc)
        ) %>%
        select(-rescaling) %>%
        recode_datasets() %>%
        ungroup() %>%
        as.data.table()
    },
    format = "fst_dt"
  ),

  expansion_scores = target(
    {
      expansions <- c()
      for (f in expansion_files) {
        if (is.na(str_match(f, "queries"))) {
          k <- str_replace(str_match(f, "expansion-(\\d+_\\d+)")[2], "_", "/")
          if (k %in% c("10/20", "5/100")) {
            cat(paste("parsing ", f, "\n"))
            dataset <- basename(str_match(f, "(.*)-expansion")[2])
            part <- read_delim(f, col_names=c("id", "expansion"), 
                              col_types="id", 
                              delim=" ") %>%
              mutate(k = k, dataset=dataset) %>%
              drop_na()
            expansions <- c(expansions, list(part))
          }
        }
      }
      bind_rows(expansions) %>%
        mutate(
          rescaling = case_when(
            k == "10/20" ~ log(2),
            k == "5/100" ~ log(20)
          ),
          logexp = rescaling/log(expansion)
        ) %>%
        select(-rescaling) %>%
        recode_datasets() %>%
        as.data.table()
    },
    format = "fst_dt"
  ),

  lid_displacement =
    {
      dt <- dcast(lid_scores, dataset + id ~ k, value.var="lid")
      ranks <- dt %>% 
        as_tibble() %>% 
        group_by(dataset) %>% 
        mutate(rank10 = row_number(desc(`10`)),
               rank100 = row_number(desc(`100`)))
    },

  rc_displacement =
    {
      dt <- dcast(rc_scores, dataset + id ~ k, value.var="rc")
      ranks <- dt %>% 
        as_tibble() %>% 
        group_by(dataset) %>% 
        mutate(rank10 = row_number(desc(`10`)),
               rank100 = row_number(desc(`100`)))
    },

  expansion_displacement =
    {
      dt <- dcast(expansion_scores, dataset + id ~ k, value.var="expansion")
      ranks <- dt %>% 
        as_tibble() %>% 
        group_by(dataset) %>% 
        mutate(rank100 = row_number(desc(`5/100`)),
               rank20 = row_number(desc(`10/20`)))
    },

  plot_lid_ridges = {
    p <- plot_displacement_ridges(lid_displacement, rank_accurate=rank100, rank_less_accurate=rank10)
    save_figure(plot=p, 
                basename="imgs/lidRidges", 
                tex_width=5, tex_height=2.8,
                png_width=8, png_height=5)
  },
  plot_rc_ridges = {
    p <- plot_displacement_ridges(rc_displacement, rank_accurate=rank100, rank_less_accurate=rank10)
    save_figure(plot=p, 
                basename="imgs/rcRidges", 
                tex_width=5, tex_height=2.8,
                png_width=8, png_height=5)
  },
  plot_expansion_ridges = {
    p <- plot_displacement_ridges(expansion_displacement, rank_accurate=rank100, rank_less_accurate=rank20)
    save_figure(plot=p, 
                basename="imgs/expansionRidges", 
                tex_width=5, tex_height=2.8,
                png_width=8, png_height=5)
  },

  plot_lid_distribution = {
    p <- plot_score_distribution(lid_scores, lid, k, param_high=100, param_low=10, xlab="Local intrinsic dimensionality", xmax=150)
    save_figure(plot=p, 
                basename="imgs/density-lid", 
                tex_width=2.8, tex_height=2.5,
                png_width=5, png_height=3)
  },  
  plot_rc_distribution = {
    p <- plot_score_distribution(rc_scores, logrc, k, param_high=100, param_low=10, xlab="Relative contrast dimension", reverse=FALSE)
    save_figure(plot=p, 
                basename="imgs/density-rc", 
                tex_width=2.8, tex_height=2.5,
                png_width=5, png_height=3)
  },  
  plot_exp_distribution = {
    p <- plot_score_distribution(expansion_scores, logexp, k, param_high="10/20", param_low="5/100", xlab="Expansion dimension", xmax=200)
    save_figure(plot=p, 
                basename="imgs/density-expansion", 
                tex_width=2.8, tex_height=2.5,
                png_width=5, png_height=3)
  },

  # ------ Scores correlation --------
  data_scores_paper = inner_join(
    lid_scores %>% filter(dataset == "GLOVE", k==100) %>% select(dataset, id, lid),
    rc_scores %>% filter(dataset == "GLOVE", k==100) %>% select(dataset, id, logrc)
  ) %>%
  inner_join(
    expansion_scores %>% filter(dataset == "GLOVE", k=="10/20") %>% select(dataset, id, logexp)
  ) %>%
  sample_n(10000),

  plot_scores_lid_exp = {
    p <- do_scatter_distribution(data_scores_paper, lid, logexp, "LID", "Expansion dimension")
    ggsave(filename = here("imgs","GLOVE-scores-lid-exp.png"),
           plot = p,
           width=8, height=8,
           units = "cm")
  },
  plot_scores_lid_rc = {
    p <- do_scatter_distribution(data_scores_paper, lid, logrc, "LID", "RC dimension")
    ggsave(filename = here("imgs","GLOVE-scores-lid-lrc.png"),
           plot = p,
           width=8, height=8,
           units = "cm")
  },
  plot_scores_rc_exp = {
    p <- do_scatter_distribution(data_scores_paper, logrc, logexp, "RC dimension", "Expansion dimension")
    ggsave(filename = here("imgs","GLOVE-scores-lrc-exp.png"),
           plot = p,
           width=8, height=8,
           units = "cm")
  },
  
  correlation_table = {
    inner_join(
      lid_scores %>% filter(k == 100) %>% select(-k),
      rc_scores %>% filter(k == 100) %>% select(-k)
    ) %>%
    inner_join(
      expansion_scores %>% filter(k == "10/20") %>% select(-k)
    ) %>%
    arrange(dataset, id) %>%
    group_by(dataset) %>%
    summarise(
      lid_rc =  cor(lid,    logrc) %>% scales::number(accuracy=0.001),
      exp_rc =  cor(logexp, logrc) %>% scales::number(accuracy=0.001),
      lid_exp = cor(lid,    logexp) %>% scales::number(accuracy=0.001)
    ) %>%
    knitr::kable(format="latex", booktabs=T)
  },

)

