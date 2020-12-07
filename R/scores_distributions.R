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
          print(paste("parsing ", f))
          k <- as.integer(str_match(f, "lid-(\\d+)")[2])
          dataset <- basename(str_match(f, "(.*)-lid")[2])
          part <- read_delim(f, col_names=c("id", "lid"), col_types="id", delim=" ") %>%
            mutate(k = k, dataset=dataset) %>%
            drop_na()
          lids <- c(lids, list(part))
        }
      }
      bind_rows(lids) %>% as.data.table()
    },
    format = "fst_dt"
  ),

  rc_scores = target(
    {
      rcs <- c()
      for (f in rc_files) {
        if (is.na(str_match(f, "queries"))) {
          print(paste("parsing ", f))
          k <- str_match(f, "rc-(\\d+)")[2]
          dataset <- basename(str_match(f, "(.*)-rc")[2])
          part <- read_delim(f, col_names=c("id", "rc"), col_types="id", delim=" ") %>%
            mutate(k = k, dataset=dataset) %>%
            drop_na()
          rcs <- c(rcs, list(part))
        }
      }
      bind_rows(rcs) %>% 
        mutate(logrc = 1/log(rc)) %>%
        as.data.table()
    },
    format = "fst_dt"
  ),

  expansion_scores = target(
    {
      expansions <- c()
      for (f in expansion_files) {
        if (is.na(str_match(f, "queries"))) {
          print(paste("parsing ", f))
          k <- str_replace(str_match(f, "expansion-(\\d+_\\d+)")[2], "_", "/")
          dataset <- basename(str_match(f, "(.*)-expansion")[2])
          part <- read_delim(f, col_names=c("id", "expansion"), 
                            col_types="id", 
                            delim=" ") %>%
            mutate(k = k, dataset=dataset) %>%
            drop_na()
          expansions <- c(expansions, list(part))
        }
      }
      bind_rows(expansions) %>%
        mutate(logexp = 1/log(expansion)) %>%
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
        mutate(rank10 = percent_rank(desc(`10`)),
               rank100 = percent_rank(desc(`100`)))
    },

  rc_displacement =
    {
      dt <- dcast(rc_scores, dataset + id ~ k, value.var="rc")
      ranks <- dt %>% 
        as_tibble() %>% 
        group_by(dataset) %>% 
        mutate(rank10 = percent_rank(desc(`10`)),
               rank100 = percent_rank(desc(`100`)))
    },

  expansion_displacement =
    {
      dt <- dcast(expansion_scores, dataset + id ~ k, value.var="expansion")
      ranks <- dt %>% 
        as_tibble() %>% 
        group_by(dataset) %>% 
        mutate(rank100 = percent_rank(desc(`10/100`)),
               rank20 = percent_rank(desc(`10/20`)))
    },

  plot_lid_displacement = plot_displacements(lid_displacement, x=rank10, y=rank100, filename=file_out("imgs/lid_displacement.png")),
  plot_rc_displacement = plot_displacements(rc_displacement, x=rank10, y=rank100, filename=file_out("imgs/rc_displacement.png")),
  plot_expansion_displacement = plot_displacements(expansion_displacement, x=rank100, y=rank20, filename=file_out("imgs/expansion_displacement.png")),

  plot_lid_distribution = {
    p <- plot_score_distribution(lid_scores, lid, k, param_high=100, param_low=10, xlab="Local intrinsic dimensionality")
    save_figure(plot=p, 
                basename="imgs/density-lid", 
                tex_width=2.8, tex_height=2.25,
                png_width=5, png_height=3)
  },  
  plot_rc_distribution = {
    p <- plot_score_distribution(rc_scores, logrc, k, param_high=100, param_low=10, xlab="1/log(Relative contrast)", reverse=TRUE)
    save_figure(plot=p, 
                basename="imgs/density-rc", 
                tex_width=2.8, tex_height=2.25,
                png_width=5, png_height=3)
  },  
  plot_exp_distribution = {
    p <- plot_score_distribution(expansion_scores, logexp, k, param_high="10/100", param_low="10/20", xlab="1/log(Expansion)", xmax=200)
    save_figure(plot=p, 
                basename="imgs/density-expansion", 
                tex_width=2.8, tex_height=2.25,
                png_width=5, png_height=3)
  },  

)

