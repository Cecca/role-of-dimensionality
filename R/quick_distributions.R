# Draw distribution plots for scores in the workloads directory
library(tidyverse)
library(cowplot)
library(ggridges)
library(here)

theme_set(theme_cowplot())

lid_files <- Sys.glob(here("workloads/*-lid-*"))
rc_files <- Sys.glob(here("workloads/*-rc-*_*"))

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
lids <- bind_rows(lids)

p <- ggplot(lids, aes(x=lid, y=factor(k), fill=factor(k))) + 
  geom_density_ridges(scale=0.9) + 
  facet_wrap(vars(dataset), ncol=1) +
  labs(title="Local intrinsic dimensionality for different values of k")

ggsave(p, filename="lids.png", width=10, height=6, dpi=300)


rcs <- c()
for (f in rc_files) {
  if (is.na(str_match(f, "queries"))) {
    print(paste("parsing ", f))
    k <- str_match(f, "rc-(\\d+_\\d+)")[2]
    dataset <- basename(str_match(f, "(.*)-rc")[2])
    part <- read_delim(f, col_names=c("id", "rc"), col_types="id", delim=" ") %>%
      mutate(k = k, dataset=dataset) %>%
      drop_na()
    rcs <- c(rcs, list(part))
  }
}
rcs <- bind_rows(rcs)

p <- ggplot(rcs, aes(x=rc, y=factor(k), fill=factor(k))) + 
  geom_density_ridges(scale=0.9) + 
  facet_wrap(vars(dataset), ncol=1) +
  scale_x_continuous(limits=c(NA, 1.5)) +
  labs(title="Relative contrast under various parameterizations",
       caption=str_wrap("Values higher than 1.5 are not reported for readability, since this distributions are very skewed"))

ggsave(p, filename="rcs.png", width=10, height=6, dpi=300)

