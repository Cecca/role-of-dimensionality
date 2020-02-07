library(tidyverse)
library(here)

expansions <- read_delim(here("glove-2m-300-angular-expansion.txt"),
                         delim = " ",
                         col_names = c("id", "expansion")) %>% 
  mutate(
    rank = rank(expansion),
  
  )

n_rows <- nrow(expansions)

lines <- expansions %>% 
  arrange(expansion) %>% 
  mutate(row = row_number()) %>% 
  filter(row %in% c(10000, n_rows - 10000))

qplot(expansion, data=expansions, geom='density') +
  geom_rug() +
  geom_vline(data=lines, mapping=aes(xintercept=expansion)) +
  scale_x_log10()

expansions %>% 
  mutate(group = ntile(expansion, 10000)) %>% 
  group_by(group) %>% 
  sample_n(1) %>% 
  ggplot(aes(expansion)) +
  geom_density() +
  geom_rug()





