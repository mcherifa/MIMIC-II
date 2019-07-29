setwd(dir = "~/Recherche/Mimic-II/shiny/")

source(file = "packages.R")

informations <- readRDS(file = "data/df_information.rds") %>%    
  subset(eevent == 0)

recherche    <- readRDS(file = "data/episode_periode_5.rds")
df_long.n    <- readRDS("data/numerics.rds") %>% 
  subset(id %in% informations$id) %>% 
  mutate(factor_id = as.factor(id))
  
saveRDS(informations,   file = "data/df_information.rds")
saveRDS(df_long.n, file = "data/numerics.rds")

