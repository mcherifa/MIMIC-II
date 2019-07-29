source("/home/menyssa/Mimic/scripts/R/toolbox/les_sources.R")

df_wide <- readRDS(paste0(dest_data,"fichier_wide_periode.rds")) 
df_numerics_sp <- readRDS(paste0(dest_data,"episode_periode_numerics.rds"))

df_numerics_sp$id <- as.numeric(as.character(df_numerics_sp$id))
df_numerics_sp$periode <- as.numeric(as.character(df_numerics_sp$periode))

df_wide$periode <- as.numeric(as.character(df_wide$periode))
df_wide$id <- as.numeric(as.character(df_wide$id))
df_wide$outcome <- df_wide$event 
df_wide <- subset(df_wide, select = c( id, age, gender,
                                       periode, amine, curare,
                                       sedate, venti, outcome))

df <- inner_join(df_wide, df_numerics_sp, 
                            by = c("id","periode"))

saveRDS(df, file = paste0(dest_data,"mimicII.rds"))
write.csv(df, file = paste0(dest_data,"mimicII.csv"))
