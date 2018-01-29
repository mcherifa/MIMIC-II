#######################################################
#-------- Numerics par patients
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Mimic/scripts/R/toolbox/les_sources.R")

#######################################################
# -------- Lecture 
#######################################################

df_information <- readRDS(paste0(dest_data,"df_information.rds")) %>%
  mutate(
    periode = as.character(periode),
    periode = gsub(" ","",periode),
    identif = paste(as.character(id),
                    as.character(periode),sep="-"),
    id = as.character(id)
  )  

df_long <- readRDS(paste0(dest_data,"numerics.rds")) %>% 
  subset(id %in% df_information$id) %>%
  drop.levels(id)

#######################################################
# -------- Découpage des numérics par périodes d'observation
#######################################################

# Période de 90 min

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)
v_id <- unique(df_long$id)

df_numerics <- foreach( sujet = v_id,.combine = rbind) %dopar% {
  
  numerics_episode(base::subset(df_long, id %in% sujet))
}

df_numerics$id <- drop.levels(df_numerics$id)

# Période de 60 min

v_id <- unique(df_numerics$id)

df_numerics_sp <- foreach( sujet = v_id,.combine = rbind) %dopar% {
  
  soixante_prems(base::subset(df_numerics, id %in% sujet))
}

# Suppression des NA sur les périodes 

df_numerics_sp <- na.omit(df_numerics_sp)

nombre_time <- sum(table(df_numerics_sp$id, df_numerics_sp$periode)==60)

df_numerics_sp <- df_numerics_sp %>% 
  mutate(
  	id = drop.levels(id),
    time = rep(seq(1,60,1),nombre_time),
    identif = paste(as.character(id),
                    as.character(periode),sep="-")
  )
stopCluster(cl)

#######################################################
# -------- Découpage des numérics par périodes d'observation
#######################################################

no_cores <- detectCores() - 1
cl       <- makeCluster(no_cores, type = "FORK")
registerDoParallel(cl)

v_id <- unique(df_numerics_sp$id)
transp = NULL
for (sujet in v_id){
  print(sujet)
  transp <- rbind(transp, to.balanced(base::subset(df_numerics_sp, id %in% sujet),
                               match("identif",names(df_numerics_sp)),
                               match("time", names(df_numerics_sp)),
                               match(c("hr","spo2","abpsys","abpdias",
                                       "abpmean"),names(df_numerics_sp))))
}


transp <- transp %>% mutate(
    identif = as.character(identif),
    id = as.character(identif),
    id = as.character(str_match(id,"(.*)-")[,2]),
    periode = as.character(str_match(identif,"-(.*)")[,2])
)

#######################################################
# ------- Data final : wide
#######################################################                           

df_final_wide <- inner_join(df_information, transp, 
                            by = c("identif","id","periode")) %>% 
                            na.omit()

#######################################################
# ------- Data final : long
#######################################################   

# df_information = readRDS(paste0(dest_data,"df_information.rds"))
# df_numerics_sp = readRDS(paste0(dest_data,"episode_periode_numerics.rds"))

temp <- merge(df_information,df_numerics_sp, by.x =c("identif", "id","periode"),
              by.y = c("identif", "id","periode"),all.x = T,all.y = T)  

temp <- subset(temp, id %in% df_final_wide$id)                        

temp_num <- temp[,c("identif","age","sapsi_first","sofa_first",
                    "hr", "abpsys", "abpdias", "abpmean", "spo2")]

temp_long <- aggregate(. ~ identif, data = temp_num, mean)

temp_infos <- unique(temp[,c("identif","id", "periode", "gender",
                             "care_unit","eevent", "event",
                             "event_cum", "event_24h",
                             "amine", "curare","sedate","venti")])

df_final_long <- merge( temp_infos, temp_long,
                        by.x ="identif",
                        by.y="identif",
                        all.x = F,all.y = T) 

df_final_long <- subset(df_final_long, select = -identif)

saveRDS(df_numerics_sp, file = paste0(dest_data,"episode_periode_numerics.rds"))
saveRDS(transp, file = paste0(dest_data,"transp.rds"))
saveRDS(df_final_wide, file = paste0(dest_data,"fichier_wide_periode.rds"))
saveRDS(df_final_long, file = paste0(dest_data,"fichier_long_periode.rds"))     

print("------------------ DONE------------------")


# fichier de fabrice 
# df_numerics_sp <- read.csv("numerics_fabrice_a_transformer.csv")
# df_numerics_sp <- subset(df_numerics_sp,select=c(id_fabrice, event, eevent, id,periode))

#df_numerics_sp <- df_numerics_sp[which(is.na(df_numerics_sp$periode) == F &
#                                   is.na(df_numerics_sp$event) == F &
#                                   is.na(df_numerics_sp$spo2) == F),]


