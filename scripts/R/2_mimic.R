#######################################################
#-------- Combiner les données cliniques
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Mimic/scripts/R/toolbox/les_sources.R")

#######################################################
# -------- Lecture 
#######################################################

# Numerics 
df_long     <- readRDS(paste0(dest_data,"numerics.rds"))

# Infos de mimic 2
infos       <- readRDS(paste0(dest_data,"infos_admission.RDS"))  
infos       <- subset(infos, select = - admission_type_descr)

# Données réactualisées
curare      <- readRDS(paste0(dest_data,"curare.RDS")) 
sedate      <- readRDS(paste0(dest_data,"sedation.RDS")) 
amine       <- readRDS(paste0(dest_data,"amine.RDS")) 
ventilation <- readRDS(paste0(dest_data,"ventilation.RDS")) 

#######################################################
# --------  Recherche episode hypotensif de 10 min
#######################################################

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)
v_id <- unique(df_long$id)
recherche_episode <- foreach( sujet = v_id,.combine = rbind) %dopar% {

  periode.episode(data = base::subset(df_long, id %in% sujet),
                  drapeau = 5)

}
recherche_episode <- na.omit(recherche_episode) # 63463
#######################################################
# ------- Médicaments pdt la période
#######################################################

# Vasopresseurs 
ap <- medoc_periode(df1 = recherche_episode, df2 = amine)
colnames(ap) <- c("id","jour","amine")

# Curare
cu <- medoc_periode(df1 = recherche_episode, df2 = curare)
colnames(cu) <- c("id","jour","curare")

# Sedation
se <- medoc_periode(df1 = recherche_episode, df2 = sedate)
colnames(se) <- c("id","jour","sedate")

# Jointure
df <- recherche_episode %>%
  full_join(ap,  by = c("jour","id")) %>%
  full_join(cu,  by = c("jour","id")) %>%
  full_join(se,  by = c("jour","id")) 

df[is.na(df$amine),"amine"] 		<- 0
df[is.na(df$curare),"curare"]   <- 0
df[is.na(df$sedate),"sedate" ]  <- 0
 

#######################################################
# ------- Ventilation dans la période 
#######################################################

ventilation <- subset(ventilation, subject_id %in% df$id)
v_id <- unique(df$id)
vp <- foreach( sujet = v_id,.combine = rbind) %dopar% {

  ventilation_periode(dt1 = base::subset(df, id %in% sujet) ,
                      dt2 = base::subset(ventilation, subject_id %in% sujet))
}
stopCluster(cl)


vp <- vp %>% mutate(
    id   = as.numeric(as.character(id)),
    jour = as.POSIXct(strptime(jour,"%Y-%m-%d %H:%M:%S"))
  ) %>% unique()

df <- df %>%
  mutate(id = as.numeric(as.character(id))) %>%
  inner_join(vp,  by = c("id","jour","periode")) %>%
  unique() %>% 
  na.omit()

#########################################################
# -------- Informations sur les patients 
# #######################################################

infos <- infos %>% 
  subset(subject_id %in% df$id) %>%
  unique() %>%
  mutate(id = subject_id )
infos <- infos[!duplicated(infos$id), ]

df <- df %>%
  mutate(id = as.numeric(as.character(id))) %>%
  inner_join(infos, by = "id") %>%
  na.omit() %>%
  select_("id","gender","age","sapsi_first", "sofa_first",
          "care_unit","eevent","event","event_cum","event_24h",
          "periode","amine","curare","sedate","venti") %>%
  subset(age < 100)

df <- df[order(df$id,df$periode),]

#########################################################
# -------- Sauvegarde
# #######################################################

# vp <- readRDS(paste0(dest_data,"recherche_ventilation.rds")) 
# recherche_episode = readRDS(paste0(dest_data,"episode_periode_5.rds")) 

saveRDS(recherche_episode, file = paste0(dest_data,"episode_periode_5.rds"))
saveRDS(vp, file = paste0(dest_data,"recherche_ventilation.rds"))
saveRDS(df, file = paste0(dest_data,"df_information.rds"))

print("------------------------------------------------------------")
print("------------------ MIMIC INFORMATIONS DONE------------------")
print("------------------------------------------------------------")



