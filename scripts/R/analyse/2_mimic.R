#######################################################
#-------- Combiner les données cliniques
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

source("~/Recherche/Mimic-II/scripts/R/toolbox/fonctions.R")

library(parallel)
library(doParallel)
library(data.table)
library(dplyr)

chemin <- "~/Recherche/Mimic-II/data/clean/mimic2"
    
# Numerics 
numerics <- readRDS(paste0(chemin,"/numerics.rds"), refhook = NULL)

# Données réactualisées
load(file.path(chemin,"sedation.RData"))
load(file.path(chemin,"amine.RData"))
load(file.path(chemin,"ventilation.RData"))

# infos_admission de mimic 2
load(file.path(chemin,"infos_admission.RData"))
length(unique(infos_admission$subject_id))

# Différence entre patients avec numerics et sans numerics 
# Patients qui ont des donneés numerics mais pas des infos cliniques
setdiff(numerics$id, infos_admission$subject_id)

# Patients qui ont des donneés infos cliniques mais pas de numerics
setdiff(infos_admission$subject_id, numerics$id)

#- 1  Recherche episode hypotensif de 5 min (drapeau)
 no_cores <- detectCores() - 1
 cl <- makeCluster(no_cores, type="FORK")
 registerDoParallel(cl)
 v_id <- unique(numerics$id)
 recherche_episode <- foreach(sujet = v_id,.combine = rbind) %dopar% {
   periodeAHE(dossier_patient = sujet,  outcome = 65)
 }
load("~/Recherche/Mimic-II/resultats_rewieving/recherche_episode.RData")
recherche_episode$debut_periode <-  as.POSIXct(strptime(recherche_episode$debut_periode,"%Y-%m-%d %H:%M:%S"))
recherche_episode <- na.omit(recherche_episode) # 63463

# - 2 Médicaments pdt la période
# Vasopresseurs
ap = se = vp = NULL
for(sujet in  unique(recherche_episode$id)){
  cat(sujet, "\n")
  ap <- rbind(ap, medoc_periode(df1 = recherche_episode, df2 = amine, sujet = sujet, type = "amine"))
}
# Sedation
for(sujet in  unique(recherche_episode$id)){
  cat(sujet, "\n")
  se <- rbind(se, medoc_periode(df1 = recherche_episode, df2 = sedation, sujet = sujet, type = "sedation"))
} 

# Jointure
recherche_episode$subject_id <- recherche_episode$id
recherche_episode$start <- recherche_episode$debut_periode
df <- recherche_episode %>%  
  full_join(ap,  by = c("start","subject_id")) %>%
  full_join(se,  by = c("start","subject_id","end"))

# - 3 Ventilation dans la période
ventilation <- subset(ventilation, mechvent == 1)

# Ventilation
for(sujet in  unique(recherche_episode$id)){
  cat(sujet, "\n")
  vp <- rbind(vp,  ventilation_periode(df = df, sujet = sujet))
}

# 3 - Liaison entre sejour et numerics et outcome
patient.record <- unique(vp$id)
all_sejour <- NULL

for(pp in patient.record){ #10419
  cat(pp, "\t")
  clinical <- subset(infos_admission, subject_id == pp) %>%
    dplyr::mutate(
      date_admission_icu = as.POSIXct(strptime(admission_icu,"%Y-%m-%d")),
      date_admission_hos = as.POSIXct(strptime(admission_hos,"%Y-%m-%d")),
      date_discharge_icu  = as.POSIXct(strptime(discharge_icu,"%Y-%m-%d")),
      date_discharge_hos  = as.POSIXct(strptime(discharge_hop,"%Y-%m-%d"))
    )
  date_admission_sejour <-  data.frame(hos = clinical$date_admission_hos,
                                       icu = clinical$date_admission_icu)
  date_admission_sejour <-  unique(transform(date_admission_sejour, min = pmin(hos, icu))$min)
  
  date_sortie_sejour <-  data.frame(hos = clinical$date_discharge_hos,
                                    icu = clinical$date_discharge_icu)
  date_sortie_sejour <-   unique(transform(date_sortie_sejour, max = pmax(hos, icu))$max)
  
  out_come <- na.omit(subset(vp, id == pp)) %>%
    dplyr::mutate(debut_periode = as.POSIXct(strptime(debut_periode,"%Y-%m-%d %H:%M:%S")),
                  fin_periode = as.POSIXct(strptime(debut_periode,"%Y-%m-%d %H:%M:%S")) + (60 * 90),
                  date_debut_periode = as.POSIXct(strptime(debut_periode,"%Y-%m-%d")))

  
  for(i in 1:length(date_admission_sejour)){
  	all_sejour_patient <- NULL
    tmp_out_come <-  dplyr::filter(out_come,(date_debut_periode >= date_admission_sejour[i]))
    tmp_out_come <-  dplyr::filter(tmp_out_come,(fin_periode <= date_sortie_sejour[i]))
    
    sejour <- merge(clinical, out_come, by.x ="subject_id", by.y = "subject_id")
    all_sejour_patient <- rbind(all_sejour_patient, sejour)
  }
  all_sejour <- rbind(all_sejour, all_sejour_patient)
}

# 4 - Informations sur les patients 
df_information <- all_sejour
#  select(-c(admission_icu, discharge_icu, admission_hos, discharge_hop,
#            debut_periode, id, start, end))

df_information <- unique(df_information[order(df_information$subject_id,df_information$periode),])

# 4 - Sauvegarde
save(df_information, numerics, vp, recherche_episode,
     file = "~/Recherche/Mimic-II/data/clean/mimic2/2_mimic.RData")

print("------------------------------------------------------------")
print("------------------ MIMIC INFORMATIONS DONE------------------")
print("------------------------------------------------------------")
# infos_admission$had_numerics <- ifelse(infos_admission$subject_id %in% numerics$id, 1, 0)
# infos_admission$had_numerics_90min <- ifelse(infos_admission$subject_id %in% recherche_episode$id, 1, 0)
# infos_admission$had_numerics_no90min <- ifelse((infos_admission$subject_id %in% numerics$id) &
#                                                  !(infos_admission$subject_id %in% recherche_episode$id) , 1, 0)
# save(infos_admission, file = "~/Recherche/Mimic-II/data/clean/mimic2/infos_admission.RData")
# save(recherche_episode, file = "~/Recherche/Mimic-II/resultats_rewieving/recherche_episode.RData")
