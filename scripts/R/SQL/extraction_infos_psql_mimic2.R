#######################################################
#-------- Extraction des données cliniques dans la base MIMIC 2
# By Menyssa CHERIFA 
# GITHUB : https://github.com/mcherifa/MIMIC
#######################################################

library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(VIM)

# chemin données
dest_data <- "~/Mimic/data/clean/"

# Connexion à la base SQL 
source("~/Mimic/scripts/R/SQL/connection_mimic2.R")

#######################################################
# -------- Tables présentent dans la base PSQL 
#######################################################

dbListTables(con)

#######################################################
# -------- Informations sur les patients : à l'admission 
#######################################################

infos.mimic2 <- dbGetQuery(con,"
	select subject_id, icustay_id, hadm_id, gender,
	round(icustay_admit_age) as age, 
	((hospital_admit_dt -  dob) / 365.25 ) as age_bis,
	height, weight_first, sapsi_first, sofa_first
	from icustay_detail 
	where icustay_admit_age IS NOT NULL 
	order by subject_id;"
) 

# Motif admission : pas standardisé

motif_admission <- dbGetQuery(con,"
	select subject_id, admission_type_descr
	from demographic_detail ;"
)

# Type réanimation (première unité)

type_reanimation <- dbGetQuery(con,"
select subject_id, icustay_id,
case
  when icustay_first_careunit = 'MICU' then 1
  when icustay_first_careunit = 'SICU' then 2
  when icustay_first_careunit = 'TSICU' then 3
  when icustay_first_careunit = 'CSRU' then 4
  when icustay_first_careunit = 'NICU' then 5
  else 6 end as care_unit
from icustay_detail
;")


infos_admission <- infos.mimic2 %>%
  full_join(motif_admission,  by = c("subject_id")) %>%
  full_join(type_reanimation,  by = c("subject_id","icustay_id")) 

infos_admission <- subset(infos_admission, 
                          select = c(subject_id,gender,age, sapsi_first,
                                     sofa_first, admission_type_descr, care_unit
                          ))

# 14293 patients
saveRDS(infos_admission, paste0(dest_data,"infos_admission.RDS"))


#######################################################
# -------- Informations sur les patients : données réactualisées
#######################################################

# Lactate 

lactate <- dbGetQuery(con,"
	select  subject_id, icustay_id, charttime, valuenum,
	RANK() OVER (PARTITION BY icustay_id ORDER BY charttime ASC) AS rank  
	from labevents
	where itemid = 50010;"
)

# Vasopresseurs

amine <- dbGetQuery(con,"
		select subject_id, itemid, starttime, startrealtime, endtime, duration 	
		from a_meddurations
		where itemid in(42,43,44,46,47,51,119,120,
										125,127,128,306,307,309,311)"
)

# Sédation : 'Propofol' or'Midazolam' or Ketamine

sedation <- dbGetQuery(con,"
		select subject_id, itemid, starttime, startrealtime, endtime, duration 	
		from a_meddurations
		where itemid in(124,131,151,130)"
)

# Curare : Atracurium Cisatracurium Diltiazem Doxacurium Fentanyl

curare <- dbGetQuery(con,"
		select subject_id, itemid, starttime, startrealtime, endtime, duration 	
		from a_meddurations
		where itemid in(113,114,116,129,138,136);"
)

# Hypovolémie  

hypovolemie <- dbGetQuery(con,"
		select subject_id, itemid, starttime, startrealtime, endtime, duration 	
		from a_meddurations
		where itemid in(18,352,158,160,159,60,61,11,
                    10,209,21,190,101,102,107,108,
                    298,296);"
)

# Ventilation 

ventilation <- dbGetQuery(con,"
		select subject_id,icustay_id, charttime, 
		max(
    case
      when itemid is null or value1 is null then 0 
      when itemid = 720 and value1 != 'Other/Remarks' THEN 1  
      when itemid = 223848 and value1 != 'Other' THEN 1
      when itemid = 223849 then 1 -- ventilator mode
      when itemid = 467 and value1 = 'Ventilator' THEN 1 
      when itemid = 648 and value1 = 'Intubated/trach' THEN 1 
      when itemid = 223900 and value1 = 'No Response-ETT' THEN 1
      when itemid in
        (
        445, 448, 449, 450, 1340, 1486, 1600, 224687, 
        639, 654, 681, 682, 683, 684,224685,224684,224686,
        218,436,535,444,459,224697,224695,224696,224746,224747, 
        221,1,1211,1655,2000,226873,224738,224419,224750,227187, 
        543, 5865,5866,224707,224709,224705,224706, 
        60,437,505,506,686,220339,224700,3459,501,502,
        503,224702,223,667,668,669,670,671,672,
        157,158,1852,3398,3399,3400,3401,3402,3403,
        3404,8382,227809,227810, 224701 
        )
        THEN 1
      else 0
    end
    ) as MechVent
    , max(
      case when itemid is null or value1 is null then 0
        when itemid = 640 and value1 = 'Extubated' then 1
        when itemid = 640 and value1 = 'Self Extubation' then 1
        when itemid = 226732 and value1 in
        (
          'Nasal cannula', 
          'Face tent', 
          'Aerosol-cool',
          'Trach mask ', 
          'High flow neb',
          'Non-rebreather',
          'Venti mask ', 
          'Medium conc mask ',
          'T-piece', 
          'High flow nasal cannula',
          'Ultrasonic neb', 
          'Vapomist'
        ) then 1
        when itemid = 467 and value1 in
        (
          'Cannula', 
          'Nasal Cannula', 
          'None', 
          'Face Tent', 
          'Aerosol-Cool',
          'Trach Mask',
          'Hi Flow Neb',
          'Non-Rebreather',
          'Venti Mask',
          'Medium Conc Mask', 
          'Vapotherm', 
          'T-Piece', 
          'Hood', 
          'Hut',
          'TranstrachealCat', 
          'Heated Neb',
          'Ultrasonic Neb'
        ) then 1
      else 0
      end
      )
      as Extubated
    , max(
      case when itemid is null or value1 is null then 0
       when itemid = 640 and value1 = 'Self Extubation' then 1
      else 0
      end
      )
      as SelfExtubated
	from chartevents 
	where value1 is not null
	and itemid in(
     648, 223900, 
     720, 223849, 
     223848, 
     445, 448, 449, 450, 1340, 1486, 1600, 224687, 
     639, 654, 681, 682, 683, 684,224685,224684,224686, 
     218,436,535,444,224697,224695,224696,224746,224747,
     221,1,1211,1655,2000,226873,224738,224419,224750,227187, 
     543, 5865,5866,224707,224709,224705,224706,
     60,437,505,506,686,220339,224700,
    3459, 
     501,502,503,224702,
     223,667,668,669,670,671,672, 
     157,158,1852,3398,3399,3400,3401,3402,3403,3404,8382,227809,227810 ,
     224701,640,468 ,469,470,471,227287, 226732, 
     223834,
     467)
	group by subject_id , icustay_id, charttime;
")


saveRDS(amine,         paste0(dest_data,"amine.RDS"))
saveRDS(lactate,       paste0(dest_data,"lactate.RDS"))
saveRDS(sedation,      paste0(dest_data,"sedation.RDS"))
saveRDS(curare,        paste0(dest_data,"curare.RDS"))
saveRDS(ventilation,   paste0(dest_data,"ventilation.RDS"))
saveRDS(hypovolemie,   paste0(dest_data,"hypovolemie.RDS"))



# test <- dbGetQuery(con,"
#	select * from icustay_id 
#	where false
# ; "
# )
# write.csv(test,"test.csv")





