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

# Connexion à la base SQL 
source("~/Recherche/Mimic-II/scripts/R/SQL/connection_mimic2.R")

#######################################################
# -------- Tables présentent dans la base PSQL 
#######################################################

dbListTables(con)

#######################################################
# -------- Informations sur les patients : à l'admission 
#######################################################

infos.mimic2 <- dbGetQuery(con,"
WITH tmp as
(
    SELECT adm.hadm_id, adm.subject_id, adm.admit_dt, adm.disch_dt,
    pat.subject_id, pat.dob , pat.dod
    , ROW_NUMBER() OVER (PARTITION BY hadm_id ORDER BY admit_dt) AS FirstAdmission
    FROM admissions adm
    FULL JOIN d_patients pat
    ON adm.subject_id = pat.subject_id
    -- Hospitalisations des plus de 15 ans 
    -- AND extract(YEAR FROM admit_dt) - extract(YEAR FROM dob) > 15
)
SELECT
 	-- table icustayevents ie
  ie.subject_id,
  ie.icustay_id,
  ie.intime as admission_icu,
  ie.outtime as discharge_icu, 
  -- table temporaire temp
  temp.dob as date_of_birth, 
  temp.dod as date_of_dead,
  temp.admit_dt as admission_hos,
  temp.disch_dt as discharge_hop,
  -- Table icustay_detail ic
  case
  when ic.icustay_first_careunit = 'MICU'  then 1
  when ic.icustay_first_careunit = 'SICU'  then 2
  when ic.icustay_first_careunit = 'TSICU' then 3
  when ic.icustay_first_careunit = 'CSRU'  then 4
  when ic.icustay_first_careunit = 'NICU'  then 5
  else 6 end as care_unit,
  ic.gender, 
	round(cast(ic.icustay_admit_age AS NUMERIC),0) as age, 
	ic.weight_first / ((ic.height/ 100)^2) AS bmi,
  ic.sapsi_first, 
  ic.sofa_first,
	round(cast(ic.hospital_los / (24*60) AS NUMERIC),0) as los_hospital,
  ic.hospital_expire_flg as hospital_death,
	ic.icustay_first_careunit as origin,
  round(cast(ic.icustay_los / (24*60) AS NUMERIC),0) as los_icu,
  ic.icustay_expire_flg as icu_death,
  round(cast(CASE WHEN temp.dod < temp.admit_dt + interval '30' day THEN 1 ELSE 0 END AS NUMERIC),0)as J28_death,
  round(cast(CASE WHEN temp.dod < temp.admit_dt + interval '1' year  THEN 1 ELSE 0 END AS NUMERIC),0) as Y1_death,
  RANK() OVER (PARTITION BY ic.subject_id ORDER BY ic.icustay_intime) AS rank,
  -- table demographique
  dem.admission_type_descr,
  -- table diagnostique
  dia.sequence, 
  dia.code,
  dia.description
  from icustayevents ie
	FULL join icustay_detail ic
	on ic.icustay_id = ie.icustay_id
	FULL join tmp temp
	on temp.hadm_id = ic.hadm_id
	FULL join icd9 dia
	on temp.hadm_id = dia.hadm_id
	FULL join demographic_detail dem
	on temp.hadm_id = dem.hadm_id
	-- Premier diagnostique
	--where sequence < 6
  ;"
)

infos.mimic2.dialyse <- dbGetQuery(con,"
  select ie.icustay_id
    , max(
        case
          when ce.itemid in (152,148,149,146,147,151,150) and value1 is not null then 1
          when ce.itemid in (229,235,241,247,253,259,265,271) and value1 = 'Dialysis Line' then 1
          when ce.itemid = 582 and value1 in
          ('CAVH Start','CAVH D/C','CVVHD Start','CVVHD D/C','Hemodialysis st','Hemodialysis end') then 1 else 0 end) as RRT
  from icustay_detail ie
  inner join chartevents ce
  on ie.icustay_id = ce.icustay_id
  and ce.itemid in
    (152 ,148,149,146 ,147 ,151,150,229 ,235 ,241 ,247 ,253 ,259,265 ,271,582)
    and ce.value1 is not null
  group by ie.icustay_id ;"
)

# Patients de plus de 15 ans
infos_admission <- infos.mimic2 %>%
  # subset(age >= 15)%>%
  mutate(
    admission_hos  = as.POSIXct(ifelse(admission_hos > admission_icu, admission_icu, admission_hos),origin ="1970-01-01"),
    discharge_hop = as.POSIXct(ifelse(discharge_hop < discharge_icu, discharge_icu, discharge_hop), origin ="1970-01-01"),
    delais_hospital = round(difftime(time1 = discharge_hop, 
                                     time2 = admission_hos, 
                                     units = c("days")),0),
    delais_icu = round(difftime(time1 = discharge_icu, 
                                time2 = admission_icu, 
                                units = c("days")),0)
  ) %>% 
  dplyr::select(c(subject_id, icustay_id, rank, gender, age, sapsi_first,
                  sofa_first, bmi, care_unit, admission_type_descr,
                  admission_icu , discharge_icu, admission_hos, discharge_hop,
                  # description,
                  los_icu, los_hospital, delais_hospital, delais_icu, icu_death,
                  hospital_death, j28_death))%>%
  unique() %>%
  full_join( infos.mimic2.dialyse,  by="icustay_id")%>%
  mutate(
    rrt = ifelse(is.na(rrt), 0, rrt)
  )
length(unique(infos_admission$subject_id)) 

# 23756 patients
save(infos_admission, file = "~/Recherche/Mimic-II/data/clean/mimic2/infos_admission.RData")

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
		select subject_id,icustay_id, realtime, 
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
	group by subject_id , icustay_id, realtime;
")

dialyse <- dbGetQuery(con,"
  select subject_id,icustay_id, realtime
  , max(
    case
    when itemid in (152,148,149,146,147,151,150) and value1 is not null then 1
    when itemid in (229,235,241,247,253,259,265,271) and value1 = 'Dialysis Line' then 1
    when itemid = 582 and value1 in ('CAVH Start','CAVH D/C','CVVHD Start','CVVHD D/C','Hemodialysis st','Hemodialysis end') then 1
    else 0 end
  ) as RRT
  from chartevents
  group by subject_id , icustay_id, realtime;
  ")

save(amine, file = "~/Recherche/Mimic-II/data/clean/mimic2/amine.RData")
save(dialyse, file = "~/Recherche/Mimic-II/data/clean/mimic2/dialyse.RData")
save(lactate, file = "~/Recherche/Mimic-II/data/clean/mimic2/lactate.RData")
save(sedation, file = "~/Recherche/Mimic-II/data/clean/mimic2/sedation.RData")
save(curare, file = "~/Recherche/Mimic-II/data/clean/mimic2/curare.RData")
save(ventilation, file = "~/Recherche/Mimic-II/data/clean/mimic2/ventilation.RData")
save(hypovolemie, file = "~/Recherche/Mimic-II/data/clean/mimic2/hypovolemie.RData")

# sous_data <- subset(infos.mimic2, select = c(subject_id, sequence, description))
# sous_data <- subset(sous_data, subject_id %in% df_modelisation$id)
# write.csv(sous_data,"diagnostic.csv", row.names = F)
