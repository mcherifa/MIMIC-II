#######################################################
#-------- Extraction des données cliniques dans la base MIMIC 3
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

source("~/Mimic/scripts/R/SQL/connection_mimic3.R")

#######################################################
# -------- Tables présentent dans la base PSQL 
#######################################################

dbListTables(con)

#######################################################
# -------- Informations sur les patients : à l'admission 
#######################################################

# Age, sexe, poids 
demographique <- dbGetQuery(con,"
WITH
creation_age as
(
  select ie.subject_id,ie.hadm_id, ie.icustay_id, ie.intime, ie.outtime, pat.dob,
  round( (cast(ie.intime as date) - cast(pat.dob as date) ) / 365.242 ,0) as age,
  pat.gender as gender
  from icustays ie
  inner join patients pat
  on ie.subject_id = pat.subject_id
), poids as
(
  select ie.icustay_id,
  avg(case
        when itemid IN (762, 763, 3723, 3580, 226512)
          then valuenum
        -- convert lbs to kgs
        when itemid IN (3581)
          then valuenum * 0.45359237
        when itemid IN (3582)
          then valuenum * 0.0283495231
        else null
      end) AS weight
  from icustays ie
  left join chartevents c
  on ie.icustay_id = c.icustay_id
  where valuenum IS NOT NULL
  and itemid IN
  (
  762, 763, 3723, 3580,
  3581,
  3582,
  226512 -- Metavision: Admission Weight (Kg)
  )
  and valuenum != 0
  and charttime between ie.intime - interval '1' day and ie.intime + interval '1' day
  and c.error IS DISTINCT FROM 1
  group by ie.icustay_id
), echo2 as
(
  select echo.subject_id, ie.icustay_id, avg(weight * 0.45359237) as weight
  from icustays ie
  left join echodata echo
  on ie.hadm_id = echo.hadm_id
  and echo.charttime > ie.intime - interval '7' day
  and echo.charttime < ie.intime + interval '1' day
  where weight is not null
  group by ie.icustay_id, echo.subject_id
), creation_poids as
(
  select ec.subject_id, ec.icustay_id, coalesce(wt.weight,ec.weight) as poids
  from poids as wt
  inner join echo2 as ec
  on wt.icustay_id = ec.icustay_id
)
select distinct wt.subject_id, wt.icustay_id ,ca.age, ca.gender, wt.poids
from creation_age ca
inner join creation_poids as wt
on wt.subject_id = ca.subject_id
--where age between'18' and '90'
;"
)

# Dialyse  : mettre dans les infos patients 
dialyse <- dbGetQuery(con,"
WITH
cv as
(
  select ie.icustay_id
  , max(
    case
    when ce.itemid in (152,148,149,146,147,151,150) and value is not null then 1
    when ce.itemid in (229,235,241,247,253,259,265,271) and value = 'Dialysis Line' then 1
    when ce.itemid = 582 and value in ('CAVH Start','CAVH D/C','CVVHD Start','CVVHD D/C','Hemodialysis st','Hemodialysis end') then 1
    else 0 end
  ) as RRT
  from icustays ie
  inner join chartevents ce
  on ie.icustay_id = ce.icustay_id
  and ce.itemid in
  (
    152 -- 'Dialysis Type';61449
    ,148 -- 'Dialysis Access Site';60335
    ,149 -- 'Dialysis Access Type';60030
    ,146 -- 'Dialysate Flow ml/hr';57445
    ,147 -- 'Dialysate Infusing';56605
    ,151 -- 'Dialysis Site Appear';37345
    ,150 -- 'Dialysis Machine';27472
    ,229 -- INV Line#1 [Type]
    ,235 -- INV Line#2 [Type]
    ,241 -- INV Line#3 [Type]
    ,247 -- INV Line#4 [Type]
    ,253 -- INV Line#5 [Type]
    ,259 -- INV Line#6 [Type]
    ,265 -- INV Line#7 [Type]
    ,271 -- INV Line#8 [Type]
    ,582 -- Procedures
  )
  and ce.value is not null
  where ie.dbsource = 'carevue'
  -- exclude rows marked as error
  and ce.error IS DISTINCT FROM 1
  group by ie.icustay_id
)
, mv_ce as
(
  select icustay_id
  , 1 as RRT
  from chartevents ce
  where itemid in
  (
    -- Checkboxes
    225126 -- | Dialysis patient        | Adm History/FHPA        | chartevents        | Checkbox
    , 226118 -- | Dialysis Catheter placed in outside facility      | Access Lines - Invasive | chartevents        | Checkbox
    , 227357 -- | Dialysis Catheter Dressing Occlusive              | Access Lines - Invasive | chartevents        | Checkbox
    , 225725 -- | Dialysis Catheter Tip Cultured                    | Access Lines - Invasive | chartevents        | Checkbox
    -- Numeric values
    , 226499 -- | Hemodialysis Output     | Dialysis                | chartevents        | Numeric
    , 224154 -- | Dialysate Rate          | Dialysis                | chartevents        | Numeric
    , 225810 -- | Dwell Time (Peritoneal Dialysis)                  | Dialysis                | chartevents        | Numeric
    , 227639 -- | Medication Added Amount  #2 (Peritoneal Dialysis) | Dialysis                | chartevents        | Numeric
    , 225183 -- | Current Goal                     | Dialysis | chartevents        | Numeric
    , 227438 -- | Volume not removed               | Dialysis | chartevents        | Numeric
    , 224191 -- | Hourly Patient Fluid Removal     | Dialysis | chartevents        | Numeric
    , 225806 -- | Volume In (PD)                   | Dialysis | chartevents        | Numeric
    , 225807 -- | Volume Out (PD)                  | Dialysis | chartevents        | Numeric
    , 228004 -- | Citrate (ACD-A)                  | Dialysis | chartevents        | Numeric
    , 228005 -- | PBP (Prefilter) Replacement Rate | Dialysis | chartevents        | Numeric
    , 228006 -- | Post Filter Replacement Rate     | Dialysis | chartevents        | Numeric
    , 224144 -- | Blood Flow (ml/min)              | Dialysis | chartevents        | Numeric
    , 224145 -- | Heparin Dose (per hour)          | Dialysis | chartevents        | Numeric
    , 224149 -- | Access Pressure                  | Dialysis | chartevents        | Numeric
    , 224150 -- | Filter Pressure                  | Dialysis | chartevents        | Numeric
    , 224151 -- | Effluent Pressure                | Dialysis | chartevents        | Numeric
    , 224152 -- | Return Pressure                  | Dialysis | chartevents        | Numeric
    , 224153 -- | Replacement Rate                 | Dialysis | chartevents        | Numeric
    , 224404 -- | ART Lumen Volume                 | Dialysis | chartevents        | Numeric
    , 224406 -- | VEN Lumen Volume                 | Dialysis | chartevents        | Numeric
    , 226457 -- | Ultrafiltrate Output             | Dialysis | chartevents        | Numeric
  )
  and ce.valuenum > 0 -- also ensures it's not null
  -- exclude rows marked as error
  and ce.error IS DISTINCT FROM 1
  group by icustay_id
)
, mv_ie as
(
  select icustay_id
  , 1 as RRT
  from inputevents_mv
  where itemid in
  (
  227536 --	KCl (CRRT)	Medications	inputevents_mv	Solution
  , 227525 --	Calcium Gluconate (CRRT)	Medications	inputevents_mv	Solution
  )
  and amount > 0 -- also ensures it's not null
  group by icustay_id
)
, mv_de as
(
  select icustay_id
  , 1 as RRT
  from datetimeevents
  where itemid in
  (
    -- TODO: unsure how to handle 'Last dialysis'
    --  225128 -- | Last dialysis           | Adm History/FHPA        | datetimeevents     | Date time
    225318 -- | Dialysis Catheter Cap Change                      | Access Lines - Invasive | datetimeevents     | Date time
    , 225319 -- | Dialysis Catheter Change over Wire Date           | Access Lines - Invasive | datetimeevents     | Date time
    , 225321 -- | Dialysis Catheter Dressing Change                 | Access Lines - Invasive | datetimeevents     | Date time
    , 225322 -- | Dialysis Catheter Insertion Date                  | Access Lines - Invasive | datetimeevents     | Date time
    , 225324 -- | Dialysis CatheterTubing Change                    | Access Lines - Invasive | datetimeevents     | Date time
  )
  group by icustay_id
)
, mv_pe as
(
  select icustay_id
  , 1 as RRT
  from procedureevents_mv
  where itemid in
  (
    225441 -- | Hemodialysis            | 4-Procedures            | procedureevents_mv | Process
    , 225802 -- | Dialysis - CRRT         | Dialysis                | procedureevents_mv | Process
    , 225803 -- | Dialysis - CVVHD        | Dialysis                | procedureevents_mv | Process
    , 225805 -- | Peritoneal Dialysis     | Dialysis                | procedureevents_mv | Process
    , 224270 -- | Dialysis Catheter       | Access Lines - Invasive | procedureevents_mv | Process
    , 225809 -- | Dialysis - CVVHDF       | Dialysis                | procedureevents_mv | Process
    , 225955 -- | Dialysis - SCUF         | Dialysis                | procedureevents_mv | Process
    , 225436 -- | CRRT Filter Change               | Dialysis | procedureevents_mv | Process
  )
  group by icustay_id
)
select ie.subject_id, ie.hadm_id, ie.icustay_id
, case
when cv.RRT = 1 then 1
when mv_ce.RRT = 1 then 1
when mv_ie.RRT = 1 then 1
when mv_de.RRT = 1 then 1
when mv_pe.RRT = 1 then 1
else 0
end as RRT
from icustays ie
left join cv
on ie.icustay_id = cv.icustay_id
left join mv_ce
on ie.icustay_id = mv_ce.icustay_id
left join mv_ie
on ie.icustay_id = mv_ie.icustay_id
left join mv_de
on ie.icustay_id = mv_de.icustay_id
left join mv_pe
on ie.icustay_id = mv_pe.icustay_id
order by ie.icustay_id;"
)

# Type réanimation (première unité)
type_reanimation <- dbGetQuery(con,"
select subject_id, icustay_id, los,
case
  when first_careunit = 'MICU' then 1
  when first_careunit = 'SICU' then 2
  when first_careunit = 'TSICU' then 3
  when first_careunit = 'CSRU' then 4
  when first_careunit = 'NICU' then 5
  else 6 end as care_unit
from icustays
;")

# Score de gravité : SAPSII et SOFA
scores <- dbGetQuery(con,"
select sa.subject_id, sa.icustay_id, sa.sapsii, so.sofa
from sapsii sa
inner join sofa so
on sa.icustay_id = so.icustay_id;"
)

#######################################################
# -------- Informations sur les patients : données réactualisées
#######################################################

# Lactate 
lactate <- dbGetQuery(con,"
select distinct ic.subject_id, ic.icustay_id, 
le.hadm_id,le.charttime, le.value,le.flag,
li.itemid, li.label,
RANK() OVER (PARTITION BY ic.icustay_id ORDER BY le.charttime ASC) AS rank  
from icustays ic
inner join labevents le
on  ic.hadm_id = le.hadm_id 
inner join d_labitems li
on le.itemid = li.itemid
where le.itemid = 50813;"
)


# Vasopresseurs
amine <- dbGetQuery(con,"
select subject_id, icustay_id, startdate, enddate,
case 
when 
drug like 'Norepinephrine' or 
drug like 'Epinephrine' or 
drug like 'Phenylephrine' or 
drug like 'Vasopressin' or 
drug like 'Dopamine' or 
drug like 'Milrinone' then 1
else 0 end as amine
from prescriptions;")


# Sédation 
sedation <- dbGetQuery(con,"
select subject_id, icustay_id, startdate, enddate,
case 
when drug like 'Propofol' or
drug like 'propofol' or
drug like 'Midazolam' or
drug like 'Diprivan' or
drug like 'Hypnovel' or
drug like 'Versed' or
drug like 'Thiopental' or
drug like 'Ketamin' or
drug like 'Ketalar' or
drug like 'Etomidate' or
drug like 'Amidate' then 1
else 0 end as sedation
from prescriptions
;")

# Curarisé
curare <- dbGetQuery(con,"
select subject_id, icustay_id, startdate, enddate, 
case 
when drug like 'Cisatracrium' or
drug like 'Atracrium' or
drug like 'Tracrium' or
drug like 'Midazolam' or
drug like 'Nimbex' or
drug like 'Rocuronium' or
drug like 'Esmeron' or
drug like 'Succinylcholine' then 1
else 0 end as curare
from prescriptions
;")

# ventilation

ventilation <- dbGetQuery(con,"
select subject_id,icustay_id, charttime
-- case statement determining whether it is an instance of mech vent
, max(
  case
  when itemid is null or value is null then 0 -- can't have null values
  when itemid = 720 and value != 'Other/Remarks' THEN 1  -- VentTypeRecorded
  when itemid = 223848 and value != 'Other' THEN 1
  when itemid = 223849 then 1 -- ventilator mode
  when itemid = 467 and value = 'Ventilator' THEN 1 -- O2 delivery device == ventilator
  when itemid = 648 and value = 'Intubated/trach' THEN 1 -- Speech = intubated
  when itemid = 223900 and value = 'No Response-ETT' THEN 1
  when itemid in
  (
  445, 448, 449, 450, 1340, 1486, 1600, 224687 -- minute volume
  , 639, 654, 681, 682, 683, 684,224685,224684,224686 -- tidal volume
  , 218,436,535,444,459,224697,224695,224696,224746,224747 
  , 221,1,1211,1655,2000,226873,224738,224419,224750,227187 -- Insp pressure
  , 543 -- PlateauPressure
  , 5865,5866,224707,224709,224705,224706 -- APRV pressure
  , 60,437,505,506,686,220339,224700 -- PEEP
  , 3459 -- high pressure relief
  , 501,502,503,224702 -- PCV
  , 223,667,668,669,670,671,672 -- TCPCV
  , 157,158,1852,3398,3399,3400,3401,3402,3403,3404,8382,227809,227810 -- ETT
  , 224701 -- PSVlevel
  )
  THEN 1
  else 0
  end
) as MechVent
  , max(
  case when itemid is null or value is null then 0
  -- extubated indicates ventilation event has ended
  when itemid = 640 and value = 'Extubated' then 1
  when itemid = 640 and value = 'Self Extubation' then 1
  -- initiation of oxygen therapy indicates the ventilation has ended
  when itemid = 226732 and value in
  (
  'Nasal cannula', -- 153714 observations
  'Face tent', -- 24601 observations
  'Aerosol-cool', -- 24560 observations
  'Trach mask ', -- 16435 observations
  'High flow neb', -- 10785 observations
  'Non-rebreather', -- 5182 observations
  'Venti mask ', -- 1947 observations
  'Medium conc mask ', -- 1888 observations
  'T-piece', -- 1135 observations
  'High flow nasal cannula', -- 925 observations
  'Ultrasonic neb', -- 9 observations
  'Vapomist' -- 3 observations
  ) then 1
  when itemid = 467 and value in
  (
  'Cannula', -- 278252 observations
  'Nasal Cannula', -- 248299 observations
  'None', -- 95498 observations
  'Face Tent', -- 35766 observations
  'Aerosol-Cool', -- 33919 observations
  'Trach Mask', -- 32655 observations
  'Hi Flow Neb', -- 14070 observations
  'Non-Rebreather', -- 10856 observations
  'Venti Mask', -- 4279 observations
  'Medium Conc Mask', -- 2114 observations
  'Vapotherm', -- 1655 observations
  'T-Piece', -- 779 observations
  'Hood', -- 670 observations
  'Hut', -- 150 observations
  'TranstrachealCat', -- 78 observations
  'Heated Neb', -- 37 observations
  'Ultrasonic Neb' -- 2 observations
  ) then 1
  else 0
  end
  )
  as Extubated
  , max(
  case when itemid is null or value is null then 0
  when itemid = 640 and value = 'Self Extubation' then 1
  else 0
  end
  )
  as SelfExtubated
  from chartevents ce
  where ce.value is not null
  -- exclude rows marked as error
  -- and ce.error IS DISTINCT FROM 1
  and itemid in
  (
  -- the below are settings used to indicate ventilation
  648, 223900 -- speech
  , 720, 223849 -- vent mode
  , 223848 -- vent type
  , 445, 448, 449, 450, 1340, 1486, 1600, 224687 -- minute volume
  , 639, 654, 681, 682, 683, 684,224685,224684,224686 -- tidal volume
  , 218,436,535,444,224697,224695,224696,224746,224747
  , 221,1,1211,1655,2000,226873,224738,224419,224750,227187 -- Insp pressure
  , 543 -- PlateauPressure
  , 5865,5866,224707,224709,224705,224706 -- APRV pressure
  , 60,437,505,506,686,220339,224700 -- PEEP
  , 3459 -- high pressure relief
  , 501,502,503,224702 -- PCV
  , 223,667,668,669,670,671,672 -- TCPCV
  , 157,158,1852,3398,3399,3400,3401,3402,3403,3404,8382,227809,227810 -- ETT
  , 224701 -- PSVlevel
  
  -- the below are settings used to indicate extubation
  , 640 -- extubated
  
  -- the below indicate oxygen/NIV, i.e. the end of a mechanical vent event
  , 468 -- O2 Delivery Device#2
  , 469 -- O2 Delivery Mode
  , 470 -- O2 Flow (lpm)
  , 471 -- O2 Flow (lpm) #2
  , 227287 -- O2 Flow (additional cannula)
  , 226732 -- O2 Delivery Device(s)
  , 223834 -- O2 Flow
  
  -- used in both oxygen + vent calculation
  , 467 -- O2 Delivery Device
  )
  group by subject_id , icustay_id, charttime
  UNION
  -- add in the extubation flags from procedureevents_mv
  -- note that we only need the start time for the extubation
  -- (extubation is always charted as ending 1 minute after it started)
  select subject_id,icustay_id, starttime as charttime
  , 0 as MechVent
  , 1 as Extubated
  , case when itemid = 225468 then 1 else 0 end as SelfExtubated
  from procedureevents_mv
  where itemid in
  (227194, 225468 , 225477 );"
)

# Expension volémique ---- demander Thaïs
hypovolemie <- dbGetQuery(con,"
select subject_id, icustay_id, startdate, enddate, 
case 
when drug like 'Sodium Chloride 0.9%  Flush' or
drug like 'Sodium Chloride'                 or        
drug like 'Isotonic Sodium Chloride'        or       
drug like 'Iso-Osmotic Sodium Chloride'     or     
drug like 'Sodium Chloride '                or   
drug like '0.9% Sodium Chloride'            or        
drug like '0.45% Sodium Chloride'           or        
drug like 'Albumin 5% (25 g)'               or        
drug like 'Albumin 25% (12.5 g)'            or        
drug like 'Albumin, Human'                  or
drug like '0.9% Sodium Chloride (Mini Bag Plus)' or  
drug like 'Albumin 5%'                          or
drug like 'Albumin 25% (12.5gm)'                or    
drug like 'Albumin 5% (25g / 500mL)'            or    
drug like 'Albumin 5% (12.5g / 250mL)'          or    
drug like 'Albumin 25% (12.5g / 50mL)'          or    
drug like 'Albumin 5% (12.5gm)'                 or    
drug like 'Albumin 5% (12.5 g)'                 or    
drug like 'Sodium Chloride 23.4%'               or    
drug like 'Sodium Chloride 0.9%'                or    
drug like 'Syringe (0.9% Sodium Chloride)'      or    
drug like 'Albumin 5% (12.5g)'                  or    
drug like 'Albumin 5% ('                        or
drug like 'Albumin '                            or
drug like 'Lactated Ringers'                    or    
drug like 'Dextran 40 10%'                      or
drug like 'Albumin 5'                           or
drug like '0.83% Sodium Chloride'               or
drug like 'Epinephrine-Sodium Chloride'         or          
drug like 'Iron Dextran (Test Dose)'            or    
drug like 'Iron Dextran Complex'                or    
drug like 'NS  W/  0.1% Albumin'                or
drug like 'Albumin'                             or
drug like 'Albumin 5% '                         or
drug like 'ALBUMIN'                             or
drug like 'Albumin 5% (2.5gm)'                  or    
drug like 'Dextran  1 (Promit)'                 or    
drug like 'Albumin 25%'                         or
drug like 'Albumin 5% (2.5 g)'                  or    
drug like 'Iron Dextran'                        or
drug like 'Sodium Chloride (Hypertonic)'        or   
drug like 'Sodium Chloride (23.4%) Kit'         or  
drug like '0.9% Sodium Chloride (EXCEL BAG)'        or 
drug like '0.9% Sodium Chloride P.F. (Syringe)'     or  
drug like 'Sodium Chloride 0.9 % or No Intervention'or
drug like 'Dextran 40 10% in NS'                    or
drug like 'Albumin 25% '                            or
drug like 'Dextran 40 10% in D5W'                   
then 1
else 0 end as expension
from prescriptions
;")


# Récupérer les chartevents

chartvitals <- dbGetQuery(con,"
SELECT pvt.subject_id, pvt.hadm_id, pvt.icustay_id

-- Easier names
, min(case when VitalID = 1 then valuenum else null end) as HeartRate_Min
, max(case when VitalID = 1 then valuenum else null end) as HeartRate_Max
, avg(case when VitalID = 1 then valuenum else null end) as HeartRate_Mean
, min(case when VitalID = 2 then valuenum else null end) as SysBP_Min
, max(case when VitalID = 2 then valuenum else null end) as SysBP_Max
, avg(case when VitalID = 2 then valuenum else null end) as SysBP_Mean
, min(case when VitalID = 3 then valuenum else null end) as DiasBP_Min
, max(case when VitalID = 3 then valuenum else null end) as DiasBP_Max
, avg(case when VitalID = 3 then valuenum else null end) as DiasBP_Mean
, min(case when VitalID = 4 then valuenum else null end) as MeanBP_Min
, max(case when VitalID = 4 then valuenum else null end) as MeanBP_Max
, avg(case when VitalID = 4 then valuenum else null end) as MeanBP_Mean
, min(case when VitalID = 5 then valuenum else null end) as RespRate_Min
, max(case when VitalID = 5 then valuenum else null end) as RespRate_Max
, avg(case when VitalID = 5 then valuenum else null end) as RespRate_Mean
, min(case when VitalID = 6 then valuenum else null end) as TempC_Min
, max(case when VitalID = 6 then valuenum else null end) as TempC_Max
, avg(case when VitalID = 6 then valuenum else null end) as TempC_Mean
, min(case when VitalID = 7 then valuenum else null end) as SpO2_Min
, max(case when VitalID = 7 then valuenum else null end) as SpO2_Max
, avg(case when VitalID = 7 then valuenum else null end) as SpO2_Mean
, min(case when VitalID = 8 then valuenum else null end) as Glucose_Min
, max(case when VitalID = 8 then valuenum else null end) as Glucose_Max
, avg(case when VitalID = 8 then valuenum else null end) as Glucose_Mean

FROM  (
  select ie.subject_id, ie.hadm_id, ie.icustay_id
  , case
    when itemid in (211,220045) and valuenum > 0 and valuenum < 300 then 1 -- HeartRate
    when itemid in (51,442,455,6701,220179,220050) and valuenum > 0 and valuenum < 400 then 2 -- SysBP
    when itemid in (8368,8440,8441,8555,220180,220051) and valuenum > 0 and valuenum < 300 then 3 -- DiasBP
    when itemid in (456,52,6702,443,220052,220181,225312) and valuenum > 0 and valuenum < 300 then 4 -- MeanBP
    when itemid in (615,618,220210,224690) and valuenum > 0 and valuenum < 70 then 5 -- RespRate
    when itemid in (223761,678) and valuenum > 70 and valuenum < 120  then 6 -- TempF, converted to degC in valuenum call
    when itemid in (223762,676) and valuenum > 10 and valuenum < 50  then 6 -- TempC
    when itemid in (646,220277) and valuenum > 0 and valuenum <= 100 then 7 -- SpO2
    when itemid in (807,811,1529,3745,3744,225664,220621,226537) and valuenum > 0 then 8 -- Glucose

    else null end as VitalID
      -- convert F to C
  , case when itemid in (223761,678) then (valuenum-32)/1.8 else valuenum end as valuenum

  from icustays ie
  left join chartevents ce
  on ie.subject_id = ce.subject_id and ie.hadm_id = ce.hadm_id and ie.icustay_id = ce.icustay_id
  -- and ce.charttime between ie.intime and ie.intime + interval '3' day
  -- exclude rows marked as error
  and ce.error IS DISTINCT FROM 1
  where ce.itemid in
  (
  -- HEART RATE
  211, --Heart Rate
  220045, --Heart Rate

  -- Systolic/diastolic

  51, --	Arterial BP [Systolic]
  442, --	Manual BP [Systolic]
  455, --	NBP [Systolic]
  6701, --	Arterial BP #2 [Systolic]
  220179, --	Non Invasive Blood Pressure systolic
  220050, --	Arterial Blood Pressure systolic

  8368, --	Arterial BP [Diastolic]
  8440, --	Manual BP [Diastolic]
  8441, --	NBP [Diastolic]
  8555, --	Arterial BP #2 [Diastolic]
  220180, --	Non Invasive Blood Pressure diastolic
  220051, --	Arterial Blood Pressure diastolic


  -- MEAN ARTERIAL PRESSURE
  456, --NBP Mean
  52, --Arterial BP Mean
  6702, --	Arterial BP Mean #2
  443, --	Manual BP Mean(calc)
  220052, --Arterial Blood Pressure mean
  220181, --Non Invasive Blood Pressure mean
  225312, --ART BP mean

  -- RESPIRATORY RATE
  618,--	Respiratory Rate
  615,--	Resp Rate (Total)
  220210,--	Respiratory Rate
  224690, --	Respiratory Rate (Total)


  -- SPO2, peripheral
  646, 220277,

  -- GLUCOSE, both lab and fingerstick
  807,--	Fingerstick Glucose
  811,--	Glucose (70-105)
  1529,--	Glucose
  3745,--	BloodGlucose
  3744,--	Blood Glucose
  225664,--	Glucose finger stick
  220621,--	Glucose (serum)
  226537,--	Glucose (whole blood)

  -- TEMPERATURE
  223762, -- Temperature Celsius
  676,	-- Temperature C
  223761, -- Temperature Fahrenheit
  678 --	Temperature F

  )
) pvt
group by pvt.subject_id, pvt.hadm_id, pvt.icustay_id
order by pvt.subject_id, pvt.hadm_id, pvt.icustay_id
;")


all_vitals <- dbGetQuery(con,"
   select ce.subject_id, ce.hadm_id, ce.icustay_id, ce.itemid,
    case
    when ce.itemid in (211,220045) and valuenum > 0 and valuenum < 300 then 1 -- HeartRate
    when ce.itemid in (51,442,455,6701,220179,220050) and valuenum > 0 and valuenum < 400 then 2 -- SysBP
    when ce.itemid in (8368,8440,8441,8555,220180,220051) and valuenum > 0 and valuenum < 300 then 3 -- DiasBP
    when ce.itemid in (456,52,6702,443,220052,220181,225312) and valuenum > 0 and valuenum < 300 then 4 -- MeanBP
    when ce.itemid in (615,618,220210,224690) and valuenum > 0 and valuenum < 70 then 5 -- RespRate
    when ce.itemid in (223761,678) and valuenum > 70 and valuenum < 120  then 6 -- TempF, converted to degC in valuenum call
    when ce.itemid in (223762,676) and valuenum > 10 and valuenum < 50  then 6 -- TempC
    when ce.itemid in (646,220277) and valuenum > 0 and valuenum <= 100 then 7 -- SpO2
    when ce.itemid in (807,811,1529,3745,3744,225664,220621,226537) and valuenum > 0 then 8 -- Glucose
    else null end as VitalID
      -- convert F to C
  , case when ce.itemid in (223761,678) then (valuenum-32)/1.8 else valuenum end as valuenum
 from chartevents ce

")
# 
# 
#  left join chartevents_1 	ce_1
#  	on  ce.subject_id = ce_1.subject_id 
#   and    ce.hadm_id = ce_1.hadm_id
#   and ce.icustay_id = ce_1.icustay_id
#  left join chartevents_2  ce_2
#    on  ce.subject_id = ce_2.subject_id 
#   and    ce.hadm_id  = ce_2.hadm_id
#   and ce.icustay_id  = ce_2.icustay_id
#  left join chartevents_3  ce_3  
#    on  ce.subject_id = ce_3.subject_id 
#   and    ce.hadm_id  = ce_3.hadm_id
#   and ce.icustay_id  = ce_3.icustay_id
#  left join chartevents_4  ce_4  
#    on  ce.subject_id = ce_4.subject_id 
#   and    ce.hadm_id  = ce_4.hadm_id
#   and ce.icustay_id  = ce_4.icustay_id
#  left join chartevents_5  ce_5  
#    on  ce.subject_id = ce_5.subject_id 
#   and    ce.hadm_id  = ce_5.hadm_id
#   and ce.icustay_id  = ce_5.icustay_id
#  left join chartevents_6  ce_6  
#    on  ce.subject_id = ce_6.subject_id 
#   and    ce.hadm_id  = ce_6.hadm_id
#   and ce.icustay_id  = ce_6.icustay_id
#  left join chartevents_7  ce_7  
#    on  ce.subject_id = ce_7.subject_id 
#   and    ce.hadm_id  = ce_7.hadm_id
#   and ce.icustay_id  = ce_7.icustay_id
#  left join chartevents_8  ce_8  
#    on  ce.subject_id = ce_8.subject_id 
#   and    ce.hadm_id  = ce_8.hadm_id
#   and ce.icustay_id  = ce_8.icustay_id
#  left join chartevents_9  ce_9
#    on  ce.subject_id = ce_9.subject_id 
#   and    ce.hadm_id  = ce_9.hadm_id
#   and ce.icustay_id  = ce_9.icustay_id
#  left join chartevents_10 ce_10  
#    on  ce.subject_id = ce_10.subject_id 
#   and    ce.hadm_id  = ce_10.hadm_id
#   and ce.icustay_id  = ce_10.icustay_id
#  left join chartevents_11 ce_11 
#    on  ce.subject_id = ce_11.subject_id 
#   and    ce.hadm_id  = ce_11.hadm_id
#   and ce.icustay_id  = ce_11.icustay_id
#  left join chartevents_12 ce_12  
#   on  ce.subject_id = ce_12.subject_id 
#   and    ce.hadm_id = ce_12.hadm_id
#   and ce.icustay_id = ce_12.icustay_id 
#  left join chartevents_13 ce_13 
#    on  ce.subject_id = ce_13.subject_id 
#   and    ce.hadm_id  = ce_13.hadm_id
#   and ce.icustay_id  = ce_13.icustay_id
#  left join chartevents_14 ce_14 
#    on  ce.subject_id = ce_14.subject_id 
#   and    ce.hadm_id  = ce_14.hadm_id
#   and ce.icustay_id  = ce_14.icustay_id;

#######################################################
# -------- Infos à l'admission 
#######################################################

infos_admission <- demographique %>%
  full_join(scores, by = c("icustay_id")) %>%
  full_join(type_reanimation, by = c("icustay_id")) %>%
  full_join(dialyse, by = c("icustay_id")) %>%
  unique()

infos_admission$subject_id <- infos_admission$subject_id.y.y
saveRDS(infos_admission, paste0(dest_data,"infos_admission.RDS"))

#######################################################
# -------- Infos réactualisées
#######################################################

saveRDS(chartvitals, paste0(dest_data,"vitals.RDS"))
saveRDS(curare, paste0(dest_data,"curare.RDS"))
saveRDS(sedation, paste0(dest_data,"sedation.RDS"))
saveRDS(amine, paste0(dest_data,"amine.RDS"))
saveRDS(lactate, paste0(dest_data,"lactate.RDS"))
saveRDS(ventilation, paste0(dest_data,"ventilation.RDS"))
saveRDS(hypovolemie, paste0(dest_data,"hypovolemie.RDS"))

