 	create materialized view allcharts as
   select ce.subject_id, ce.hadm_id, ce.icustay_id
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
 from chartevents ce
 left join chartevents_1 	ce_1
 	on  ce.subject_id = ce_1.subject_id 
  and    ce.hadm_id = ce_1.hadm_id
  and ce.icustay_id = ce_1.icustay_id
 left join chartevents_2  ce_2
   on  ce.subject_id = ce_2.subject_id 
  and    ce.hadm_id  = ce_2.hadm_id
  and ce.icustay_id  = ce_2.icustay_id
 left join chartevents_3  ce_3  
   on  ce.subject_id = ce_3.subject_id 
  and    ce.hadm_id  = ce_3.hadm_id
  and ce.icustay_id  = ce_3.icustay_id
 left join chartevents_4  ce_4  
   on  ce.subject_id = ce_4.subject_id 
  and    ce.hadm_id  = ce_4.hadm_id
  and ce.icustay_id  = ce_4.icustay_id
 left join chartevents_5  ce_5  
   on  ce.subject_id = ce_5.subject_id 
  and    ce.hadm_id  = ce_5.hadm_id
  and ce.icustay_id  = ce_5.icustay_id
 left join chartevents_6  ce_6  
   on  ce.subject_id = ce_6.subject_id 
  and    ce.hadm_id  = ce_6.hadm_id
  and ce.icustay_id  = ce_6.icustay_id
 left join chartevents_7  ce_7  
   on  ce.subject_id = ce_7.subject_id 
  and    ce.hadm_id  = ce_7.hadm_id
  and ce.icustay_id  = ce_7.icustay_id
 left join chartevents_8  ce_8  
   on  ce.subject_id = ce_8.subject_id 
  and    ce.hadm_id  = ce_8.hadm_id
  and ce.icustay_id  = ce_8.icustay_id
 left join chartevents_9  ce_9
   on  ce.subject_id = ce_9.subject_id 
  and    ce.hadm_id  = ce_9.hadm_id
  and ce.icustay_id  = ce_9.icustay_id
 left join chartevents_10 ce_10  
   on  ce.subject_id = ce_10.subject_id 
  and    ce.hadm_id  = ce_10.hadm_id
  and ce.icustay_id  = ce_10.icustay_id
 left join chartevents_11 ce_11 
   on  ce.subject_id = ce_11.subject_id 
  and    ce.hadm_id  = ce_11.hadm_id
  and ce.icustay_id  = ce_11.icustay_id
 left join chartevents_12 ce_12  
  on  ce.subject_id = ce_12.subject_id 
  and    ce.hadm_id = ce_12.hadm_id
  and ce.icustay_id = ce_12.icustay_id 
 left join chartevents_13 ce_13 
   on  ce.subject_id = ce_13.subject_id 
  and    ce.hadm_id  = ce_13.hadm_id
  and ce.icustay_id  = ce_13.icustay_id
 left join chartevents_14 ce_14 
   on  ce.subject_id = ce_14.subject_id 
  and    ce.hadm_id  = ce_14.hadm_id
  and ce.icustay_id  = ce_14.icustay_id
 
