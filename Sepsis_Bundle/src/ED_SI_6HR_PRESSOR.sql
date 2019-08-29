/*******************************************************************************/
/*@file ED_SI_6HR_PRESSOR.sql
/*
/*in: ED_SI_6HR_screen, pressor_cd, observation_fact
/*
/*params: @dblink, &&i2b2
/*   
/*out: ED_SI_6HR_PRESSOR
/*
/*action: write
/********************************************************************************/
create table ED_SI_6HR_PRESSOR as
with collect_pressor as (
select distinct
       e.patient_num
      ,e.encounter_num
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd as modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-e.hypot2_dt)*24,2)+e.hypot2_since_triage start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-e.hypot2_dt)*24,2)+e.hypot2_since_triage end_since_triage
from ED_SI_6HR_screen e
join &&i2b2data.observation_fact@dblink obs  
on obs.patient_num=e.patient_num and obs.encounter_num = e.encounter_num
where obs.start_date > e.hypot2_dt and 
      obs.concept_cd like 'KUH|MEDICATION_ID%' and
      obs.modifier_cd not in ('MedObs|MAR:Stopped',
                              'MedObs|MAR:Dropped',
                              'MedObs|MAR:Canceled Entry',
                              'MedObs|MAR:Missed',
                              'MedObs|MAR:Held',
                              'MedObs|MAR:Med Not Available',
                              'MedObs|MAR:See ED Trauma Flowsheet') and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
)
select distinct
       cl.patient_num
      ,cl.encounter_num
      ,'vasopressor' variable
      ,cl.nval
      ,cl.units
      ,ht.med_name tval
      ,cl.code
      ,cl.modifier
      ,cl.instance
      ,cl.start_dt
      ,cl.start_since_triage
      ,cl.end_dt
      ,cl.end_since_triage
      ,row_number() over (partition by cl.patient_num, cl.encounter_num order by cl.start_since_triage) rn
from collect_pressor cl
join PRESSOR_CD ht on ('KUH|MEDICATION_ID:'||ht.MED_ID) = cl.code





