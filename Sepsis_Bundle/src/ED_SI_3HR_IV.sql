/*******************************************************************************/
/*@file ED_SI_3HR_IV.sql
/*
/*in: ED_SI_3HR_screen, observation_fact
/*
/*params: &&i2b2_db_schema
/*   
/*out: ED_SI_3HR_IV
/*
/*action: write
/********************************************************************************/
create table ED_SI_3HR_IV as
with collect_iv as (
select distinct
       e.patient_num
      ,e.encounter_num
      ,obs.nval_num nval
      ,obs.concept_cd code
      ,'flowsheet' as modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-e.start_dt)*24,2) start_since_trigger
      ,round((obs.start_date-e.start_dt)*24+start_since_triage,2) start_since_triage
      ,round((obs.end_date-obs.start_date)*24,2) iv_duration
from ED_SI_3HR_screen e
join &&i2b2_db_schemadata.observation_fact obs  
on obs.patient_num=e.patient_num and obs.encounter_num = e.encounter_num
where obs.start_date >= e.start_dt and
      obs.concept_cd in ('KUH|FLO_MEAS_ID:1994',
                         'KUH|FLO_MEAS_ID:9789') and 
      obs.nval_num is not null and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
union all
select distinct
       e.patient_num
      ,e.encounter_num
      ,obs.nval_num nval
      ,obs.concept_cd code
      ,obs.modifier_cd as modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-e.start_dt)*24,2) start_since_trigger
      ,round((obs.start_date-e.start_dt)*24+start_since_triage,2) start_since_triage
      ,round((obs.end_date-obs.start_date)*24,2) iv_duration
from ED_SI_3HR_screen e
join &&i2b2_db_schemadata.observation_fact obs  
on obs.patient_num=e.patient_num and obs.encounter_num = e.encounter_num
where obs.start_date >= e.start_dt and 
      obs.concept_cd in ('KUH|MEDICATION_ID:27838',
                         'KUH|MEDICATION_ID:309580',
                         'KUH|MEDICATION_ID:4318',
                         'KUH|MEDICATION_ID:15882',
                         'KUH|MEDICATION_ID:8982') and
      obs.nval_num is not null and upper(obs.units_cd)='ML' and
      obs.modifier_cd not in ('MedObs|MAR:Stopped',
                              'MedObs|MAR:Dropped',
                              'MedObs|MAR:Canceled Entry',
                              'MedObs|MAR:Missed',
                              'MedObs|MAR:Held',
                              'MedObs|MAR:Med Not Available',
                              'MedObs|MAR:See ED Trauma Flowsheet') and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
)
   ,iv_clean as (
select patient_num
      ,encounter_num
      ,nval
      ,sum(nval) over (partition by patient_num, encounter_num, modifier order by start_dt asc) cum_nval
      ,modifier
      ,start_dt
      ,start_since_trigger
      ,start_since_triage
      ,iv_duration
      ,dense_rank() over (partition by patient_num, encounter_num, modifier order by start_dt asc) rn
from collect_iv
)
   ,iv_stg as(
select distinct
       iv.patient_num
      ,iv.encounter_num
      ,'IV Fluid Bolus' variable
      ,iv.cum_nval nval
      ,('IV ' || floor(iv.cum_nval/1000) || 'L') tval
      ,iv.modifier
      ,floor(iv.cum_nval/bd.iv_bd_int) as flag
      ,iv.start_dt
      ,iv.start_since_trigger
      ,iv.start_since_triage
      ,iv.iv_duration
      ,iv.rn iv_order
from iv_clean iv
left join ED_SI_3HR_screen bd
on iv.patient_num = bd.patient_num and iv.encounter_num = bd.encounter_num and
   iv.cum_nval > 0
)
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,modifier
      ,flag
      ,start_dt
      ,start_since_trigger
      ,start_since_triage
      ,iv_duration
      ,iv_order
      ,row_number() over (partition by patient_num, encounter_num, tval order by start_since_triage) rn_liter
      ,row_number() over (partition by patient_num, encounter_num, flag order by start_since_triage) rn_flag
from iv_stg






