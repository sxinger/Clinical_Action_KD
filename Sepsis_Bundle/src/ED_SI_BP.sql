/*******************************************************************************/
/*@file ED_SI_BP.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: &&i2b2_db_schema, &&within_d
/*       
/*out: ED_SI_BP
/*
/*action: write
/********************************************************************************/
create table ED_SI_BP as 
with sbp_raw as (
select init.patient_num 
      ,init.encounter_num
      ,'SBP' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_date) rn
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd = 'KUH|FLO_MEAS_ID:5_SYSTOLIC' and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and 
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      init.case_ctrl=1
)
     ,dbp_raw as (
select init.patient_num 
      ,init.encounter_num
      ,'DBP' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_date) rn
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd = 'KUH|FLO_MEAS_ID:5_DIASTOLIC' and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      init.case_ctrl=1
)
    ,calc_map as (
select sbp.patient_num
      ,sbp.encounter_num
      ,'MAP' variable
      ,round(dbp.nval+(sbp.nval - dbp.nval)/3,2) nval
      ,sbp.units
      ,'KUH|FLO_MEAS_ID:5' code
      ,'calculated' modifier
      ,sbp.instance
      ,sbp.start_dt
      ,sbp.start_since_triage
      ,sbp.end_dt
      ,sbp.end_since_triage
from sbp_raw sbp
join dbp_raw dbp
on sbp.patient_num = dbp.patient_num and 
   sbp.encounter_num = dbp.encounter_num and
   sbp.instance = dbp.instance
union all
select init.patient_num
      ,init.encounter_num
      ,'MAP' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,'flwsht' modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd in ('KUH|FLO_MEAS_ID:301250',
                         'KUH|FLO_MEAS_ID:14516',
                         'KUH|FLO_MEAS_ID:7701') and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and                       
      init.case_ctrl=1
)
select distinct
       patient_num 
      ,encounter_num
      ,variable
      ,nval
      ,units
      ,code
      ,modifier
      ,instance
      ,case when nval < 70 then 1 else 0
       end as flag
      ,start_dt
      ,start_since_triage
      ,end_dt
      ,end_since_triage
from calc_map
union all
select distinct
       patient_num 
      ,encounter_num
      ,variable
      ,nval
      ,units
      ,code
      ,modifier
      ,instance
      ,case when nval < 90 then 1 else 0
       end as flag
      ,start_dt
      ,start_since_triage
      ,end_dt
      ,end_since_triage
from sbp_raw





