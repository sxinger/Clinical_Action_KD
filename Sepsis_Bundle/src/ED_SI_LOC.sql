/*******************************************************************************/
/*@file ED_SI_LOC.sql
/*
/*in: SI_case_ctrl,observation_fact, concept_dimension 
/*
/*params: &&i2b2_db_schema, &&within_d 
/*       
/*out: ED_SI_LOC
/*
/*action: write
/********************************************************************************/
create table ED_SI_LOC as
with loc_cd as (
select distinct concept_cd,name_char
from &&i2b2_db_schemadata.concept_dimension
where concept_cd like 'KUH|FLO_MEAS_ID+LINE:1459%'
)
    ,collect_loc as (
select distinct 
       obs.patient_num
      ,obs.encounter_num
      ,'LOC' variable
      ,obs.tval_char tval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd like 'KUH|FLO_MEAS_ID+LINE:1459%' and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'and
      init.case_ctrl=1
)
select distinct 
       patient_num
      ,encounter_num
      ,variable
      ,tval
      ,units
      ,code
      ,modifier
      ,instance
      ,case when tval not in ('Alert')
            then 1 else 0
       end as flag
      ,start_dt
      ,start_since_triage
      ,end_dt
      ,end_since_triage
from collect_loc




