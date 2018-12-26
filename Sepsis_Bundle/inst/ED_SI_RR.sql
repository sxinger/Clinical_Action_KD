/*******************************************************************************/
/*@file ED_SI_RR.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: @dblink, &&i2b2, &&within_d
/*       
/*out: ED_SI_RR
/*
/*action: write
/********************************************************************************/
create table ED_SI_RR as
select distinct 
       obs.patient_num
      ,obs.encounter_num
      ,'Respiratory rate' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,case when obs.nval_num > 20 then 1 else 0
       end as flag
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_date) rn
from SI_case_ctrl init
join &&i2b2data.observation_fact@dblink obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd in ('KUH|FLO_MEAS_ID:9',
                         'KUH|FLO_MEAS_ID:1190') and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      init.case_ctrl=1
      

