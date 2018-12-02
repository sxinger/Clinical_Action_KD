/*******************************************************************************/
/*@file SI_ServDep.sql
/*
/*in: SI_case_ctrl, blueherondata.observation_fact,blueherondata.concept_dimension 
/*
/*params: @dblink, &&i2b2
/*
/*out: SI_ServDep
/*
/*action: write
/********************************************************************************/
create table SI_ServDep as
with collect_dep as (
select distinct
       si.patient_num
      ,si.encounter_num
      ,obs.concept_cd ServDep_code
      ,obs.start_date IO_time
      ,round((obs.start_date - triage_start)*24,3) hr_since_triage
from SI_case_ctrl si
join &&i2b2data.observation_fact@dblink obs
on si.patient_num = obs.patient_num and 
   si.encounter_num = obs.encounter_num and
   (obs.concept_cd like 'KUH|VISITDETAIL|HSP%' or
    obs.concept_cd like 'KUH|VISITDETAIL|AMB%' or
    obs.concept_cd like 'KUH|HOSP%')
)
select d.patient_num
      ,d.encounter_num
      ,d.ServDep_code
      ,replace((cd.concept_path || cd.name_char),'\i2b2\Visit Details\ENC_TYPE','') ServDep_name
      ,d.IO_time
      ,d.hr_since_triage
      ,dense_rank() over (partition by d.patient_num, d.encounter_num order by d.hr_since_triage,d.ServDep_code) ServDep_code_order
from collect_dep d
join &&i2b2data.concept_dimension@dblink cd
on d.ServDep_code = cd.concept_cd
where to_char(d.IO_time,'HH24:MI:SS') <> '00:00:00'


