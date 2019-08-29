/*******************************************************************************/
/*@file ED_SI_DDMIER.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: &&i2b2_db_schema, &&within_d
/*       
/*out: ED_SI_DDMIER
/*
/*action: write
/********************************************************************************/
create table ED_SI_DDMIER as
select obs.patient_num
      ,obs.encounter_num
      ,'D-Dimer' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,case when obs.nval_num > 200 then 1 else 0
       end as flag
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd = 'KUH|COMPONENT_ID:3094' and 
      obs.modifier_cd = '@' and obs.nval_num is not null and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      init.case_ctrl=1




