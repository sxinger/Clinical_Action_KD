/*******************************************************************************/
/*@file SI_obs_at_enc.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: &&i2b2_db_schema
/*
/*out: SI_obs_at_enc
/*
/*action: write
/********************************************************************************/
create table SI_obs_at_enc as
select tr.patient_num
      ,tr.encounter_num
      ,case when obs.concept_cd like 'LDA%' then regexp_substr(obs.concept_cd,'[^\|]+',1,1)
            else regexp_substr(obs.concept_cd,'[^\:]+',1,1)
       end as concept_prefix --used for partition
      ,obs.concept_cd
      ,obs.nval_num
      ,obs.units_cd
      ,obs.tval_char
      ,obs.instance_num
      ,obs.modifier_cd
      ,obs.start_date start_dt
      ,round((obs.start_date-tr.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-tr.triage_start)*24,2) end_since_triage
from SI_case_ctrl tr
join &&i2b2_db_schemadata.observation_fact obs
on tr.patient_num = obs.patient_num and tr.encounter_num = obs.encounter_num and
   obs.start_date <= tr.triage_start + least(7,tr.end_since_triage/24) and
   to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
   tr.end_since_triage is not null



