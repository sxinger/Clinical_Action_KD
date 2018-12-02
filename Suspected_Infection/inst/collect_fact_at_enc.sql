/*******************************************************************************/
/*@file SI_obs_at_enc.sql
/*
/*in: ED_18up, ED_Culture, ED_Antibio, ED_SI, SI_case_ctrl
/*
/*params: @dblink, &&i2b2
/*
/*out: SI_obs_at_enc
/*
/*action: write
/********************************************************************************/
create table SI_obs_at_enc as
select tr.patient_num
      ,tr.encounter_num
      ,regexp_substr(obs.concept_cd,'[^\:]+',1,1) concept_prefix --used for partition
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
join &&i2b2data.observation_fact@dblink obs
on tr.patient_num = obs.patient_num and
   tr.encounter_num = obs.encounter_num
where obs.start_date between tr.triage_start and
                             tr.triage_start + coalesce(tr.pred_point,tr.end_since_triage) and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'



