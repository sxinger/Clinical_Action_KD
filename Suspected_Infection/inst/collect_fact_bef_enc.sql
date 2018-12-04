/*******************************************************************************/
/*@file SI_obs_bef_enc.sql
/*
/*in: ED_18up, ED_Culture, ED_Antibio, ED_SI, SI_case_ctrl
/*
/*params: @dblink, &&i2b2, &&start_date
/*
/*out: SI_obs_bef_enc
/*
/*action: write
/********************************************************************************/
create table SI_obs_bef_enc as
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
      ,round(obs.start_date-tr.triage_start,3) day_bef_triage
from SI_case_ctrl tr
join &&i2b2data.observation_fact@dblink obs
on tr.patient_num = obs.patient_num
where obs.start_date < tr.triage_start and
      (obs.concept_cd like 'KUH|MEDICATION_ID%' or
       obs.concept_cd like 'NDC%' or
       obs.concept_cd like 'CPT%' or
       obs.concept_cd like 'KUH|DX_ID%' or
       obs.concept_cd like 'ICD%' or
       obs.concept_cd like 'KUH|PAT_ENC%') and
      obs.start_date >= Date &&start_date and
      obs.start_date >= tr.triage_start - 365.25




