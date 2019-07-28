/*******************************************************************************/
/*@file SI_obs_bef_enc.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: @dblink, &&i2b2, &&cohort, &&start_date
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
      ,round(obs.start_date-tr.triage_start) day_bef_triage
      ,row_number() over (partition by tr.patient_num, tr.encounter_num, obs.concept_cd order by obs.start_date) rn
      ,row_number() over (partition by tr.patient_num, tr.encounter_num, obs.concept_cd order by obs.start_date desc) rn_desc
from &&cohort tr
join &&i2b2data.observation_fact@dblink obs
on tr.patient_num = obs.patient_num
where obs.encounter_num <> tr.encounter_num and
      (obs.concept_cd like 'KUH|DX_ID%' or
       obs.concept_cd like 'ICD%' or
       (obs.concept_cd like 'KUH|MEDICATION_ID%' and 
        obs.modifier_cd in ('MedObs:Dispensed','MedObs:Historical','MedObs|MAR:Given',
                            'Surescripts:Amount','Surescripts:Days Supply','Surescripts|Status:Claim','Surescripts|Status:Fill'))) and
      obs.start_date >= Date &&start_date and
      obs.start_date >= tr.first_fact_dt - 180




