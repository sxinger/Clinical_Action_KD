/*******************************************************************************/
/*@file SI_hist_dx.sql
/*
/*in: SI_case_ctrl, observaton_fact, concept_dimension
/*
/*params: @dblink, &&i2b2, &&start_date
/*
/*out: SI_hist_dx
/*
/*action: write
/********************************************************************************/
create table SI_hist_dx as
with icd_cd as (
select concept_cd, name_char
from &&i2b2data.concept_dimension
where concept_path like '\i2b2\Diagnoses%' and concept_cd like 'ICD%'
) 
    ,dx_cd as (
select concept_cd, name_char, 
       regexp_substr(concept_path,'[^\\]+',1,3) DX_type,
       regexp_substr(concept_path,'[^\\]+',1,regexp_count(concept_path,'\\A',1,'i')+3) DX_parent
from &&i2b2data.concept_dimension
where concept_path like '\i2b2\Diagnoses\ICD%' and concept_cd like 'KUH|DX_ID%'
)
select distinct
       tr.patient_num
      ,tr.encounter_num
      ,obs.concept_cd
      ,dx_cd.name_char
      ,dx_cd.DX_type icd_type
      ,dx_cd.DX_parent icd_code
      ,obs.modifier_cd modifier
      ,obs.start_date start_dt
      ,round(obs.start_date-tr.triage_start) day_bef_triage
from SI_case_ctrl tr
join &&i2b2data.observation_fact@dblink obs on tr.patient_num = obs.patient_num
left join dx_cd on dx_cd.concept_cd = obs.concept_cd and
     obs.encounter_num <> tr.encounter_num and
     obs.concept_cd like 'KUH|DX_ID%' and
     obs.start_date >= Date &&start_date and
     obs.start_date < tr.first_fact_dt
union all
select distinct
       tr.patient_num
      ,tr.encounter_num
      ,obs.concept_cd
      ,icd_cd.name_char
      ,null icd_type
      ,null icd_code
      ,obs.modifier_cd modifier
      ,obs.start_date start_dt
      ,round(obs.start_date-tr.triage_start) day_bef_triage
from SI_case_ctrl tr
join &&i2b2data.observation_fact@dblink obs on tr.patient_num = obs.patient_num
left join icd_cd on icd_cd.concept_cd = obs.concept_cd and
     obs.encounter_num <> tr.encounter_num and
     obs.concept_cd like 'ICD%' and
     obs.start_date >= Date &&start_date and
     obs.start_date < tr.first_fact_dt






