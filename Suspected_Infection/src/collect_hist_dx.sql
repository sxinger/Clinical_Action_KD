/*******************************************************************************/
/*@file collect_hist_dx.sql
/*
/*in: SI_case_ctrl, observaton_fact, concept_dimension, comorb_dx_cd
/*
/*params: &&i2b2_db_schema, &&start_date, &&cohort
/*
/*out: SI_comorb_dx
/*
/*action: write
/********************************************************************************/
create table SI_hist_dx as
with dx_cd as (
select cd.concept_cd
      ,cd.name_char
      ,ht.c_hlevel
      ,replace(' \ ','\',ht.c_tooltip,cd) concept_path
      ,max(term_id) over (partition by ht.c_basecode)
from &&i2b2_db_schemadata.concept_dimension cd
join &&i2b2_db_schemadatametadata.heron_terms ht
on cd.concept_cd = ht.c_basecode and
   concept_path like '\i2b2\Diagnoses\ICD%'
)
    ,collect_dx as(
select tr.patient_num
      ,tr.encounter_num
      ,obs.concept_cd
      ,obs.modifier_cd modifier
      ,obs.start_date start_dt
      ,round(tr.triage_start-obs.start_date) day_bef_triage
from &&cohort tr
join &&i2b2_db_schemadata.observation_fact obs 
on tr.patient_num = obs.patient_num and 
   obs.encounter_num <> tr.encounter_num and 
   (obs.concept_cd like 'KUH|DX_ID%' or obs.concept_cd like 'ICD%') and
   trunc(obs.start_date) < trunc(tr.first_fact_dt) and obs.start_date >= Date &&start_date
)
select distinct
       cl.patient_num
      ,cl.encounter_num
      ,cl.concept_cd
      ,dx_cd.name_char
      ,dx_cd.icd_cd
      ,dx_cd.icd_label
      ,dx_cd.weight
      ,cl.modifier
      ,cl.start_dt
      ,cl.day_bef_triage
      ,row_number() over (partition by cl.patient_num, cl.encounter_num, cl.icd_cd order by cl.day_bef_triage) rn
from collect_dx cl
join dx_cd on dx_cd.concept_cd = obs.concept_cd





