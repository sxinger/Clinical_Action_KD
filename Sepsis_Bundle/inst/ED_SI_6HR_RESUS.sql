/*******************************************************************************/
/*@file ED_SI_6HR_RESUS.sql
/*
/*in: ED_SI_6HR_screen,observation_fact, concept_dimension 
/*
/*params: @dblink, &&i2b2
/*       
/*out: ED_SI_6HR_RESUS
/*
/*action: write
/********************************************************************************/
create table ED_SI_6HR_RESUS as
with order_cd as (
select distinct concept_cd, regexp_substr(name_char,'[^#]+',1,2) name_char, 'CVP' label
from &&i2b2data.concept_dimension@dblink
where regexp_like(name_char, '(( CVP )|(central venous pressure))+','i') and
      concept_cd like 'KUH|FLO_MEAS%'
union all
select distinct concept_cd, name_char, 'CVP' label
from &&i2b2data.concept_dimension@dblink
where regexp_like(name_char, '(( CVP )|(central venous pressure))+','i') and
      concept_cd like 'KUH|PROC_ID%'
union all
select distinct concept_cd, name_char, 'Echo' label
from &&i2b2data.concept_dimension@dblink
where regexp_like(name_char, '(echocardiogram)+','i') and
      concept_cd like 'KUH|COMPONENT_ID%'
union all
select distinct concept_cd, regexp_substr(name_char,'[^#]+',1,2) name_char, 'ScvO2' label
from &&i2b2data.concept_dimension@dblink
where regexp_like(name_char, '((ScvO2)|(CENTRAL VENOUS OXYGEN SATURATION))+','i') and
      concept_cd like 'KUH|FLO_MEAS%'
union all
select distinct concept_cd, name_char, 'ScvO2' label
from &&i2b2data.concept_dimension@dblink
where regexp_like(name_char, '((ScvO2)|(CENTRAL VENOUS OXYGEN SATURATION))+','i') and
      concept_cd like 'KUH|PROC_ID%'
union all
select distinct concept_cd, name_char, 'NICOM' label
from &&i2b2data.concept_dimension@dblink
where regexp_like(name_char, '((NICOM)|(EV-1000)|(Flo-Trac)|(Swan-Ganz))+','i') and
      concept_cd like 'KUH|FLO_MEAS%'
)
  ,collect_resus as (
select distinct
       obs.patient_num
      ,obs.encounter_num
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-e.hypot2_dt)*24,2)+e.hypot2_since_triage start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-e.hypot2_dt)*24,2)+e.hypot2_since_triage end_since_triage
from ED_SI_6HR_screen e 
join &&i2b2data.observation_fact@dblink obs
on e.patient_num = obs.patient_num and e.encounter_num = obs.encounter_num
where (obs.concept_cd like 'KUH|FLO_MEAS%' or obs.concept_cd like 'KUH|PROC_ID%' or obs.concept_cd like 'KUH|COMPONENT_ID%') and
       obs.start_date > e.hypot2_dt and 
       to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
)
select distinct
       cl.patient_num
      ,cl.encounter_num
      ,'resuscitation' variable
      ,cl.nval
      ,cl.units
      ,ht.label tval
      ,cl.code
      ,cl.modifier
      ,cl.instance
      ,cl.start_dt
      ,cl.start_since_triage
      ,cl.end_dt
      ,cl.end_since_triage
from collect_resus cl
join order_cd ht on ht.concept_cd = cl.code







