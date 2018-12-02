/*******************************************************************************/
/*@file ED_Antibio.sql
/*
/*in: ED_Culture,blueherondata.observation_fact 
/*
/*params: @dblink, &&i2b2
/*       
/*out: ED_Antibio
/*
/*action: write
/********************************************************************************/
create table ED_Antibio as
with culture_uniquet as (
select distinct 
       patient_num
      ,encounter_num
      ,start_dt
      ,start_since_triage
from ED_Culture
)
   ,mar_admin as (
select c.patient_num
      ,c.encounter_num
      ,obs.concept_cd
      ,obs.nval_num 
      ,obs.units_cd 
      ,obs.start_date start_dt
      ,(obs.start_date - c.start_dt)*24+c.start_since_triage start_since_triage
      ,obs.end_date end_dt
      ,(obs.end_date - c.start_dt)*24+c.start_since_triage end_since_triage
      ,obs.modifier_cd
      ,obs.instance_num
      ,obs.start_date - c.start_dt hr_since_culture
from culture_uniquet c 
join &&i2b2data.observation_fact@dblink obs
on c.patient_num = obs.patient_num and
   c.encounter_num = obs.encounter_num and
   regexp_like(obs.modifier_cd,
              '((MAR:Given)|(MAR_Dose)|(MAR:New Bag)|
                (MAR:Cabinet Pull)|(MAR:Downtime)|(MAR:Bolus)|
                (MAR:ED-Infusing Upon Admit))','i') and /*must be administered*/
   to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and /*hourly*/
   (obs.start_date - c.start_dt)*24+c.start_since_triage > 0 /*should occur after triage start*/
)
select distinct
       a.patient_num
      ,a.encounter_num
      ,ht.med_group abx_group
      ,ht.med_name abx_name
      ,a.concept_cd
      ,a.nval_num
      ,a.units_cd
      ,a.start_dt
      ,a.start_since_triage
      ,a.end_dt
      ,a.end_since_triage
      ,a.modifier_cd
      ,a.instance_num
      ,ht.injectable_ind
      ,dense_rank() over (partition by a.patient_num,a.encounter_num order by a.start_dt asc) rn
      ,dense_rank() over (partition by a.patient_num,a.encounter_num order by a.start_dt desc) rn_desc
from mar_admin a
join MED_ANTIBIO_CD ht
on a.concept_cd = ht.med_id


