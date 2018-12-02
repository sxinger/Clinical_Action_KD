/*******************************************************************************/
/*@file ED_Culture.sql
/*
/*in: ED_eligb,blueherondata.observation_fact 
/*
/*params: @dblink, &&i2b2
/*       
/*out: ED_Culture
/*
/*action: write
/********************************************************************************/
create table ED_Culture as
with culture_all as (
select distinct
       tri.patient_num
      ,tri.encounter_num
      ,'Blood Culture' variable
      ,obs.concept_cd
      ,obs.start_date start_dt
      ,(obs.start_date - tri.triage_start)*24 start_since_triage
      ,obs.end_date end_dt
      ,(obs.end_date - tri.triage_start)*24 end_since_triage
from ED_eligb tri
join &&i2b2data.observation_fact@dblink obs
on obs.patient_num = tri.patient_num and 
   obs.encounter_num = tri.encounter_num
where obs.start_date > tri.triage_start and  /*after triage*/
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      obs.concept_cd in ('KUH|PROC_ID:10201618',
                         'KUH|PROC_ID:10201638')
union all
select distinct
       tri.patient_num
      ,tri.encounter_num
      ,'Other Fluid Culture' variable
      ,obs.concept_cd
      ,obs.start_date start_dt
      ,(obs.start_date - tri.triage_start)*24 start_since_triage
      ,obs.end_date end_dt
      ,(obs.end_date - tri.triage_start)*24 end_since_triage
from ED_eligb tri
join &&i2b2data.observation_fact@dblink obs
on obs.patient_num = tri.patient_num and 
   obs.encounter_num = tri.encounter_num
where obs.start_date > tri.triage_start and  /*after triage*/
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      obs.concept_cd in ('KUH|PROC_ID:10201614',
                         'KUH|PROC_ID:10201622',
                         'KUH|PROC_ID:10201626',
                         'KUH|PROC_ID:10201630',
                         'KUH|PROC_ID:10201642',
                         'KUH|PROC_ID:10201654',
                         'KUH|PROC_ID:10201660')
)
select patient_num
      ,encounter_num
      ,variable
      ,concept_cd code
      ,start_dt
      ,start_since_triage
      ,end_dt
      ,end_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt asc) rn
from culture_all 


