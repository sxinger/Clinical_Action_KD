/*******************************************************************************/
/*@file ED_SI_SCR.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: &&i2b2_db_schema, &&within_d
/*       
/*out: ED_SI_SCR
/*
/*action: write
/********************************************************************************/
create table ED_SI_SCR as
with SI_Scr as (
select distinct 
       init.patient_num
      ,init.encounter_num
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,row_number() over (partition by obs.patient_num,obs.encounter_num order by obs.start_date) rn
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd in ('KUH|COMPONENT_ID:2009','KUH|COMPONENT_ID:3730') and
      regexp_like(obs.units_cd, '(mg/dL)','i') and
      obs.modifier_cd = '@' and obs.nval_num is not null and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and 
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      init.case_ctrl=1
)
    ,prior_cr as (
select distinct
       pat.patient_num
      ,pat.encounter_num
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.start_date start_dt
      ,round((obs.start_date-pat.start_dt)*24,2)+pat.start_since_triage start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-pat.end_dt)*24,2)+pat.end_since_triage end_since_triage
      ,row_number() over (partition by pat.patient_num,pat.encounter_num order by obs.start_date desc) rn
from SI_Scr pat
join &&i2b2_db_schemadata.observation_fact obs
on pat.patient_num = obs.patient_num
where obs.concept_cd in ('KUH|COMPONENT_ID:2009','KUH|COMPONENT_ID:3730') and
      regexp_like(obs.units_cd, '(mg/dL)','i') and 
      obs.modifier_cd = '@' and obs.nval_num is not null and
      trunc(obs.start_date) < trunc(pat.start_dt) and
      trunc(obs.start_date) >= (trunc(pat.start_dt) - 60) and
      pat.rn = 1
)
    ,SI_Scr_base as (
select distinct 
       fcr.patient_num
      ,fcr.encounter_num
      ,coalesce(pcr.nval,fcr.nval) nval
      ,coalesce(pcr.units,fcr.units) units
      ,coalesce(pcr.code,fcr.code) code
      ,coalesce(pcr.start_dt,fcr.start_dt) start_dt
      ,coalesce(pcr.start_since_triage,fcr.start_since_triage) start_since_triage
      ,coalesce(pcr.end_dt,fcr.end_dt) end_dt
      ,coalesce(pcr.end_since_triage,fcr.end_since_triage) end_since_triage
from (select * from SI_Scr where rn=1) fcr 
left join (select * from prior_cr where rn=1) pcr 
on fcr.patient_num = pcr.patient_num and fcr.encounter_num = pcr.encounter_num
)
select a.patient_num
      ,a.encounter_num
      ,'Creatinine' variable
      ,round(avg(a.nval),2) nval
      ,b.nval nval_base
      ,round(avg(a.nval),2)-b.nval as nval_inc
      ,a.units
      ,'KUH|COMPONENT_ID:2009' code
      ,case when (round(avg(a.nval),2)-b.nval) > 3 or round(avg(a.nval),2) > 4 then 1 else 0 
       end as flag
      ,a.start_dt
      ,a.start_since_triage
      ,(a.start_since_triage-b.start_since_triage) start_since_baseline
      ,a.end_dt
      ,a.end_since_triage
from SI_Scr a
join SI_Scr_base b
on a.patient_num = b.patient_num and a.encounter_num = b.encounter_num
group by a.patient_num,a.encounter_num,'Creatinine',b.nval,a.units,'KUH|COMPONENT_ID:2009'
        ,a.start_dt,a.start_since_triage,a.end_dt,a.end_since_triage
        ,(a.start_since_triage-b.start_since_triage)


