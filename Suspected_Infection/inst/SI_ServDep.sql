/*******************************************************************************/
/*@file SI_ServDep.sql
/*
/*in: ED_SI, blueherondata.observation_fact,blueherondata.concept_dimension 
/*
/*params: @dblink, &&i2b2
/*
/*out: SI_ServDep
/*
/*action: write
/********************************************************************************/
create table SI_ServDep as
with collect_dep as (
select distinct
       si.patient_num
      ,si.encounter_num
      ,obs.concept_cd ServDep_code
      ,obs.start_date IO_time
      ,round((obs.start_date - si.triage_start)*24,3) hr_since_triage
from ED_eligb si
join &&i2b2data.observation_fact@dblink obs
on si.patient_num = obs.patient_num and 
   si.encounter_num = obs.encounter_num and
   (obs.concept_cd like 'KUH|VISITDETAIL|HSP%' or
    obs.concept_cd like 'KUH|VISITDETAIL|AMB%' or
    obs.concept_cd like 'KUH|HOSP%') and
   obs.concept_cd <> 'KUH|HOSP_ADT_CLASS:103' and /*exclude ED class*/
   obs.concept_cd <> 'KUH|HOSP_ADT_CLASS:102'     /*exclude AV class*/
)
   ,trans_dep_vs as (
select distinct
       d.patient_num
      ,d.encounter_num
      ,replace((cd.concept_path || cd.name_char),'\i2b2\Visit Details\ENC_TYPE','') ServDep_name
      ,d.IO_time
      ,d.hr_since_triage
from collect_dep d
join &&i2b2data.concept_dimension@dblink cd
on d.ServDep_code = cd.concept_cd
where to_char(d.IO_time,'HH24:MI:SS') <> '00:00:00'
)
   ,trans_dep_ord as (
select distinct
       si.patient_num
      ,si.encounter_num
      ,case when obs.concept_cd in ('KUH|PROC_ID:263','KUH|PROC_ID:10261622','KUH|PROC_ID:10261623','KUH|PROC_ID:10250968','KUH|PROC_ID:10211122','KUH|PROC_ID:10282092') then '\IP\101\Inpatient' 
            when obs.concept_cd in ('KUH|PROC_ID:10261672','KUH|PROC_ID:10261677') then '\OT\123\Rehab'
            when obs.concept_cd in ('KUH|PROC_ID:10261673','KUH|PROC_ID:10261678') then '\OT\124\Psych'
            when obs.concept_cd in ('KUH|PROC_ID:10219158','KUH|PROC_ID:10261675','KUH|PROC_ID:10261680','KUH|PROC_ID:10211123') then '\OT\127\Extended Recovery'
            when obs.concept_cd in ('KUH|PROC_ID:10211124','KUH|PROC_ID:10219157','KUH|PROC_ID:10261674','KUH|PROC_ID:10261679') then '\OS\104\Observation'
            when obs.concept_cd in ('KUH|PROC_ID:10261676','KUH|PROC_ID:10261681','KUH|PROC_ID:10219161') then '\OT\125\Procedure'
            when obs.concept_cd in ('KUH|PROC_ID:10219346') then '\AV\126\Outpatient Surgery'
            else null
       end as ServDep_name
      ,case when obs.concept_cd in ('KUH|PROC_ID:10261623','KUH|PROC_ID:10261677','KUH|PROC_ID:10261678',
                                    'KUH|PROC_ID:10261684','KUH|PROC_ID:10282092','KUH|PROC_ID:10261680',
                                    'KUH|PROC_ID:10261679','KUH|PROC_ID:10261681') then 0 
            else 1
       end as Bed_Request
      ,obs.start_date IO_time
      ,round((obs.start_date - si.triage_start)*24,3) hr_since_triage
      ,row_number() over (partition by obs.patient_num, obs.encounter_num order by obs.start_date) rn
from ED_eligb si
join &&i2b2data.observation_fact@dblink obs
on si.patient_num = obs.patient_num and 
   si.encounter_num = obs.encounter_num and
   obs.concept_cd in ('KUH|PROC_ID:263','KUH|PROC_ID:10261622','KUH|PROC_ID:10261623','KUH|PROC_ID:10250968','KUH|PROC_ID:10211122', /*admit to IP*/
                      'KUH|PROC_ID:10261672','KUH|PROC_ID:10261677', /*admit to REHAB*/
                      'KUH|PROC_ID:10261673','KUH|PROC_ID:10261678', /*admit to PSYCH*/
                      'KUH|PROC_ID:10219158','KUH|PROC_ID:10261675','KUH|PROC_ID:10261680','KUH|PROC_ID:10211123', /*extended recovery*/
                      'KUH|PROC_ID:10211124','KUH|PROC_ID:10219157','KUH|PROC_ID:10261674','KUH|PROC_ID:10261679', /*observational*/
                      'KUH|PROC_ID:10261676','KUH|PROC_ID:10261681','KUH|PROC_ID:10219161', /*procedure*/
                      'KUH|PROC_ID:10219346' /*outpatient surgery*/
                      ) and
   to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
   obs.start_date >= si.triage_start
)
select distinct
       vs.patient_num
      ,vs.encounter_num
      ,vs.ServDep_name ServDep_name_vs
      ,vs.IO_time IO_time_vs
      ,vs.hr_since_triage hr_since_triage_vs
      ,ord.ServDep_name ServDep_name_ord
      ,ord.IO_time IO_time_ord
      ,ord.hr_since_triage hr_since_triage_ord
      ,case when vs.hr_since_triage>0 or (ord.hr_since_triage is null and vs.ServDep_name='\IP\101\Inpatient') then vs.ServDep_name
            else ord.ServDep_name
       end as ServDep_name
      ,case when vs.hr_since_triage>0 or (ord.hr_since_triage is null and vs.ServDep_name='\IP\101\Inpatient') then vs.IO_time
            else ord.IO_time
       end as IO_time
      ,case when vs.hr_since_triage>0 or (ord.hr_since_triage is null and vs.ServDep_name='\IP\101\Inpatient') then vs.hr_since_triage
            else ord.hr_since_triage
       end as hr_since_triage
from trans_dep_vs vs
left join (select * from trans_dep_ord where rn = 1) ord
on vs.patient_num = ord.patient_num and vs.encounter_num = ord.encounter_num and
   vs.ServDep_name=ord.ServDep_name






