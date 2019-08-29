/*******************************************************************************/
/*@file SI_ServDep.sql
/*
/*in: ED_SI, blueherondata.observation_fact,blueherondata.concept_dimension 
/*
/*params: &&i2b2_db_schema
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
      ,obs.concept_cd
      ,obs.start_date IO_time
      ,round((obs.start_date - si.triage_start)*24,3) hr_since_triage
from ED_eligb si
join &&i2b2_db_schemadata.observation_fact obs
on si.patient_num = obs.patient_num and 
   si.encounter_num = obs.encounter_num and
   (obs.concept_cd like 'KUH|VISITDETAIL|HSP%' or
    obs.concept_cd like 'KUH|VISITDETAIL|AMB%' or
    obs.concept_cd like 'KUH|HOSP%' or
    obs.concept_cd like 'KUMC|VISITDETAIL|HSP%' or
    obs.concept_cd like 'KUMC|VISITDETAIL|POS(O2)' or
    obs.concept_cd in ('KUMC|REPORTS|NOTETYPES:19',
                       'KUMC|REPORTS|NOTETYPES:6')) and
   (obs.concept_cd <> 'KUH|HOSP_ADT_CLASS:103' and 
    obs.concept_cd <> 'KUMC|VISITDETAIL|HSPSERVICES:148' and 
    obs.concept_cd <> 'KUMC|VISITDETAIL|HSPSERVICES:22' and 
    obs.concept_cd <> 'KUMC|VISITDETAIL|POS(O2):23') and /*exclude ED class*/
  to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
)
   ,trans_dep_vs as (
select distinct
       d.patient_num
      ,d.encounter_num
      ,replace((cd.concept_path || cd.name_char),'\i2b2\Visit Details','') ServDep_name
      ,d.IO_time
      ,d.hr_since_triage
      ,case when d.hr_since_triage <=0 then 0 else 1 end as post_triage_ind
      ,case when regexp_like(cd.concept_path,'(inpatient)+','i') then 0 else 1 end as nonIP_ind
from collect_dep d
join &&i2b2_db_schemadata.concept_dimension cd
on d.concept_cd = cd.concept_cd and
   d.concept_cd not in ('KUMC|REPORTS|NOTETYPES:19','KUMC|REPORTS|NOTETYPES:6')
)
   ,trans_dep_note as (
select vs.patient_num
      ,vs.encounter_num
      ,max(nt.IO_time) IO_time
      ,max(nt.hr_since_triage) hr_since_triage
from trans_dep_vs vs
join collect_dep nt
on vs.patient_num = nt.patient_num and vs.encounter_num = nt.patient_num and
   nt.concept_cd in ('KUMC|REPORTS|NOTETYPES:19','KUMC|REPORTS|NOTETYPES:6') /*ED notes*/
group by vs.patient_num, vs.encounter_num
)
select distinct
       vs.patient_num
      ,vs.encounter_num
      ,vs.ServDep_name
      ,greatest(vs.IO_time,coalesce(vsn.IO_time,vs.IO_time)) IO_time
      ,greatest(vs.hr_since_triage,coalesce(vsn.hr_since_triage,vs.hr_since_triage)) hr_since_triage
      ,row_number() over (partition by vs.patient_num, vs.encounter_num, vs.post_triage_ind order by abs(vs.hr_since_triage),vs.nonIP_ind) rn
      ,dense_rank() over (partition by vs.patient_num, vs.encounter_num, vs.post_triage_ind order by abs(vs.hr_since_triage),vs.nonIP_ind) rn_dense
      ,vs.post_triage_ind
from trans_dep_vs vs
left join trans_dep_note vsn
on vs.patient_num = vsn.patient_num and vs.encounter_num = vsn.encounter_num


