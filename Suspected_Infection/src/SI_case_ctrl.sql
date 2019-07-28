/*******************************************************************************/
/*@file SI_case_ctrl.sql
/*
/*in: ED_SI, ED_eligb, SI_ServDep, observation_fact
/*
/*params: &&i2b2_db_schema
/*
/*out: SI_case_ctrl
/*
/*action: write
/********************************************************************************/
create table SI_case_ctrl as
with disch_ud as (
select e.patient_num
      ,e.encounter_num
      ,e.triage_start
      ,e.ed_dest_assgm_dt
      ,e.ed_dest_assgm
      ,e.first_fact_dt
      ,e.last_flosht_dt
      ,e.enc_end
      ,max(obs.start_date) ed_last_note_dt
from ED_eligb e
left join &&i2b2_db_schemadata.observation_fact obs
on e.patient_num = obs.patient_num and e.encounter_num = obs.encounter_num and
   obs.concept_cd = 'KUMC|REPORTS|NOTETYPES:6' and
   obs.start_date > e.triage_start 
group by e.patient_num,e.encounter_num,e.triage_start,e.ed_dest_assgm_dt,e.ed_dest_assgm,e.first_fact_dt,e.last_flosht_dt,e.enc_end
)
   ,ctrl_tri as (
select eu.patient_num,eu.encounter_num,eu.triage_start,eu.ed_dest_assgm_dt,eu.ed_dest_assgm,
       eu.first_fact_dt,eu.last_flosht_dt,eu.ed_last_note_dt,eu.enc_end
from disch_ud eu
where not exists (select 1 from ED_SI si where eu.patient_num=si.patient_num and eu.encounter_num=si.encounter_num)
)
   ,ctrl as (
select tri.patient_num
      ,tri.encounter_num
      ,a.abx_name antibio_subtype
      ,tri.triage_start
      ,tri.ed_dest_assgm_dt
      ,tri.ed_dest_assgm
      ,tri.ed_last_note_dt
      ,tri.first_fact_dt
      ,tri.last_flosht_dt
      ,tri.enc_end
      ,round(c.start_since_triage,2) c_since_triage
      ,round(a.start_since_triage,2) abx_since_triage
      ,(tri.ed_dest_assgm_dt-tri.triage_start)*24 dest_since_triage
      ,(tri.ed_last_note_dt-tri.triage_start)*24 last_note_since_triage
      ,(tri.last_flosht_dt-tri.triage_start)*24 last_flosht_since_triage
      ,(tri.enc_end-tri.triage_start)*24 end_since_triage
      ,row_number() over (partition by tri.patient_num, tri.encounter_num order by coalesce(a.start_since_triage,c.start_since_triage)) rn
from ctrl_tri tri
left join (select * from ED_Culture where rn =1) c 
on tri.patient_num=c.patient_num and tri.encounter_num=c.encounter_num
left join (select * from ED_Antibio where rn = 1) a 
on tri.patient_num=a.patient_num and tri.encounter_num=a.encounter_num
)
    ,case_ctrl as (
select si.patient_num
      ,si.encounter_num
      ,si.antibio_subtype
      ,tri.triage_start
      ,tri.ed_dest_assgm_dt
      ,tri.ed_dest_assgm
      ,tri.ed_last_note_dt      
      ,tri.first_fact_dt
      ,tri.last_flosht_dt
      ,tri.enc_end 
      ,si.c_since_triage
      ,si.abx_since_triage
      ,si.si_since_triage
      ,round((tri.ed_dest_assgm_dt-tri.triage_start)*24,2) dest_since_triage
      ,round((tri.ed_last_note_dt-tri.triage_start)*24,2) last_note_since_triage
      ,round((tri.last_flosht_dt-tri.triage_start)*24,2) last_flosht_since_triage
      ,round((tri.enc_end-tri.triage_start)*24,2) end_since_triage
      ,1 case_ctrl
from ED_SI si
left join disch_ud tri
on si.patient_num = tri.patient_num and si.encounter_num = tri.encounter_num
union all
select ctrl.patient_num
      ,ctrl.encounter_num
      ,ctrl.antibio_subtype
      ,ctrl.triage_start
      ,ctrl.ed_dest_assgm_dt
      ,ctrl.ed_dest_assgm
      ,ctrl.ed_last_note_dt   
      ,ctrl.first_fact_dt
      ,ctrl.last_flosht_dt
      ,ctrl.enc_end 
      ,ctrl.c_since_triage
      ,ctrl.abx_since_triage
      ,null si_since_triage
      ,round(ctrl.dest_since_triage,2) dest_since_triage
      ,round(ctrl.last_note_since_triage,2) last_note_since_triage
      ,round(ctrl.last_flosht_since_triage,2) last_flosht_since_triage
      ,round(ctrl.end_since_triage,2) end_since_triage
      ,0 case_ctrl
from ctrl
where ctrl.rn = 1
)
   ,nonED_enc as (
select patient_num
      ,encounter_num
      ,ServDep_name
      ,IO_time
      ,hr_since_triage
from SI_ServDep
where post_triage_ind = 1 and rn = 1
)
   ,nonED_enc2 as (
select s.patient_num
      ,s.encounter_num
      ,s.ServDep_name
      ,s.IO_time
      ,s.hr_since_triage
from SI_ServDep s
where not exists (select 1 from nonED_enc ned
                  where ned.patient_num = s.patient_num and ned.encounter_num = s.encounter_num) and
      s.rn = 1
)
select cc.patient_num
      ,cc.encounter_num
      ,cc.antibio_subtype
      ,cc.triage_start
      ,cc.ed_dest_assgm_dt
      ,cc.ed_dest_assgm
      ,cc.ed_last_note_dt   
      ,cc.first_fact_dt
      ,cc.last_flosht_dt
      ,cc.enc_end
      ,noned.ServDep_name
      ,noned.IO_time trans_time
      ,cc.c_since_triage
      ,cc.abx_since_triage
      ,cc.si_since_triage
      ,noned.hr_since_triage trans_since_triage
      ,cc.dest_since_triage
      ,cc.last_note_since_triage
      ,cc.last_flosht_since_triage
      ,cc.end_since_triage
      ,cc.case_ctrl
      ,case when noned.hr_since_triage is not null and noned.hr_since_triage > 0 then 'T'
            when noned.hr_since_triage <= 0 then 'U'
            else 'D'
       end as DT_class
from case_ctrl cc
left join (select * from nonED_enc union all select * from nonED_enc2) noned
on cc.patient_num = noned.patient_num and cc.encounter_num = noned.encounter_num






