/*******************************************************************************/
/*@file SI_case_ctrl.sql
/*
/*in: ED_SI, ED_eligb, SI_ServDep
/*
/*out: SI_case_ctrl
/*
/*action: write
/********************************************************************************/
create table SI_case_ctrl as
with ctrl_tri as (
select e.patient_num,e.encounter_num,e.triage_start,e.enc_end
from ED_eligb e
where not exists (select 1 from ED_SI si where e.patient_num=si.patient_num and e.encounter_num=si.encounter_num)
)
   ,ctrl as (
select tri.patient_num
      ,tri.encounter_num
      ,a.abx_name antibio_subtype
      ,tri.triage_start
      ,round(c.start_since_triage,2) c_since_triage
      ,round(a.start_since_triage,2) abx_since_triage
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
      ,si.c_since_triage
      ,si.abx_since_triage
      ,si.si_since_triage
      ,round((tri.enc_end-tri.triage_start)*24,2) end_since_triage
      ,1 case_ctrl
from ED_SI si
left join ED_eligb tri
on si.patient_num = tri.patient_num and si.encounter_num = tri.encounter_num
union all
select ctrl.patient_num
      ,ctrl.encounter_num
      ,ctrl.antibio_subtype
      ,ctrl.triage_start
      ,ctrl.c_since_triage
      ,ctrl.abx_since_triage
      ,null si_since_triage
      ,round(ctrl.end_since_triage,2) end_since_triage
      ,0 case_ctrl
from ctrl
where ctrl.rn = 1
)
   ,nonED_enc as (
select patient_num
      ,encounter_num
      ,ServDep_code
      ,ServDep_name
      ,IO_time
      ,hr_since_triage
      ,row_number() over (partition by patient_num, encounter_num order by hr_since_triage) rn
from SI_ServDep
where ServDep_code <> 'KUH|HOSP_ADT_CLASS:103' and hr_since_triage>=0
)
select cc.patient_num
      ,cc.encounter_num
      ,cc.antibio_subtype
      ,cc.triage_start
      ,noned.ServDep_name
      ,noned.IO_time trans_time
      ,cc.c_since_triage
      ,cc.abx_since_triage
      ,cc.si_since_triage
      ,noned.hr_since_triage trans_since_triage
      ,cc.end_since_triage
      ,coalesce(cc.SI_since_triage,noned.hr_since_triage,cc.end_since_triage) pred_point
      ,cc.case_ctrl
from case_ctrl cc
left join nonED_enc noned
on cc.patient_num = noned.patient_num and cc.encounter_num = noned.encounter_num and noned.rn = 1





