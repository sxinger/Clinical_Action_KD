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
with case_ctrl as (
select si.patient_num
      ,si.encounter_num
      ,si.antibio_subtype
      ,tri.triage_start
      ,si.SI_since_triage
      ,round((tri.enc_end-tri.triage_start)*24,2) end_since_triage
      ,1 case_ctrl
from ED_SI si
left join ED_eligb tri
on si.patient_num = tri.patient_num and
   si.encounter_num = tri.encounter_num
union all
select tri.patient_num
      ,tri.encounter_num
      ,null antibio_subtype
      ,tri.triage_start
      ,null SI_since_triage
      ,round((tri.enc_end-tri.triage_start)*24,2) end_since_triage
      ,0 case_ctrl
from ED_eligb tri
where not exists (select 1 from ED_SI si 
                  where tri.patient_num=si.patient_num and 
                        tri.encounter_num=si.encounter_num)
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
where ServDep_code <> 'KUH|HOSP_ADT_CLASS:103' and
      hr_since_triage>=0
)
select cc.patient_num
      ,cc.encounter_num
      ,cc.antibio_subtype
      ,cc.triage_start
      ,noned.ServDep_name
      ,noned.IO_time trans_time
      ,cc.SI_since_triage
      ,noned.hr_since_triage trans_since_triage
      ,cc.end_since_triage
      ,coalesce(cc.SI_since_triage,noned.hr_since_triage,cc.end_since_triage) pred_point
      ,cc.case_ctrl
from case_ctrl cc
left join nonED_enc noned
on cc.patient_num = noned.patient_num and 
   cc.encounter_num = noned.encounter_num and
   noned.rn = 1



