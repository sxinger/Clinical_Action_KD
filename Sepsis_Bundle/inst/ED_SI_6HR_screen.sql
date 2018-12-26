/*******************************************************************************/
/*@file ED_SI_6HR_screen.sql
/*
/*in: ED_SI_BP, ED_SI_3HR_IV
/*
/*out: ED_SI_6HR_screen
/*
/*action: write
/********************************************************************************/
create table ED_SI_6HR_screen as
with iv_end as (
select patient_num,encounter_num
      ,min(start_dt) start_dt
      ,min(start_since_triage) start_since_triage
      ,min(start_since_trigger) start_since_trigger
from ED_SI_3HR_IV
where flag = 1
group by patient_num,encounter_num
)
    ,all_post_hypot as (
select iv_end.patient_num
      ,iv_end.encounter_num
      ,iv_end.start_dt iv_cmplt_dt
      ,iv_end.start_since_triage iv_cmplt_since_triage
      ,iv_end.start_since_trigger iv_cmplt_since_trigger
      ,hypo.variable
      ,hypo.nval
      ,hypo.modifier
      ,hypo.start_dt hypot2_dt
      ,hypo.start_since_triage hypot2_since_triage
      ,hypo.start_since_triage-iv_end.start_since_triage+iv_end.start_since_trigger hypot2_since_trigger
      ,row_number() over (partition by iv_end.patient_num,iv_end.encounter_num order by hypo.start_dt) rn
from ED_SI_BP hypo
join iv_end
on iv_end.patient_num = hypo.patient_num and iv_end.encounter_num = hypo.encounter_num and
   hypo.flag = 1 and hypo.start_dt > iv_end.start_dt
)
select patient_num
      ,encounter_num
      ,'Remain Hypotensive' as trigger_type
      ,(variable || ':' || nval) tval
      ,iv_cmplt_dt
      ,iv_cmplt_since_triage
      ,iv_cmplt_since_trigger
      ,hypot2_dt
      ,hypot2_since_triage
      ,hypot2_since_trigger
from all_post_hypot
where rn = 1



