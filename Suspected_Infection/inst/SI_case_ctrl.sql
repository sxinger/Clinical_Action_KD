/*******************************************************************************/
/*@file SI_case_ctrl.sql
/*
/*in: ED_SI, ED_eligb, SI_ServDep, observation_fact
/*
/*params: @dblink, &&i2b2
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
      ,e.first_fact_dt
      ,e.last_fact_dt
      ,max(least(greatest(e.enc_end,e.triage_start),coalesce(obs.start_date,e.last_fact_dt))) enc_end
from ED_eligb e
left join &&i2b2data.observation_fact@dblink obs
on e.patient_num = obs.patient_num and e.encounter_num = obs.encounter_num and
   obs.concept_cd in ('KUMC|REPORTS|NOTETYPES:5',
                      'KUMC|REPORTS|NOTETYPES:60',
                      'KUMC|REPORTS|NOTETYPES:600000',
                      'KUMC|REPORTS|NOTETYPES:600028',
                      'KUMC|REPORTS|NOTETYPES:600069',
                      'KUMC|REPORTS|NOTETYPES:600070',
                      'KUMC|REPORTS|NOTETYPES:600071',
                      'KUMC|REPORTS|NOTETYPES:600072',
                      'KUMC|REPORTS|NOTETYPES:61',
                      'KUMC|REPORTS|NOTETYPES:62',
                      'KUMC|REPORTS|NOTETYPES:63',
                      'KUMC|REPORTS|NOTETYPES:64',
                      'KUMC|REPORTS|NOTETYPES:65',
                      'KUMC|REPORTS|NOTETYPES:66',
                      'KUMC|REPORTS|NOTETYPES:67',
                      'KUMC|REPORTS|NOTETYPES:68') and
    obs.start_date > e.triage_start 
group by e.patient_num,e.encounter_num,e.triage_start,e.first_fact_dt,e.last_fact_dt
)
   ,ctrl_tri as (
select eu.patient_num,eu.encounter_num,eu.triage_start,eu.first_fact_dt,eu.last_fact_dt,eu.enc_end
from disch_ud eu
where not exists (select 1 from ED_SI si where eu.patient_num=si.patient_num and eu.encounter_num=si.encounter_num)
)
   ,ctrl as (
select tri.patient_num
      ,tri.encounter_num
      ,a.abx_name antibio_subtype
      ,tri.triage_start
      ,tri.first_fact_dt
      ,tri.last_fact_dt
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
      ,tri.first_fact_dt
      ,tri.last_fact_dt
      ,si.c_since_triage
      ,si.abx_since_triage
      ,si.si_since_triage
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
      ,ctrl.first_fact_dt
      ,ctrl.last_fact_dt
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
      ,cc.first_fact_dt
      ,cc.last_fact_dt
      ,noned.ServDep_name
      ,noned.IO_time trans_time
      ,cc.c_since_triage
      ,cc.abx_since_triage
      ,cc.si_since_triage
      ,noned.hr_since_triage trans_since_triage
      ,cc.end_since_triage
      ,coalesce(cc.SI_since_triage,noned.hr_since_triage,cc.end_since_triage) pred_point
      ,cc.case_ctrl
      ,case when noned.hr_since_triage is not null and noned.hr_since_triage > 0 then 'T'
            when noned.hr_since_triage <= 0 then 'U'
            else 'D'
       end as DT_class
from case_ctrl cc
left join (select * from nonED_enc union all select * from nonED_enc2) noned
on cc.patient_num = noned.patient_num and cc.encounter_num = noned.encounter_num





