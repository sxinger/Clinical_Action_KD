/*******************************************************************************/
/*@file ED_SI_3HR_screen.sql
/*
/*in: ED_SI_BP, ED_SI_LACTATE, observation_fact
/*
/*params: &&i2b2_db_schema
/*   
/*out: ED_SI_3HR_screen
/*
/*action: write
/********************************************************************************/
create table ED_SI_3HR_screen as
with hypo_il_stack as (
select hypo.patient_num 
      ,hypo.encounter_num
      ,'Hypotension' trigger_type
      ,min(hypo.start_dt) start_dt
      ,min(hypo.start_since_triage) start_since_triage
from ED_SI_BP hypo
where flag = 1
group by hypo.patient_num,hypo.encounter_num,'Hypotension'
union all
select il.patient_num
      ,il.encounter_num
      ,'High_Initial_Lactate' trigger_type
      ,min(il.start_dt) start_dt
      ,min(il.start_since_triage) start_since_triage
from ED_SI_LACTATE il
where il.nval > 4
group by il.patient_num,il.encounter_num, 'High_Initial_Lactate'
)
   ,trigger_agg as (
select patient_num
      ,encounter_num
      ,listagg(trigger_type,'+') within group (order by trigger_type) over (partition by patient_num,encounter_num,start_since_triage) trigger_type
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num, encounter_num order by start_since_triage) rn
from hypo_il_stack
)
   ,add_wt as (
select tri.patient_num
      ,tri.encounter_num
      ,tri.trigger_type
      ,tri.start_dt
      ,tri.start_since_triage
      ,avg(0.0283495*obs.nval_num) over (partition by tri.patient_num, tri.encounter_num,abs(tri.start_dt-obs.start_date) order by abs(tri.start_dt-obs.start_date) asc) nval
      ,round(tri.start_dt-obs.start_date) wt_meas_since
      ,row_number() over (partition by tri.patient_num, tri.encounter_num order by abs(tri.start_dt-obs.start_date) asc) wt_rn
from trigger_agg tri
left join &&i2b2_db_schemadata.observation_fact obs
on tri.patient_num = obs.patient_num and tri.encounter_num = obs.encounter_num and
   obs.concept_cd = 'KUH|PAT_ENC:WEIGHT'and tri.rn = 1
)
select patient_num
      ,encounter_num
      ,trigger_type
      ,start_dt
      ,start_since_triage
      ,round(nval,2) wt_kg
      ,case when nval is null or nval<1000 then 2000 else round(30*nval) end as iv_bd
      ,case when nval is null or nval<1000 then 2000 else round(30*nval/1000)*1000 end as iv_bd_int
      ,wt_meas_since
from add_wt
where wt_rn = 1



