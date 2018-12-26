/*******************************************************************************/
/*@file ED_SI_OD.sql
/*
/*in: ED_SI_GCS, ED_SI_LOC, ED_SI_BP, ED_SI_BILRB, ED_SI_SCR, ED_SI_DDMIER, ED_SI_INR, ED_SI_PTT, ED_SI_PLATLT, ED_SI_SPO2, ED_SI_PAO2, ED_SI_PAFIO2, ED_SI_LACTATE
/*
/*out: ED_SI_OD
/*
/*action: write
/********************************************************************************/
create table ED_SI_OD as
with ams_meas as (
select patient_num
      ,encounter_num
      ,'OD:AMS' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_GCS
where flag = 1
union all
select patient_num
      ,encounter_num
      ,'OD:AMS' variable
      ,(variable || ':' || tval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_LOC
where flag = 1
)
    ,hypot_meas as (
select patient_num
      ,encounter_num
      ,'OD:Hypotension' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_BP
where flag = 1
)
    ,lf_meas as (
select patient_num
      ,encounter_num
      ,'OD:LF' variable
      ,(variable || ':' || nval || '_' || nval_inc) tval
      ,start_dt
      ,start_since_triage
from ED_SI_BILRB
where flag = 1
)
    ,kf_meas as (
select patient_num
      ,encounter_num
      ,'OD:KF' variable
      ,(variable || ':' || nval || '_' || nval_inc) tval
      ,start_dt
      ,start_since_triage
from ED_SI_SCR
where flag = 1
)
    ,ac_meas as (
select patient_num
      ,encounter_num
      ,'OD:AC' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_PLATLT
where flag = 1
union all
select patient_num
      ,encounter_num
      ,'OD:AC' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_INR
where flag = 1
union all
select patient_num
      ,encounter_num
      ,'OD:AC' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_PTT
where flag = 1
union all
select patient_num
      ,encounter_num
      ,'OD:AC' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_DDMIER
where flag = 1
)
   ,hypox_meas as (
select patient_num
      ,encounter_num
      ,'OD:Hypoxemia' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_SPO2
where flag = 1
union all
select patient_num
      ,encounter_num
      ,'OD:Hypoxemia' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_PAO2
where flag = 1
union all
select patient_num
      ,encounter_num
      ,'OD:Hypoxemia' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_PAFIO2
where flag = 1
)
  ,il_meas as (
select patient_num
      ,encounter_num
      ,'OD:IL' variable
      ,(variable || ':' || nval) tval
      ,start_dt
      ,start_since_triage
from ED_SI_LACTATE
where flag = 1
)
   ,any_OD as (
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from ams_meas
union all
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from hypot_meas
union all
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from lf_meas
union all
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from kf_meas
union all
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from ac_meas
union all
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from hypox_meas
union all
select patient_num
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num,encounter_num order by start_dt) rn
from il_meas
)
      ,od_merge as (
select patient_num 
      ,encounter_num
      ,variable
      ,listagg(tval,';') within group (order by tval) tval
      ,start_dt
      ,start_since_triage
from (select * from any_OD where rn=1) OD_stack
group by patient_num, encounter_num, variable, start_dt, start_since_triage
)
   ,od_order as (
select patient_num 
      ,encounter_num
      ,variable
      ,tval
      ,start_dt
      ,start_since_triage
      ,dense_rank() over (partition by patient_num, encounter_num order by start_dt) ds_rn
      ,row_number() over (partition by patient_num, encounter_num order by start_dt) rn
from od_merge
)
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 1
group by patient_num, encounter_num, start_dt,start_since_triage
union all
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 2
group by patient_num, encounter_num, start_dt,start_since_triage
union all
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 3
group by patient_num, encounter_num, start_dt,start_since_triage
union all
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 4
group by patient_num, encounter_num, start_dt,start_since_triage
union all
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 5
group by patient_num, encounter_num, start_dt,start_since_triage
union all
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 6
group by patient_num, encounter_num, start_dt,start_since_triage
union all
select patient_num 
      ,encounter_num
      ,(max(rn) ||' OD') variable
      ,start_dt
      ,start_since_triage
      ,listagg(variable,'+') within group (order by variable) tval
from od_order
where ds_rn = 7
group by patient_num, encounter_num, start_dt,start_since_triage



