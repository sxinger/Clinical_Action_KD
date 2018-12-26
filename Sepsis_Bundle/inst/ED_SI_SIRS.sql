/*******************************************************************************/
/*@file ED_SI_SIRS.sql
/*
/*in: ED_SI_Temp,ED_SI_HR,ED_SI_RR,ED_SI_WBC
/*
/*out: ED_SI_SIRS
/*
/*action: write
/********************************************************************************/
create table ED_SI_SIRS as
with sirs1_stack as (
select * from ED_SI_Temp
where flag = 1 and rn = 1
union all 
select * from ED_SI_HR
where flag = 1 and rn = 1
union all 
select * from ED_SI_RR
where flag = 1 and rn = 1
union all 
select * from ED_SI_WBC
where flag = 1 and rn = 1
)
   ,sirs_order as (
select distinct
       patient_num
      ,encounter_num
      ,variable
      ,nval
      ,units
      ,code
      ,start_dt
      ,start_since_triage
      ,row_number() over (partition by patient_num,encounter_num order by start_dt,variable) rn
from sirs1_stack
order by patient_num, encounter_num, rn
)
   ,concat_2sirs as (
select so.patient_num
      ,so.encounter_num
      ,'2 SIRS' variable
      ,listagg(so.variable,'+') within group (order by so.start_dt,so.variable) tval
      ,listagg(so.code,'+') within group (order by so.start_dt,so.variable) code
      ,max(so.start_dt) start_dt
      ,max(so.start_since_triage) start_since_triage
      ,max(so.rn) rn
from sirs_order so
where so.rn <= 2
group by so.patient_num, so.encounter_num, '2 SIRS'
)
   ,concat_3sirs as (
select so.patient_num 
      ,so.encounter_num
      ,'3 SIRS' variable
      ,listagg(so.variable,'+') within group (order by so.start_dt,so.variable) tval
      ,listagg(so.code,'+') within group (order by so.start_dt,so.variable) code
      ,max(so.start_dt) start_dt
      ,max(so.start_since_triage) start_since_triage
      ,max(so.rn) rn
from sirs_order so
where so.rn <= 3
group by so.patient_num,so.encounter_num, '3 SIRS'
)
   ,concat_4sirs as (
select so.patient_num 
      ,so.encounter_num
      ,'4 SIRS' variable
      ,listagg(so.variable,'+') within group (order by so.start_dt,so.variable) tval
      ,listagg(so.code,'+') within group (order by so.start_dt,so.variable) code
      ,max(so.start_dt) start_dt
      ,max(so.start_since_triage) start_since_triage
      ,max(so.rn) rn
from sirs_order so
where so.rn <= 4
group by so.patient_num,so.encounter_num, '4 SIRS'
)
select distinct
       s1.patient_num
      ,s1.encounter_num
      ,'1 SIRS' variable
      ,s1.variable tval
      ,s1.code
      ,s1.start_dt
      ,s1.start_since_triage
from sirs_order s1
where s1.rn=1
union all
select distinct 
       s2.patient_num
      ,s2.encounter_num
      ,s2.variable
      ,s2.tval
      ,s2.code
      ,s2.start_dt
      ,s2.start_since_triage
from concat_2sirs s2
where s2.rn = 2
union all
select distinct 
       s3.patient_num
      ,s3.encounter_num
      ,s3.variable
      ,s3.tval
      ,s3.code
      ,s3.start_dt
      ,s3.start_since_triage
from concat_3sirs s3
where s3.rn = 3
union all
select distinct 
       s4.patient_num
      ,s4.encounter_num
      ,s4.variable
      ,s4.tval
      ,s4.code
      ,s4.start_dt
      ,s4.start_since_triage
from concat_4sirs s4
where s4.rn = 4


