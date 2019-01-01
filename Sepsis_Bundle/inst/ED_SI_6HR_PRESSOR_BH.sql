/*********************************************************************************************************************************************************/
/*@file ED_SI_6HR_PRESSOR_BH.sql
/*
/*in: ED_SI_6HR_PRESSOR_NH@id
/*
/*out: ED_SI_6HR_PRESSOR
/*
/*action: write
/**********************************************************************************************************************************************************/
create table ED_SI_6HR_PRESSOR as
select distinct 
       obs.patient_num
      ,obs.encounter_num
      ,'vasopressor' variable
      ,obs.nval
      ,obs.units
      ,obs.tval
      ,obs.code
      ,obs.modifier
      ,obs.instance
      ,obs.start_dt
      ,round((obs.start_dt-e.hypot2_dt)*24,2)+e.hypot2_since_triage start_since_triage
      ,obs.start_dt end_dt
      ,round((obs.start_dt-e.hypot2_dt)*24,2)+e.hypot2_since_triage end_since_triage
from ED_SI_6HR_PRESSOR_NH@id obs
left join ED_SI_6HR_screen e
on obs.patient_num=e.patient_num and obs.encounter_num = e.encounter_num and
   obs.start_dt > e.hypot2_dt and





