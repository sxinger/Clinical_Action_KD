/*********************************************************************************************************************************************************/
/*@file ED_SI_6HR_PRESSOR_NH.sql
/*
/*in: ED_SI_6HR_screen@deid, encounter_mapping, patient_mapping, patient_dimension, heron_etl_1.kuh_med_orders, clarity.mar_admin_info, clarity.zc_mar_rslt
/*
/*params: @dblink, &&i2b2
/*   
/*out: ED_SI_6HR_PRESSOR_NH
/*
/*action: write
/**********************************************************************************************************************************************************/
create table ED_SI_6HR_PRESSOR_NH as
with ide_enc_cw as (
select distinct 
       e.encounter_num
      ,ide.encounter_ide
      ,ide.patient_ide
from ED_SI_6HR_screen@deid e
join &&i2b2data.encounter_mapping@dblink ide on e.encounter_num = ide.encounter_num and
     patient_ide_source = 'Epic@kumed.com' and encounter_ide_source = 'Epic@kumed.com'
)
   ,ide_pn_cw as (
select epic.encounter_num
      ,epic.encounter_ide
      ,epic.patient_ide
      ,pm.patient_num
from ide_enc_cw epic
join &&i2b2data.patient_mapping@dblink pm on epic.patient_ide = pm.patient_ide and
     pm.patient_ide_source = 'Epic@kumed.com'
)
    ,ide_cw as (
select n.encounter_num
      ,n.encounter_ide
      ,n.patient_num
      ,n.patient_ide
      ,p.date_shift
from ide_pn_cw n
join &&i2b2data.patient_dimension@dblink p on n.patient_num = p.patient_num
)
     ,mar_vaso_order as (
select cw.patient_num
      ,cw.encounter_num
      ,m.concept_cd
      ,v.MED_NAME
      ,m.units_cd
      ,m.instance_num
      ,m.order_med_id
      ,m.start_date order_date
      ,e.date_shift
from ide_cw cw
join heron_etl_1.kuh_med_orders m on cw.patient_ide = m.patient_ide and cw.encounter_ide = m.encounter_ide
join PRESSOR_CD v on ('KUH|MEDICATION_ID:'||v.MED_ID) = m.concept_cd
)
select distinct 
       o.patient_num
      ,o.encounter_num
      ,to_number(mar.sig) nval
      ,o.units_cd units
      ,o.med_name tval
      ,o.concept_cd code
      ,('MedObs|MAR:' || cmr.name) modifier
      ,o.instance_num instance
      ,mar.taken_time + o.date_shift start_dt
from mar_vaso_order o
join clarity.mar_admin_info mar on o.order_med_id = mar.order_med_id
join clarity.zc_mar_rslt cmron mar.mar_action_c = cmr.result_c
where cmr.name not in ('Stopped','Dropped','Canceled Entry',
                       'Missed','Held','Med Not Available',
                       'See ED Trauma Flowsheet')





