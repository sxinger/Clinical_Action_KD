/*********************************************************************************************************************************************/
/*@file ED_SI_SEPSIS_STAGE.sql
/*
/*in: SI_case_ctrl, ED_SI_SIRS, ED_SI_OD, ED_SI_3HR_screen, ED_SI_3HR_ABX, ED_SI_3HR_IV, ED_SI_6HR_screen, ED_SI_6HR_PRESSOR, ED_SI_6HR_RESUS
/*
/*out: ED_SI_SEPSIS_STAGE
/*
/*action: write
/*********************************************************************************************************************************************/
create table ED_SI_SEPSIS_STAGE as
with sirs_pivot as (
select * from
(select patient_num, encounter_num,variable,start_since_triage
 from ED_SI_SIRS)
pivot
(min(start_since_triage) since_triage
 for variable in ('1 SIRS' as SIRS_1,
                  '2 SIRS' as SIRS_2,
                  '3 SIRS' as SIRS_3,
                  '4 SIRS' as SIRS_4)
                  
 )order by patient_num, encounter_num
)
    ,od_pivot as (
select * from
(select patient_num, encounter_num,variable,start_since_triage
 from ED_SI_OD)
pivot
(min(start_since_triage) since_triage
 for variable in ('1 OD' as OD_1,
                  '2 OD' as OD_2,
                  '3 OD' as OD_3,
                  '4 OD' as OD_4,
                  '5 OD' as OD_5,
                  '6 OD' as OD_6,
                  '7 OD' as OD_7)
                  
 )order by patient_num, encounter_num
)
    ,od_pivot2 as (
select * from
(select patient_num, encounter_num,variable,tval
 from ED_SI_OD)
pivot
(min(tval) tval
 for variable in ('1 OD' as OD_1,
                  '2 OD' as OD_2,
                  '3 OD' as OD_3,
                  '4 OD' as OD_4,
                  '5 OD' as OD_5,
                  '6 OD' as OD_6,
                  '7 OD' as OD_7)
                  
 )order by patient_num, encounter_num
)
    ,iv_pivot as (
select * from
(select patient_num, encounter_num,tval,start_since_triage
 from ED_SI_3HR_IV
 where rn_liter=1)
pivot
(min(start_since_triage) since_triage
 for tval in ('IV 0L' as IV_0,
                  'IV 1L' as IV_1,
                  'IV 2L' as IV_2,
                  'IV 3L' as IV_3)
                  
 )order by patient_num, encounter_num
)
select distinct
       si.patient_num
      ,si.encounter_num
      ,replace(regexp_substr(si.antibio_subtype,'[^( |\\()]+',1,1),'/','-') antibio_generic
      ,si.antibio_subtype antibio_fullname
      ,si.triage_start
      ,si.first_fact_dt
      ,si.servdep_name
      ,si.trans_time
      ,si.c_since_triage
      ,si.abx_since_triage
      ,si.si_since_triage
      ,sirs.SIRS_1_since_triage
      ,sirs.SIRS_2_since_triage
      ,sirs.SIRS_3_since_triage
      ,sirs.SIRS_4_since_triage
      ,od.OD_1_since_triage
      ,od2.OD_1_tval
      ,od.OD_2_since_triage
      ,od2.OD_2_tval
      ,od.OD_3_since_triage
      ,od2.OD_3_tval
      ,od.OD_4_since_triage
      ,od2.OD_4_tval
      ,od.OD_5_since_triage
      ,od2.OD_5_tval
      ,od.OD_6_since_triage
      ,od2.OD_6_tval
      ,od.OD_7_since_triage
      ,od2.OD_7_tval
      ,lac.start_since_triage LACTATE_since_triage
      ,ivt.start_since_triage iv_trigger_since_triage
      ,ivs.IV_0_since_triage
      ,ivs.IV_1_since_triage
      ,ivs.IV_2_since_triage
      ,ivs.IV_3_since_triage
      ,ivc.start_since_triage iv_complt_since_triage
      ,lac_rp.start_since_triage LACTATE2_since_triage
      ,hypot2.hypot2_since_trigger
      ,pressor.start_since_triage pressor_since_triage
      ,resus.start_since_triage resus_since_triage
      ,si.dest1_since_triage
      ,si.dest2_since_triage
      ,si.trans_since_triage
      ,si.end_since_triage
      ,si.dt_class
      ,case when coalesce(sirs.SIRS_2_since_triage,sirs.SIRS_3_since_triage,sirs.SIRS_4_since_triage) is not null and 
                 coalesce(od.OD_1_since_triage,od.OD_2_since_triage,od.OD_3_since_triage,od.OD_4_since_triage,od.OD_5_since_triage,od.OD_6_since_triage,od.OD_7_since_triage) is not null then 1 
       else 0 end as sepsis_ind
      ,greatest(si_since_triage,
                coalesce(sirs.SIRS_2_since_triage,sirs.SIRS_3_since_triage,sirs.SIRS_4_since_triage),
                coalesce(od.OD_1_since_triage,od.OD_2_since_triage,od.OD_3_since_triage,od.OD_4_since_triage,od.OD_5_since_triage,od.OD_6_since_triage,od.OD_7_since_triage)) sepsis_since_triage
from (select * from SI_case_ctrl where case_ctrl = 1) si
left join sirs_pivot sirs on si.patient_num=sirs.patient_num and si.encounter_num = sirs.encounter_num
left join od_pivot od on si.patient_num=od.patient_num and si.encounter_num = od.encounter_num
left join od_pivot2 od2 on si.patient_num=od2.patient_num and si.encounter_num = od2.encounter_num
left join (select * from ED_SI_LACTATE where rn=1) lac on si.patient_num=lac.patient_num and si.encounter_num = lac.encounter_num
left join ED_SI_3HR_SCREEN ivt on si.patient_num=ivt.patient_num and si.encounter_num = ivt.encounter_num
left join iv_pivot ivs on si.patient_num=ivs.patient_num and si.encounter_num = ivs.encounter_num
left join (select * from ED_SI_3HR_IV where rn_flag=1 and flag=1) ivc on si.patient_num=ivc.patient_num and si.encounter_num = ivc.encounter_num
left join (select * from ED_SI_LACTATE where repeat_ind=1) lac_rp on si.patient_num=lac_rp.patient_num and si.encounter_num = lac_rp.encounter_num
left join ED_SI_6HR_SCREEN hypot2 on si.patient_num=hypot2.patient_num and si.encounter_num = hypot2.encounter_num
left join (select * from ED_SI_6HR_PRESSOR where rn=1) pressor on si.patient_num=pressor.patient_num and si.encounter_num = pressor.encounter_num
left join (select card.*,row_number() over (partition by card.patient_num,card.encounter_num order by card.start_since_triage) rn 
           from ED_SI_6HR_CARDIAC card where rn_label=1) resus 
     on si.patient_num=resus.patient_num and si.encounter_num = resus.encounter_num and resus.rn = 1


