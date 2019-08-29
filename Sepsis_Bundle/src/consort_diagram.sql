/*******************************************************************************/
/*@file SI_CASE_CTRL.sql
/*
/*in: ED_18up, ED_SI_SEPSIS_STAGE  
/*
/*action: query
/********************************************************************************/
select 1 as idx,'Initial' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_18up
union all
select 2 as idx,'Suspected Infection' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
union all
select 3 as idx, 'Alert-2 SIRS' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is not null
union all
select 4 as idx,'Alert-2 SIRS, No Organ Dysfunction' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where SIRS_2_since_triage is not null and 
      coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is null
union all
select 5 as idx,'Alert-1st Organ Dysfunction' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null
union all
select 6 as idx,'Alert-1st Organ Dysfunction, No SIRS' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null and
      coalesce(SIRS_1_since_triage,SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is null
union all
select 7 as idx,'Alert-1st Organ Dysfunction, No 2 SIRS' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null and
      coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is null
union all
select 8 as idx,'Alert-1st Organ Dysfunction, 2 SIRS - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null and
      coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is not null
union all
select 9 as idx,coalesce(OD_1_tval,OD_2_tval,OD_3_tval,OD_4_tval,OD_5_tval,OD_6_tval,OD_7_tval) CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_tval,OD_2_tval,OD_3_tval,OD_4_tval,OD_5_tval,OD_6_tval,OD_7_tval) is not null and
      coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is not null
group by coalesce(OD_1_tval,OD_2_tval,OD_3_tval,OD_4_tval,OD_5_tval,OD_6_tval,OD_7_tval)
union all
select 10 as idx,'Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1
union all
select 11 as idx,'Sepsis - Hypotensive' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and iv_trigger_since_triage is not null
union 
select 12 as idx,'Sepsis - Non-Hypotensive' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE 
where sepsis_ind=1 and iv_trigger_since_triage is null
union all
select 13 as idx,'Initial Lactate - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE_since_triage is not null and sepsis_ind=0
union all
select 14 as idx,'Initial Lactate - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE_since_triage is not null and sepsis_ind=1
union all
select 15 as idx,'Initial Lactate - Hypotensive-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE_since_triage is not null and sepsis_ind=1 and iv_trigger_since_triage is not null
union all
select 16 as idx,'Initial Lactate - NonHypotensive-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE_since_triage is not null and sepsis_ind=1 and iv_trigger_since_triage is null
union all
select 17 as idx,'Sepsis before Culture' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and c_since_triage > sepsis_since_triage
union all
select 18 as idx,'Hypotensive-Sepsis before Culture' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and c_since_triage > sepsis_since_triage and iv_trigger_since_triage is not null
union all
select 19 as idx,'NonHypotensive-Sepsis before Culture' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and c_since_triage > sepsis_since_triage and iv_trigger_since_triage is null
union all
select 20 as idx,'Sepsis before Lactate' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and LACTATE_since_triage > sepsis_since_triage
union all
select 21 as idx,'Hypotensive-Sepsis before Lactate' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and LACTATE_since_triage > sepsis_since_triage and iv_trigger_since_triage is not null
union all
select 22 as idx,'NonHypotensive-Sepsis before Lactate' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and LACTATE_since_triage > sepsis_since_triage and iv_trigger_since_triage is null
union all
select 23 as idx,'Sepsis before Diagnostic Component' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(c_since_triage,coalesce(LACTATE_since_triage,c_since_triage)) > sepsis_since_triage
union all
select 24 as idx,'Hypotensive-Sepsis before Diagnostic Components' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(c_since_triage,coalesce(LACTATE_since_triage,c_since_triage)) > sepsis_since_triage and iv_trigger_since_triage is not null
union all
select 25 as idx,'NonHypotensive-Sepsis before Diagnostic Components' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(c_since_triage,coalesce(LACTATE_since_triage,c_since_triage)) > sepsis_since_triage and iv_trigger_since_triage is null
union all
select 26 as idx,'IV triggered - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_trigger_since_triage is not null and sepsis_ind=0
union all
select 27 as idx,'IV initiated - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(IV_0_since_triage,IV_1_since_triage,IV_2_since_triage,IV_3_since_triage) is not null and sepsis_ind=0
union all
select 28 as idx,'IV completed - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_complt_since_triage is not null and sepsis_ind=0
union all
select 29 as idx,'IV triggered - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_trigger_since_triage is not null and sepsis_ind=1
union all
select 30 as idx,'IV initiated - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(IV_0_since_triage,IV_1_since_triage,IV_2_since_triage,IV_3_since_triage) is not null and sepsis_ind=1
union all
select 31 as idx,'IV completed - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_complt_since_triage is not null and sepsis_ind=1
union all
select 32 as idx,'Sepsis before IV triggered' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_trigger_since_triage is not null and sepsis_ind=1 and sepsis_since_triage < iv_trigger_since_triage
union all
select 33 as idx,'Sepsis before IV initiated' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where IV_0_since_triage is not null and sepsis_ind=1 and sepsis_since_triage < IV_0_since_triage
union all
select 34 as idx,'Sepsis before IV completed' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_complt_since_triage is not null and sepsis_ind=1 and sepsis_since_triage < iv_complt_since_triage
union all
select 35 as idx,'Sepsis before Antibiotics' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and abx_since_triage > sepsis_since_triage
union all
select 36 as idx,'Hypotensive-Sepsis before Antibiotics' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and abx_since_triage > sepsis_since_triage and iv_trigger_since_triage is not null
union all
select 37 as idx,'NonHypotensive-Sepsis before Antibiotics' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and abx_since_triage > sepsis_since_triage and iv_trigger_since_triage is null
union all
select 38 as idx,'Sepsis before Therapautic Components' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(abx_since_triage,coalesce(IV_0_since_triage,abx_since_triage)) > sepsis_since_triage
union all
select 39 as idx,'Hypotensive-Sepsis before Therapautic Components' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(abx_since_triage,coalesce(IV_0_since_triage,abx_since_triage)) > sepsis_since_triage and iv_trigger_since_triage is not null
union all
select 40 as idx,'NonHypotensive-Sepsis before Therapautic Components' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(abx_since_triage,coalesce(IV_0_since_triage,abx_since_triage)) > sepsis_since_triage and iv_trigger_since_triage is null
union all
select 41 as idx,'Repeat Lactate' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE2_since_triage is not null
union all
select 42 as idx,'Remain Hypotensive' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where hypot2_since_trigger is not null
union all
select 43 as idx,'Vasopressor initiated' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where pressor_since_triage is not null
union all
select 44 as idx,'Cardiac events' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where resus_since_triage is not null


