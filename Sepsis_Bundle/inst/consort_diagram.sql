/*******************************************************************************/
/*@file SI_CASE_CTRL.sql
/*
/*in: ED_18up, ED_Culture, ED_Antibio, ED_SI, SI_case_ctrl  
/*
/*action: query
/********************************************************************************/
select 'Initial' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_18up
union all
select 'Suspected Infection' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
union all
select 'Alert-2 SIRS' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is not null
union all
select 'Alert-2 SIRS, No Organ Dysfunction' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where SIRS_2_since_triage is not null and 
      coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is null
union all
select 'Alert-1st Organ Dysfunction' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null
union all
select 'Alert-1st Organ Dysfunction, No SIRS' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null and
      coalesce(SIRS_1_since_triage,SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is null
union all
select 'Alert-1st Organ Dysfunction, No 2 SIRS' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null and
      coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is null
union all
select 'Alert-1st Organ Dysfunction, 2 SIRS - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_since_triage,OD_2_since_triage,OD_3_since_triage,OD_4_since_triage,OD_5_since_triage,OD_6_since_triage,OD_7_since_triage) is not null and
      coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is not null
union all
select coalesce(OD_1_tval,OD_2_tval,OD_3_tval,OD_4_tval,OD_5_tval,OD_6_tval,OD_7_tval) CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where coalesce(OD_1_tval,OD_2_tval,OD_3_tval,OD_4_tval,OD_5_tval,OD_6_tval,OD_7_tval) is not null and
      coalesce(SIRS_2_since_triage,SIRS_3_since_triage,SIRS_4_since_triage) is not null
group by coalesce(OD_1_tval,OD_2_tval,OD_3_tval,OD_4_tval,OD_5_tval,OD_6_tval,OD_7_tval)
union all
select 'Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1
union all
select 'Initial Lactate - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE_since_triage is not null and sepsis_ind=0
union all
select 'Initial Lactate - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE_since_triage is not null and sepsis_ind=1
union all
select 'Sepsis before Culture' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and c_since_triage > sepsis_since_triage
union all
select 'Sepsis before Lactate' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and LACTATE_since_triage > sepsis_since_triage
union all
select 'Sepsis before Diagnostic Component' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(c_since_triage,coalesce(LACTATE_since_triage,c_since_triage)) > sepsis_since_triage
union all
select 'IV triggered - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_trigger_since_triage is not null and sepsis_ind=0
union all
select 'IV completed - Non-Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_complt_since_triage is not null and sepsis_ind=0
union all
select 'IV triggered - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_trigger_since_triage is not null and sepsis_ind=1
union all
select 'IV initiated - Sepsis' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where IV_0_since_triage is not null and sepsis_ind=1
union all
select 'Sepsis before IV triggered' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_trigger_since_triage is not null and sepsis_ind=1 and sepsis_since_triage < iv_trigger_since_triage
union all
select 'Sepsis before IV initiated' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where IV_0_since_triage is not null and sepsis_ind=1 and sepsis_since_triage < IV_0_since_triage
union all
select 'Sepsis before IV completed' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where iv_complt_since_triage is not null and sepsis_ind=1 and sepsis_since_triage < iv_complt_since_triage
union all
select 'Sepsis before Antibiotics' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and abx_since_triage > sepsis_since_triage
union all
select 'Sepsis before Therapautic Component' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where sepsis_ind=1 and least(abx_since_triage,coalesce(IV_0_since_triage,abx_since_triage)) > sepsis_since_triage
union all
select 'Repeat Lactate' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where LACTATE2_since_triage is not null
union all
select 'Remain Hypotensive' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where hypot2_since_trigger is not null
union all
select 'Vasopressor initiated' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where pressor_since_triage is not null
union all
select 'Cardiac events' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI_SEPSIS_STAGE
where resus_since_triage is not null


