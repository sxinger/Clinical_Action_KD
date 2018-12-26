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
select 'Culture_ordered' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_Culture
union all
select 'Culture_ordered_and_Antibio_administered' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_Antibio
union all
select 'Culture_Antibio_within_4hr' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from ED_SI
union all
select ('Final_Case_Ctrl_' || case_ctrl),
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
group by case_ctrl
union all
select 'SI_within_ED' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is not null and (si_since_triage <= trans_since_triage or trans_since_triage is null)
union all
select 'SI_after_ED' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is not null and si_since_triage > trans_since_triage
union all
select ('SI_after_ED_within_' || ServDep_name) CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is not null and si_since_triage > trans_since_triage
group by ServDep_name
union all
select 'nonSI_discharge_at_ED' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where  si_since_triage is null and trans_since_triage is null
union all
select 'nonSI_transtion_from_ED' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is null and trans_since_triage is not null
union all
select ('nonSI_transtion_from_ED_to_' || ServDep_name) CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is null and trans_since_triage is not null
group by ServDep_name
union all
select 'nonSI_w_ABX_w_Culture' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is null and abx_since_triage is not null and c_since_triage is not null
union all
select 'nonSI_no_ABX_w_Culture' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is null and abx_since_triage is null and c_since_triage is not null
union all
select 'nonSI_no_Culture_no_ABX' CNT_TYPE,
       count(distinct patient_num) PAT_CNT,
       count(distinct encounter_num) ENC_CNT
from SI_case_ctrl
where si_since_triage is null and abx_since_triage is null and abx_since_triage is null and c_since_triage is null


