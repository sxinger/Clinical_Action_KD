/*******************************************************************************/
/*@file ED_eligb.sql
/*
/*in: ED_18up, observation_fact
/*
/*params: &&i2b2_db_schema
/*   
/*out: ED_eligb
/*
/*action: write
/********************************************************************************/
create table ED_eligb as
with ed_dest as (
select te.patient_num
      ,te.encounter_num
      ,obs.start_date ed_dest_assgm_dt
      ,obs.tval_char ed_dest_assgm
      ,row_number() over (partition by te.patient_num, te.encounter_num order by obs.start_date asc) rn_asc
      ,row_number() over (partition by te.patient_num, te.encounter_num order by obs.start_date desc) rn_desc
from ED_18up te
join &&i2b2_db_schemadata.observation_fact obs
on te.patient_num = obs.patient_num and te.encounter_num = obs.encounter_num and
   obs.concept_cd = 'KUH|FLO_MEAS_ID:16029_SEL' /*flwsht concept for ED destination*/
)
select te.patient_num
      ,te.encounter_num
      ,te.triage_start
      ,ed_dest.ed_dest_assgm_dt ed_dest_assgm1_dt
      ,ed_dest.ed_dest_assgm ed_dest_assgm1
      ,ed_dest2.ed_dest_assgm_dt ed_dest_assgm2_dt
      ,ed_dest2.ed_dest_assgm ed_dest_assgm2
      ,min(obs.start_date) first_fact_dt
      ,max(obs.start_date) last_flosht_dt
      ,te.enc_end
from ED_18up te
left join &&i2b2_db_schemadata.observation_fact obs
on te.patient_num = obs.patient_num and te.encounter_num = obs.encounter_num and
   obs.concept_cd like 'KUH|FLO_MEAS_ID%' and
   to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
left join ed_dest on te.patient_num = ed_dest.patient_num and te.encounter_num = ed_dest.encounter_num and ed_dest.rn_asc = 1
left join ed_dest ed_dest2 on te.patient_num = ed_dest2.patient_num and te.encounter_num = ed_dest2.encounter_num and ed_dest2.rn_desc = 1
group by te.patient_num,te.encounter_num,te.triage_start,ed_dest.ed_dest_assgm_dt,ed_dest.ed_dest_assgm,ed_dest2.ed_dest_assgm_dt,ed_dest2.ed_dest_assgm,te.enc_end
