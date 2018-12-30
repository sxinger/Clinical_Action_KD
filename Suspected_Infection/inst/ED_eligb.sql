/*******************************************************************************/
/*@file ED_eligb.sql
/*
/*in: ED_18up, observation_fact
/*
/*params: @dblink, &&i2b2
/*   
/*out: ED_eligb
/*
/*action: write
/********************************************************************************/
create table ED_eligb as
select te.patient_num
      ,te.encounter_num
      ,te.triage_start
      ,min(obs.start_date) first_fact_dt
      ,te.enc_end
--      ,least(te.triage_start,min(obs.start_date)) triage_start
from ED_18up te
left join &&i2b2data.observation_fact@dblink obs
on te.patient_num = obs.patient_num and te.encounter_num = obs.encounter_num and
   obs.concept_cd like 'KUH|FLO_MEAS_ID%' and
   to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
group by te.patient_num,te.encounter_num,te.triage_start,te.enc_end
