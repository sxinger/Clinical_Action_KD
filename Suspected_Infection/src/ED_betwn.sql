/*******************************************************************************/
/*@file ED_betwn.sql
/*
/*in: blueherondata.observation_fact 
/*
/*params: &&i2b2_db_schema, &&start_date, &&end_date
/*       
/*out: ED_betwn
/*
/*action: write
/********************************************************************************/
create table ED_betwn as
select distinct 
       patient_num
      ,encounter_num
      ,start_date
from &&i2b2_db_schemadata.observation_fact
where concept_cd = 'KUH|FLO_MEAS_ID:16045_SEL' and /*triage concept_cd may change*/
      start_date between Date &&start_date and Date &&end_date
