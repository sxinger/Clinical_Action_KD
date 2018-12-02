/*******************************************************************************/
/*@file ED_betwn.sql
/*
/*in: blueherondata.observation_fact 
/*
/*params: @dblink, &&i2b2, &&start_date, &&end_date
/*       
/*out: ED_betwn
/*
/*action: write
/********************************************************************************/
create table ED_betwn as
select distinct patient_num
               ,encounter_num
               ,start_date
               ,end_date
from &&i2b2data.observation_fact@dblink
where concept_cd in ('KUH|ED_EPISODE',
                     'KUH|FLO_MEAS_ID+LINE:16045_1') and
      start_date between Date &&start_date and Date &&end_date


