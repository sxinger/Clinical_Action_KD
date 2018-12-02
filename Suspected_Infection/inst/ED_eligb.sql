/*******************************************************************************/
/*@file ED_eligb.sql
/*
/*in: ED_18up
/*
/*out: ED_eligb
/*
/*action: write
/********************************************************************************/
create table ED_eligb as
select patient_num
      ,encounter_num
      ,min(start_date) triage_start
      ,max(end_date) enc_end
from ED_18up
group by patient_num,encounter_num


