/*******************************************************************************/
/*@file ED_18up.sql
/*
/*in: blueherondata.patient_dimension 
/*
/*params: @dblink, &&i2b2
/*       
/*out: ED_18up
/*
/*action: write
/********************************************************************************/
create table ED_18up as
select pat.patient_num
      ,pat.birth_date
      ,floor((trunc(vis.start_date)-pat.birth_date)/365.25) age_at_ed
      ,vis.encounter_num
      ,vis.start_date
      ,vis.end_date
from ED_betwn vis
join &&i2b2data.patient_dimension@dblink pat
on vis.patient_num = pat.patient_num
where (vis.start_date-pat.birth_date)/365.25 >= 18 /*age >= 18*/ 


