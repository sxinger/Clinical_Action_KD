/*******************************************************************************/
/*@file ED_18up.sql
/*
/*in: blueherondata.patient_dimension 
/*
/*params: &&i2b2_db_schema
/*       
/*out: ED_18up
/*
/*action: write
/********************************************************************************/
create table ED_18up as
with ED_age as(
select pat.patient_num
      ,pat.birth_date
      ,floor((trunc(vis.start_date)-pat.birth_date)/365.25) age_at_ed
      ,vis.encounter_num
      ,vis.start_date
      ,row_number() over (partition by pat.patient_num,vis.encounter_num order by vis.start_date) rn
from ED_betwn vis
join &&i2b2_db_schemadata.patient_dimension pat
on vis.patient_num = pat.patient_num
where (vis.start_date-pat.birth_date)/365.25 >= 18 /*age >= 18*/ 
)
select a.patient_num
      ,a.birth_date
      ,a.age_at_ed
      ,a.encounter_num
      ,a.start_date triage_start
      ,max(obs.start_date) enc_end
from (select * from ED_age where rn = 1) a
left join &&i2b2_db_schemadata.observation_fact obs
on a.patient_num = obs.patient_num and a.encounter_num = obs.encounter_num and
   ((obs.concept_cd like 'KUMC|REPORTS|NOTETYPES%' and
     regexp_like(obs.tval_char, 'discharge (instruction|documentation)','i')) or
     regexp_like(obs.concept_cd, 'discharge','i'))
group by a.patient_num,a.birth_date,a.age_at_ed,a.encounter_num,a.start_date
