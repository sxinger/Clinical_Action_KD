/*******************************************************************************/
/*@file SI_pat_at_enc.sql
/*
/*in: SI_case_ctrl, blueherondata.patient_dimension
/*
/*params: @dblink, &&i2b2
/*
/*out: SI_pat_at_enc
/*
/*action: write
/********************************************************************************/
create table SI_pat_at_enc as
select tr.patient_num
      ,tr.encounter_num
      ,floor((tr.triage_start-pat.birth_date)/365.25) age
      ,case when pat.sex_cd='m' then 1
            else 0 
       end as sex_male
      ,case when pat.race_cd in ('declined','@','not used',NULL) then 'unknown' 
            else pat.race_cd
       end as race
      ,pat.marital_status_cd married_status
      ,case when pat.ethnicity_cd in ('declined','@',NULL,'NI') then 'unknown' 
            else pat.ethnicity_cd 
       end as ethnicity
      ,case when pat.religion_cd in ('declined','@',NULL) then 'unknown' 
            else pat.religion_cd 
       end as religion
      ,case when pat.language_cd in ('declined','@',NULL) then 'unknown' 
            else pat.language_cd 
       end as language
      ,pat.death_date
from SI_case_ctrl tr
join &&i2b2data.patient_dimension@dblink pat
on tr.patient_num = pat.patient_num




