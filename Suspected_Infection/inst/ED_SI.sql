/*******************************************************************************/
/*@file ED_SI.sql
/*
/*in: ED_Culture, ED_Antibio, ED_eligb 
/*
/*out: ED_SI
/*
/*action: write
/********************************************************************************/
create table ED_SI as
with culture_unique as (
select distinct 
       patient_num
      ,encounter_num
      ,variable
      ,start_dt
      ,start_since_triage
from ED_Culture
)
    ,id_si as (
select distinct 
       c.patient_num 
      ,c.encounter_num
      ,(am.abx_group || ' and ' || c.variable) si_identifier
      ,c.variable culture_type
      ,am.abx_group antibio_type
      ,am.abx_name antibio_subtype
      ,am.nval_num antibio_val
      ,am.injectable_ind
      ,c.start_dt culture_date
      ,am.start_dt antibio_date
      ,least(c.start_dt,am.start_dt) si_date
      ,least(am.start_since_triage,c.start_since_triage) hr_since_triage
from culture_unique c
left join ED_Antibio am
on c.patient_num = am.patient_num and 
   c.encounter_num = am.encounter_num
where c.start_since_triage - am.start_since_triage > 0 and 
      c.start_since_triage - am.start_since_triage <= 4
union all
select distinct 
       c.patient_num
      ,c.encounter_num
      ,(c.variable || ' and ' || am.abx_group) si_identifier
      ,c.variable culture_type
      ,am.abx_group antibio_type
      ,am.abx_name antibio_subtype
      ,am.nval_num antibio_val
      ,am.injectable_ind
      ,c.start_dt culture_date
      ,am.start_dt antibio_date
      ,least(c.start_dt,am.start_dt) si_date
      ,least(am.start_since_triage,c.start_since_triage) hr_since_triage
from culture_unique c
left join ED_Antibio am
on c.patient_num = am.patient_num and 
   c.encounter_num = am.encounter_num
where am.start_since_triage-c.start_since_triage between 0 and 4
)
  ,si_events as (
select id_si.*
      ,row_number() over (partition by id_si.patient_num, id_si.encounter_num order by id_si.si_date,antibio_val, injectable_ind desc) rn_enc
from id_si
left join ED_eligb enc
on id_si.patient_num = enc.patient_num and 
   id_si.encounter_num = enc.encounter_num
)
select patient_num
      ,encounter_num
      ,si_identifier
      ,culture_type
      ,antibio_type
      ,antibio_subtype
      ,antibio_val
      ,injectable_ind
      ,culture_date
      ,antibio_date
      ,si_date 
      ,round(hr_since_triage,2) si_since_triage
from si_events
where rn_enc = 1


