/*******************************************************************************/
/*@file ED_SI_6HR_PRESSOR.sql
/*
/*in: ED_SI_6HR_screen, observation_fact
/*
/*params: @dblink, &&i2b2
/*   
/*out: ED_SI_6HR_PRESSOR
/*
/*action: write
/********************************************************************************/
create table ED_SI_6HR_PRESSOR as
with pressor_cd as(
select distinct
       c_basecode concept_cd
      ,c_name label
from 
 (select c_basecode, regexp_substr(c_name,'[^\[]+',1,1) c_name, c_tooltip, c_fullname, 
         term_id, max(term_id) over (partition by c_basecode) as max_term_id
  from blueheronmetadata.heron_terms
  where c_fullname like '\i2b2\Medications\%' and
        regexp_like((c_fullname || ' ' || c_tooltip || ' ' || c_name), 
                    '((dopamine)|(norepinephrine)|(phenylephrine)|(epinephrine)|(vasopressin)|(dobutamine))+','i') and
        regexp_like((c_fullname || ' ' || c_name),'( ij| inj| inv| iv| im|injection|injectable)','i'))
where term_id = max_term_id and c_basecode like 'KUH|MEDICATION_ID%'
order by c_basecode
)
   ,collect_pressor as (
select distinct
       e.patient_num
      ,e.encounter_num
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd as modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-e.hypot2_dt)*24,2)+e.hypot2_since_triage start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-e.hypot2_dt)*24,2)+e.hypot2_since_triage end_since_triage
from ED_SI_6HR_screen e
join &&i2b2data.observation_fact@dblink obs  
on obs.patient_num=e.patient_num and obs.encounter_num = e.encounter_num
where obs.start_date > e.hypot2_dt and 
      obs.concept_cd like 'KUH|MEDICATION_ID%' and
      obs.modifier_cd not in ('MedObs|MAR:Stopped',
                              'MedObs|MAR:Dropped',
                              'MedObs|MAR:Canceled Entry',
                              'MedObs|MAR:Missed',
                              'MedObs|MAR:Held',
                              'MedObs|MAR:Med Not Available',
                              'MedObs|MAR:See ED Trauma Flowsheet') and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'
)
select distinct
       cl.patient_num
      ,cl.encounter_num
      ,'vasopressor' variable
      ,cl.nval
      ,cl.units
      ,ht.label tval
      ,cl.code
      ,cl.modifier
      ,cl.instance
      ,cl.start_dt
      ,cl.start_since_triage
      ,cl.end_dt
      ,cl.end_since_triage
from collect_pressor cl
join pressor_cd ht on ht.concept_cd = cl.code





