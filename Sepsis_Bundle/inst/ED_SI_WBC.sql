/*******************************************************************************/
/*@file ED_SI_WBC.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: @dblink, &&i2b2, &&within_d
/*       
/*out: ED_SI_WBC
/*
/*action: write
/********************************************************************************/
create table ED_SI_WBC as
select distinct 
       obs.patient_num
      ,obs.encounter_num
      ,'WBC' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,case when (obs.units_cd is null and obs.nval_num not between 4000 and 12000) then 1
            when (obs.units_cd is not null and obs.nval_num not between 4 and 12) then 1
            else 0
       end as flag
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_date) rn
from SI_case_ctrl init
join &&i2b2data.observation_fact@dblink obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd = 'KUH|COMPONENT_ID:3009' and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      obs.modifier_cd='@' and
      init.case_ctrl=1
union all
select distinct 
       obs.patient_num
      ,obs.encounter_num
      ,'WBC' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,case when obs.nval_num > 10 then 1 else 0
       end as flag
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_date) rn
from SI_case_ctrl init
join &&i2b2data.observation_fact@dblink obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd = 'KUH|COMPONENT_ID:3027' and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and 
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      obs.modifier_cd='@' and
      init.case_ctrl=1



