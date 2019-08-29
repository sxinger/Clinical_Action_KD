/*******************************************************************************/
/*@file ED_SI_LACTATE.sql
/*
/*in: SI_case_ctrl, observation_fact
/*
/*params: &&i2b2_db_schema, &&within_d
/*       
/*out: ED_SI_LACTATE
/*
/*action: write
/********************************************************************************/
create table ED_SI_LACTATE as
with lac_order as (
select obs.patient_num
      ,obs.encounter_num
      ,'Lactate' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,case when obs.nval_num > 2.0 then 1 else 0
       end as flag
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
      ,dense_rank() over (partition by obs.patient_num, obs.encounter_num order by obs.start_date) rn_out
      ,row_number() over (partition by obs.patient_num, obs.encounter_num, obs.start_date order by obs.end_date) rn_in 
from SI_case_ctrl init
join &&i2b2_db_schemadata.observation_fact obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd in ('KUH|COMPONENT_ID:2015',
                         'KUH|COMPONENT_ID:2016') and 
      obs.modifier_cd = '@' and obs.nval_num is not null and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      init.case_ctrl=1
)
select lo.patient_num
      ,lo.encounter_num
      ,lo.variable
      ,lo.nval
      ,lo.units
      ,lo.code
      ,lo.flag
      ,lo.start_dt
      ,lo.start_since_triage
      ,lo.end_dt
      ,lo.end_since_triage
      ,lo.rn_out rn
      ,case when lo.rn_out=2 and lo.rn_in = 1 and lo.start_since_triage-lac1.start_since_triage<=6 then 1
            when lo.rn_out=2 and lo.rn_in = 1 and lo.start_since_triage-lac1.start_since_triage>6 then 0
            else null
       end as repeat_ind
from lac_order lo
left join (select * from lac_order where rn_out = 1 and rn_in = 1) lac1
on lo.patient_num = lac1.patient_num and lo.encounter_num = lac1.encounter_num







