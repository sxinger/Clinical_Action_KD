/*******************************************************************************/
/*@file ED_SI_LOC.sql
/*
/*in: SI_case_ctrl,observation_fact, concept_dimension 
/*
/*params: @dblink, &&i2b2, &&within_d 
/*       
/*out: ED_SI_LOC
/*
/*action: write
/********************************************************************************/
create table ED_SI_LOC as
with loc_cd as (
select distinct concept_cd,name_char
from &&i2b2data.concept_dimension@dblink
where concept_cd in ('KUH|FLO_MEAS_ID+LINE:1459_1',
                     'KUH|FLO_MEAS_ID+LINE:1459_2',
                     'KUH|FLO_MEAS_ID+LINE:1459_3',
                     'KUH|FLO_MEAS_ID+LINE:1459_4',
                     'KUH|FLO_MEAS_ID+LINE:1459_5',
                     'KUH|FLO_MEAS_ID+LINE:1459_6',
                     'KUH|FLO_MEAS_ID+LINE:1459_7')
)
    ,collect_loc as (
select distinct 
       obs.patient_num
      ,obs.encounter_num
      ,'LOC' variable
      ,obs.nval_num nval
      ,obs.units_cd units
      ,obs.concept_cd code
      ,obs.modifier_cd modifier
      ,obs.instance_num instance
      ,obs.start_date start_dt
      ,round((obs.start_date-init.triage_start)*24,2) start_since_triage
      ,obs.end_date end_dt
      ,round((obs.end_date-init.triage_start)*24,2) end_since_triage
from SI_case_ctrl init
join &&i2b2data.observation_fact@dblink obs
on init.patient_num = obs.patient_num and init.encounter_num = obs.encounter_num
where obs.concept_cd in ('KUH|FLO_MEAS_ID+LINE:1459_1',
                         'KUH|FLO_MEAS_ID+LINE:1459_2',
                         'KUH|FLO_MEAS_ID+LINE:1459_3',
                         'KUH|FLO_MEAS_ID+LINE:1459_4',
                         'KUH|FLO_MEAS_ID+LINE:1459_5',
                         'KUH|FLO_MEAS_ID+LINE:1459_6',
                         'KUH|FLO_MEAS_ID+LINE:1459_7') and
      obs.start_date between init.triage_start and 
                             init.triage_start + &&within_d and
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00'and
      init.case_ctrl=1
)
select distinct 
       cl.patient_num
      ,cl.encounter_num
      ,cl.variable
      ,ht.name_char tval
      ,cl.units
      ,cl.code
      ,cl.modifier
      ,cl.instance
      ,case when cl.code not in ('KUH|FLO_MEAS_ID+LINE:1459_1',
                                 'KUH|FLO_MEAS_ID+LINE:1459_6')
            then 1 else 0
       end as flag
      ,cl.start_dt
      ,cl.start_since_triage
      ,cl.end_dt
      ,cl.end_since_triage
from collect_loc cl
left join loc_cd ht
on cl.code = ht.concept_cd



