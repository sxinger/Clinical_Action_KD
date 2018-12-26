/*******************************************************************************/
/*@file ED_SI_3HR_ABX.sql
/*
/*in: ED_Antibio, SI_case_ctrl
/*
/*out: ED_SI_3HR_ABX
/*
/*action: write
/********************************************************************************/
create table ED_SI_3HR_ABX as
select distinct 
       abx.patient_num
      ,abx.encounter_num
      ,'Abx' variable
      ,abx.nval_num nval
      ,abx.abx_name tval
      ,abx.units_cd units
      ,abx.concept_cd code
      ,abx.modifier_cd modifier
      ,abx.instance_num
      ,abx.injectable_ind
      ,abx.abx_group
      ,abx.start_dt
      ,round(abx.start_since_triage) start_since_triage
      ,abx.end_dt
      ,round(abx.end_since_triage) end_since_triage
      ,abx.rn
from ED_Antibio abx
where exists (select 1 from SI_case_ctrl si
              where si.patient_num = abx.patient_num and si.encounter_num = abx.encounter_num and
                    si.case_ctrl = 1)



