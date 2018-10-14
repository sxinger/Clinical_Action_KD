/*******************************************************************************/
/*@file cohort_initial.sql
/*
/*in: PCORNET_CDM tables 
/*
/*params: @dblink, &&PCORNET_CDM, &&start_date, &&end_date
/*       
/*out: AKI_Initial
/*
/*action: write
/********************************************************************************/
alter session set NLS_DATE_FORMAT = 'DD-MM-YYYY HH24:MI:SS'; 


/*ED episode*/
-- https://bmi-work.kumc.edu/work/ticket/3083
-- https://github.com/kumc-bmi/heron/blob/528fa03464c53c966624402b91ee04a9a4ccb66d/heron_load/epic_ed_episode_transform.sql
drop table ED_betwn purge;
create table ED_betwn as
select distinct patient_num
               ,encounter_num
               ,instance_num
               ,start_date
               ,end_date
from blueherondata.observation_fact
where concept_cd in ('KUH|ED_EPISODE',
                     'KUH|FLO_MEAS_ID+LINE:16045_1') and
      start_date >= '01-Jan-2007'
;
select count(distinct patient_num), /*216,448*/
       count(distinct encounter_num), /*548,738*/
       count(distinct instance_num) /*1,072,952*/
from ED_betwn;

drop table ED_18up PURGE;
create table ED_18up as
select pat.patient_num
      ,pat.birth_date
      ,floor((trunc(vis.start_date)-pat.birth_date)/365.25) age_at_ed
      ,vis.encounter_num
      ,vis.instance_num
      ,vis.start_date
      ,vis.end_date
from ED_betwn vis
join blueherondata.patient_dimension pat
on vis.patient_num = pat.patient_num
where (vis.start_date-pat.birth_date)/365.25 >= 18 /*age >= 18*/ 
--      to_char(vis.start_date,'HH24:MI:SS') <> '00:00:00'
; 
select min(start_date), /*01-JAN-07*/
       max(start_date), /*03-JUL-18*/
       count(distinct patient_num) pat_cnt, /*187,531*/
       count(distinct encounter_num) enc_cnt, /*482,923*/
       count(distinct instance_num) ins_cnt /*945,288*/
from ED_18up;

select * from ED_18up 
where encounter_num = 100966;

drop table ED_eligb PURGE;
create table ED_eligb as
select encounter_num 
      ,min(patient_num) patient_num
      ,min(start_date) triage_start
      ,max(end_date) enc_end
from ED_18up
group by encounter_num
;
select * from ED_eligb;
select count(distinct patient_num) pat_cnt, /*187,531*/
       count(distinct encounter_num) enc_cnt, /*482,923*/
       count(*) /*482,931*/
from ED_eligb;

--duplicates?
select encounter_num, count(*)
from ED_eligb
group by encounter_num
having count(*) > 1
;
--7199763
--7175025
--7136549
--6041944
--6041258
--6516858
--5002000
--3851409
select * from ED_eligb
where encounter_num in (7199763,7175025,7136549,6041944,
                        6041258,6516858,5002000,3851409)
order by encounter_num
;

select distinct patient_num, encounter_num 
from blueherondata.observation_fact
where encounter_num in (7199763,7175025,7136549,6041944,
                        6041258,6516858,5002000,3851409)
order by encounter_num
;

create index ED_eligb_ENCDT_IDX on ED_eligb(encounter_num,triage_start); 

drop table ED_TRIPLAN purge;
create table ED_TRIPLAN as
with add_dst_acuity as (
select tri.patient_num
      ,tri.encounter_num
      ,tri.triage_start
      ,flo.concept_cd
      ,flo.start_date start_dt
      ,(flo.start_date - tri.triage_start)*24 start_since_triage
      ,flo.end_date end_dt
      ,(flo.end_date - tri.triage_start)*24 end_since_triage
from ED_eligb tri
join blueherondata.observation_fact flo
on tri.encounter_num = flo.encounter_num
where flo.concept_cd like 'KUH|FLO_MEAS_ID%:16029_%' or
      flo.concept_cd like 'KUH|FLO_MEAS_ID+%16054%'
)
select distinct
       obs.patient_num
      ,obs.encounter_num
      ,obs.triage_start
      ,'Destination' variable
      ,dst.descriptor tval
      ,obs.start_since_triage
      ,obs.end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_dt) rn
from add_dst_acuity obs
join FLWSHT_ED_DST dst
on obs.concept_cd = dst.code
union all
select distinct
       obs.patient_num
      ,obs.encounter_num
      ,obs.triage_start
      ,'Acuity' variable
      ,acut.descriptor tval
      ,obs.start_since_triage
      ,obs.end_since_triage
      ,dense_rank() over (partition by obs.encounter_num order by obs.start_dt) rn
from add_dst_acuity obs
join FLWSHT_ED_ACUITY acut
on obs.concept_cd = acut.code
;
select variable
      ,count(distinct encounter_num) /*460,392*/
      ,count(*) /*969,777*/
from ED_TRIPLAN
group by variable;
--Destination	459421
--Acuity	461955

--multiple entries exist within one encounter
--inspect an example
select * from ED_TRIPLAN
where encounter_num = 100966
order by start_since_triage;


/*sanity checks*/
-- ED LOS > 3
select count(distinct encounter_num) from ED_eligb
where enc_end - triage_start > 3; /*79,612*/
-- exists! which is not possible, should be cases where they admitted throught ED and became inpatients


drop table ED_Culture purge;
create table ED_Culture as
with culture_all as (
select distinct
       tri.encounter_num
      ,'Blood Culture' variable
      ,obs.concept_cd
      ,obs.start_date start_dt
      ,(obs.start_date - tri.triage_start)*24 start_since_triage
      ,obs.end_date end_dt
      ,(obs.end_date - tri.triage_start)*24 end_since_triage
from ED_eligb tri
join blueherondata.observation_fact obs
on obs.encounter_num = tri.encounter_num
where obs.start_date > tri.triage_start and  /*after triage*/
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      obs.concept_cd in ('KUH|PROC_ID:10201618',
                         'KUH|PROC_ID:10201638')
union all
select distinct
       tri.encounter_num
      ,'Other Fluid Culture' variable
      ,obs.concept_cd
      ,obs.start_date start_dt
      ,(obs.start_date - tri.triage_start)*24 start_since_triage
      ,obs.end_date end_dt
      ,(obs.end_date - tri.triage_start)*24 end_since_triage
from ED_eligb tri
join blueherondata.observation_fact obs
on obs.encounter_num = tri.encounter_num
where obs.start_date > tri.triage_start and  /*after triage*/
      to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and
      obs.concept_cd in ('KUH|PROC_ID:10201614',
                         'KUH|PROC_ID:10201622',
                         'KUH|PROC_ID:10201626',
                         'KUH|PROC_ID:10201630',
                         'KUH|PROC_ID:10201642',
                         'KUH|PROC_ID:10201654',
                         'KUH|PROC_ID:10201660')
)
select encounter_num
      ,variable
      ,concept_cd code
      ,start_dt
      ,start_since_triage
      ,end_dt
      ,end_since_triage
      ,dense_rank() over (partition by encounter_num order by start_dt asc) rn
from culture_all 
;
select count(distinct encounter_num) from ED_Culture; /*99,169*/


drop table ED_Antibio purge;
create table ED_Antibio as
with culture_uniquet as (
select distinct 
       encounter_num
      ,start_dt
      ,start_since_triage
from ED_Culture
)
   ,mar_admin as (
select c.encounter_num
      ,obs.concept_cd
      ,obs.nval_num 
      ,obs.units_cd 
      ,obs.start_date start_dt
      ,(obs.start_date - c.start_dt)*24+c.start_since_triage start_since_triage
      ,obs.end_date end_dt
      ,(obs.end_date - c.start_dt)*24+c.start_since_triage end_since_triage
      ,obs.modifier_cd
      ,obs.instance_num
      ,obs.start_date - c.start_dt hr_since_culture
from culture_uniquet c 
join blueherondata.observation_fact obs
on c.encounter_num = obs.encounter_num and
   regexp_like(obs.modifier_cd,
              '((MAR:Given)|(MAR_Dose)|(MAR:New Bag)|
                (MAR:Cabinet Pull)|(MAR:Downtime)|(MAR:Bolus)|
                (MAR:ED-Infusing Upon Admit))','i') and /*must be administered*/
   to_char(obs.start_date,'HH24:MI:SS') <> '00:00:00' and /*hourly*/
   (obs.start_date - c.start_dt)*24+c.start_since_triage > 0 /*should occur after triage start*/
)
select distinct
       a.encounter_num
      ,ht.med_group abx_group
      ,ht.med_name abx_name
      ,a.concept_cd
      ,a.nval_num
      ,a.units_cd
      ,a.start_dt
      ,a.start_since_triage
      ,a.end_dt
      ,a.end_since_triage
      ,a.modifier_cd
      ,a.instance_num
      ,ht.injectable_ind
      ,dense_rank() over (partition by a.encounter_num order by a.start_dt asc) rn
      ,dense_rank() over (partition by a.encounter_num order by a.start_dt desc) rn_desc
from mar_admin a
join MED_ANTIBIO_CD ht
on a.concept_cd = ht.med_id
;
select count(distinct encounter_num) from ED_Antibio; /*67,783*/

drop table ED_SI purge;
create table ED_SI as
with culture_unique as (
select distinct 
       encounter_num
      ,variable
      ,start_dt
      ,start_since_triage
from ED_Culture
)
    ,id_si as (
select distinct 
       c.encounter_num
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
on c.encounter_num = am.encounter_num
where c.start_since_triage - am.start_since_triage > 0 and 
      c.start_since_triage - am.start_since_triage <= 4
union all
select distinct 
       c.encounter_num
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
on c.encounter_num = am.encounter_num
where am.start_since_triage-c.start_since_triage between 0 and 4
)
  ,si_events as (
select id_si.*
      ,row_number() over (partition by id_si.encounter_num order by id_si.si_date,antibio_val, injectable_ind desc) rn_enc
from id_si
left join ED_eligb enc
on id_si.encounter_num = enc.encounter_num
)
select encounter_num
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
;
select count(distinct encounter_num) from ED_SI; /*57,598*/


/*Determine suspected infection onset*/
drop table SI_case_control purge;
create table SI_case_control as
with si_acu as (
select a.patient_num
      ,a.encounter_num
      ,a.triage_start
      ,a.tval Acu
      ,'case' case_control
from ED_TRIPLAN a
where a.variable = 'Acuity' and a.rn = 1 and
      exists (select 1 from ED_SI e
              where e.encounter_num = a.encounter_num)
)
    ,nonsi_acu as (
select a.patient_num
      ,a.encounter_num
      ,a.triage_start
      ,a.tval Acu
      ,'control' case_control
from ED_TRIPLAN a
where a.variable = 'Acuity' and a.rn = 1 and
      not exists (select 1 from ED_SI e
                  where e.encounter_num = a.encounter_num)
)
   ,train_sample as (
select * from si_acu --SAMPLE(70)
union all
select * from nonsi_acu --SAMPLE(70)
)
select tr.*, 
       d.tval Dst,
       si.si_identifier,
       si.si_since_triage si_since_triage
from train_sample tr
left join ED_TRIPLAN d
on tr.encounter_num = d.encounter_num and
   d.variable = 'Destination' and d.rn = 1
left join ED_SI si
on tr.encounter_num = si.encounter_num;





