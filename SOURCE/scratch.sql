select * from qsys2.systables  ;

select * from util.menf01  ;
select * from prem.nrtuimenu  ;
select * from util.rpll  ;

-- Distinct libraries from Library List table (excluding Dev/Test)
select distinct lib from util.rpll,
lateral (values libr1r, libr2r, libr3r, libr4r, libr5r, libr6r, libr7r) as q (lib)
where librnr not in ('AD','CLMP','CONX','DAVE','GP','JIM','JS','KEYX','MIKE',
  'NONE','NRCX','NRTX','NRX','OL5G','OLOL','OLP','SEC','TLX','WMSN','XCON','XWSS')
  and libr1r <> ' '
  and envirr not like ('%DEV%')
  and envirr not like ('%TEST%')
  and envirr not like ('%SANDBOX%')
  and lib <> ' '
order by 1  ;

-- Extract example
select extract(microsecond from current_timestamp)
from sysibm.sysdummy1  ;

-- UDTF to retrieve a jobs library list (current job = no parm)
select * from table(sqltools.LIBL('795759/FAYNBERG/QPADEV004G'))  ;

-- UDF to retrieve contents of a data area
values sqltools.dtaara('tddev/devdta')  ;

-- UDTF to list all data areas for specified library
select * from table(sqltools.dtaara_list('*libl'))  ;

-- UDTF to list User Profiles for a specific Group Profile
select * from table(sqltools.authuser_list('*ALL','*GRPPRF'))  ;

-- UDTF to list objects not used within specified Period
select * from table(sqltools.idle_objects('CONPGM','*ALL','*PGM',12))  ;

-- UDTF to list program references like DSPPGMREF
select * from table(sqltools.pgmrefs('TDDEV','WL02R'))  ;

-- UDTF to list a source file member
select srcdata from table
(sqltools.readsrc('tddev','source','scratch'))  ;

-- Varchar_format example
select varchar_format
('2024-05-24-17.30.42.000000', 'Month dd, YYYY') as date
from sysibm.sysdummy1  ;

-- Read from an IFS file
select * from table(QSYS2.IFS_READ(PATH_NAME => '/http_debug_log.txt'))  ;

-- Libraries with files (exclude display)
select distinct table_schema
from qsys2.systables where file_type<>'D'
order by table_schema  ;

-- Source members in a specific library
select system_table_schema as "Library",
       system_table_name as "File",
       system_table_member as "Member",
       source_type as "Type",
       to_char(number_rows,'999G999G999G999') as "Records",
       to_char(data_size,'999G999G999G999') as "Size",
       create_timestamp as "Created",
       last_source_update_timestamp as "Last updated",
       partition_text as "Member text"
 from qsys2.syspartitionstat
where system_table_schema like 'ECNCT%'
  and source_type is not null
order by 1,2,3  ;

-- WRKUSRJOB
select distinct a.job_name,
       substr(a.job_name,1,6) as job_number,
       substr(a.job_name,8,posstr(substr(job_name,8),'/') - 1) as job_user,
       substr(substr(a.job_name,8),posstr(substr(a.job_name,8),'/') + 1) as job_name_sin,
       b.v_active_job_status as job_status,
       b.v_active_job_type as job_type,
       b.v_run_priority as run_priority,
       b.v_authorization_name as authorization_name,
       b.v_sbs_name as job_subsystem,
       b.v_cpu_used as cpu_used,
       b.v_temp_storage_used_mb as temp_storage_used_mb,
       b.v_aux_io_requested as aux_io_requested,
       b.v_page_faults as page_faults,
       b.v_client_ip_address as client_ip_address,
       b.v_client_wrkstnname as client_wrkstnname,
       b.v_client_applname as client_applname,
       b.v_client_acctng as client_acctng,
       b.v_client_programid as client_programid,
       b.v_client_userid as client_userid,
       b.v_sql_statement_text as sql_statement_text,
       b.v_sql_stmt_status as sql_statement_status,
       b.v_sql_stmt_start_timestamp as sql_statement_start_timestamp,
       b.v_query_options_lib_name as query_options_lib_name,
       b.v_pj_reuse_count as pj_reuse_count,
       b.v_pj_maxuse_count as pj_maxuse_count
from qsys2.object_lock_info a,
    lateral (select * from table(qsys2.get_job_info(a.job_name))) b
where system_object_schema = 'QSYS'
  and system_object_name = 'DUNGANT'
  and object_type = '*USRPRF'
  and lock_state = '*SHRRD'
  and lock_scope = 'JOB'  ;

-- Object Usage statistics report
with objUsage (objlib, objname, objtype, objattr, lastUsedDate, crtdate, objAge,
      DaysUsedResetDate, days_used_counter, days_available, usage_percentage,
      objowner, objcreator, objsize, objtext)
     as (select cast(left(objlongschema,10) as varchar(10)),
           objname, objtype, objattribute, date(last_used_timestamp) LastUsedDate,
           date(objcreated) CrtDate, current_date - date(objcreated) ObjAge,
           date(last_Reset_Timestamp) UsageResetDate, days_used_count DaysUsed,
           current_date - date(nvl(last_Reset_timeStamp, objcreated)) + 1 DaysAvailable,
           cast((days_used_count / (current_date - date(objcreated) + 1))
              * 100 as dec(5, 2)) Usage_Percentage,
           objowner, objDefiner, objsize, objtext
           -- Change the two parameters to '*ALLUSR' and '*ALL'
           -- to scan your entire system, otherwise change the
           -- Library and object type parameters as desired.
      from table(object_statistics('OLPGM5D', '*PGM *SRVPGM')) OL )
select *
from objUsage
  -- Omit the ORDER BY clause to speed up the initial resultSet
order by Usage_Percentage desc,
         days_used_counter desc,
         objlib,
         objname  ;

-- Objects recently created/changed
select objlib,objname,objtext,
      date(objcreated) as "create_date",
      date(change_timestamp) as "change_date",
      objowner
from table(qsys2.object_statistics('OLPGM5D','*PGM'))
where objcreated >= '2024-10-20'
   or change_timestamp >= '2024-10-20'  ;

-- identify sql jobs (current user)
select * from table (qsys2.active_query_info(job_name => '*'))  ;

-- identify sql jobs (specified user)
select qualified_job_name,user_name,query_type
from table (qsys2.active_query_info(user_name => 'DUNGANT'))  ;

-- identify sql jobs (most recent)
select full_open_timestamp,
       qualified_job_name,
       job_user,
       job_number,
       query_type,
       library_name,
       file_name
from table (qsys2.active_query_info(job_name => '*ALL'))
order by full_open_timestamp
limit 5  ;

-- SQL Error Logging Facility (SELF)
select logged_sqlcode, logged_sqlstate, number_occurrences,
cast(statement_text as char(2048)), statement_operation,
statement_operation, reason_code, program_library,
program_name, program_type, module_name, logged_time,
job_name, thread_id, adopted_user_name, user_name,
system_user_name, client_acctng, client_applname,
client_programid, client_userid, client_wrkstnname,
rdb_name, initial_logged_time, initial_job_name,
initial_thread_id, initial_adopted_user_name
from qsys2.sql_error_log
where number_occurrences > 1
order by number_occurrences desc  ;

-- Indexes Advised
select sys_dname as library_name,
       sys_tname as table_name,
       firstadv as first_advised,
       timesadv as times_advised,
       keysadv as advised_keys
from qsys2.sysixadv
where system_table_schema = 'OLFILE5'
  and system_table_name = 'ORDR'
order by times_advised desc  ;





-- Pickup vs Dropoff Location Comparison (details)
select to_date(substr(char(PICDTCD),2,8),'YYMMDD') pickup_date,
sslcdch,cont#cd,picdtcd,ordr#cd,ordr2cd,pulocch,concdor,orderch
from olfile5.ctdt
join olfile5.cthd on ordercd = orderch and term#ch = '61'
join olfile5.ordr on ordr2cd = orderor
where picdtcd >= 1240101
and ordr#cd <> 0 and ordr2cd <> 0
order by sslcdch, pickup_date  ;

-- Pickup vs Dropoff Location Comparison (summary by month w/%)
with con as (
select dec(substr(char(PICDTCD),4,2),2,0) as pickup_month,
monthname(timestamp_format(char(PICDTCD+19000000),'YYYYMMDD'))
  ||' ('||substr(char(PICDTCD),4,2)||')' as month_name,
sslcdch as steamship_line,
cont#cd,ordr#cd,ordr2cd,pulocch,concdor,orderch,1 as con_ct,
(case when pulocch<>concdor then 1 else 0 end) as diff_ct
from olfile5.ctdt
join olfile5.cthd on ordercd = orderch and term#ch = '61'
join olfile5.ordr on ordr2cd = orderor
where picdtcd >= 1240101
and ordr#cd <> 0 and ordr2cd <> 0
)
select steamship_line, month_name,
sum(con_ct) total_containers,
sum(diff_ct) diff_containers,
(sum(diff_ct)*100)/sum(con_ct) percentage_diff
from con
group by month_name, steamship_line
order by right(month_name,4), steamship_line  ;

-- Terminal 99 PickUp report
select vname8,addrv8,cityv8,statv8,digits(zipcv8) zip,
       nvl(nullif(cname1,''),cname8) contact,trim(email8x) email,
       '('||phonea8||')'||left(phone8,3)||'-'||right(phone8,4) phone,
       auth#0 specialeqp,ponum0 po#,
       substr(consn2,locate_in_string(consn2,'/',1,1)+1) destination,
       prfdc# podestid,
       topal1 pallets,ctnqt0 cartons,weigh0 weight,cubft0 cube,
       varchar_format(date(to_date(char(avldt1+19000000),'YYYYMMDD')),'MM/DD/YYYY') rdydate,
       varchar_format(date(to_date(char(consdt),'YYYYMMDD')),'MM/DD/YYYY') canceldate,
       varchar_format(date(to_date(char(lddrdt),'YYYYMMDD')),'MM/DD/YYYY'),dept,acrec1 stckpallet,
       varchar_format(date(to_date(char(caldt1+19000000),'YYYYMMDD')),'MM/DD/YYYY') reqdate
from olfile5.puhd
join olfile5.pudt on punum1 = punum2
join olfile5.pupo on punum2 = punum0 and cons#2 = cons#0
join olfile5.tj250dtl on ponum0 = ponum
  and chain = (case when left(consn2,2) = 'HO' then '28'
                    when left(consn2,2) = 'MA' then '10'
                    when left(consn2,2) = 'TJ' then '08' end)
join olfile5.loct on vend#1 = locid8
left join olfile5.locx on locid8 = locid8x
where term#1 = '99'
  and stat11 = ' '
order by punum1,cons#2  ;

-- TJX WEB shipment report
select punum1, topal1, toctn1, towgt1, tocub1,
       consn2, ponum0, auth#0, term#1,
       date(to_date(right(avldt1,6),'YYMMDD')) as "dateavldt1",
       date(to_date(right(caldt1,6),'YYMMDD')) as "datecaldt1",
       caltm1, vname8, cityv8, statv8, ppdrv1, stat11
from wolfiasp.olfile5.puhd
inner join wolfiasp.olfile5.pudt on punum1 = punum2
inner join wolfiasp.olfile5.pupo on punum2 = punum0 and lin##2 = lin##0
left outer join wolfiasp.olfile5.loct on vend#1 = locid8
where ( sourc1 = 'WEB' and caldt1 >= 1241124)
order by 13, 6  ;





-- optimizing random statement
-- + eliminated distinct clause
-- + inverted order logic
-- + inverted date logic
Update tddev.tdinbound
Set processedcode = ' '
where processedcode <> 'P' and data0002 in                   -- was integer(data0002)
(select char(erorder) from itpgm5.rmerror                    -- was distinct erorder
 join olfile5.ordr on erorder = orderor
 where statuor = 'D' and ertimest >= timestamp(Current Date) -- was date(ertimest) = Current Date
 and erdesc1 like '%Missing%')  ;

-- optimizing a similar statement
Declare c1 Cursor For
Select Distinct Date(ertimest),
                orderst,
                seqnost,
                depdtst,
                deptmst
From itpgm5.formi00004
--Inner Join itpgm5.rmerror On data0002 = Char(erorder)      -- avoid temp hash scan & random i/o
Inner Join itpgm5.rmerror On dec(data0002,7,0) = erorder
--Inner Join olfile5.stop On data0002 = Char(orderst)        -- avoid table scan on STOP file
Inner Join olfile5.stop st On dec(data0002,7,0) = orderst
-- Where Date(ertimest) = Current Date                       -- migrate logic to index probe
Where ertimest >= timestamp(Current Date)
And proce00001 = 'P'
-- And formnumber = '010'                                    -- migrate logic to index probe
And rtrim(formnumber) = '010'
And data00015 = Char(stkeyst)
And (depdtst = '0001-01-01' And deptmst <> 0)  ;

-- playing with dates/timestamps
select current_date - (dayofweek(current_date) - 1 ) days WeekStart,
current_date + (7 - dayofweek(current_date)) days WeekEnd,
varchar_format(current_timestamp, 'YYYY-MM-DD HH24:MI:SS') Punch,
char(current_date)||'T'||char(current_time) Test
from sysibm.sysdummy1  ;



drop table qtemp.outfile  ;

create table qtemp.outfile (column_1,column_2) as
    (select substr(cast(line as char(12)), 1, locate_in_string(line,' ',1,1) -1),
    substr(cast(line as char(12)), locate_in_string(line,' ',1,1) +1 )
    from table (qsys2.ifs_read('/home/dungant/test.txt')))
with data  ;

select * from qtemp.outfile ;

call qsys2.ifs_write('/home/dungant/test.txt',
                     overwrite => 'APPEND',
                     end_of_line => 'CRLF')  ;

call qsys2.ifs_write('/home/dungant/test.txt',
                     'Added line 1',
                     overwrite => 'APPEND',
                     end_of_line => 'CRLF')  ;

call qsys2.ifs_write('/home/dungant/new_test.txt',
                     'New file and a new line',
                     end_of_line => 'CRLF')  ;

select path_name,object_type,create_timestamp
  from table (qsys2.ifs_object_statistics('/home/dungant'))  ;

csv: select * from qcodelab.customer  ;
json: select * from qcodelab.customer  ;
cl: dspfd qcodelab/customer  ;

select inmbsqnr,
       cast(inbox as varchar(10) ccsid 37) inbox,
       cast(indta as varchar(135) ccsid 37) indata
from edi4xxdta.edinbx
 fetch first 500 rows only  ;

select * from qiws.qcustcdt  ;
