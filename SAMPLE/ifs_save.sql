      --%METADATA                                                      *
      -- %TEXT save data to an IFS file                                *
      --%EMETADATA                                                     *
SET PATH *LIBL ;

CREATE OR REPLACE FUNCTION SQLTOOLS.IFS_SAVE (
  PATH_NAME DBCLOB(16777216) CCSID 1200 ,
  TEXTDATA CLOB(2147483647) CCSID 1208 ,
  FILE_CCSID INTEGER DEFAULT  1208  ,
  DATA_OPTION VARCHAR(10) DEFAULT  'APPEND'  ,
  FILE_OPTION VARCHAR(10) DEFAULT  'CREATE'  ,
  EOL VARCHAR(5) DEFAULT  'NONE'  )
  RETURNS INTEGER
  LANGUAGE SQL
  SPECIFIC SQLTOOLS.ST_IFSSAVE
  NOT DETERMINISTIC
  CONTAINS SQL
  CALLED ON NULL INPUT
  NO EXTERNAL ACTION
  NOT FENCED
  SET OPTION  ALWBLK = *ALLREAD ,
  ALWCPYDTA = *OPTIMIZE ,
  COMMIT = *CHG ,
  DECRESULT = (31, 31, 00) ,
  DFTRDBCOL = SQLTOOLS ,
  DLYPRP = *NO ,
  DYNDFTCOL = *NO ,
  DYNUSRPRF = *USER ,
  SRTSEQ = *HEX ,
  BINDOPT = 'BNDSRVPGM(SQLTOOLS/SQLTOOLS)'
  R : BEGIN
CALL SQLTOOLS . IFS_WRITE ( PATH_NAME , TEXTDATA ,
FILE_CCSID , DATA_OPTION , FILE_OPTION , EOL ) ;
RETURN 0 ;
END R  ;

COMMENT ON SPECIFIC FUNCTION SQLTOOLS.ST_IFSSAVE
  IS
  'Writes data to an IFS file. using our IFS_WRITE procedure.
  This Function version of IFS_WRITE allows it to be used on
  a SELECT statement or as a VALUES argument to save data to
  an IFS file. The returned value is always 0 and can be ignored.
  If the file already exists the data is written to the file
  in the CCSID of the file. If the file does not exist it
  is created as UTF-8 (CCSID 1208) file unless the FILE_CCSID parameter
  is specified with a different CCSID value.
  The input data is assumed to be in the CCSID of the job, and is
  automatically converted to the CCSID of the file.
  Data in the file can be either replaced or appended to using the
  DATA_OPTION parameter. Use this function to write several rows
  from a SELECT statement or to save an entire file resulting from
  one of the HTTP RESTFUL SQL Services, such as QSYS2.HTTP_GET or
  QSYS2.HTTP_POST.' ;

COMMENT ON PARAMETER SPECIFIC FUNCTION SQLTOOLS.ST_IFSSAVE (
 PATH_NAME IS
  'The fully-qualified IFS file name to be written to.
  If the file doesn''t exist, then by default it will be created.
  When the file is created by this procedure, the FILE_CCSID parameter
  is used to assign the CCSID to the new file.
  The PATH_NAME may be up to 16 megabytes in length and is converted to
  CCSID 1200 if it is not already in that CCSID. This allows the path name
  to be specified in any supported character set, including double-byte data.' ,
 TEXTDATA IS
  'The textual data to be written to the IFS File. Up to 2GB of
  data may be specified, so really an entire file can be written out at once if
  necessary. The data passed to this parameter from your job is automatically
  converted to the CCSID specified on the FILE_CCSID parameter.
  It is received on this parameter as a CLOB(2G) value in the CCSID of the job.
  The data is written out to the target file in up to 4k blocks.
  Any CRLF values from the EOL (end of line) parameter are written after all
  data passed to this parameter has been written, unless EOL=>''*NONE''
  is specified.' ,
 FILE_CCSID IS
  'The CCSID to assigned to the file object that is created.
  The default is 1208 (UTF-8) and creates an ASCII text file. If the CCSID
  is 0, thenthe Job''s runtime CCSID is used. If the CCSID is < 0 (e.g., -1)
  then the "no-CCSID" value which is 65535, is assigned. CCSID(65535) is
  used for indicate a "binary" file or no translation is to be performed.
  Any valid CCSID may be specified.
  <b>This parameter is ignored if the file already exists.</b>' ,
 DATA_OPTION IS
  'Controls whether an existing IFS stream file''s content
  (data) is replaced or added to (appended). The valid choices are:<ul>
  <li><u>*APP</u>  - Append (Add) to the file</li>
  <li>*ADD - Add (append) data to the file. Same as APPEND</li>
  <li>*TRUN - Truncate existing data. Same as REPLACE</li>
  <li>*REPL - Replace the data in the file. Same as Truncate</li>
  <li>*OVER - Replace or "overwrite" the existing data</li>
  </ul>The parameter ignores upper/lower case,
  the leading asterisk is optional, and only the first letter of
  the parameter value is checked. that, is A, T, R, O, etc.
  For example, the following two variations are identical:
  <ul><li><i>DATA_OPTION=></i><b>''*append''</b></li>
  <li><i>DATA_OPTION=></i><b>''Add''</b></li></ul>
  Using the Replace or Overwrite option causes the data in an existing file
  to be removed before the new data is written. If the file already exists,
  then this parameter is ignored.' ,
 FILE_OPTION IS
  'Controls whether or not the IFS file is created or replaced
  before the data is written to it. The valid choices are:<ul>
  <li><u>*CREATE</u> - If the file does not already exist, it is created.
  If the file already exists, this parameter is ignored.</li>
  <li>*REPLACE - If the file already exists, it is deleted, then it
  is created. If the file does not exist, it is created.</li>
  <li>*NONE or *NO - No file creation or delete is performed.
  The file must already exist or the procedure will fail.</li></ul>
  Use this parameter when the file must be deleted and the CCSID is to
  be assigned to the file, for example if a FILEA was created with CCSID(37)
  and you need it to be CCS(1208) then FILE_OPTION=>''*REPLACE'' will
  delete the file and then create it with the FILE_CCSID parameter''s CCSID.
  The leading asterisk is optional and upper/lower case is ignored.' ,
 EOL IS
  'End of Line character(s). Specify one or more characters to insert
  at the end of the data being written to the file. This occurs only once per
  call to this procedure. Any 2 characters may be specified and are converted
  to ASCII (UTF-8) before inserting them into the file. In addition, the
  parameter accepts the following special values in any order:<ul>
  <li>LFCR - Inserts the Ascii Linefeed and carriage return.</li>
  <li>CRLF - Inserts the Ascii Carriage return and Linefeed.</li>
  <li>LF - Inserts the Ascii Linefeed.</li>
  <li>CR - Inserts the Ascii Carriage return.</li>
  <li><u>NONE</u> - Inserts no end of line symbols.</li>
  </ul>Upper/lower case is ignored. Use the letters ''LF'' and ''CR'' to
  symbolize the order in which you want the ASCII linefeed (X''0A'') and
  carriage return (X''0D'') written to the file.
  The default is NONE which causes no additinal LF/CR to be written to the file.
  This differs from the SQL Tools IFS_WRITE which defaults to ''LFCR'' for this
  parameter. It defaults to NONE since typically an entire file is written
  at once and adding the LF/CR sequence isn''t necessary.
  For more information on the EOL parameter, See the documentation for
  the SQL Tools IFS_WRITE procedure.'
 ) ;

LABEL ON SPECIFIC FUNCTION SQLTOOLS.ST_IFSSAVE
  IS 'Save data to a file on the IFS.' ;

GRANT EXECUTE
ON SPECIFIC FUNCTION SQLTOOLS.ST_IFSSAVE
TO PUBLIC ;

GRANT ALTER , EXECUTE
ON SPECIFIC FUNCTION SQLTOOLS.ST_IFSSAVE
TO QPGMR WITH GRANT OPTION ;
