      /*%METADATA                                                     */
      /* %TEXT copy source members to IFS directory (cmd)             */
      /*%EMETADATA                                                    */
             CMD        PROMPT('Copy source file member to IFS')

             PARM       KWD(FROMFILE) TYPE(FILE) MIN(1) PROMPT('File' 1)

             PARM       KWD(TODIR) TYPE(*PNAME) LEN(128) MIN(1) EXPR(*YES) +
                          PROMPT('Directory' 4)

             PARM       KWD(MBR) TYPE(*CHAR) LEN(10) DFT(*) SPCVAL((*)) +
                          EXPR(*YES) PROMPT('Member' 2)

             PARM       KWD(LIBASPDEV) TYPE(*NAME) LEN(10) DFT(*SYSBAS) +
                          SPCVAL((*SYSBAS)) EXPR(*YES) PROMPT('Library ASP +
                          device name' 3)

             PARM       KWD(CRTDIR) TYPE(*CHAR) LEN(4) RSTD(*YES) DFT(*NO) +
                          VALUES(*NO *YES) EXPR(*YES) PROMPT('Create +
                          directory' 5)

             PARM       KWD(STMFEXT) TYPE(*CHAR) LEN(10) DFT(*DFT) +
                          SPCVAL((*DFT) (*NONE) (*TYPE)) EXPR(*YES) +
                          PROMPT('STMF extension' 6)

             PARM       KWD(STMFCCSID) TYPE(*INT4) DFT(*PCASCII) RANGE(1 +
                          65533) SPCVAL((*PCASCII 1252) (*STDASCII 850)) +
                          EXPR(*YES) PROMPT('STMF CCSID' 7)

             PARM       KWD(ENDLINFMT) TYPE(*CHAR) LEN(6) DFT(*CRLF) +
                          SPCVAL((*CRLF) (*LF) (*CR) (*LFCR) (*FIXED)) +
                          EXPR(*YES) PROMPT('End of line characters' 8)

             PARM       KWD(AUT) TYPE(*CHAR) LEN(10) RSTD(*YES) +
                          DFT(*INDIR) VALUES(*INDIR *DFT *FILE *INDIRFILE) +
                          EXPR(*YES) PROMPT('Authority' 9)

             PARM       KWD(STMFTXT) TYPE(*CHAR) LEN(4) RSTD(*YES) +
                          DFT(*NO) VALUES(*NO *YES) EXPR(*YES) +
                          PROMPT('STMF description' 10)

             PARM       KWD(REPORT) TYPE(*CHAR) LEN(4) RSTD(*YES) DFT(*NO) +
                          VALUES(*NO *YES) EXPR(*YES) PROMPT('Report (CSV +
                          format)' 11)

             PARM       KWD(SLOC) TYPE(*CHAR) LEN(4) RSTD(*YES) DFT(*NO) +
                          VALUES(*NO *YES) EXPR(*YES) PMTCTL(LOC) +
                          PROMPT('Count physical SLOC (CBL/RPG)' 12)

 FILE:       QUAL       TYPE(*NAME) LEN(10) SPCVAL((*ALL)) EXPR(*YES)
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) SPCVAL((*LIBL) +
                          (*CURLIB)) EXPR(*YES) PROMPT('Library')

 LOC:        PMTCTL     CTL(REPORT) COND((*EQ *YES)) NBRTRUE(*ALL)
