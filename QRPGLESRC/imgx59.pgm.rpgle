      *%METADATA                                                       *
      * %TEXT Scan Checks / Invoices for A/P Documents - X             *
      *%EMETADATA                                                      *
     F/TITLE  IMGX58 - SCAN Voucher Documents.
     F********************************************************************
     F*  PROGRAM NAME: IMGX59   Check / Invoice for A/P.                 *
     F*          DATE: 07-October  2024                                  *
     F*        AUTHOR: Peter Rusin                                       *
     F*   ENVIRONMENT: OS/400 V5R3M0                                     *
     F*                                                                  *
     F*                                                                  *
     F*       PURPOSE:                                                   *
     F*         Program Scans MultiPage Documents.                       *
     F*                                                                  *
     F*                                                                  *
     F*     NARRATIVE:                                                   *
     F*                                                                  *
     F*      Image System "X".                                           *
     F*                                                                  *
     F*                                                                  *
     F*                                                                  *
     F*     NARRATIVE:                                                   *
     F*      FOR THE SCANS, THESE VALUES ARE LOADED.
     F*                                                                  *
     F*        INDEX 1 - Company Code                                    *
     F*        INDEX 2 - Vendor#                                         *
     F*        INDEX 3 - Check#                                          *
     F*        INDEX 4 -                                                 *
     F*        INDEX 5 -                                                 *
     F*        INDEX 6 -                                                 *
     F*        INDEX 7 - Document Type.                                  *
     F*                                                                  *
     F********************************************************************
     F*
     F**********************************************************************
     F* Client Files.
     F**********************************************************************
     F*
     F*
     F*** Client Unit File.
     F*   -----------------
     F*INZDOWL   IF   E           K DISK
     F*
     F**********************************************************************
     F* Program Files.
     F**********************************************************************
     F*
     F*
     F* DISPLAY FILE.
     F* -------------
     FIMGX59D   CF   E             WORKSTN
     F                                     INFDS(INFDS)
     FINFORVEND IF   E           K DISK    EXTFILE('LAWINTF/INFORVEND')
     D*
     D*----------------------------------------------------------------
     D*   DEFINE VECTORS/ARRAYS                                        --*
     D*----------------------------------------------------------------
     D*
     D*
     D*** MESSAGES ARRAY.
     D*
     D MSG             S             60    DIM(20) CTDATA PERRCD(1)
     D*
     D ICOMP           S                   LIKE(DVENVNDGRP)
     D IVEND#          S                   LIKE(DVENVENDOR)
     D*
     D*** FILE INFORMATION FEEDBACK AREA FOR DISPLAY FILE.
     D*
     D INFDS           DS
     D*  Cursor Location (row, column)
     D  $XCLOC               370    371B 0
     D*
     D*
     D*** SYSTEM DATA STRUCTURE.
     D*
     D                SDS
     D  PGMNM                  1     10
     D  JOBNM                244    253
     D  USRNM                254    263
     D*
     D*
     D*** WORK AREA FOR SYSTEM TIME / DATE.
     D*
     D                 DS
     D  TIMDAT                 1     12  0
     D  SYSTIM                 1      6  0
     D  SYSDAT                 7     12  0
     D*
     D*
     D*** DEFINE SYSTEM CODE AND 7 INDEXES FOR REAL VISION CALLS       --*
     D*
     D                 DS
     D  RQFLD                  1    211
     D  IWSYS                  1      1
     D  IWK1                   2     31
     D  IWK2                  32     61
     D  IWK3                  62     91
     D  IWK4                  92    121
     D  IWK5                 122    151
     D  IWK6                 152    181
     D  IWK7                 182    211
     C*
     C*-------------------------------------------------------
     C* Key Fields
     C*
     C     INF_KEY       KLIST
     C                   KFLD                    ICOMP
     C                   KFLD                    IVEND#
     C*
     C*
     C*-------------------------------------------------------------------------
     C*   MAINLINE
     C*-------------------------------------------------------------------------
     C*
     C                   EXSR      $HKEEP
     C*
     C     *INLR         DOWEQ     '0'
     C                   EXSR      $PRO
     C                   ENDDO
     C*
     C                   RETURN
     C*
     C*
     C*-------------------------------------------------------------------------
     C*   HOUSEKEEPING
     C*-------------------------------------------------------------------------
     C     $HKEEP        BEGSR
     C*
     C*
     C*** DECLARE WORKING VARIABLES.
     C*
     C                   Z-ADD     *ZEROS        @SCRNO            3 0
     C                   MOVE      *BLANKS       @EDTS1            2
     C                   MOVE      *BLANKS       @MSCAN            3
     C*
     C*
     C*** SET IMAGE SYSTEM TO SYSTEM 'L'
     C*
     C                   MOVE      'X'           W@SYS             1
     C*
     C*
     C*** SET SYSTEM TIME DATE
     C*
     C                   TIME                    TIMDAT
     C*
     C*
     C*** SET SCREEN NUMBER TO ZERO TO CAUSE LOAD INITIAL SCREEN.
     C*
     C                   Z-ADD     *ZEROS        @SCRNO
     C*
     C                   ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   PROCESS
     C*-------------------------------------------------------------------------
     C*
     C     $PRO          BEGSR
     C*
     C*
     C*** RESET KEYS FOR SCREEN TO BE DISPLAYED.
     C*
     C                   EXSR      $RKEYS
     C*
     C*
     C*** EXECUTE REQUESTED SCREEN.
     C*
     C                   SELECT
     C     @SCRNO        WHENEQ    1
     C                   EXFMT     SCR01
     C                   MOVE      *OFF          *IN24                          POS CUR OFF
     C                   MOVE      *BLANKS       S1MSG
     C                   EXSR      $KEYS1
     C                   OTHER
     C                   MOVE      *BLANKS       S1MSG
     C                   EXSR      $SETS1
     C                   ENDSL
     C*
     C     XPRO          ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   $SETS1   - SET SCREEN 1 FOR DISPLAY.
     C*-------------------------------------------------------------------------
     C*
     C     $SETS1        BEGSR
     C*
     C*
     C*** INIT SCREEN FIELDS.
     C*
     C                   MOVE      *BLANKS       S1COMP
     C                   MOVE      *BLANKS       S1VEND#
     C                   MOVE      *BLANKS       S1CHECK#
     C                   MOVEL(p)  'CHK'         S1TYPE
     C*
     C*
     C*** POSITION CURSOR.
     C*
     C                   Z-ADD     03            @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C*
     C*
     C*** SET SCREEN NUMBER TO SCREEN 1.
     C*
     C                   Z-ADD     01            @SCRNO
     C*
     C*
     C*** PUT INITIAL MESSAGE.
     C*
     C                   MOVEL     MSG(20)       S1MSG
     C*
     C     XSETS1        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   $CLRS1   - RESET SCREEN 1 FOR REDISPLAY
     C*-------------------------------------------------------------------------
     C*
     C     $CLRS1        BEGSR
     C*
     C*
     C*** CLEAR SCREEN FIELDS.
     C*
     C*****              MOVE      *BLANKS       S1COMP
     C*****              MOVE      *BLANKS       S1VEND#
     C                   MOVE      *BLANKS       S1CHECK#
     C                   MOVEL(p)  'CHK'         S1TYPE
     C                   Z-ADD     5             @LINE
     C*
     C     XCLRS1        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   $RKEYS  - RESET SCREEN KEYS
     C*-------------------------------------------------------------------------
     C     $RKEYS        BEGSR
     C*
     C*
     C*** RESET THE KEYS FOR THE SCREEN JUST DISPLAYED.
     C*
     C                   SELECT
     C     @SCRNO        WHENEQ    01
     C                   MOVE      '0'           *IN03
     C                   MOVE      '0'           *IN04
     C                   MOVE      '0'           *IN12
     C                   ENDSL
     C*
     C     XRKEYS        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   $
     C*-------------------------------------------------------------------------
     C     $KEYS1        BEGSR
     C*
     C*
     C*
     C*** EXIT.
     C*
     C                   SELECT
     C     *IN03         WHENEQ    *ON
     C                   MOVE      *ON           *INLR
     C*
     C*
     C*
     C*** PROMPT FOR DOC TYPE.
     C*
     C     *IN04         WHENEQ    *ON
     C                   EXSR      $PRMPT
     C*
     C*
     C*** CANCEL.
     C*
     C     *IN12         WHENEQ    *ON
     C                   MOVE      *ON           *INLR
     C*
     C                   OTHER
     C*
     C*
     C*** ENTER KEY --> DO SCAN THEN REDISPLAY.
     C*
     C     S1COMP        IFEQ      SCOMP
     C     S1VEND#       ANDEQ     SVEND#
     C     S1CHECK#      ANDEQ     SCHECK#
     C     @EDTS1        ANDEQ     'OK'
     C                   EXSR      $OPT01
     C     @MSCAN        IFEQ      'YES'
     C                   EXSR      $CLRS1
     C                   ENDIF
     C                   ELSE
     C                   EXSR      $EDTS1
     C                   MOVEL     S1COMP        SCOMP             3
     C                   MOVEL     S1VEND#       SVEND#            5
     C                   MOVEL     S1CHECK#      SCHECK#           7
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C     XKEYS1        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C* $EDTS1    - VALIDATE SCREEN #1 INPUT.
     C*-------------------------------------------------------------------------
     C     $EDTS1        BEGSR
     C*
     C                   Do        1
     C*
     C*
     C*** RESET FLAG.
     C*
     C                   MOVEL     'OK'          @EDTS1
     C*
     C*
     C*** Company Not Entered.
     C*
     C     S1COMP        IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(1)        S1MSG
     C                   Z-ADD     3             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*
     C     S1COMP        IFNE      'NRS'
     C     S1COMP        ANDNE     'NRT'
     C     S1COMP        ANDNE     'KEY'
     C     S1COMP        ANDNE     'MIT'
     C     S1COMP        ANDNE     'WL '
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(1)        S1MSG
     C                   Z-ADD     3             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*
     C     S1VEND#       IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(2)        S1MSG
     C                   Z-ADD     4             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*
     C                   EVAL      ICOMP = 'NRS'
     C                   MOVE      S1VEND#       IVEND#
     C     INF_KEY       CHAIN     INFORVEND
     C                   IF        NOT %FOUND(INFORVEND)
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(2)        S1MSG
     C                   Z-ADD     4             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   EVAL      VNAME = *BLANKS
     C                   Leave
     C                   ENDIF
     C*
     C                   IF        %FOUND(INFORVEND)
     C                   EVAL      VNAME = DVENVNDVNM
     C                   ENDIF
     C*
     C     S1CHECK#      IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(3)        S1MSG
     C                   Z-ADD     5             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*
     C*
     C*** format Vendor# and Check#.
     C*
     C                   EvalR     S1VEND# = %trimr(s1VEND#)
     C                   Eval      s1VEND# = %xlate(' ':'0':s1VEND#)
     C                   EvalR     S1CHECK# = %trimr(s1CHECK#)
     C                   Eval      s1CHECK# = %xlate(' ':'0':s1CHECK#)
     C*
     C*
     C*** Document Type Must Be Selected.
     C*
     C     S1TYPE        IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(4)        S1MSG
     C                   Z-ADD     11            @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*
     C*
     C*** IF ALL OK...LOAD MESSAGE.
     C*
     C                   MOVEL     MSG(18)       S1MSG
     C*
     C                   Enddo
     C*
     C     XEDTS1        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   $OPT01    - SCAN DOCUMENTS.
     C*-------------------------------------------------------------------------
     C     $OPT01        BEGSR
     C*
     C*
     C*** INIT PARMS FOR MVC013D CALL.
     C*
     C                   MOVE      *BLANKS       RQFLD
     C                   MOVE      *BLANKS       RTNCD             2
     C                   MOVE      *BLANKS       ROUTE            30
     C                   MOVE      *BLANKS       PAGES             3
     C                   MOVE      *BLANKS       BCODE             8
     C*
     C*
     C*** LOAD PARMS FOR MVC013D - DOCUMENT SCAN DIRECT.
     C*
     C*   DESC           NAME  LEN       PURPOSE
     C*   -------------  ----- ---       ------------------------------------------
     C*   STD PARM       RQFLD 211       STD RVI CALL.
     C*   RTN CODE       RTNCD   2       LOAD WITH 'RT' TO DO ROUTING AUTOMATICALLY.
     C*   ROUTE          ROUTE  30       ROUTING PROFILE.
     C*   PAGES          PAGES   3       PAGES PER DOCUMENT.
     C*   BCODE          BCODE   8       BARCODE PROFILE.
     C*
     C*
     C*
     C*** LOAD IMAGE SYSTEM CODE.
     C*
     C                   MOVE      W@SYS         IWSYS
     C*
     C*
     C*** INDEX 1.
     C*
     C                   MOVEL     S1COMP        IWK1
     C*
     C*
     C*** INDEX 2.
     C*
     C                   MOVEL     S1VEND#       IWK2
     C*
     C*
     C*** INDEX 3.
     C*
     C                   MOVEL     S1CHECK#      IWK3
     C*
     C*
     C*** INDEX 4.
     C*
     C                   MOVE      *BLANKS       IWK4
     C*
     C*
     C*** INDEX 5.
     C*
     C                   MOVE      *BLANKS       IWK5
     C*
     C*
     C*** INDEX 6.
     C*
     C                   MOVE      *BLANKS       IWK6
     C*
     C*
     C*** LOAD DOCUMENT TYPE TO INDEX 7.
     C*
     C                   MOVEL     S1TYPE        IWK7
     C*
     C*
     C*** DO SCAN.
     C*
     C                   MOVE      'D  '         @MSCAN
     C                   EXSR      $MSCAN
     C*
     C*
     C*** IF SCAN WAS ACCEPTED, LOAD ACCEPTED MESSAGE.
     C*
     C     @MSCAN        IFEQ      'YES'
     C                   MOVEA     MSG(14)       S1MSG
     C                   ELSE
     C                   MOVEL     MSG(13)       S1MSG
     C                   ENDIF
     C*
     C     XOPT01        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C*   $UPL01    - SCAN DOCUMENTS WITH UPLOAD.
     C*-------------------------------------------------------------------------
     C     $UPL01        BEGSR
     C*
     C*
     C*** INIT PARMS FOR MVC013 CALL.
     C*
     C                   MOVE      *BLANKS       RQFLD
     C                   MOVE      *BLANKS       RTNCD             2
     C*
     C*
     C*** LOAD PARMS FOR MVC013 - DOCUMENT SCAN WITH UPLOAD.
     C*
     C*   DESC           NAME  LEN       PURPOSE
     C*   -------------  ----- ---       ------------------------------------------
     C*   STD PARM       RQFLD 211       STD RVI CALL.
     C*   RTN CODE       RTNCD   2       LOAD WITH 'RT' TO DO ROUTING AUTOMATICALLY.
     C*
     C*
     C*
     C*** LOAD IMAGE SYSTEM CODE.
     C*
     C                   MOVE      W@SYS         IWSYS
     C*
     C*
     C*** INDEX 1.
     C*
     C                   MOVEL     S1COMP        IWK1
     C*
     C*
     C*** INDEX 2.
     C*
     C                   MOVEL     S1VEND#       IWK2
     C*
     C*
     C*** INDEX 3.
     C*
     C                   MOVEL     S1CHECK#      IWK3
     C*
     C*
     C*** INDEX 4.
     C*
     C                   MOVE      *BLANKS       IWK4
     C*
     C*
     C*** INDEX 5.
     C*
     C                   MOVE      *BLANKS       IWK5
     C*
     C*
     C*** INDEX 6.
     C*
     C                   MOVE      *BLANKS       IWK6
     C*
     C*
     C*** LOAD DOCUMENT TYPE TO INDEX 7.
     C*
     C                   MOVEL     S1TYPE        IWK7
     C*
     C*
     C*** DO SCAN.
     C*
     C                   MOVE      'U  '         @MSCAN
     C                   EXSR      $MSCAN
     C*
     C*
     C*** IF SCAN WAS ACCEPTED, LOAD ACCEPTED MESSAGE.
     C*
     C     @MSCAN        IFEQ      'YES'
     C                   MOVEA     MSG(14)       S1MSG
     C                   ELSE
     C                   MOVEL     MSG(13)       S1MSG
     C                   ENDIF
     C*
     C     XUPL01        ENDSR
     C*
     C*-------------------------------------------------------------------------
     C* SUBROUTINE  -  $MSCAN - PERFORM SCAN OF DOCUMENTS.
     C*-------------------------------------------------------------------------
     C     $MSCAN        BEGSR
     C*
     C*
     C*** SCAN DOCUMENTS.
     C*
     C     @MSCAN        IFEQ      'U'
     C                   CALL      'MVC013'                                     SCAN UPLOAD
     C                   PARM                    RQFLD
     C                   PARM                    RTNCD
     C                   ENDIF
     C     @MSCAN        IFEQ      'D'
     C                   CALL      'MVC013D'                                    SCAN DIRECT
     C                   PARM                    RQFLD
     C                   PARM                    RTNCD
     C                   PARM                    ROUTE
     C                   PARM                    PAGES
     C                   PARM                    BCODE
     C                   ENDIF
     C*
     C*
     C*** IF SCAN ACCEPTED, LOAD OK TO SUBROUTINE PARM.
     C*
     C     RTNCD         IFNE      'OK'
     C                   MOVEL     'NO '         @MSCAN
     C                   ELSE
     C                   MOVEL     'YES'         @MSCAN
     C                   ENDIF
     C*
     C     XMSCAN        ENDSR
     C*
     C*****************************************************************
     C*                                                               *
     C* SUBROUTINE  -  $PRMPT - PROMPT BASED ON THE LOCATION.         *
     C*                                                               *
     C*****************************************************************
     C     $PRMPT        BEGSR
     C*
     C*
     C*** CALL PROMPT PGM BASED ON CUR LOCATION.
     C*
     C                   SELECT
     C     @FIELD        WHENEQ    'S1TYPE'
     C                   MOVE      *Blanks       P@TAB             2
     C                   MOVE      *BLANKS       P@KEY            10
     C                   Eval      P@TAB = W@Sys + '0'
     C                   CALL      'IMGRU32'
     C                   PARM                    P@TAB
     C                   PARM                    P@KEY
     C                   MOVEL     P@KEY         S1TYPE
     C*
     C                   OTHER
     C                   MOVEL     MSG(19)       S1MSG
     C                   ENDSL
     C*
     C     XPRMPT        ENDSR
     C*-------------------------------------------------------------------------
     C*-------------------------------------------------------------------------
**   MSG - TABLE OF SCREEN MESSAGES -----LEN(60)-----------X
Valid Company Code Is Required.
Valid Vendor# Is Required.
Check# Is Required.
Document Type Must Be Selected.
5
6
7
8
9
10
11
12
Last Scan Was Not Accepted.
Last Scan Was Accpted.
15
16
17
Press Enter To Scan.
Field Does Not Have Prompting Available.
Enter Required Fields & Hit Enter To Get Add'tl Info.
