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
     F*       PURPOSE:                                                   *
     F*         Program Scans MultiPage Documents.                       *
     F*                                                                  *
     F*     NARRATIVE:                                                   *
     F*      Image System "X".                                           *
     F*      FOR THE SCANS, THESE VALUES ARE LOADED.                     *
     F*        INDEX 1 - Company Code                                    *
     F*        INDEX 2 - Vendor#                                         *
     F*        INDEX 3 - Check#                                          *
     F*        INDEX 4 -                                                 *
     F*        INDEX 5 -                                                 *
     F*        INDEX 6 -                                                 *
     F*        INDEX 7 - Document Type.                                  *
     F********************************************************************
     FIMGX59D   CF   E             WORKSTN
     F                                     INFDS(INFDS)
     FINFORVEND IF   E           K DISK    EXTFILE('LAWINTF/INFORVEND')
     D*** MESSAGES ARRAY.
     D MSG             S             60    DIM(20) CTDATA PERRCD(1)
     D*
     D ICOMP           S                   LIKE(DVENVNDGRP)
     D IVEND#          S                   LIKE(DVENVENDOR)
     D*** FILE INFORMATION FEEDBACK AREA FOR DISPLAY FILE.
     D INFDS           DS
     D*  Cursor Location (row, column)
     D  $XCLOC               370    371B 0
     D*** SYSTEM DATA STRUCTURE.
     D                SDS
     D  PGMNM                  1     10
     D  JOBNM                244    253
     D  USRNM                254    263
     D*** WORK AREA FOR SYSTEM TIME / DATE.
     D                 DS
     D  TIMDAT                 1     12  0
     D  SYSTIM                 1      6  0
     D  SYSDAT                 7     12  0
     D*** DEFINE SYSTEM CODE AND 7 INDEXES FOR REAL VISION CALLS
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
     C* Key Fields
     C     INF_KEY       KLIST
     C                   KFLD                    ICOMP
     C                   KFLD                    IVEND#
     C*-------------------------------------------------------------------------
     C*   MAINLINE
     C*-------------------------------------------------------------------------
     C                   EXSR      $HKEEP
     C*
     C     *INLR         DOWEQ     '0'
     C                   EXSR      $PRO
     C                   ENDDO
     C*
     C                   RETURN
     C*-------------------------------------------------------------------------
     C*   HOUSEKEEPING
     C*-------------------------------------------------------------------------
     C     $HKEEP        BEGSR
     C*** DECLARE WORKING VARIABLES.
     C                   Z-ADD     *ZEROS        @SCRNO            3 0
     C                   MOVE      *BLANKS       @EDTS1            2
     C                   MOVE      *BLANKS       @MSCAN            3
     C*** SET IMAGE SYSTEM TO SYSTEM 'L'
     C                   MOVE      'X'           W@SYS             1
     C*** SET SYSTEM TIME DATE
     C                   TIME                    TIMDAT
     C*** SET SCREEN NUMBER TO ZERO TO CAUSE LOAD INITIAL SCREEN.
     C                   Z-ADD     *ZEROS        @SCRNO
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------------
     C*   PROCESS
     C*-------------------------------------------------------------------------
     C     $PRO          BEGSR
     C*** RESET KEYS FOR SCREEN TO BE DISPLAYED.
     C                   EXSR      $RKEYS
     C*** EXECUTE REQUESTED SCREEN.
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
     C*-------------------------------------------------------------------------
     C*    $SETS1      - SET SCREEN 1 FOR DISPLAY
     C*-------------------------------------------------------------------------
     C     $SETS1        BEGSR
     C*** INIT SCREEN FIELDS.
     C                   MOVE      *BLANKS       S1COMP
     C                   MOVE      *BLANKS       S1VEND#
     C                   MOVE      *BLANKS       S1CHECK#
     C                   MOVEL(p)  'CHK'         S1TYPE
     C*** POSITION CURSOR.
     C                   Z-ADD     03            @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C*** SET SCREEN NUMBER TO SCREEN 1.
     C                   Z-ADD     01            @SCRNO
     C*** PUT INITIAL MESSAGE.
     C                   MOVEL     MSG(20)       S1MSG
     C*
     C     XSETS1        ENDSR
     C*-------------------------------------------------------------------------
     C*    $CLRS1      - RESET SCREEN 1 FOR REDISPLAY
     C*-------------------------------------------------------------------------
     C     $CLRS1        BEGSR
     C*** CLEAR SCREEN FIELDS.
     C                   MOVE      *BLANKS       S1CHECK#
     C                   MOVEL(p)  'CHK'         S1TYPE
     C                   Z-ADD     5             @LINE
     C*
     C     XCLRS1        ENDSR
     C*-------------------------------------------------------------------------
     C*    $RKEYS      - RESET SCREEN KEYS
     C*-------------------------------------------------------------------------
     C     $RKEYS        BEGSR
     C*** RESET THE KEYS FOR THE SCREEN JUST DISPLAYED.
     C                   SELECT
     C     @SCRNO        WHENEQ    01
     C                   MOVE      '0'           *IN03
     C                   MOVE      '0'           *IN04
     C                   MOVE      '0'           *IN12
     C                   ENDSL
     C*
     C     XRKEYS        ENDSR
     C*-------------------------------------------------------------------------
     C*    $KEYS1      - PROCESS SCREEN 1 INPUT
     C*-------------------------------------------------------------------------
     C     $KEYS1        BEGSR
     C*** EXIT.
     C                   SELECT
     C     *IN03         WHENEQ    *ON
     C                   MOVE      *ON           *INLR
     C*** PROMPT FOR DOC TYPE.
     C     *IN04         WHENEQ    *ON
     C                   EXSR      $PRMPT
     C*** CANCEL.
     C     *IN12         WHENEQ    *ON
     C                   MOVE      *ON           *INLR
     C*
     C                   OTHER
     C*** ENTER KEY --> DO SCAN THEN REDISPLAY.
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
     C*-------------------------------------------------------------------------
     C*    $EDTS1      - VALIDATE SCREEN 1 INPUT
     C*-------------------------------------------------------------------------
     C     $EDTS1        BEGSR
     C*
     C                   Do        1
     C*** RESET FLAG.
     C                   MOVEL     'OK'          @EDTS1
     C*** Company Not Entered.
     C     S1COMP        IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(1)        S1MSG
     C                   Z-ADD     3             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*** Validate Company.
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
     C*** Vendor Not Entered.
     C     S1VEND#       IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(2)        S1MSG
     C                   Z-ADD     4             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*** format Company.
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
     C*** Check# Not Entered.
     C     S1CHECK#      IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(3)        S1MSG
     C                   Z-ADD     5             @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*** format Vendor# and Check#.
     C                   EvalR     S1VEND# = %trimr(s1VEND#)
     C                   Eval      s1VEND# = %xlate(' ':'0':s1VEND#)
     C                   EvalR     S1CHECK# = %trimr(s1CHECK#)
     C                   Eval      s1CHECK# = %xlate(' ':'0':s1CHECK#)
     C*** Document Type Must Be Selected.
     C     S1TYPE        IFEQ      *BLANKS
     C                   MOVEL     'NO'          @EDTS1
     C                   MOVEL     MSG(4)        S1MSG
     C                   Z-ADD     11            @LINE
     C                   Z-ADD     17            @POS
     C                   MOVE      *ON           *IN24
     C                   Leave
     C                   ENDIF
     C*** IF ALL OK...LOAD MESSAGE.
     C                   MOVEL     MSG(18)       S1MSG
     C*
     C                   Enddo
     C*
     C     XEDTS1        ENDSR
     C*-------------------------------------------------------------------------
     C*    $OPT01      - SCAN DOCUMENTS
     C*-------------------------------------------------------------------------
     C     $OPT01        BEGSR
     C*** INIT PARMS FOR MVC013D CALL.
     C                   MOVE      *BLANKS       RQFLD
     C                   MOVE      *BLANKS       RTNCD             2
     C                   MOVE      *BLANKS       ROUTE            30
     C                   MOVE      *BLANKS       PAGES             3
     C                   MOVE      *BLANKS       BCODE             8
     C*** LOAD IMAGE SYSTEM CODE.
     C                   MOVE      W@SYS         IWSYS
     C                   MOVEL     S1COMP        IWK1                           Index 1 Company
     C                   MOVE      S1VEND#       IWK2                           Index 2 Vendor
     C                   MOVE      S1CHECK#      IWK3                           Index 3 Check
     C                   MOVE      *BLANKS       IWK4                           Index 4
     C                   MOVE      *BLANKS       IWK5                           Index 5
     C                   MOVE      *BLANKS       IWK6                           Index 6
     C                   MOVEL     S1TYPE        IWK7                           Index 7 Doc Type
     C*** DO SCAN.
     C                   MOVE      'D  '         @MSCAN
     C                   EXSR      $MSCAN
     C*** IF SCAN WAS ACCEPTED, LOAD ACCEPTED MESSAGE.
     C     @MSCAN        IFEQ      'YES'
     C                   MOVEA     MSG(14)       S1MSG
     C                   ELSE
     C                   MOVEL     MSG(13)       S1MSG
     C                   ENDIF
     C*
     C     XOPT01        ENDSR
     C*-------------------------------------------------------------------------
     C*    $UPL01      - SCAN DOCUMENTS WITH UPLOAD
     C*-------------------------------------------------------------------------
     C     $UPL01        BEGSR
     C*** INIT PARMS FOR MVC013 CALL.
     C                   MOVE      *BLANKS       RQFLD
     C                   MOVE      *BLANKS       RTNCD             2
     C*** LOAD IMAGE SYSTEM CODE.
     C                   MOVE      W@SYS         IWSYS
     C                   MOVEL     S1COMP        IWK1                           Index 1 Company
     C                   MOVE      S1VEND#       IWK2                           Index 2 Vendor
     C                   MOVE      S1CHECK#      IWK3                           Index 3 Check
     C                   MOVE      *BLANKS       IWK4                           Index 4
     C                   MOVE      *BLANKS       IWK5                           Index 5
     C                   MOVE      *BLANKS       IWK6                           Index 6
     C                   MOVEL     S1TYPE        IWK7                           Index 7 Doc Type
     C*** DO SCAN.
     C                   MOVE      'U  '         @MSCAN
     C                   EXSR      $MSCAN
     C*** IF SCAN WAS ACCEPTED, LOAD ACCEPTED MESSAGE.
     C     @MSCAN        IFEQ      'YES'
     C                   MOVEA     MSG(14)       S1MSG
     C                   ELSE
     C                   MOVEL     MSG(13)       S1MSG
     C                   ENDIF
     C*
     C     XUPL01        ENDSR
     C*-------------------------------------------------------------------------
     C*    $MSCAN      - PERFORM SCAN OF DOCUMENTS
     C*-------------------------------------------------------------------------
     C     $MSCAN        BEGSR
     C*** SCAN DOCUMENTS.
     C     @MSCAN        IFEQ      'U'
     C                   CALL      'MVC013'                                     SCAN UPLOAD
     C                   PARM                    RQFLD                          STD RVI CALL
     C                   PARM                    RTNCD                          'RT' TO DO ROUTING
     C                   ENDIF
     C     @MSCAN        IFEQ      'D'
     C                   CALL      'MVC013D'                                    SCAN DIRECT
     C                   PARM                    RQFLD                          STD RVI CALL
     C                   PARM                    RTNCD                          'RT' TO DO ROUTING
     C                   PARM                    ROUTE                          ROUTING PROFILE
     C                   PARM                    PAGES                          PAGES PER DOCUMENT
     C                   PARM                    BCODE                          BARCODE PROFILE
     C                   ENDIF
     C*** IF SCAN ACCEPTED, LOAD OK TO SUBROUTINE PARM.
     C     RTNCD         IFNE      'OK'
     C                   MOVEL     'NO '         @MSCAN
     C                   ELSE
     C                   MOVEL     'YES'         @MSCAN
     C                   ENDIF
     C*
     C     XMSCAN        ENDSR
     C*-------------------------------------------------------------------------
     C*    $PRMPT      - PROMPT BASED ON THE LOCATION
     C*-------------------------------------------------------------------------
     C     $PRMPT        BEGSR
     C*** CALL PROMPT PGM BASED ON CUR LOCATION.
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
