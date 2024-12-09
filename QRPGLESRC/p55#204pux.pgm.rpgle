      *%METADATA                                                       *
      * %TEXT 04 04 23 Create PUHD,.. new                              *
      *%EMETADATA                                                      *
     H DFTACTGRP(*NO) BNDDIR('B55#BNDDIR') DATEDIT(*YMD)
     FR204#H01  UF   E           K DISK
     FR204#H05  IF   E           K DISK
     FR204#H15  IF   E           K DISK
     FR204#D01  IF   E           K DISK
     FR204#D05  IF   E           K DISK
     FR204#D10  IF   E           K DISK
     FR204#D35  IF   E           K DISK
     FR204#D15  IF   E           K DISK    prefix(d15)
     FR204#L01  IF   E           K DISK    prefix(L01)
     FR204#D25  IF   E           K DISK
     FR204#D30  UF   E           K DISK
     FF55#EDCTL IF   E           K DISK
     FLOCT14    IF   E           K DISK
     FPUHD      O    E             DISK    block(*no)                           ml01 add block(*no)
     FPUDT      O    E             DISK
     FPUPO01    IF A E           K DISK
     FBLHD01    UF   E           K DISK
     FBLPO01    UF A E           K DISK
     FBLPO04    UF   E           K DISK    rename(blpofm:blpo4)
     FZIPS01    IF   E           K DISK
     FCONS01    IF   E           K DISK
     FS990#H01L2UF   E           K DISK    prefix(S_)
     FS990#H05  UF A E           K DISK    prefix(S5_)
     FWNSCWF    O    E             DISK    block(*no)                           ml01 add block(*no)
      *
     D                SDS
     D  #PGM                   1     10
     D  #USR                 254    263
      *
      /COPY QRPGLESRC,#SPPROT
      *
      *****************************************************************
     D DIVPO2_TCA      S              6    DIM(70) CTDATA PERRCD(1)             DIV(4 pos),PO(2 pos)
     D CONS#_TCA       S                   DIM(70) ALT(DIVPO2_TCA) LIKE(CONS#9)     5,0
     D DIVPO2_TNJ      S              6    DIM(37) CTDATA PERRCD(1)             DIV(4 pos),PO(2 pos)
     D CONS#_TNJ       S                   DIM(37) ALT(DIVPO2_TNJ) LIKE(CONS#9)     5,0
      *
     D CONS_ARR        S             39    DIM(150)                             was 99
     D SameConsPO_ARR  S             24    DIM(150)                             was 99
     d
      *************************************************************************
     D  DIVPO12        DS
     D  div                    1      4
     D  PO_12                  5      6
      *
     D  CONS#_PO_DS    DS
     D  PO#                    1     15
     D  cons#_PO              16     20  0
     D  div_PO                21     24
     D  QTY_PO                25     29  0
     D  WGT_PO                30     34  0
     D  VOL_PO                35     39  0
      *
     D  SameConsPO_DS  DS
     D  PO#C                   1     15
     D  cons#_PO#C            16     20  0
     D  div_PO#C              21     24
     d CALLCLRTS       s              1
     d RTSSPID         s                    like(THSPID)
      *************************************************************************
      *
     D  WkPu#          s                   LIKE(punum1)
     D  PMMBOX         s                   LIKE(THMBOX)
     D  PMNAME         s                   LIKE(IENAME)
     D  PMKEY1         s                   LIKE(IEKEY1)
     D  WKQUAL         s                   LIKE(TH_QU_REF)
     D  WKSTPSQ        s                   LIKE(TDSTPSQ)
     D  WKTP214        s                   LIKE(THTPID)
     D  p_ut306u_1     s              4    INZ
     D  p_ut306u_2     s              5    INZ
     D  p_ut306u_3     s              7  0 INZ
     D  CUSTID         s              4    INZ
     D  STPCDE         s             15    INZ
     D  LOCTID         s                   LIKE(LOCID8)
     D  FLG            s              1    INZ
      *
     ddatISO           S               D   DATFMT(*ISO)
     DDATECYMD         S              7  0
     dseq#             s                   like(seqno0)
     dpro##            s                   like(pro##h)
     dpo_exist         s              1
     dseqpo            s                   like(seqno0)
     dAPPT#            s                   like(AUTH#0)
     D  zip3_n         S              3  0 INZ
     D  zip3_a         S              3    INZ
     D  zip5_a         S              5    INZ
     D  ponum          S                   INZ  like(threfid)
     D  added_po       S              1    INZ
     D  totpallets     S              5  0
     D  ctnqt          S                   like(ctnqt0)
     D  weigh          S                   like(weigh0)
     D  cubft          S                   like(cubft0)
     d  wgt7_n         s              7  0
     d  vol7_n         s              7  0
      *
     d  L11MCI_REF     s                    like(THREFID)
     d  PUDT_LIN#      s              3  0
     d  CONS_ARR_CTR   s              3  0
     d  SameConsPO_CT  s              3  0
     d  max_arr_size   s              3  0 INZ(150)                             was 99
     d  CTR            s              3  0
     d  CTR_P          s              3  0
     d  I              s              2  0
     d  prv_cons#      s                    like(cons#2)
     d  prv_po#        s                    like(ponum0)
     d  prv_div        s                    like(div)
     d  CONS_QTY       s                    like(ctnqt2)
     d  CONS_WGT       s                    like(weigh2)
     d  CONS_VOL       s                    like(cubft2)
     d SameConsPOfld   s             24
     d S5_QUAL         s                    like(s5_GH_QU_REF)
     d PUHD_ADDED      s              1
     d PRV_CITY        s                    like(TDN401)
     d PRV_STATE       s                    like(TDN402)
     d PU1_TDSTPSQ     s                    like(TDSTPSQ)
     d PUSTOPS_#       s              3  0
     d PUSTOPS_M       s              1
     d RATED           s              1
      *************************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    PMMBOX
     C                   PARM                    PMPUNUM           7            P/U Number
      *
     C     K#H05         KLIST
     C                   KFLD                    PMMBOX
     C                   KFLD                    WKQUAL
      *
     C     K#STOP        KLIST
     C                   KFLD                    PMMBOX
     C                   KFLD                    WKSTPSQ
      *
     C     K#EDCTL       KLIST
     C                   KFLD                    PMNAME
     C                   KFLD                    PMKEY1
      *
     c     K#S990L2      KLIST
     C                   KFLD                    THMBOX
     C                   KFLD                    THTPID                         tpid for edi204
     C                   KFLD                    THSPID
      *
     c     K#S990h05     KLIST
     C                   KFLD                    S_GHEDOC
     C                   KFLD                    S5_QUAL
      *
     c                   eval      callclrts = ' '
     c                   eval      RTSSPID = *blanks
     C                   EVAL      PUSTOPS_M = ' '
     c                   eval      RATED = ' '
     C     PMMBOX        CHAIN(n)  R204#H01
     c                   if        THSTOP > 2
     c                                and THTPID = 'DSW204LTL'
      *  TRY IF CAN MERGE PU STOPS
     C                   EXSR      $DSWLTL
     C                   IF        PUSTOPS_M = 'Y'
     C                   ELSE
      * cannot have more than 2 stops fr LTL -- 1 PU and 1 delivery
     C     PMMBOX        CHAIN     R204#H01
     c                   eval      thstat = 'C'
     c                   eval      thproc = 'X'
     c                   update    I204H01
     c                   goto      #EXITPGM
     C                   ENDIF                                                  IF PUSTOPS_M.
     c                   endif
      *
      * Get P/U   Number
     C                   if        PMPUNUM = *blanks
     C                   EXSR      $GetNumbers
     C                   EVAL      FLG      =  '1'
     C                   else
     C                   EVAL      WkPu#    =  %int(PMPUNUM)
     C                   EVAL      FLG      =  '0'
     C                   endif
     C
      * ----------------------------------------------------------------------
      * Map to PUHD
      * ----------------------------------------------------------------------
     C     PMMBOX        CHAIN(n)  R204#H01
     C                   IF        %FOUND (R204#H01) AND FLG = '1'
      **
     c                   exsr      $hdrdet                                      write puhd,pudt
     c                   IF        THTPID <> 'TJM204LTL'                         (T1)
     c                               and
     c                             THTPID <> 'WIN204LTL'                         (T1)
     c                   exsr      $po                                          write pupo
     c                   ENDIF
      * Get PRO# that may have been generated for the 990
      *  and update pick up number into the BLPO file
     C                   EVAL      WKQUAL   = 'CN'
     C     K#H05         CHAIN     R204#H05
     C                   IF        %FOUND(R204#H05)
     C                   MONITOR
     C                   EVAL      PRO##    = %INT(THREFID)
     C                   ON-ERROR  *ALL
     C                   EVAL      PRO##    = 0
     C                   ENDMON
     C                   ELSE
     C                   EVAL      PRO##    = 0
TTTT C                   ENDIF
      *
     c     proqual       klist
     c                   kfld                    pro##
     c                   kfld                    qual              1
      * Update BLHD w/ pick up number if record has been populated
      *
     C                   IF        PRO##   > 0
     c                   eval      qual='N'
      *
     C     PRO##         CHAIN     BLHD01
     C                   If        %FOUND(BLHD01)
      *********
     C                   IF        SHCODH = *BLANKS
     C                   EVAL      SHCODH =  %trim(TH_OLOCT)
     C                   ENDIF
     C                   IF        COCODH = *BLANKS
     C                   EVAL      COCODH   =  %trim(TH_DLOCT)
     C                   ENDIF
     C                   UPDATE    BLHDFM
      *
     C     PROQUAL       CHAIN     BLPO04
     C                   IF        %FOUND(BLPO04)
      *****
     C                   IF        PONUMO = *BLANKS
     C                   movel     punum1        ponumo
     c                   update    blpo4
     C                   ENDIF                                                  if ponumo =
      *****
     c                   ELSE                                                   N qual not found
      *****
     C     PRO##         SETGT     BLPO01
     C     PRO##         READPE(N) BLPO01
     c                   if        %eof(blpo01)
     c                   eval      seq#=1
     c                   else
     c                   eval      seq#= seqnoo + 1
     c                   endif
     c                   clear                   blpofm
     c                   eval      pro##o=pro##
     c                   eval      seqnoo = seq#
     C                   movel     punum1        ponumo
     c                   eval      qualfo='N'
     c                   write     blpofm
      *****
     C                   ENDIF                                                  BLPO04
      *********
     C                   Endif                                                  BLHD01
      *
     C                   ENDIF                                                  if pro# <> 0
      **
     C                   ENDIF                                                  if not eof H01
TTTT  *
TTTT  * ----------------------------------------------------------------------
TTTT  *
TTTT c     #EXITPGM      TAG
      *=====================================================================================
     c                   IF        thtpid = 'RTS204NLRT'                        G99
     c                                  and
     c                             callclrts = 'Y'
     c                   eval      ref# = *blanks
     c                   eval      ref# = %editc (punum1:'X')
     c                   call      'CLRTS204WN'
     c                   parm      'W'           coast             1
     c                   parm                    RTSSPID                        15
     c                   parm                    ref#             12
     c                   parm      'P'           ordertype         1
     c                   parm      'A'           status            1
     c                   parm      PMMBOX        wkmbox           10
      *
      * and take out flag from edi 990 and put PU#    in it
      *    and send out the edi 990 NOW
     c                   eval      S5_QUAL = 'CN'
     c     K#S990L2      setgt     S990#h01L2
     c     K#S990L2      readpe    S990#h01L2
     c                   if        NOT %EOF(S990#H01L2)                         (GP190926)
     c                               and
     c                             S_GHFLAG = 'H'
     c                   eval      S_GHFLAG = ' '
     c                   eval      S_GHCRTUSR = 'P55#204PU'
     c                   eval      S_GHCRTDTE = %DATE()
     C                   TIME                    S_GHCRTTME
     c*** do after s990#H05                   UPDATE    I990H01
     c     K#S990H05     chain     S990#h05
     c                   If        NOT %found(S990#H05)                          (B)
     c                   eval      S5_GHEDOC  = S_GHEDOC
     c                   eval      S5_GHTP990 = S_GHTP990
     c                   eval      S5_GHREFID = ref#                            PU#
     c                   eval      S5_GH_QU_REF= 'CN'
     c                   eval      S5_GHREFDES = *blanks
     c                   select
     c                   when      S_ghtp990 = 'RTS990NLRT'
     c                   eval      S5_GHREFDES = 'NLRT'
     c                   when      S_ghtp990 = 'RTS990NRSH'                     will never be this
     c                   eval      S5_GHREFDES = 'NRSH'
     c                   endsl
     C                   EVAL      S5_GHMBOX  = S_GHMBOX
     C                   EVAL      S5_GHTP204 = S_GHTP204
     C                   EVAL      S5_GHVRID  = S_GHVRID
     c                   eval      S5_GHCRTUSR = 'P55#204PU'
     c                   eval      S5_GHCRTDTE = %DATE()
     C                   TIME                    S5_GHCRTTME
     c                   WRITE     I990H05
     c                   Else                                                    ELSE for (B)
     c                   eval      S5_GH_QU_REF = 'CN'
     c                   eval      S5_GHREFID = ref#                            PU#
     c                   eval      S5_GHCRTUSR = 'P55#204PU'
     c                   eval      S5_GHCRTDTE = %DATE()
     C                   TIME                    S5_GHCRTTME
     C                   UPDATE    I990H05
     c                   Endif                                                  ENDIF for (B)
     c                   UPDATE    I990H01
      ** below done only for call to #PM990ADD for this acct which is under
      **     DISTRIBUTION LIST in edi setup (see pgm P56#990V03 too)
     c                   IF        S_GHTP990 = 'RTS990NLRT'
     c                   eval      S_GHTP990 = 'RTS990NLRT_D'
     c                   ENDIF
     C                   CALLB     '#PM990ADD'
     C                   PARM                    S_GHTP990
     C                   PARM                    S_GHEDOC
     c                   endif                                                  ENDIF (GP190926)
      *
     c                   ENDIF                                                  ENDIF for IF NLRT
      *=====================================================================================
      *
     c                   if        RATED = 'Y'
     c**** CALL MIKE pgm to rate
     c                   call      'LD010Z'
     c                   parm                    wkPu#
     c                   endif
TTTT  **
TTTT C                   EVAL      *INLR = *ON
      ************************************************************************
     C     $DSWLTL       Begsr
      ************************************************************************
      *
      * LTL PU can obnly have 1 PU STOP
      *   DSW can send seveaarl PU stops but if they are all same city and state
      *    merge it into 1 stop
     C     K#D30         KLIST
     C                   KFLD                    THMBOX
     C                   KFLD                    TDSTPSQ
      *
     c                   eval      PUSTOPS_# = 0
     c                   eval      PUSTOPS_# = 0
     c                   eval      PRV_CITY   = *BLANKS
     c                   eval      PRV_STATE = *BLANKS
     c                   eval      PU1_TDSTPSQ = 0
      *
      *    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     C     THMBOX        SETLL     R204#D01
     c                   dou       %EOF(R204#D01)
     C     THMBOX        reade     R204#D01
     c                   if        not %EOF(R204#D01)                           D01
     C                   eval      STPCDE = TDSTPCD
      ** ----------------------------
     C                   if        #SPSTPTYP(STPCDE) = 'P'
     C     k#d30         chain     R204#D30
      *//////
     c                   if        %FOUND(R204#D30)
      *
     c                   IF        TDSTPSQ > 1
     c                                AND
     c                             PRV_CITY = TDN401                             city
     c                                AND
     c                             PRV_STATE = TDN402                            state
      * will be treated as a MERGED stop with PU Stop 1
     c                   ELSE
     c                   eval      PUSTOPS_#  += 1
     c                   ENDIF
     c                   eval      PRV_CITY   = TDN401                           CITY
     c                   eval      PRV_STATE  = TDN402                           STATE
      *
     c                   endif                                                  IF %FOUND(R204#D30)
      *//////
     c                   endif                                                  #SPSTPTYP(STPCDE)= P
      ** ----------------------------
     c                   endif                                                  if for D01
     c                   enddo                                                  enddo for D01
      *    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *
     c                   if        pustops_# > 1
     c                   eval      PUSTOPS_M = ' '
     c                   else
     c                   eval      PUSTOPS_M = 'Y'
     c                   endif
      *
      * EXTRA PU STOPS will be merged with stop 1
      *   TD_N1 of 2nd PU stop will show TD_N1 = 1
      *    and
      *   TD_N1 of 3rd PU stop will show TD_N1 = 1
      *     etc
      ***--------------------------
     c                   if        PUSTOPS_M = 'Y'
      ***  THIS PART ALREADY DONE IN P55#204INL -- that is why no UPDATE to D30 file here
     C     THMBOX        SETLL     R204#D30
     c                   doU       %eof(R204#D30)
     C     THMBOX        READE     R204#D30
     C                   IF        NOT %EOF(R204#D30)
     c                                and
     C                             #SPSTPTYP(STPCDE) = 'P'
      * for fistt PU Stop will get TD_N1 = '002'
     C                   if        tdstpsq = 1
     c                   eval      PU1_TDSTPSQ = TDSTPSQ
     C                   EVAL      TD_N1 = '002'
     c                   endif
      * all other PU stops will get TD_N1 = '001'
     c                   IF        tdstpsq <> 1
     C                   EVAL      TD_N1 = %EDITC(PU1_TDSTPSQ:'X')
     c                   endif
      ***
     C                   ENDIF
     C                   ENDDO
     c                   endif                                                  if PUSTOPS_M = Y
      ***--------------------------
      *
     C                   Endsr
      ************************************************************************
     C     $GetNumbers   Begsr
      ************************************************************************
     C                   call      'UT306U'
     C                   parm      'GET '        p_ut306u_1
     c                   parm      'PU#  '       p_ut306u_2        5
     C                   parm      0             p_ut306u_3
      *
     C                   EVAL      WkPU#    =  p_ut306u_3
     C                   EVAL      PMPUNUM  =  %EDITC(WkPU#:'X')
      *
     C                   Endsr
TTTT  **
TTTT  ************************************************************************
TTTT C     $HDRDET       Begsr
TTTT  ************************************************************************
      *
     c                   clear                   puhdfm
     c                   clear                   pudtfm
     c                   eval      L11MCI_REF = *BLANKS
     c                   clear                   CONS_ARR
     c                   eval      cons_ARR_CTR=0
     C                   EVAL      PUHD_ADDED = ' '
     C                   EVAL      ACREC1   =  *Blanks
     C                   EVAL      PUNUM1   =  WkPu#
     C                   EVAL      vend#1   =  th_oloct
     C     THMBOX        SETLL     R204#D35
     C     THMBOX        READE     R204#D35
     C                   IF        NOT %EOF(R204#D35)
     c                   eval      cname1=tdconnam
     c                   ENDIF
      *
      *-------ROUTE1
     c                   eval      route1=*blanks
     C     vend#1        chain     loct14                             92
      *    get terminal and zip
     C                   IF        not*IN92
     C                   eval      term#1 = term#8
     c                   eval      state1 = statv8
     c                   eval      zipcd1 = zipcv8
     c                   if        cname1 = *blanks
     C                   eval      cname1 = cname8
     c                   endif
      *
     c                   if        exten8 > *all'0'
     c                   movel(p)  exten8        cextn1
     c                   endif
      *
      * !!! NEW way to get the term#
      *         for MACY204 LTL
      *       if dest is CA - -term# is 75
      *       if dest is NJ and --
      *               . stop 1 is NJ, NY, CT -- term# is 66
      *               . stop 1 is MA, NH, VT, ME, RI  then term# is 22
      *               . term# 22 closing -- change to 31
      *               . for all other stop 1 states, term# is 31
      *       if dest is not NJ and not CA ,  term# is 31
      *
     c                   IF        thtpid = 'MACY204'
     c                   EXSR      $MACY_TERM#
     c                   ENDIF                                                  if thtpid = MACY204
      *
     c                   IF        thtpid = 'STM204LTL'
     c                   EXSR      $STM_TERM#
     c                   ENDIF                                                  if thtpid = MACY204
      *
     c                   IF        thtpid = 'RTS204NLRT'
     C                                     AND
     c                             thdsts = 'CA'                                dest is CA
     c                   eval      term#1='77  '
     c                   ENDIF                                                  if thtpid = RTS204NL
      *
     C     terzip        KLIST
     C                   KFLD                    term#1
     C                   KFLD                    zipcv8
      *
     c                   if        term#1 <> '30  '  and term#1 <> '31  '
     C     terzip        chain     zips01                             93
      *
     C                   IF        not*IN93
     C                   eval      route1 = route5
     C                   ENDIF
      *
     c                   else
     c                   eval      route1=state1
     c                   endif                                                  if term#8 <> 30 ...
      *
     C                   ENDIF
      *-------END ROUTE1
      *
      * call date = today
     c                   move      udate         datISO
     C     *cymd         move      DATISO        caldt1
     C                   time                    timis             6 0
     C                   movel     timis         caltm1
      *
      * AVL DATE AND TIME FROM R204#D10 for stop# 1
      *  the first G62 comes in with this date and time with date
      *   qaul 68  for stop#1   for avl date
     C     THMBOX        SETLL     R204#D10
     C     THMBOX        READE     R204#D10
     C                   IF        NOT %EOF(R204#D10)
     C                   EVAL      datiso   = %DATE(TDDATE)
     C     *cymd         move      datiso        avldt1
     C                   IF        TDTIME  <> *BLANKS
     C                   EVAL      AVLTM1   = %INT(%SUBST(TDTIME:1:4))
     C                   EVAL      AVLAP1='A'
     c                   if        avltm1 > 1159
     c                   sub       1200          avltm1
     C                   EVAL      AVLAP1='P'
     c                   endif
     c                   if        avltm1 = 0
     C                   EVAL      AVLTM1   = 1
     c                   eval      avlap1='A'
     c                   endif
     C                   ELSE
     C                   EVAL      AVLTM1   = 1
     c                   eval      avlap1='A'
     C                   ENDIF                                                  if tdtime <> ' '
     C                   ENDIF                                                  not eof #d10
      *
     c                   eval      topal1 = 0
     C     THMBOX        chain     R204#H15
     c                   if        not %eof(r204#h15)
     c                   eval      topal1 = thplqty
     c                   endif
     c                   eval      stat11=*blanks
     c                   eval      toamt1=0
     c                   eval      run##1=0
     c                   clear                   pudat1
      *
      * get cons#2 from bill-to acct setup in XTB_LINKS
      *
     c                   eval      L11MCI_REF = *BLANKS
     C                   EVAL      WKQUAL   = 'MCI'
     C     K#H05         CHAIN     R204#H05
     C                   IF        %FOUND(R204#H05)
     c                   eval      L11MCI_REF   = THREFID
     C                   ENDIF
      *
      * ----------------------------------  set up of PUDT consignee for all LTL cust ------------
      *                                EXCEPT for TJM204LTL whihc is a TJX LTL
      *                                               and  WInners one (which some come
      *                                                 TJX too and reset from Winners)
      *
     c                   IF        THTPID <> 'TJM204LTL'
     c                                and
     c                             THTPID <> 'WIN204LTL'
      **  this is a NOT Winners LTL coming from TJX or Winners and it is NOT LTL from TJX
      * If Bill-To Acct# cannot be retrieved in XTB_LINKS, use Default BillTo in TPID
     C                   IF        #SPBILLTOL(THTPID:TH_ON104:TH_DN104:         B01
     C                                       THOSTS:THDSTS) <> 0
     C                   EVAL      cons#2   =
     C                                #SPBILLTOL(THTPID:TH_ON104:TH_DN104:
     C                                          THOSTS:THDSTS)
     C                   ELSE                                                   Else of B01
     C                   EVAL      cons#2   = #SPDFTBILL(THTPID)
     c                   Select
     c                   When      thtpid = 'MACY204'
      **** !!!! MACYS  LTL  !!!!!
     c                   eval      cons#2 = 10007
     c                   if        th_dn104 = 'CP'
     c                   eval      cons#2 = 10805
     c                   endif
     c                   When      thtpid = 'STM204LTL'
      **** !!!! STEINMART LTL  !!!!
     c                   eval      cons#2 = 75001
     c                   if        THDSTS   = 'NJ'
     c                   eval      cons#2 = 60809
     c                   endif
     c                   When      thtpid = 'WIN204LTL'
      **** !!!! WINNERS LTL from TJX !!!!!!
      ***********************
      * this load is a Winners LTL     (since we getting some 204 LTL that are  Winners
      *    from TJX still but the MCI qualifier for that is not NRCS-L AND not NRC-C  )
      *  We will stop getting WInners EDI204 LTL from TJMAX eventually -- most now are coming
      *      from Winners via APLL Logisitics via AS2
      ** we may get 2 diff codes in the OID01 which will need 2 diff cons# in PUDT(for Winners)
      ** so multiple PUDT records
     c                   eval      cons#2 = 35900
     c                   if        THDSTS   = 'CA'
     c                   eval      cons#2 = 65600
     c                   endif
      ***********************
     c                   Other
     c                   Endsl
     C                   ENDIF                                                  Endif B01
     c                   ENDIF                                                  IF not TJM204LTL...'
      * ----------------------------------------------------------------------
     c                   eval      hotsh1=*blanks
     c                   eval      saidt1=*blanks
     c                   eval      sourc1='EDI'
     c                   eval      remar1=*blanks
     c                   eval      %subst(remar1:1:8)='Trip ID:'
     c                   eval      %subst(remar1:9:30)  = thspid                   Trip Id
     C                   EVAL      WKQUAL   = 'BB'                              Auth#
     C     K#H05         CHAIN     R204#H05
     C                   IF        %FOUND(R204#H05)
     c                   eval      %subst(remar1:25:6)='Auth#:'
     c                   eval      %subst(remar1:32)  = threfid                 Auth#
     c                   ENDIF
     c                   exsr      $appt
     c                   eval      vend31=*blanks
     c                   eval      putim1=0
     c                   eval      appdt1=0
     c                   eval      apptm1=0
     c                   eval      appfl1=*blanks
     c                   eval      stop#1='10'
     c                   eval      asntm1=0
      *
      * ---- detail file
      * Prepare PUDT info
      * goh,ctn... from R204#D01 for stop # 1
      * work with detail file p/u stops to build PUDT and some flds for PUHD
      * ------------------------
      *
     c                   eval      totpallets = 0
     C     THMBOX        SETLL     R204#D01
     c                   dow       not %eof(R204#D01)
     C     THMBOX        READE     R204#D01
     C                   IF        NOT %EOF(R204#D01)
     c                              and (tdstpcd='CL' or tdstpcd='PL'  or
     c                                   tdstpcd='LD')
     c                   eval      toshp1 = toshp1 + 1
      *
     c                   eval      wkstpsq=tdstpsq
      *
     c                   if        topal1 = 0
     c                   eval      wkstpsq=tdstpsq
     c     k#stop        chain     R204#D15
     C                   IF        %FOUND(R204#D15)
     c                   eval      totpallets = totpallets + d15TDPLQTY
     c                   ENDIF
     c                   endif
      *
     c                   SELECT
     c                   WHEN      TDSQTY > 0
     c                   select
     c                   when      tdquom='CT'
     c                                or tdquom = 'PC'
     c                   eval      toctn1=tdsqty + toctn1
     c                   endsl
     c                   if        TDWGT   < 100000
     c                   eval      towgt1=tdwgt + towgt1
     c                   eval      tocub1=tdvol + tocub1
     c                   endif
      **
     c                   WHEN      TDSQTY = 0
     c                   eval      wkstpsq=tdstpsq
     c     k#stop        chain     R204#D15
     C                   IF        %FOUND(R204#D15)
     c                   eval      toctn1=d15tdltqty2 + toctn1
     c                   if        d15TDLTWGT < 100000
     c                   eval      towgt1=d15tdltwgt + towgt1
     c                   endif
     c                   eval      tocub1=d15tdltvol + tocub1
     c                   ENDIF
      **
     c                   ENDSL
      *
     c                   if           TDTPID = 'DSW204LTL'
     c                   if        wkstpsq = 1                                  1ST PU STOP
     c                   eval      TDNOTES = *blanks
     c     k#stop        chain     R204#d25
     c                   MOVEL     TDNOTES       REMAR1
     c                   endif                                                  wkstpsq = 1
     c                   endif
      * stat21 is to be building code
     c                   eval      stat21=*blanks
     c                   eval      consn2=*blanks
     c     cons#2        chain     cons01
     c                   if        not %eof(cons01)
     c                   eval      consn2=consn9
     c                   if        stat21=*blanks
     c                   eval      stat21=BUILD9
     c                   endif
     c                   endif
      *
     c                   eval      acrec2=' '
     c                   eval      punum2=punum1
     c                   eval      lin##2=1
      * consignee code is in fld TH_DLOCT
      *  if it is N bergen building --
      *  if it is Compton  building --
     c***???             eval cons#2=  already set above
     c***???             eval consn2=  alaready set above
      * and wgt,qty,... from R204#D01
     c                   eval      ppdco2='C'
     c                   select
     c                   when      L11MCI_REF = 'LTL_V_ADZ '
     c                   eval      ppdco2='C'
     c                   when      L11MCI_REF = 'LTL_V_ADV_CZ '
     c                   eval      ppdco2='P'
     c                   eval      rated = 'Y'
     c                   when      L11MCI_REF = 'LTL_V_ADV_CZE'
     c                   eval      ppdco2='C'
     c                   when      thmeth='CC'
     c                   eval      ppdco2='C'
     c                   when      thmeth='PP'
     c                   eval      ppdco2='P'
     c                   when      thmeth='TP'                                  Third Party
     c                   eval      ppdco2='T'
     c                   when      thmeth='DE'                                  per contract
     c                   eval      ppdco2='D'
     c                   endsl
     C                   EVAL      weigh2   = towgt1
     C                   EVAL      ctnqt2   = toctn1
     C                   EVAL      gohqt2   = togoh1
     C                   EVAL      cubft2   = tocub1
     c                   eval      class2='100'
      *
      *
     c                   If        THTPID =  'DSW204LTL'                         (DSWL)
     c                   eval      wkstpsq=tdstpsq                               stop 1
     c     k#stop        chain     R204#D30
     c                   if        %found(R204#d30)
     c                               and TD_N1 = '002'                           1st stop will have
     c                   exsr      $ACCUM_PUSTOPS
     c                   endif
     c                   Endif                                                   (DSWL)
      *
     c                   If        THTPID <> 'TJM204LTL'                         (T1)
     c                               and
     c                             THTPID <> 'WIN204LTL'                         (T1)
     C                   Write     PUDTFM
     c                   ELSE
      *                   ------FOR TJX LTL from TJX --------------
      *                         OR  a WINNERS LTL (some of these come from TJX still
      *                        (and most comes from Winners via APLL)
     c                   EXSR      $D01_STOP_PO
     c                   Endif                                                    END (T1)
      *
     c                   ENDIF                                                  not eof r204#d01
     c                   enddo                                                  dow not eof r204#d01
      *
      * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      * Check if appointment required
      * MACYS LTL will not require appt -- so this part of logic
      * (Still kept here for other clients)
      * will not give any result
      * DISCUSS with DANIEL
      * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      * Check if appointment required
     C                   EVAL      APPFL1  = ' '
     C                   EVAL      WKTP214 = #SPTP214(THTPID)
      * if no records for 214 statuses found in EDCTL
      *   means no 214 required to be sent back
     C                   EVAL      PMNAME  = 'EDI214STATUS'
     C                   EVAL      PMKEY1 = WKTP214
     C     K#EDCTL       SETLL     F55#EDCTL
     C     K#EDCTL       READE     F55#EDCTL
     C                   DOW       NOT %EOF(F55#EDCTL)
      *
     C                   IF        %SUBST(IEDESC:78:2) = 'H1' or
     C                             %SUBST(IEDESC:78:2) = 'H2' or
     C                             %SUBST(IEDESC:78:2) = 'H3' or
     C                             %SUBST(IEDESC:78:2) = 'H4'
      * If Assigned Date/Time Pair is H1, i.e. Avaiable Date/Time in PUHD,
      *  set Appointment Flag ON
     C                   IF        %SUBST(IEDESC:78:2) = 'H1'
     C                             or %SUBST(IEDESC:78:2) = 'H3'
     C                   IF        %SUBST(IEDESC:80:1) = 'A'
     c                              or
     C                             %SUBST(IEDESC:80:1) = 'S'                    est appt date/time
     C                   EVAL      APPFL1  = 'E'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     K#EDCTL       READE     F55#EDCTL
     C                   ENDDO
      *
     c                   if        topal1 = 0 and totpallets > 0
     c                   if        totpallets <=  99
     c                   eval      topal1 = totpallets
     c                   else
     c                   eval      topal1 = 99
     c                   endif
     c                   endif
     c                   movel     thspid        refno1
     c                   eval      qualf1 = 'L'
      *
     c                   IF        (THTPID = 'TJM204LTL'
     c                                  and (L11MCI_REF =  'NRCS-L' OR
     c                                      L11MCI_REF =  'NRC-C'   ) )
     c                   eval      hotsh1 = 'Y'
     c                   eval      stat21 = 'M'
     c                   ENDIF
      *
     c                   IF        (THTPID = 'TJM204LTL'                        Winners LTL frm
     c                                  and (L11MCI_REF =  'NRCS-L' OR
     c                                      L11MCI_REF =  'NRC-C' OR
     c                                      L11MCI_REF =  'LTL_V_ADZ ' Or
     c                                      L11MCI_REF =  'LTL_V_ADV_CZ' Or
     c                                      L11MCI_REF =  'LTL_V_ADV_CZE'))
      * populate grp code = TJ and due date to latter date received at the PU stop
      *                                 in the EDI 204
     c                   eval      custc1 = 'TJ '
     c                   clear                   duedt1
     C     THMBOX        SETLL     R204#D10
     C     THMBOX        READE     R204#D10                                     1st G62 date
     C                   IF        NOT %EOF(R204#D10)                           (G62A)
     c                   eval      duedt1 = %DATE(TDDATE :*ISO)
     C     THMBOX        READE     R204#D10                                     2nd G62 date if have
     c                   if        NOT%EOF(R204#D10)                            (G62B)
     c                   clear                   duedt1
     c                   eval      duedt1 = %DATE(TDDATE :*ISO)
     c                   endif                                                    endif (G62B)
     c                   ENDIF                                                    ENDIF (G62A)
      *
     c                   ENDIF                                                    TJM204LTL
      *
      *
     c                   if        thtpid = 'DSW204LTL'
     c                   eval      custc1 = 'DSW'
     c                   endif
      *
     C                   Write     PUHDFM
     C                   EVAL      PUHD_ADDED = 'Y'
      *-------==================================================================
      *    WRITE TO RTSF records
     c                   IF        thtpid = 'RTS204NLRT'
     c                   eval      callclrts = 'Y'
     c                   eval      RTSSPID = THSPID
     c                   ENDIF
      *-------==================================================================
      * for TJM204LTL tpid (Winners LTL) write info to WNSCWF ile
     c                   IF        (THTPID = 'TJM204LTL') OR                    Winners LTL frm
     c                             (THTPID = 'WIN204LTL')                       Winners LTL frm
      *                                                                           WInners
     c                   CLEAR                   WNSCFM
     c                   eval      pup#wn = punum1
     c                   WRITE     WNSCFM
     c                   Endif
      *
     c                   monitor
     c                   call      'OL677U'
     c                   parm                    punum1
     c                   on-error
     c                   endmon
      *
     C                   Endsr
      ************************************************************************
     C     $ACCUM_PUSTOPSBegsr
      ************************************************************************
      *   FOR DSW204LTl   ONLY
      *
     c                   dow       not %eof(R204#D01)
     C     THMBOX        READE     R204#D01
     C                   IF        NOT %EOF(R204#D01)
     c                              and (tdstpcd='CL' or tdstpcd='PL'  or
     c                                   tdstpcd='LD')
     c                   eval      toshp1 = toshp1 + 1
      *
     c                   eval      wkstpsq=tdstpsq
      *
     c                   eval      wkstpsq=tdstpsq
     c     k#stop        chain     R204#D15
     C                   IF        %FOUND(R204#D15)
     c                   eval      totpallets = totpallets + d15TDPLQTY
     c                   ENDIF
      *
     c                   SELECT
     c                   WHEN      TDSQTY > 0
     c                   select
     c                   when      tdquom='CT'
     c                                or tdquom = 'PC'
     c                   eval      toctn1=tdsqty + toctn1
     c                   endsl
     c                   if        TDWGT   < 100000
     c                   eval      towgt1=tdwgt + towgt1
     c                   eval      tocub1=tdvol + tocub1
     c                   endif
      **
     c                   WHEN      TDSQTY = 0
     c                   eval      wkstpsq=tdstpsq
     c     k#stop        chain     R204#D15
     C                   IF        %FOUND(R204#D15)
     c                   eval      toctn1=d15tdltqty2 + toctn1
     c                   if        d15TDLTWGT < 100000
     c                   eval      towgt1=d15tdltwgt + towgt1
     c                   endif
     c                   eval      tocub1=d15tdltvol + tocub1
     c                   ENDIF
      **
     c                   ENDSL
      *
     c                   if           TDTPID = 'DSW204LTL'
     c                   if        wkstpsq = 1                                  1ST PU STOP
     c                   eval      TDNOTES = *blanks
     c     k#stop        chain     R204#d25
     c                   MOVEL     TDNOTES       REMAR1
     c                   endif                                                  wkstpsq = 1
     c                   endif
      *
     C                   EVAL      weigh2   = towgt1
     C                   EVAL      ctnqt2   = toctn1
     C                   EVAL      gohqt2   = togoh1
     C                   EVAL      cubft2   = tocub1
     c                   eval      class2='100'
      *
     c                   ENDIF                                               if not eof R204#H01
     C     THMBOX        READE     R204#D01
     c                   enddo                                               dow  not %eof(R204#D01)
      *
     c                   Endsr
      ************************************************************************
     C     $D01_STOP_PO  Begsr
      ************************************************************************
      *   special processing for PUDT and PUPO for a stop
      *     A stop may have several PO -- need to check if all will
      *      end up with 1 consignee (1 PUDT line)  OR
      *      end up with  more that 1 consignee (multiple PUDT lines)
      *        depending on the PO div and 1st 2 pos of each PO# for this stop
      *
      *    Build the CONS_ARR array -- each entry will be
      *              PO# (from the R204#l01), Consignee#, Div of the PO (from R204#L01)
      *                 qty, wgt, VOl of the PO
      *
     c                   CLEAR                   DIVPO12
     c                   clear                   CONS_ARR
     c                   eval      cons_ARR_CTR=0
      *
      * go thru the TJM , WInners LTL
      *
     C     Kd05          SETLL     R204#L01                               96
     c                   if        *in96 = *off
      * NO PO at all for the D01 stop
     c                   exsr      $TJMAX_DEFAULT
     c                   WRITE     PUDTFM
     c                   LEAVESR
     c                   endif
      *
      * HAVE PO for D01 stop
     c                   dow       not %eof(R204#L01)
     C     Kd05          READE     R204#L01
     C                   IF        NOT %EOF(R204#L01)
      **
     c                   eval      div = %TRIM(L01TLREFID)                      ?? will it be len 4
     C                   EVAL      PO_12= %subst(L01TLPONUM:1:2)
     C                   Z-ADD     1             I                 2 0
      *
     C                   SELECT
     C                   WHEN      THDSTS = 'CA'
     C     DIVPO12       LOOKUP    DIVPO2_TCA(I)                          91
     C     *IN91         IFEQ      '0'                                          NOT FOUND
     c                   exsr      $TJMAX_DEFAULT
     c                   ELSE
     c                   eval      cons#_PO = cons#_TCA(I)
     C                   ENDIF
      *
     C                   WHEN      THDSTS = 'NJ'
     C     DIVPO12       LOOKUP    DIVPO2_TNJ(I)                          91
     C     *IN91         IFEQ      '0'                                          NOT FOUND
     c                   exsr      $TJMAX_DEFAULT
     c                   ELSE
     c                   eval      cons#_PO = cons#_TNJ(I)
     C                   ENDIF
      *
     C                   OTHER
     c                   exsr      $TJMAX_DEFAULT
      *
     c                   ENDSL
      *
     c                   eval      PO# = %subst(%triml(L01TLPONUM):1:15)
     c                   eval      div_PO  = DIV
     c                   eval      QTY_PO  = 0
     c                   if        L01TLPOQTY <= 99999
     c                   eval      QTY_PO = L01TLPOQTY
     c                   endif
     c                   eval      WGT_PO  = 0
     c                   if        L01TLPOWGT <= 99999
     c                   eval      WGT_PO = L01TLPOWGT
     c                   endif
     c                   eval      VOL_PO  = 0
     c                   eval(h)   vol7_n=L01tlpovol
     c                   if        vol7_n <= 99999
     c                   move      vol7_n        VOL_PO
     c                   endif
     c                   eval      cons_ARR_CTR += 1
     c                   eval      cons_ARR(cons_ARR_ctr) = CONS#_PO_DS
      **
     c                   CLEAR                   DIVPO12
     c                   ENDIF
     c                   enddo
      *
     c                   CLEAR                   DIVPO12
      *
     c                   if        cons_ARR_CTR <> 0
     c                   SORTA     CONS_ARR
     c                   exsr      $TJWNLTL
     c                   endif
     c
      *
     C                   Endsr
      ************************************************************************
     C     $TJMAX_DEFAULTBegsr
      ************************************************************************
     c                   select
     c                   when      L11MCI_REF  = 'NARC-L'                       TJ LTL frm TJ
     c                                 or
     c                             L11MCI_REF  = 'NARC-LCSA'                    TJ LTL frm TJ
     c                   eval      cons#_PO = 36605
     c                   other
     c                   eval      cons#_PO = 99999                               Winners LTL
     c                   endsl
      *
     c                   ENDSR
      ************************************************************************
     C     $TJWNLTL      Begsr
      ************************************************************************
      *     Build the PUDT and PUPO
      *
     c                   clear                   prv_cons#
     c                   eval      ctr = 0
     c                   eval      PUDT_LIN# = 0
      *
     c                   clear                   SameConsPO_ARR
     c                   eval      sameConsPO_ct = 0
      *
     c                   eval      ctr = max_arr_size - cons_arr_ctr
      *
     c                   dou       ctr = max_arr_size
     c                   eval      ctr += 1
     c                   move      cons_ARR(ctr) CONS#_PO_DS
     c                   IF        cons#_PO = prv_CONS#
     c                                and prv_CONS# <> 0
     c                   eval      cons_qty += qty_PO
     c                   eval      cons_wgt += wgt_PO
     c                   eval      cons_vol += vol_PO
      * build the POs for the same consignee into one array --- SameConsPO_ARR
     c                   CLEAR                   SameConsPO_DS
     c                   eval      PO#C = PO#
     c                   eval      CONS#_PO#C = CONS#_PO
     c                   eval      DIV_PO#C   = DIV_PO
     c                   eval      SameConsPo_CT += 1
     c                   eval      SameConsPO_ARR(SameConsPo_ct) = SameConsPo_DS
     c                   ITER
      *
     c                   ELSE
      *
     c                   if        prv_cons# <> 0                               cons#_PO<>prv_cons#
     c                   eval      pudt_lin# += 1
     c                   EVAL      lin##2   = pudt_lin#
     c                   EVAL      cons#2   = prv_CONS#
     c                   eval      consn2=*blanks
     c     cons#2        chain     cons01
     c                   if        not %eof(cons01)
     c                   eval      consn2=consn9
     c                   if        stat21=*blanks
     c                   eval      stat21=BUILD9
     c                   endif
     c                   endif
     C                   EVAL      weigh2   = cons_wgt
     C                   EVAL      ctnqt2   = cons_qty
     C                   EVAL      gohqt2   = 0
     C                   EVAL      cubft2   = cons_vol
     c                   WRITE     PUDTFM
           c                   endif
     c                   eval      prv_cons# = cons#_po                         start for new cons#
     c                   eval      prv_po#   = PO#
     c                   eval      prv_div   = DIV_PO
     c                   eval      cons_qty = qty_PO
     c                   eval      cons_wgt = wgt_PO
     c                   eval      cons_vol = vol_PO
      * build the POs for the same consignee into one array --- SameConsPO_ARR
     c                   CLEAR                   SameConsPO_DS
     c                   eval      SameConsPo_CT = 0
     c                   eval      PO#C = PO#
     c                   eval      CONS#_PO#C = CONS#_PO
     c                   eval      DIV_PO#C   = DIV_PO
     c                   eval      sameConsPO_CT += 1
     c                   eval      SameConsPO_ARR(sameConsPO_ct) = SameConsPO_DS
      *
     c                   ENDIF
     c                   enddo                                                  dou ctr =
      *
     c                   if        prv_cons# <> 0                               cons#_PO<>prv_cons#
     c                   eval      pudt_lin# += 1
     c                   EVAL      lin##2   = pudt_lin#
     c                   EVAL      cons#2   = prv_CONS#
     c                   eval      consn2=*blanks
     c     cons#2        chain     cons01
     c                   if        not %eof(cons01)
     c                   eval      consn2=consn9
     c                   if        stat21=*blanks
     c                   eval      stat21=BUILD9
     c                   endif
     C                   Else
     c                   eval      consn2=*blanks
     c                   eval      stat21=*blanks
     c                   endif
     C                   EVAL      weigh2   = cons_wgt
     C                   EVAL      ctnqt2   = cons_qty
     C                   EVAL      gohqt2   = 0
     C                   EVAL      cubft2   = cons_vol
     c                   WRITE     PUDTFM
     c                   endif
      *
     c                   Exsr      $TJWNLTL_PO
      *
     c                   ENDSR
      *
      ************************************************************************
     C     $TJWNLTL_PO   Begsr
      ************************************************************************
      *     Build the PUDT and PUPO
      *
      *
     c                   eval      seqno0 = 0
     c     PUNUM1        setgt     PUPO01
     c     PUNUM1        readpe    PUPO01
      *
     c     k#stop        setll     R204#L01
     c                   dou       %eof(R204#L01)
     c     k#stop        reade     R204#L01
     c                   if        not %EOF(R204#L01)
      *
     c                   eval      lin##0 = lin##2
     c                   eval      seqno0 = seqno0 + 1
     c                   eval      cons#0=cons#2
     c                   eval      ponum0= %trim(L01TLPONUM)
     c                   eval      ctnqt0  = 0
     c                   if        L01TLPOQTY <= 99999
     c                   eval      ctnqt0 = L01TLPOQTY
     c                   endif
     c                   eval      weigh0  = 0
     c                   if        L01TLPOWGT <= 99999
     c                   eval      weigh0 = L01TLPOWGT
     c                   endif
     c                   eval      cubft0  = 0
     c                   eval(h)   vol7_n=L01tlpovol
     c                   if        vol7_n <= 99999
     c                   move      vol7_n        cubft0
     c                   endif
     c                   write     pupofm
      **
     c                   endif
     c                   enddo
      *
     c                   ENDSR
      ************************************************************************
     C     $MACY_TERM#   Begsr
      ************************************************************************
      *  !!!!!!!!!!! ANY changes made to terminal assignment must
      *               be done in P55#204INI and p55#UPDPU
      ************************************************************************
      *         for MACY204 LTL
      *       if dest is CA - -term# is 75
      *       if dest is NJ and --
      *               . stop 1 is NJ, NY, CT -- term# is 66
      *               . stop 1 is MA, NH, VT, ME, RI  then term# is 22
      *               . term# 22 closing -- change to 31
      *               . for all other stop 1 states, term# is 31
      *       if dest is not NJ and not CA ,  term# is 31
      *
      *
     c                   SELECT
      * DEST is CALIFORNIA ---------------------
     c                   WHEN      thdsts = 'CA'                                dest is CA
     c                   eval      term#1='75  '
      *
      * DEST is NEW JERSEY ---------------------
     c                   WHEN      thdsts = 'NJ'                                dest is NJ
      **
     c                   Select                                                 stop 1 state
      **
     c                   When      thosts = 'NY'
     c                   eval      term#1  ='66  '
     c     th_oloct      chain     loct14
     C                   IF        %FOUND(LOCT14)
     c                   move      zipcv8        zip5_a
     c                   movel     zip5_a        zip3_a
     c                   movel     zip3_a        zip3_n
     c                   if        zip3_n >= 120 and zip3_n <= 149
     c                   eval      term#1  ='31  '
     c                   endif
     c                   ENDIF                                                  found(LOCT14)
     c
      **
     c                   When      thosts = 'NJ'
     c                               OR
     c                             thosts = 'CT'
     c                   eval      term#1='66  '
      **
     c                   When      thosts = 'MA'
     c                               OR
     c                             thosts = 'NH'
     c                               OR
     c                             thosts = 'VT'
     c                               OR
     c                             thosts = 'ME'
     c                               OR
     c                             thosts = 'RI'
     c                   eval      term#1='31  '
      **
      * Origin stop 1 not NJ , NY, CT,  MA, NH, VT, ME, RI
     c                   OTher
     c                   eval      term#1='31  '                                stop 1 origin state
      **
     c                   Endsl
      *
      * Dest is not NJ or CA ---------------------
     c                   OTHER                                                  DEST is not CA, NJ
     c                   eval      term#1='31  '
      *
     c                   ENDSL
      *
     C     PMMBOX        setll     R204#H01
     C     PMMBOX        CHAIN     R204#H01
     C                   IF        %FOUND (R204#H01) AND FLG = '1'
     c                   eval      threspon = term#1
     c                   update    I204H01
     c                   ENDIF
      *
     C                   Endsr
      ************************************************************************
     C     $STM_TERM#    Begsr
      ************************************************************************
      *
     c                   eval      term#1='66  '                                dest is NJ
      **
     c                   select
     c                   WHEN      thdsts = 'CA'                                dest is CA
     c                   eval      term#1='75  '
     c                   WHEN      thdsts = 'NJ'
     C                   if        thosts = 'MA' or
     c                             thosts  = 'RI' or
     c                             thosts  = 'VT' or
     c                             thosts  = 'NH' or
     c                             thosts  = 'ME'
     c                   eval      term#1='31  '
     c                   endif
     C                   if        thosts = 'WV'
     c                   eval      term#1='31  '
     c                   endif
      **
     c                   endsl
      *
     C                   Endsr
      ************************************************************************
     C     $APPT         Begsr
      ************************************************************************
      *
     C                   EVAL      APPT#=*BLANKS
      *
     c                   IF        thtpid = 'MACY204'
     C     thmbox        SETgt     R204#D05
     C     thmbox        readpe    R204#D05
     C                   IF        NOT %EOF(R204#D05)
     c                             and td_qu_ref = 'AO'
     c                   eval      %subst(remar1:52:6) = 'Appt#:'               appt#
     c                   eval      %subst(remar1:60:8) = tDrefid                appt#
     C                   EVAL      APPT#=%SUBST(TDREFID:1:8)
     c                   ENDIF
     c                   ENDIF
      *
      *
     C                   Endsr
      ************************************************************************
     C     $PO           Begsr
      ************************************************************************
      *      FOr all EDI parnters except TJM204LTL and WIN204LTL
     c     kd05          klist
     c                   kfld                    tdmbox
     c                   kfld                    tdstpsq
      * Map to PUPO
     c                   clear                   pupofm
      ** PO numbers will come in the L11 segment
     C                   EVAL      WKQUAL   = 'PO'
     c                   eval      seqpo=0
      *
     C                   if        THTPID = 'DSW204LTL'
     C     THMBOX        SETLL     R204#L01                               99
     c                   if        *in99=*ON
     c                   exsr      $PO_DTL
     c                   goto      #PO
     c                   endif
     c                   endif
      *
     C     K#H05         SETLL     R204#H05                               97
     c                   select
     c                   when      *in97=*on
     c                   exsr      $PO_HDR
     c                   when      *in97=*off
     c                   exsr      $PO_DTL
     c                   endsl
      *
     C     #PO           Endsr
      ************************************************************************
     C     $PO_DTL       Begsr
      ************************************************************************
      *   OID or L11
      *
     c                   eval      added_po = 'N'
      *
     C     THMBOX        SETLL     R204#L01                               99
      *
     c                   IF        *in99=*on
     c                   exsr      $po_dtl_L01                                  OID segemnt info
     c                   if        added_po = 'N'
     c                   exsr      $po_dtl_D05
     c                   endif
     c                   else
     c                   exsr      $po_dtl_D05                                  L11 detail info
     c                   ENDIF
      *
     C                   Endsr
      ************************************************************************
     C     $PO_DTL_L01   Begsr
      ************************************************************************
      *   R204#L01  (OID segment in the incoming EDI 204)
      *
     C     THMBOX        SETLL     R204#D01
     c                   DOW       not %eof(R204#D01)
     C     THMBOX        READE     R204#D01
     C                   IF        NOT %EOF(R204#D01)
     c                              and (tdstpcd='CL' or tdstpcd='PL'  or
     c                                   tdstpcd='LD')
      *
     C     Kd05          SETLL     R204#L01
     c                   dow       not %eof(R204#L01)
     C     Kd05          READE     R204#L01
     C                   IF        NOT %EOF(R204#L01)
      * only write the PO if it does not exist in the PUPO already for this p/u
      *   since MACYS EDI LTL 204 can give the same PO multiple times
      *
     c                   eval      ponum = L01TLPONUM
     c                   eval      ctnqt =0
     c                   eval      weigh =0
     c                   eval      cubft =0
     c                   eval      wgt7_n=0
     c                   eval      vol7_n=0
     c                   move      L01tlpoqty    ctnqt
     c                   eval(h)   wgt7_n=L01tlpowgt
     c                   if        wgt7_n <= 99999
     c                   move      wgt7_n        weigh
     c                   endif
     c                   eval(h)   vol7_n=L01tlpovol
     c                   if        vol7_n <= 99999
     c                   move      vol7_n        cubft
     c                   endif
     c                   exsr      $po_chk
      **
     c                   ENDIF
     c                   enddo
      *
     c                   ENDIF
     c                   ENDDO
      *
     C                   Endsr
      ************************************************************************
     C     $PO_DTL_D05   Begsr
      ************************************************************************
      *   R204#D05  (L11 detail segment after the S5 in the incomign EDI 204)
      *
     C     THMBOX        SETLL     R204#D01
     c                   DOW       not %eof(R204#D01)
     C     THMBOX        READE     R204#D01
     C                   IF        NOT %EOF(R204#D01)
     c                              and (tdstpcd='CL' or tdstpcd='PL'  or
     c                                   tdstpcd='LD')
      *
     C     Kd05          SETLL     R204#D05
     c                   dow       not %eof(R204#D05)
     C     Kd05          READE     R204#D05
     C                   IF        NOT %EOF(R204#D05)
     c                             and  td_qu_ref = 'PO'
      **
      *
      * only write the PO if it does not exist in the PUPO already for this p/u
      *   since MACYS EDI LTL 204 can give the same PO multiple times
      *
     c                   eval      ponum = tdrefid
     c                   eval      ctnqt =0
     c                   eval      weigh =0
     c                   eval      cubft =0
     c                   exsr      $po_chk
      **
     c                   ENDIF
     c                   enddo
      *
     c                   ENDIF
     c                   ENDDO
      *
     C                   Endsr
      ************************************************************************
     C     $PO_HDR       Begsr
      ************************************************************************
      *
      ** PO numbers will come in the L11 segment
     C                   EVAL      WKQUAL   = 'PO'
     C     K#H05         SETLL     R204#H05                               97
     c                   dow       not %eof(R204#H05)
     C     K#H05         READE     R204#H05                               98
     C                   IF        *in98=*off
      *
      * only write the PO if it does not exist in the PUPO already for this p/u
      *   since MACYS EDI LTL 204 can give the same PO multiple times
      *
     c                   eval      ponum = threfid
     c                   eval      ctnqt =0
     c                   eval      weigh =0
     c                   eval      cubft =0
     c                   exsr      $po_chk
      *
     c                   endif                                                  R204#H05
     c                   enddo                                                  R204#H05
      *
      *
     C                   Endsr
      ************************************************************************
     C     $PO_CHK       Begsr
      ************************************************************************
      *
     c                   eval      po_exist='N'
     c     punum1        setll     pupo01
     c                   dou       *in97=*on
     c     punum1        reade     pupo01                                 97
     C                   IF        *in97=*off  and ponum   = ponum0
     c                   eval      po_exist='Y'
     c                   leave
     c                   ENDIF
     c                   ENDDO
      *
     c                   if        po_exist = 'N'
     c                   clear                   pupofm
     c                   eval      punum0=punum1
     c                   eval      lin##0=1
     c                   eval      seqpo = seqpo + 1
     c                   eval      seqno0= seqpo
     c                   eval      cons#0=cons#2
     c                   eval      term#0=term#1
     c                   eval      ponum0=ponum
     C                   EVAL      AUTH#0=APPT#
     c                   eval      ctnqt0=ctnqt
     c                   eval      weigh0=weigh
     c                   eval      cubft0=cubft
     c                   write     pupofm
     c                   EVAL      added_po = 'Y'
     c                   endif                                                  po_exist = 'N'
      *
      *
     C                   Endsr
      * table below is division/chain -- 1st pos
      *   , the next 2 numbers is 1st 2 digits of the PO#,
      *    and last 5 pos are the consignee#
** CA TABLE- VALID DIV,PO(into table DIVPO2_T; last 6 pos CONS# into CONS#_T)
10  0862545
10  0773100
10  0673101
10  0173102
10  0373103
10  0273884
08  2073895
08  6075100
08  3075201
08  7075250
08  8075301
08  4075440
28  1037500
28  4040750
28  7042510
28  5075210
28  3082310
08  9083104
28  2073088
08  9283104
08  9383104
10  9283104
10  9383104                                          below new 08 23 21   West coast
10  0173102
10  0373102
10  0373103
10  0673101
10  0773100
10  0862545
08  1083881
08  5083881
08  2073895
08  3075201
08  4075440
08  6075100
08  7075250
08  8075301
28  1037500
28  2073088
28  3082310
28  4040750
28  5075210
28  6083070
28  7042510
10  0173102                                                           **** NEW EAST COAST
10  0273102
10  0477702
10  0373103
10  0673101
10  0773100
10  0862545
10  9573889
08  1083881
08  5083881
08  2073895
08  3075201
08  4075440
08  6075100
08  7075250
08  8075301
28  1037500
28  2073088
28  3082310
28  4040750
28  5075210
28  6083070
28  7042510
28  9442510
28  9073713
10  0773089
** NJ TABLE- VALID DIV,PO(into table DIVPO2_T; last 6 pos CONS# into CONS#_T)
04  1035900
04  2036605
04  2536605
08  1068891
08  2019700
08  3019505
08  3075201
08  4066619
08  5068891
08  6006551
08  7063050
08  8019700
08  9068417
08  9268417
08  9368417
10  0168898
10  0268898
10  0367803
10  0667801
10  0767800
10  0824013
10  9068417
10  9268417
10  9368417
10  9568889
24  2536605
28  1069111
28  2069322                                                               West coast
28  3063310
28  4040630
28  5069210
28  6018070
28  7070701
28  9470701
10  0777702
10  0477702
28  9050631
