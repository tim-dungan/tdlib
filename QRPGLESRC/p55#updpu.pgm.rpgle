      *%METADATA                                                       *
      * %TEXT Updates PickUp for EDI LTL 204                           *
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
     FF55#EDCTL IF   E           K DISK
     FLOCT14    IF   E           K DISK
     FPUHD01    UF   E           K DISK
     FPUDT01    UF A E           K DISK
     FPUPO01    UF A E           K DISK
     FZIPS01    IF   E           K DISK
     FCONS01    IF   E           K DISK
     FWNSCWF01  UF A E           K DISK
      *
     D                SDS
     D  #PGM                   1     10
     D  #USR                 254    263
      *
      /COPY QRPGLESRC,#SPPROT
      *
      *****************************************************************
     D DIVPO2_T        S              6    DIM(27) CTDATA PERRCD(1)             DIV(4 pos),PO(2 pos)
     D CONS#_T         S                   DIM(27) ALT(DIVPO2_T) LIKE(CONS#9)       5,0
      *
     D CONS_ARR        S             39    DIM(65)
     D SameConsPO_ARR  S             24    DIM(65)
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
     dpo_exist         s              1
     dseqpo            s                   like(seqno0)
     dAPPT#            s                   like(AUTH#0)
     D  zip3_n         S              3  0 INZ
     D  zip3_a         S              3    INZ
     D  zip5_a         S              5    INZ
     D  ponum          S                   INZ  like(threfid)
     D  added_po       S              1    INZ
     D  totpallets     S              5  0 INZ(0)
     D  ctnqt          S                   like(ctnqt0)
     D  weigh          S                   like(weigh0)
     D  cubft          S                   like(cubft0)
     d  wgt7_n         s              7  0
     d  vol7_n         s              7  0
     d  sav_avldt1     s                   LIKE(AVLDT1)
     d  sav_avltm1     s                   LIKE(AVLTM1)
      *
     d  L11MCI_REF     s                    like(THREFID)
     d  PUDT_LIN#      s              3  0
     d  CONS_ARR_CTR   s              3  0
     d  SameConsPO_CT  s              3  0
     d  max_arr_size   s              3  0 INZ(65)
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
      *************************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    PMMBOX
     C                   PARM                    PMPU#             7
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
     C     PMMBOX        CHAIN(n)  R204#H01
     c                   if        THSTOP > 2
     c                                and THTPID = 'DSW204LTL'
      * cannot have more than 2 stops fr LTL -- 1 PU and 1 delivery
     C     PMMBOX        CHAIN     R204#H01
     c                   eval      thstat = 'C'
     c                   eval      thproc = 'X'
     c                   update    I204H01
     c                   goto      #EXITPGM
     c                   endif
      *
      * Get P/U   Number
     C                   EVAL      WKPU#   = %INT(PMPU#)
     C     PMMBOX        CHAIN(n)  R204#H01
     C                   IF        %FOUND (R204#H01)
      **
     c                   exsr      $hdrdet                                      write puhd,pudt
     c                   IF        THTPID <> 'TJM204LTL'                         (T1)
     c                               and
     c                             THTPID <> 'WIN204LTL'                         (T1)
     c                   exsr      $po                                          write pupo
     c                   ENDIF
      * Get PRO# that may have been generated for the 990
      *  and update pick up number into the BLPO file
TTTT C                   ENDIF
TTTT  *
TTTT  * ----------------------------------------------------------------------
TTTT  *
TTTT c     #EXITPGM      TAG
TTTT  **
TTTT C                   EVAL      *INLR = *ON
TTTT  **
TTTT  ************************************************************************
TTTT C     $HDRDET       Begsr
TTTT  ************************************************************************
      *
     c*                  clear                   puhdfm
     c*                  clear                   pudtfm
     c                   eval      L11MCI_REF = *BLANKS
     c                   clear                   CONS_ARR
     c                   eval      cons_ARR_CTR=0
     C*                  EVAL      PUHD_ADDED = ' '
     C*                  EVAL      ACREC1   =  *Blanks
     C*                  EVAL      PUNUM1   =  WkPu#
      * ----------------------------------------------
      * FIRST, Remove relevant records in PUDT, PUPO file
      * ----------------------------------------------
     C     WKPU#         setll     PUDT01
     C     WKPU#         reade     PUDT01
     C                   dow       not %eof (PUDT01)
     C                   delete    PUDTFM
     C     WKPU#         reade     PUDT01
     C                   enddo
      *
     C     WKPU#         setll     PUPO01
     C     WKPU#         reade     PUPO01
     C                   dow       not %eof (PUPO01)
     C                   delete    PUPOFM
     C     WKPU#         reade     PUPO01
     C                   enddo
      *
      *------------------------------------------------------
      * Recreate records in PUDT, PUDT(clone from P55#204PU)
      *------------------------------------------------------
     c                   clear                   pudtfm
     c                   clear                   puPOfm
      *
     C     WKPU#         chain     PUHD01
     C                   IF        %FOUND (PUHD01)
      *
     c                   eval      sav_avldt1=avldt1
     c                   eval      sav_avltm1=avltm1
      *
      *
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
     c                   eval      toamt1=0
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
      *    from TJX still but the MCI qualifier for that is not NRCS-L
      * (MCI qualifier in H05 file is a TJMAX LTL if L11 MCI qualifier is a NRCS-L
      *    otherwise the 204 from TJ is a Winners LTL)
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
     c                   eval      toshp1=0
     c                   eval      toctn1=0
     c                   eval      togoh1=0
     c                   eval      towgt1=0
     c                   eval      tocub1=0
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
     c                   endif
     c                   eval      tocub1=tdvol + tocub1
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
     c                   if        topal1 = 0 and totpallets > 0
     c                   if        totpallets <=  99
     c                   eval      topal1 = totpallets
     c                   else
     c                   eval      topal1 = 99
     c                   endif
     c                   endif
      *
     c                   IF        (THTPID = 'TJM204LTL'
     c                                  and (L11MCI_REF =  'NRCS-L' OR
     c                                      L11MCI_REF =  'NRC-C'   ) )
     c                   eval      hotsh1 = 'Y'
     c                   eval      stat21 = 'M'
     c                   ENDIF
      *
     C                   UPDATE    PUHDFM
     c                   IF        (THTPID = 'TJM204LTL'
     c                                  and L11MCI_REF <> 'NRCS-L'              TJ
     c                                  and L11MCI_REF <> 'NRC-C')              TJ
     c                                 or
     c                             THTPID = 'WIN204LTL'
     c     punum1        chain     WNSCWF01
     c                   If        %found(WNSCWF01)
      **
     c                   if        (avldt1<>sav_avldt1
     c                               or
     c                             avltm1<>sav_avltm1)
     c                               AND
     c                             flagwn = 'S '                                already scheduled
     c                   eval      flagwn='R '                                  RE SCHEDULE
     c                   endif
     c                   update    WNSCFM
      **
     c                   Else                                                  if %found(WNSCWF)
      **
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
     c                   ENDIF                                                  if thtpid= TJM204LTL
     c                   ENDIF                                                  if found(puhd01)
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
     C     DIVPO12       LOOKUP    DIVPO2_T(I)                            91
     C     *IN91         IFEQ      '0'                                          NOT FOUND
     c                   exsr      $TJMAX_DEFAULT
     c                   ELSE
     c                   eval      cons#_PO = cons#_T(I)
     C                   ENDIF
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
     c                   Exsr      $TJWNLTL_PO
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
     c                   Exsr      $TJWNLTL_PO
     c                   endif
      *
      *
     c                   ENDSR
      *
      ************************************************************************
     C     $TJWNLTL_PO   Begsr
      ************************************************************************
      *     Build the PUDT and PUPO
      *
      *
      * only write the PO if it does not exist in the PUPO already for this p/u
      *
     c     K#po          klist
     c                   kfld                    punum1
     c                   kfld                    lin##2
      *
     c                   eval      ctr_P = 0
     c                   DOW       ctr_P <= SameConsPO_CT
      *
     c                   eval      ctr_P += 1
     c                   clear                   sameConsPofld
     c                   clear                   sameConsPo_DS
     c                   eval      sameConsPOfld = SameConsPO_ARR(ctr_P)
     c                   move      sameConsPOfld SameConsPO_DS
      *
     c                   eval      punum0=punum1
     c                   eval      seqno0 = 0
     c     K#po          setll     pupo01
     c                   dou       *in97=*on
     c     k#po          reade     pupo01                                 97
     c                   if        NOT %eof(PUPO01)
     c                               and
     c                             PONUM0 = PO#C
     c                   LEAVESR
     c                   endif
     c                   enddo
      *
      * If reach here -- means PO#C NOT found in PUPO file
      *    so add it  to PUPO
     c     k#stop        setll     R204#L01
     c                   dou       %eof(R204#L01)
     c     k#stop        reade     R204#L01
     c                   if        not %EOF(R204#L01)
     c                             and %trim(L01TLPONUM) = %trim(PO#C)
     c                             and %trim(%subst(L01TLREFID:1:4))=
     c                                 %trim(DIV_PO#C)
      *
     c                   eval      lin##0 = lin##2
     c                   eval      seqno0 = seqno0 + 1
     c                   eval      cons#0=cons#2
     c                   eval      ponum0= PO#C
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
     c                   LEAVE
      **
     c                   endif
     c                   enddo
      *
     c                   ENDDO
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
** NJ TABLE- VALID DIV,PO(into table DIVPO2_T; last 6 pos CONS# into CONS#_T)
04  1035900
04  2036605
04  2536605
24  2536605
10  0862545
10  0773100
10  0673101
10  0173102
10  0373103
10  0273884
10  9568889
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
