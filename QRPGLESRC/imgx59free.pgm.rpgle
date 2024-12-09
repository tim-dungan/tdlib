      *%METADATA                                                       *
      * %TEXT Scan Checks / Invoices for A/P Documents - X             *
      *%EMETADATA                                                      *
       /title  IMGX58 - SCAN Voucher Documents.
        //*******************************************************************
        //  PROGRAM NAME: IMGX59   Check / Invoice for A/P.                 *
        //          DATE: 07-October  2024                                  *
        //        AUTHOR: Peter Rusin                                       *
        //   ENVIRONMENT: OS/400 V5R3M0                                     *
        //                                                                  *
        //       PURPOSE:                                                   *
        //         Program Scans MultiPage Documents.                       *
        //                                                                  *
        //     NARRATIVE:                                                   *
        //      Image System "X".                                           *
        //      FOR THE SCANS, THESE VALUES ARE LOADED.                     *
        //        INDEX 1 - Company Code                                    *
        //        INDEX 2 - Vendor#                                         *
        //        INDEX 3 - Check#                                          *
        //        INDEX 4 -                                                 *
        //        INDEX 5 -                                                 *
        //        INDEX 6 -                                                 *
        //        INDEX 7 - Document Type.                                  *
        //*******************************************************************
       dcl-f imgx59d workstn infds(infds);
       dcl-f inforvend usage(*input) keyed extfile('LAWINTF/INFORVEND');
        //** file information feedback area for display file.
       dcl-ds infds end-ds;
        //** messages array.
       dcl-s msg        char(60)   dim(20) ctdata perrcd(1);
          //** define system code and 7 indexes for real vision calls
       dcl-ds *n;
         rqfld          char(211)  pos(1);
         iwsys          char(1)    pos(1);
         iwk1           char(30)   pos(2);
         iwk2           char(30)   pos(32);
         iwk3           char(30)   pos(62);
         iwk4           char(30)   pos(92);
         iwk5           char(30)   pos(122);
         iwk6           char(30)   pos(152);
         iwk7           char(30)   pos(182);
       end-ds;

       dcl-s @loop      packed(3:0);
       dcl-s icomp      like(dvenvndgrp);
       dcl-s ivend#     like(dvenvendor);
       dcl-s @scrno     packed(3:0) inz(0);
       dcl-s @edts1     char(2) inz('');
       dcl-s w@sys      char(1) inz('X');
       dcl-s scomp      char(3);
       dcl-s svend#     char(5);
       dcl-s scheck#    char(7);
       dcl-s rtncd      char(2);
       dcl-s route      char(30);
       dcl-s pages      char(3);
       dcl-s bcode      char(8);
       dcl-s p@tab      char(2);
       dcl-s p@key      char(10);

       dcl-pr mvc013d extpgm('MVC013D');
         rqfld          char(211);
         rtncd          char(2);
         route          char(30);
         pages          char(3);
         bcode          char(8);
       end-pr;

       dcl-pr imgru32 extpgm('IMGRU32');
         p@tab          char(2);
         p@key          char(10);
       end-pr;
        //-------------------------------------------------------------------------
        //   mainline
        //-------------------------------------------------------------------------
        //** init screen fields and position cursor.
       s1comp = *blanks;
       s1vend# = *blanks;
       s1check# = *blanks;
       s1type = 'CHK';
       @line = 03;
       @pos = 17;
       *in24 = *on;
       s1msg = msg(20);

       dow *inlr = '0';
         exsr $pro;
       enddo;

       return;
        //-------------------------------------------------------------------------
        //   process
        //-------------------------------------------------------------------------
       begsr $pro;
          //** reset keys for screen to be displayed.
         *in03 = *off;
         *in04 = *off;
         *in12 = *off;
          //** execute requested screen.
         exfmt scr01;
         *in24 = *off;
         s1msg = *blanks;
         exsr $keys1;

       endsr;
        //-------------------------------------------------------------------------
        //    $keys1      - process screen 1 input
        //-------------------------------------------------------------------------
       begsr $keys1;

         select;  //** exit.
           when *in03 = *on;
             *inlr = *on;
           when *in04 = *on;  //** prompt for doc type.
             exsr $prmpt;
           when *in12 = *on;  //** cancel.
             *inlr = *on;
           other;  //** enter key --> do scan then redisplay.
             if s1comp = scomp and s1vend# = svend# and
                s1check# = scheck# and @edts1 = 'OK';
               exsr $opt01;
               if rtncd = 'OK';
                 //** clear screen fields.
                 s1check# = *blanks;
                 s1type = 'CHK';
                 @line = 5;
               endif;
             else;
               exsr $edts1;
               scomp = s1comp;
               svend# = s1vend#;
               scheck# = s1check#;
             endif;

         endsl;

       endsr;
        //-------------------------------------------------------------------------
        //    $edts1      - validate screen 1 input
        //-------------------------------------------------------------------------
       begsr $edts1;

         for  @loop = 1 to 1;

           @edts1 = 'OK';  //** reset flag.
            //** company not entered.
           if s1comp = *blanks;
             @edts1 = 'NO';
             s1msg = msg(1);
             @line = 3;
             @pos = 17;
             *in24 = *on;
             leave;
           endif;
            //** validate company.
           if not (s1comp in %list('NRS' :'NRT' :'KEY' :'MIT' :'WL') );
             @edts1 = 'NO';
             s1msg = msg(1);
             @line = 3;
             @pos = 17;
             *in24 = *on;
             leave;
           endif;
            //** vendor not entered.
           if s1vend# = *blanks;
             @edts1 = 'NO';
             s1msg = msg(2);
             @line = 4;
             @pos = 17;
             *in24 = *on;
             leave;
           endif;
            //** validate vendor.
           icomp = 'NRS';
           evalr ivend# = %trimr(s1vend#);
           chain (icomp:ivend#) inforvend;
           if not %found(inforvend);
             @edts1 = 'NO';
             s1msg = msg(2);
             @line = 4;
             @pos = 17;
             *in24 = *on;
             vname = *blanks;
             leave;
           else;
             vname = dvenvndvnm;
           endif;
            //** check# not entered.
           if s1check# = *blanks;
             @edts1 = 'NO';
             s1msg = msg(3);
             @line = 5;
             @pos = 17;
             *in24 = *on;
             leave;
           endif;
            //** format vendor# and check#.
           evalr s1vend# = %trimr(s1vend#);
           s1vend# = %xlate(' ':'0':s1vend#);
           evalr s1check# = %trimr(s1check#);
           s1check# = %xlate(' ':'0':s1check#);
            //** document type must be selected.
           if s1type = *blanks;
             @edts1 = 'NO';
             s1msg = msg(4);
             @line = 11;
             @pos = 17;
             *in24 = *on;
             leave;
           endif;
            //** if all ok...load message.
          s1msg = msg(18);

         endfor;

       endsr;
        //-------------------------------------------------------------------------
        //    $opt01      - scan documents
        //-------------------------------------------------------------------------
       begsr $opt01;
          //** init parms for mvc013d call.
         rqfld = *blanks;
         rtncd = *blanks;
         route = *blanks;
         pages = *blanks;
         bcode = *blanks;
          //** load image system codes.
         iwsys = w@sys   ;
         iwk1  = s1comp  ;  // index 1 company
         evalr iwk2 = %trimr(s1vend#);  // index 2 vendor
         evalr iwk3 = %trimr(s1check#);  // index 3 check
         iwk4  = *blanks ;  // index 4
         iwk5  = *blanks ;  // index 5
         iwk6  = *blanks ;  // index 6
         iwk7  = s1type  ;  // index 7 doc type
         mvc013d(rqfld:rtncd:route:pages:bcode);
          //** if scan was accepted, load accepted message.
         if rtncd = 'OK';
           s1msg = msg(14);
         else;
           s1msg = msg(13);
         endif;

       endsr;
        //-------------------------------------------------------------------------
        //    $prmpt      - prompt based on the location
        //-------------------------------------------------------------------------
       begsr $prmpt;
          //** call prompt pgm based on cur location.
         select;
           when @field = 'S1TYPE';
             p@tab = *blanks;
             p@key = *blanks;
             p@tab = w@sys + '0';
             imgru32(p@tab:p@key);
             s1type = p@key;
           other;
             s1msg = msg(19);
         endsl;

       endsr;
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
