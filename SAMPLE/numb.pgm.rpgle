      *%METADATA                                                       *
      * %TEXT comfortably numb (recursion)                             *
      *%EMETADATA                                                      *
       ctl-opt option(*srcstmt: *nodebugio) dftactgrp(*no) actgrp(*new);
       dcl-s forNumber zoned(11:2);
       dcl-ds *n;
         words varchar(200);
         wordsx char(01) overLay(words : 3);
         words1 char(50) overLay(words : 3);
         words2 char(50) overLay(words : *next);
         words3 char(50) overLay(words : *next);
         words4 char(50) overLay(words : *next);
       end-ds;

       dsply 'Enter number: ' ' ' forNumber;
       words = currencyToWords(forNumber);
       wordsx = %upper(wordsx);

       dsply words1;
       dsply words2;
       dsply words3;
       dsply words4;

       *inLr = *on;
       return;

       dcl-proc currencyToWords;
         dcl-pi *n varchar(200);
           number zoned(13:2) const;
           currency varchar(20) const options(*noPass: *trim);
           decimals varchar(20) const options(*noPass: *trim);
         end-pi;

         dcl-s forCurrency varchar(20) inz('dollars');
         dcl-s forDecimals varchar(20) inz('cents');
         dcl-s cents zoned(2:0);

         if (%parms() > 1);
           forCurrency = currency;
         endIf;
         if (%parms() > 2);
           forDecimals = decimals;
         endIf;
         cents = %int((number - %int(number)) * 100);
         return numberToWords(%int(number)) + ' ' + forCurrency +
                ' and '+ numberToWords(cents) + ' ' + forDecimals;
       end-proc currencyToWords;

       dcl-proc numberToWords;
         dcl-pi *n varchar(200);
           numberIn zoned(9:0) const;
         end-pi;

         dcl-ds forOneWords;
           *n varchar(9) inz('one');
           *n varchar(9) inz('two');
           *n varchar(9) inz('three');
           *n varchar(9) inz('four');
           *n varchar(9) inz('five');
           *n varchar(9) inz('six');
           *n varchar(9) inz('seven');
           *n varchar(9) inz('eight');
           *n varchar(9) inz('nine');
           *n varchar(9) inz('ten');
           *n varchar(9) inz('eleven');
           *n varchar(9) inz('twelve');
           *n varchar(9) inz('thirteen');
           *n varchar(9) inz('fourteen');
           *n varchar(9) inz('fifteen');
           *n varchar(9) inz('sixteen');
           *n varchar(9) inz('seventeen');
           *n varchar(9) inz('eighteen');
           *n varchar(9) inz('nineteen');
          oneWords varchar(9) dim(19) pos(1);
         end-ds;

         dcl-ds forTenWords;
           *n varchar(7) inz('');
           *n varchar(7) inz('twenty');
           *n varchar(7) inz('thirty');
           *n varchar(7) inz('forty');
           *n varchar(7) inz('fifty');
           *n varchar(7) inz('sixty');
           *n varchar(7) inz('seventy');
           *n varchar(7) inz('eighty');
           *n varchar(7) inz('ninety');
           tenWords varchar(7) dim(9) pos(1);
         end-ds;

         dcl-s number    int(10);
         dcl-s mega      int(10);
         dcl-s kilo      int(10);
         dcl-s hundreds  int(10);
         dcl-s tens      int(10);
         dcl-s singles   int(10);
         dcl-s result    varchar(200);

         number = numberIn;
         mega = %int(number / 1000000);
         number -= mega * 1000000;
         kilo = %int(number / 1000);
         number -= kilo * 1000;
         hundreds = %int(number / 100);
         number -= hundreds * 100;
         tens = %int(number / 10);
         singles = number - (tens * 10);

         result = '';
         if (mega > 0) ;
           result += numberToWords(mega) + ' million, ';
         endIf;
         if (kilo > 0) ;
           result += numberToWords(kilo) + ' thousand, ';
         endIf;
         if (hundreds > 0) ;
           result += numberToWords(hundreds) + ' hundred ';
         endIf;

         if (tens > 0 or singles > 0);
           if (result <> '');
             result += 'and ';
           endIf;

           if (tens < 2);
             result += oneWords(tens * 10 + singles);
           else;
             result += tenWords(tens);
             if (singles <> 0);
               result += ' ' + oneWords(singles);
             endIf;
           endIf;

         endIf;

         if (result = '');
           result = 'zero';
         endIf;

         return result;
       end-proc numberToWords;
