       ctl-opt dftactgrp(*no) actgrp(*new) main(WL02R);
       dcl-f qcustcdt usage(*input) qualified usropn;
       dcl-f qsysprt printer(132)  usropn;

       dcl-proc  WL02R;
         dcl-pi *n;
           inState     char  (  2   )  const;
           ouTotalDue  packed(  7: 2);
           inOptions   char  ( 80   )  const options(*nopass);
         end-pi;
         dcl-s   Logging        ind;
         dcl-ds  CustomerRec    likerec(qcustcdt.cusrec);
         dcl-s   Name           varchar(24);

         if %parms() >= %parmnum(inOptions)
         and %scan('NOLOG': inOptions) = *zero
         and %scan('LOG'  : inOptions) > *zero;
           Logging = *on;
         endif;
         if Logging;
           open qsysprt;
           writeln ('Enter ' + %proc() + ' ' + %char(%timestamp));
           writeln ('> Selected state =/' + inState + '/');
         endif;

         clear ouTotalDue;
         open qcustcdt;
         dow '1';
           read  qcustcdt.CusRec  CustomerRec;
           if %eof();
             leave;
           endif;
           if CustomerRec.State = inState;
             ouTotalDue += CustomerRec.BalDue;
             Name = ReformatName (CustomerRec.LstNam: CustomerRec.Init:
                                  Logging);
             if Logging;
               writeln ('Selected customer ' +
                         %editc(CustomerRec.CusNum: '4') +
                         '  Balance = ' +
                         %editc(CustomerRec.BalDue: 'L') : 3);
               writeln ('Name=/' + Name + '/': 3);
             endif;
           endif;
         enddo;

         if Logging;
           writeln ('> Total due = ' + %editc(ouTotalDue: 'L'));
           writeln ('Leave ' + %proc() + ' ' + %char(%timestamp));
         endif;
         close *all;
         return;
       end-proc  WL02R;

       dcl-proc  ReformatName;
         dcl-pi *n          varchar(24);
           inLastName      varchar(16)    const;
           inInitials      varchar( 3)    const;
           inLogging       ind            const;
         end-pi;

         if inLogging;
           writeln ('Enter ' + %proc() : 3);
           writeln ('> Last=/' + inLastName +
                    '/ Init=/' + inInitials + '/' : 3);
         endif;

         return %subst(inInitials: 1: 1) + '. ' +
                %subst(inInitials: 3: 1) + '. ' +
                %trim(inLastName);
       end-proc  ReformatName;

       dcl-proc writeln;
         dcl-pi *n;
           inString   varchar(132)   const;
           inPosition uns(3)         const   options(*nopass);
         end-pi;
         dcl-ds   ReportLine   len(132)   end-ds;
         dcl-s    Position     uns(3);

         if %parms() >= %ParmNum(inPosition);
           Position = inPosition;
         else;
           Position = 1;
         endif;

         %subst(ReportLine: Position) = inString;
         write qsysprt ReportLine;
       end-proc writeln;
