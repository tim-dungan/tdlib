       ctl-opt option(*srcstmt: *nodebugio) dftactgrp(*no) actgrp(*new);
       dcl-c  LINE_LENGTH    132;
       dcl-f  qsysprt    printer(LINE_LENGTH);
      /copy 'SAMPLE/datadefs.rpgleinc'

       dcl-ds PrintLine      len(LINE_LENGTH);
          CustID             char ( 5);
          *n                 char ( 1);
          ProdID             char ( 6);
          *n                 char ( 1);
          OrdQty             char ( 8);
          *n                 char ( 1);
          DueDate            char (10);
          *n                 char ( 1);
          Price              char ( 9);
          *n                 char ( 1);
          ErrCode            char ( 3);
       end-ds;

       // print column headings
             clear Printline;
             CustID         = 'Cus';
             ProdID         = 'Prod';
       evalr OrdQty         = 'Qty ';
             DueDate        = 'Due';
       evalr Price          = 'Price ';
             ErrCode        = 'Err';

       writeln (PrintLine);


       TryIt (  0       :   *blank   :  0 : d'2000-01-01'   );
       TryIt (  100     :   *blank   :  0 : d'2000-01-01'   );
       TryIt (  100     :   'AB-101' :  1 : d'2000-01-01'   );

       *inlr = *on;
       return;

       dcl-proc TryIt;
         dcl-pi *n;
           inCustID         like(CustomerID_t) const;
           inProdID         like(ProductID_t)  const;
           inOrdQty         packed (7)         const;
           inDueDate        date               const;
         end-pi;

         dcl-s  ouPrice       packed (7: 2);
         dcl-s  ouErrCode     uns (3);

         CalcUnitPrice (inCustID:inProdID:inOrdQty:inDueDate:ouPrice:ouErrCode);

         // Print the results of calling the subprocedure.
         clear Printline;
         evalr CustID           = %editc( inCustID  : 'X');
               ProdID           =         inProdID        ;
         evalr OrdQty           = %editc( inOrdQty  : 'L');
               DueDate          = %char ( inDueDate      );
         evalr Price            = %editc( ouPrice   : 'L');
               ErrCode          = %char ( ouErrCode      );

         writeln (PrintLine);

       end-proc TryIt;

       dcl-proc CalcUnitPrice;
         dcl-pi *n;
           inCustID         like(CustomerID_t) const;
           inProdID         like(ProductID_t)  const;
           inOrdQty         packed (7)         const;
           inDueDate        date               const;
           ouPrice          packed (7: 2);
           ouErrCode        uns (3);
         end-pi;

         clear ouPrice;
         clear ouErrCode;

         // validate the parameters
         if inCustID <= *zero;
           ouErrCode = 1;
           return;
         endif;

         if inProdID = *blanks;
           ouErrCode = 2;
           return;
         endif;

       end-proc CalcUnitPrice;

       dcl-proc writeln;
          dcl-pi *n;
            inString   varchar(LINE_LENGTH)   const;
            inPosition uns(3)                 const   options(*nopass);
          end-pi;

          dcl-ds   ReportLine   len(LINE_LENGTH)   end-ds;
          dcl-s    Position     uns(3);

          if %parms() >= %ParmNum(inPosition);
            Position = inPosition;
          else;
            Position = 1;
          endif;

          %subst(ReportLine: Position) = inString;
          write qsysprt ReportLine;

       end-proc writeln;
