       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLTAM01.
       AUTHOR. TIMOTHY ALEC MOODE.
       DATE-WRITTEN. 6/9/2019
      * THIS PROGRAM PRINTS OUT RECORDS ABOUT SALES FROM A PIZZA SHOP
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PIZZA-MASTER
               ASSIGN TO 'C:\COBOLWI19\CBLPIZZA.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOLWI19\PAPERPIZZA.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.
       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.
       FD PIZZA-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS PM-REC
           RECORD CONTAINS 26 CHARACTERS.
       01 PM-REC.
         05 PIZZA-ID.
           10 PIZZA-ID-1 PIC X.
           10 PIZZA-ID-2 PIC X.
           10 PIZZA-ID-3 PIC XX.
         05 PIZZA-DATE.
           10 PIZZA-YEAR PIC X(4).
           10 PIZZA-MONTH PIC XX.
           10 PIZZA-DAY PIC XX.
         05 PIZZA-PRICE PIC 99V99.
         05 PIZZA-CURQUANTITY PIC 9(5).
         05 PIZZA-PREVQUANTITY PIC 9(5).
       FD PRTOUT
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 40 WITH FOOTING AT 36.
       01 PRTLINE PIC X(132).

       WORKING-STORAGE SECTION.
      * SETTING UP VARIABLES
       01 MISC.
         05 EOF PIC X VALUE 'N'.
         05 CPAGECOUNT PIC 99 VALUE 0.
         05 SALEINDEMATH PIC S9(6).
         05 SALEPRICEMATH PIC 99V99.
         05 GTAVRINDE PIC 9(7) VALUE 0.
         05 SALESCOUNT PIC 999 VALUE 0.
         05 TOTPREVQUANTITY PIC 9(6) VALUE 0.
         05 GTTOTSALES PIC 9(12)V99 VALUE 0.
         05 TOTALSALESMATH PIC 9(10)V99 VALUE 0.
         05 GTSALESINDEAMT PIC S9(7).
         05 GTINDEPERC PIC S9(5).
       01 CURDATETIME.
         05 THE-DATE.
           10 CURYEAR PIC X(4).
           10 CURMONTH PIC XX.
           10 CURDAY PIC XX.
       01 H-TITLELINE.
         05 FILLER PIC X(6) VALUE 'DATE: '.
         05 H-MONTH PIC 99.
         05 FILLER PIC X VALUE '/'.
         05 H-DAY PIC 99.
         05 FILLER PIC X VALUE '/'.
         05 H-YEAR PIC 9999.
         05 FILLER PIC X(38) VALUE SPACES.
         05 FILLER PIC X(23) VALUE 'TIMOTHY''S MONTHLY SALES'.
         05 FILLER PIC X(47) VALUE SPACES.
         05 FILLER PIC X(6) VALUE 'PAGE: '.
         05 H-PAGENUM PIC Z9.
       01 H-HEADINGS1.
         05 FILLER PIC X(5) VALUE SPACES.
         05 FILLER PIC XXXX VALUE 'ITEM'.
         05 FILLER PIC X(23) VALUE SPACES.
         05 FILLER PIC X(5) VALUE 'PRIOR'.
         05 FILLER PIC X(7) VALUE SPACES.
         05 FILLER PIC X(7) VALUE 'CURRENT'.
         05 FILLER PIC X(9) VALUE SPACES.
         05 FILLER PIC X(14) VALUE 'SALES INCREASE'.
         05 FILLER PIC X(8) VALUE SPACES.
         05 FILLER PIC X(9) VALUE 'INCR/DECR'.
         05 FILLER PIC X(10) VALUE SPACES.
         05 FILLER PIC XXXX VALUE 'SALE'.
         05 FILLER PIC X(27) VALUE SPACES.
       01 H-HEADINGS2.
         05 FILLER PIC XXXX VALUE SPACES.
         05 FILLER PIC X(6) VALUE 'NUMBER'.
         05 FILLER PIC XXXX VALUE SPACES.
         05 FILLER PIC X(10) VALUE 'SALES DATE'.
         05 FILLER PIC X(9) VALUE SPACES.
         05 FILLER PIC XXX VALUE 'QTY'.
         05 FILLER PIC X(10) VALUE SPACES.
         05 FILLER PIC XXX VALUE 'QTY'.
         05 FILLER PIC X(11) VALUE SPACES.
         05 FILLER PIC X(13) VALUE '/DECREASE AMT'.
         05 FILLER PIC X(9) VALUE SPACES.
         05 FILLER PIC X(10) VALUE 'PERCENTAGE'.
         05 FILLER PIC X(8) VALUE SPACES.
         05 FILLER PIC X(5) VALUE 'PRICE'.
         05 FILLER PIC X(10) VALUE SPACES.
         05 FILLER PIC X(11) VALUE 'TOTAL SALES'.
      * DETAIL LINE IS HERE
       01 H-DETAIL.
         05 FILLER PIC XXXX VALUE SPACES.
         05 PIZZATIME1 PIC X.
         05 FILLER PIC X VALUE '-'.
         05 PIZZATIME2 PIC X.
         05 FILLER PIC X VALUE '-'.
         05 PIZZATIME3 PIC XX.
         05 FILLER PIC XXXX VALUE SPACES.
         05 PIZZAMONTH PIC 99.
         05 FILLER PIC X VALUE '/'.
         05 PIZZADAY PIC 99.
         05 FILLER PIC X VALUE '/'.
         05 PIZZAYEAR PIC 9999.
         05 FILLER PIC X(7) VALUE SPACES.
         05 PRIORQUANTITY PIC ZZ,ZZ9.
         05 FILLER PIC X(8) VALUE SPACES.
         05 CURRENTQUANTITY PIC ZZ,ZZ9.
         05 FILLER PIC X(12) VALUE SPACES.
         05 SALEINDE PIC ZZ,ZZ9-.
         05 FILLER PIC X(14) VALUE SPACES.
         05 INDEPERCENTAGE PIC +++9.
         05 FILLER PIC X VALUE '%'.
         05 FILLER PIC X(10) VALUE SPACES.
         05 SALEPRICE PIC $$$.99.
         05 FILLER PIC X(7) VALUE SPACES.
         05 TOTALSALES PIC $$$,$$$,$$$.99.
       01 H-GRANDTOTAL.
         05 FILLER PIC X(45) VALUE SPACES.
         05 FILLER PIC X(15) VALUE 'GRAND TOTALS:  '.
         05 SALEINDEGRANDTOTAL PIC Z,ZZZ,ZZ9-.
         05 FILLER PIC X(39) VALUE SPACES.
         05 GRAND_TOTALSALES PIC $$,$$$,$$$,$$$.99.
       01 H-GRANDTOTAL2.
         05 FILLER PIC X(25) VALUE SPACES.
         05 FILLER PIC X(33) VALUE 'AVERAGE INCREASE/DECREASE AMOUNT:'.
         05 FILLER PIC X(5) VALUE SPACES.
         05 AVRINDEAMT PIC ZZ,ZZ9-.
       01 H-GRANDTOTAL3.
         05 FILLER PIC X(21) VALUE SPACES.
         05 FILLER PIC X(37) VALUE
                   'AVERAGE INCREASE DECREASE PERCENTAGE'.
         05 FILLER PIC X(7) VALUE SPACES.
         05 AVRINDEPERCENT PIC +++9.
         05 FILLER PIC XX VALUE ' %'.
       PROCEDURE DIVISION.
       L0-MAIN.
           PERFORM L1-INIT.
           PERFORM L1-MAINLINE
             UNTIL EOF = 'Y'.
           PERFORM L1-CLOSING.
           STOP RUN.

       L1-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURDATETIME.
           MOVE CURMONTH TO H-MONTH.
           MOVE CURDAY TO H-DAY.
           MOVE CURYEAR TO H-YEAR.
           OPEN INPUT PIZZA-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM L3-HEADINGS.
           PERFORM L2-INPUT.
       L1-MAINLINE.
           PERFORM L2-CALCS.
           PERFORM L2-OUTPUT.
           PERFORM L2-INPUT.
       L2-CALCS.
      * THE CALCULATIONS ARE DONE HERE
           COMPUTE SALEINDEMATH = PIZZA-CURQUANTITY -
             PIZZA-PREVQUANTITY.
           COMPUTE INDEPERCENTAGE ROUNDED = SALEINDEMATH /
             PIZZA-PREVQUANTITY * 100.
           MOVE PIZZA-PRICE TO SALEPRICEMATH.
           COMPUTE TOTALSALESMATH = PIZZA-CURQUANTITY * SALEPRICEMATH.
           MOVE SALEPRICEMATH TO PIZZA-PRICE.
           COMPUTE GTAVRINDE = GTAVRINDE + SALEINDEMATH.
           COMPUTE SALESCOUNT = SALESCOUNT + 1.
           COMPUTE TOTPREVQUANTITY = TOTPREVQUANTITY +
             PIZZA-PREVQUANTITY.
           COMPUTE GTTOTSALES = GTTOTSALES + TOTALSALESMATH.
           COMPUTE GTSALESINDEAMT = GTAVRINDE.
       L2-OUTPUT.
      * PRINTS THE DETAIL LINES
           MOVE PIZZA-ID-1 TO PIZZATIME1.
           MOVE PIZZA-ID-2 TO PIZZATIME2.
           MOVE PIZZA-ID-3 TO PIZZATIME3.
           MOVE PIZZA-MONTH TO PIZZAMONTH.
           MOVE PIZZA-DAY TO PIZZADAY.
           MOVE PIZZA-YEAR TO PIZZAYEAR.
           MOVE PIZZA-PREVQUANTITY TO PRIORQUANTITY.
           MOVE PIZZA-CURQUANTITY TO CURRENTQUANTITY.
           MOVE PIZZA-PRICE TO SALEPRICE.
           MOVE SALEINDEMATH TO SALEINDE.
           MOVE TOTALSALESMATH TO TOTALSALES.
           WRITE PRTLINE
             FROM H-DETAIL
             AFTER ADVANCING 2 LINES
               AT END-OF-PAGE
                   PERFORM L3-HEADINGS.
       L1-CLOSING.
      * AFTER THINKING SOMETHING WAS WRONG WITH MY CALCULATIONS
      * I REALIZED THAT THIS COMPANY HAS BIG UPS AND DOWNS
           COMPUTE GTINDEPERC ROUNDED = GTSALESINDEAMT /
             TOTPREVQUANTITY * 100.
           MOVE GTINDEPERC TO AVRINDEPERCENT.
           MOVE GTSALESINDEAMT TO SALEINDEGRANDTOTAL.
           COMPUTE GTAVRINDE = GTAVRINDE / SALESCOUNT.
           MOVE GTAVRINDE TO AVRINDEAMT.
           MOVE GTTOTSALES TO GRAND_TOTALSALES.
           WRITE PRTLINE
             FROM H-GRANDTOTAL
             AFTER ADVANCING 4 LINES.
           WRITE PRTLINE
             FROM H-GRANDTOTAL2
             AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
             FROM H-GRANDTOTAL3
             AFTER ADVANCING 1 LINE.
           CLOSE PIZZA-MASTER
             PRTOUT.
       L2-INPUT.
      * READINGS A FILE AND HAVING IT KICK ME OUT OF A LOOP WHEN DONE
           READ PIZZA-MASTER
               AT END
                   MOVE 'Y' TO EOF.
       L3-HEADINGS.
      * THIS WILL BE TRIGGERED WHEN A PAGE GETS FILLED
           COMPUTE CPAGECOUNT = CPAGECOUNT + 1.
           MOVE CPAGECOUNT TO H-PAGENUM.
           WRITE PRTLINE
             FROM H-TITLELINE
             AFTER ADVANCING PAGE.
           WRITE PRTLINE
             FROM H-HEADINGS1
             AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
             FROM H-HEADINGS2
             AFTER ADVANCING 1 LINE.

       END PROGRAM CBLTAM01.