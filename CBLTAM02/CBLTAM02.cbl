       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLTAM02.
       AUTHOR. TIMOTHY ALEC MOODE.
       DATE-WRITTEN. 12/17/2019.
      * THIS PROGRAM WHEN RAN, WILL CREATE A RECORD OF SALES FOR A
      * COMPANY WHO SELLS BOATS
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOAT-MASTER
               ASSIGN TO 'C:\COBOLWI19\CBLBOAT1.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOLWI19\PAPER_BOATS.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.
       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.
       FD BOAT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS BM-REC
           RECORD CONTAINS 42 CHARACTERS.
       01 BM-REC.
         05 I-LAST-NAME            PIC X(15).
         05 I-STATE                PIC XX.
         05 I-BOAT-COST            PIC 9(6)V99.
         05 I-PURCHASE-DATE.
           10 I-PURCHASE-YEAR      PIC XXXX.
           10 I-PURCHASE-MONTH     PIC XX.
           10 I-PURCHASE-DAY       PIC XX.
         05 I-BOAT-TYPE            PIC X.
         05 I-ACCESSORY-PACKAGE    PIC X.
         05 I-PREP-DELIVER-COST    PIC 9(5)V99.
       FD PRTOUT
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 40 WITH FOOTING AT 36.
       01 PRTLINE                  PIC X(132).
       WORKING-STORAGE SECTION.
       01 MISC.
         05 EOF                    PIC X           VALUE 'N'.
         05 CPAGECOUNT             PIC 99          VALUE 0.
         05 HOLDTHESAILS           PIC X.
         05 CNUMSOLD               PIC 9(10)       VALUE 0.
         05 ALLBOATCOST            PIC 9(9)V99     VALUE 0.
         05 CTOTALCOST             PIC 9(7)V99.
         05 CGTSOLD                PIC 9(9)        VALUE 0.
         05 CGTTOTALCOST           PIC 9(12)V99    VALUE 0.
       01 CURDATETIME.
         05 THE-DATE.
           10 CURYEAR              PIC X(4).
           10 CURMONTH             PIC XX.
           10 CURDAY               PIC XX.
       01 BLANKLINE.
         05 FILLER                 PIC X(132)      VALUE SPACES.
       01 H-TITLE-LINE.
         05 FILLER                 PIC X(6)        VALUE 'DATE: '.
         05 H-MONTH                PIC XX.
         05 FILLER                 PIC X           VALUE '/'.
         05 H-DAY                  PIC XX.
         05 FILLER                 PIC X           VALUE '/'.
         05 H-YEAR                 PIC XXXX.
         05 FILLER                 PIC X(39)       VALUE SPACES.
         05 FILLER                 PIC X(20)
                                       VALUE 'TIMOTHY''S BOATS INC.'.
         05 FILLER                 PIC X(49)       VALUE SPACES.
         05 FILLER                 PIC X(6)        VALUE 'PAGE: '.
         05 H-PAGENUM              PIC Z9.
       01 H-HEADING1.
         05 FILLER                 PIC X(8)        VALUE 'CUSTOMER'.
         05 FILLER                 PIC X(36)       VALUE SPACES.
         05 FILLER                 PIC XXXX        VALUE 'BOAT'.
         05 FILLER                 PIC X(9)        VALUE SPACES.
         05 FILLER                 PIC X(8)        VALUE 'PURCHASE'.
         05 FILLER                 PIC X(11)       VALUE SPACES.
         05 FILLER                 PIC X(9)        VALUE 'ACCESSORY'.
         05 FILLER                 PIC X(21)       VALUE SPACES.
         05 FILLER                 PIC XXXX        VALUE 'PREP'.
         05 FILLER                 PIC X(17)       VALUE SPACES.
         05 FILLER                 PIC X(5)        VALUE 'TOTAL'.
       01 H-HEADING2.
         05 FILLER                 PIC X(9)        VALUE 'LAST NAME'.
         05 FILLER                 PIC X(14)       VALUE SPACES.
         05 FILLER                 PIC X(5)        VALUE 'STATE'.
         05 FILLER                 PIC X(16)       VALUE SPACES.
         05 FILLER                 PIC XXXX        VALUE 'COST'.
         05 FILLER                 PIC X(9)        VALUE SPACES.
         05 FILLER                 PIC XXXX        VALUE 'DATE'.
         05 FILLER                 PIC X(15)       VALUE SPACES.
         05 FILLER                 PIC X(7)        VALUE 'PACKAGE'.
         05 FILLER                 PIC X(23)       VALUE SPACES.
         05 FILLER                 PIC XXXX        VALUE 'COST'.
         05 FILLER                 PIC X(18)       VALUE SPACES.
         05 FILLER                 PIC XXXX        VALUE 'COST'.
       01 H-BOATTYPE-LINE.
         05 FILLER                 PIC X(11)       VALUE 'BOAT TYPE: '.
         05 H-BOATTYPE             PIC X(13).
       01 H-DETAIL.
         05 CUSTLASTNAME           PIC X(15).
         05 FILLER                 PIC X(9)        VALUE SPACES.
         05 STATEABREV             PIC XX.
         05 FILLER                 PIC X(12)       VALUE SPACES.
         05 BOAT-COST              PIC ZZZ,ZZZ.99.
         05 FILLER                 PIC X(7)        VALUE SPACES.
         05 PURCHASE-YEAR          PIC XXXX.
         05 FILLER                 PIC X           VALUE '/'.
         05 PURCHASE-MONTH         PIC XX.
         05 FILLER                 PIC X           VALUE '/'.
         05 PURCHASE-DAY           PIC XX.
         05 FILLER                 PIC X(11)       VALUE SPACES.
         05 ACCESSORY-LITERAL      PIC X(15).
         05 FILLER                 PIC X(10)       VALUE SPACES.
         05 PREP-COST              PIC ZZ,ZZZ.99.
         05 FILLER                 PIC X(10)       VALUE SPACES.
         05 TOTAL-COST             PIC Z,ZZZ,ZZZ.99.
       01 H-MINORSUBTOTALS.
         05 FILLER                 PIC X(22)       VALUE SPACES.
         05 FILLER                 PIC X(14)
                                       VALUE 'SUBTOTALS FOR '.
         05 BOATDOCK               PIC X(13).
         05 FILLER                 PIC X(10)       VALUE SPACES.
         05 FILLER                 PIC X(14)
                                       VALUE 'NUMBER SOLD:  '.
         05 O-NUMSOLD              PIC Z,ZZ9.
         05 FILLER                 PIC X(38)       VALUE SPACES.
         05 O-BOATSCOST            PIC $$$$,$$$,$$$.99.
       01 H-GRANDTOTALS.
         05 FILLER                 PIC X(23)       VALUE SPACES.
         05 FILLER                 PIC X(12)
                                       VALUE 'GRAND TOTALS'.
         05 FILLER                 PIC X(25)       VALUE SPACES.
         05 FILLER                 PIC X(13)
                                       VALUE 'NUMBER SOLD: '.
         05 FINALTOTSOLD           PIC ZZ,ZZ9.
         05 FILLER                 PIC X(35)       VALUE SPACES.
         05 FINALTOTCOST           PIC $$$,$$$,$$$,$$$.99.
       PROCEDURE DIVISION.
       L0-MAIN.
           PERFORM L1-INIT.
           PERFORM L1-MAINLINE
             UNTIL EOF = 'Y'.
           PERFORM L1-CLOSING.
           STOP RUN.

       L1-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURDATETIME.
           MOVE CURYEAR TO H-YEAR.
           MOVE CURMONTH TO H-MONTH.
           MOVE CURDAY TO H-DAY.
           OPEN INPUT BOAT-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM L2-INPUT.
           PERFORM L3-HEADINGS.

       L1-MAINLINE.
           IF HOLDTHESAILS NOT EQUAL I-BOAT-TYPE
               PERFORM L7-MINOR
           END-IF.
           PERFORM L2-CALCS.
           PERFORM L2-OUTPUT.
           PERFORM L2-INPUT.

       L1-CLOSING.
           PERFORM L7-MINOR.
           PERFORM L2-GRANDTOTALS.

       L2-CALCS.
           COMPUTE CTOTALCOST = I-BOAT-COST + I-PREP-DELIVER-COST.
           COMPUTE CNUMSOLD = CNUMSOLD + 1.
           COMPUTE ALLBOATCOST = ALLBOATCOST + CTOTALCOST.
           PERFORM L9-BOATTYPE.
           PERFORM L8-ACCESSORYTYPE.

       L2-OUTPUT.
      * TOTAL-COST IS NOT WORKING. AT. ALL.
           MOVE I-LAST-NAME TO CUSTLASTNAME.
           MOVE I-STATE TO STATEABREV.
           MOVE I-BOAT-COST TO BOAT-COST.
           MOVE I-PURCHASE-YEAR TO PURCHASE-YEAR.
           MOVE I-PURCHASE-MONTH TO PURCHASE-MONTH.
           MOVE I-PURCHASE-DAY TO PURCHASE-DAY.
           MOVE I-PREP-DELIVER-COST TO PREP-COST.
           MOVE CTOTALCOST TO TOTAL-COST.
           WRITE PRTLINE
             FROM H-DETAIL
             AFTER ADVANCING 1 LINE
               AT END-OF-PAGE
                   PERFORM L3-HEADINGS.

       L2-INPUT.
           READ BOAT-MASTER
               AT END
                   MOVE 'Y' TO EOF.

       L2-GRANDTOTALS.
           MOVE CGTSOLD TO FINALTOTSOLD.
           MOVE CGTTOTALCOST TO FINALTOTCOST.
           WRITE PRTLINE
             FROM H-GRANDTOTALS
             AFTER ADVANCING 3 LINES.

       L3-HEADINGS.
           COMPUTE CPAGECOUNT = CPAGECOUNT + 1.
           MOVE CPAGECOUNT TO H-PAGENUM.
           PERFORM L9-BOATTYPE.
           WRITE PRTLINE
             FROM H-TITLE-LINE
             AFTER ADVANCING PAGE.
           WRITE PRTLINE
             FROM H-BOATTYPE-LINE
             AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
             FROM H-HEADING1
             AFTER ADVANCING 2 LINES.
           WRITE PRTLINE
             FROM H-HEADING2
             AFTER ADVANCING 1 LINE.
           WRITE PRTLINE
             FROM BLANKLINE
             AFTER ADVANCING 1 LINE.

       L7-MINOR.
           COMPUTE CGTSOLD = CGTSOLD + CNUMSOLD.
           COMPUTE CGTTOTALCOST = CGTTOTALCOST + ALLBOATCOST.
      * COMEDIAN IN THE MAKING
           MOVE H-BOATTYPE TO BOATDOCK.
           MOVE CNUMSOLD TO O-NUMSOLD.
           MOVE ALLBOATCOST TO O-BOATSCOST.
           WRITE PRTLINE
             FROM H-MINORSUBTOTALS
             AFTER ADVANCING 2 LINES.
           MOVE I-BOAT-TYPE TO HOLDTHESAILS.
           PERFORM L9-BOATTYPE.
           IF EOF = "N"
               WRITE PRTLINE
                 FROM H-BOATTYPE-LINE
                 AFTER ADVANCING 2 LINES
           END-IF.
           COMPUTE ALLBOATCOST = 0.
           COMPUTE CNUMSOLD = 0.
           IF EOF = "N"
               WRITE PRTLINE
                 FROM BLANKLINE
                 AFTER ADVANCING 1 LINE
           END-IF.

      * I KEEP MY EVALUATES DOWN HERE SO THE CODE LOOKS MORE CLEAN
       L8-ACCESSORYTYPE.
           EVALUATE I-ACCESSORY-PACKAGE
               WHEN "1"
                   MOVE "ELECTRONICS" TO ACCESSORY-LITERAL
               WHEN "2"
                   MOVE "SKI PACKAGE" TO ACCESSORY-LITERAL
               WHEN "3"
                   MOVE "FISHING PACAKGE" TO ACCESSORY-LITERAL
           END-EVALUATE.

       L9-BOATTYPE.
           EVALUATE I-BOAT-TYPE
               WHEN "B"
                   MOVE "BASS BOAT" TO H-BOATTYPE
               WHEN "P"
                   MOVE "PONTOON" TO H-BOATTYPE
               WHEN "S"
                   MOVE "SKI BOAT" TO H-BOATTYPE
               WHEN "J"
                   MOVE "JOHN BOAT" TO H-BOATTYPE
               WHEN "C"
                   MOVE "CANOE" TO H-BOATTYPE
               WHEN "R"
                   MOVE "CABIN CRUISER" TO H-BOATTYPE
           END-EVALUATE.
           MOVE I-BOAT-TYPE TO HOLDTHESAILS.
       END PROGRAM CBLTAM02.