       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLTAM00.
       AUTHOR. TIM ALEC MOODE.
       DATE-WRITTEN. 11-18-2019.
      *IDENTIFICATION DIVISINO IDENTIFIES  THE PROGRAM NAME AND AUTHOR

       ENVIRONMENT DIVISION.
      *ENVIRONMENT DIVISION IDENTIFIES FILE NAMES AND COMPUTER EQUIP
       INPUT-OUTPUT SECTION.
           SELECT STUDENT-MASTER
               ASSIGN TO 'C:\COBOLWI19\STDNTMST.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOLWI19\STDNTRPT.PRT'
                   ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
      *DATA DIVISION DESCRIBES I/O FORMAT AND DEFINES VARIABLES AND
      *CONSTANTS
       FILE SECTION.
       FD  STUDENT-MASTER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 49 CHARACTERS.
       
       01 I-REC.
           05 I-ID              PIC X(7).
           05 I-NAME.
               10 I-LNAME      PIC X(15).
               10 I-FNAME      PIC X(15).
               10 I-INIT       PIC X.
           05 I-GPA            PIC 9V99.
           05 I-EX-STRT-SAL    PIC 9(6)V99.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE         PIC X(132).

       WORKING-STORAGE SECTION.
       01  MISC.
           05  MORE-RECS        PIC XXX    VALUE 'YES'.
           05  CPAGECOUNT       PIC 99     VALUE 0.
           05  STUDCOUNT        PIC 999    VALUE 0.
       01  CURDATETIME.
           05  THE-DATE.
               10 CURYEAR       PIC XXXX.
               10 CURMONTH      PIC XX.
               10 CURDAY        PIC XX.
       01  H-TITLELINE.
           05  FILLER           PIC X(6)   VALUE 'DATE: '.
           05  H-MONTH          PIC XX.
           05  FILLER           PIC X      VALUE '/'.
           05  H-DAY            PIC XX.
           05  FILLER           PIC X      VALUE '/'.
           05  H-YEAR           PIC XXXX.
           05  FILLER           PIC X(34)  VALUE SPACES.
           05  FILLER           PIC X(9)   VALUE 'TIMOTHY S'.
           05  FILLER           PIC X(7)   VALUE ' COBOL '.
           05  FILLER           PIC X(8)   VALUE 'STUDENT '.
           05  FILLER           PIC X(6)   VALUE 'ROSTER'.
           05  FILLER           PIC X(44)  VALUE SPACES.
           05  FILLER           PIC X(6)   VALUE 'PAGE: '.
           05  H-PAGE           PIC Z9.
       01  H-HEADING1.
           05  FILLER           PIC X(119) VALUE SPACES.
           05  FILLER           PIC X(11)  VALUE 'ANTICIPATED'.
       01  H-HEADING2.
           05  FILLER           PIC XXXX   VALUE '  ID'.
           05  FILLER           PIC X(23)  VALUE SPACES.
           05  FILLER           PIC X(9)   VALUE 'LAST NAME'.
           05  FILLER           PIC X(26)  VALUE SPACES.
           05  FILLER           PIC X(10)  VALUE 'FIRST NAME'.
           05  FILLER           PIC X(26)  VALUE SPACES.
           05  FILLER           PIC XXX    VALUE 'GPA'.
           05  FILLER           PIC X(16)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE 'STARTING SALARY'.
       01  H-DETAILLINE.
           05  D-ID             PIC X(7).
           05  FILLER           PIC X(20)  VALUE SPACES.
           05  D-LNAME          PIC X(15).
           05  FILLER           PIC X(20)  VALUE SPACES.
           05  D-FNAME          PIC X(15).
           05  FILLER           PIC X(20)  VALUE SPACES.
           05  D-GPA            PIC Z.99.
           05  FILLER           PIC X(18)  VALUE SPACES.
           05  D-EXSTARTSAL     PIC $ZZZ,ZZZ.99.
       01  H-FINALLINE.
           05  FILLER           PIC X(54)  VALUE SPACES.
           05  FILLER           PIC X(15)  VALUE 'STUDENT COUNT: '.
           05  F-STUD-COUNT     PIC ZZ9.
       PROCEDURE DIVISION.
      *PROCEDURE DIVISION CONTAINS ALL OF THE LOGIC OF PROGRAM
       L0-MAIN.
           PERFORM L1-INIT.
           PERFORM L1-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM L1-CLOSING.
           STOP RUN.

       L1-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURDATETIME.
           MOVE CURMONTH TO H-MONTH.
           MOVE CURDAY TO H-DAY.
           MOVE CURYEAR TO H-YEAR.
           OPEN INPUT STUDENT-MASTER.
           OPEN OUTPUT PRTOUT.
           PERFORM L3-HEADINGS.
           PERFORM L2-INPUT.

       L1-MAINLINE.
           PERFORM L2-OUTPUT.
           PERFORM L2-INPUT.

       L1-CLOSING.
           MOVE STUDCOUNT TO F-STUD-COUNT.
           WRITE PRTLINE
               FROM H-FINALLINE
                   AFTER ADVANCING 3 LINES.
           CLOSE STUDENT-MASTER
                 PRTOUT.
       L2-OUTPUT.
           MOVE I-ID TO D-ID.
           MOVE I-FNAME TO D-FNAME.
           MOVE I-LNAME TO D-LNAME.
           MOVE I-GPA TO D-GPA.
           MOVE I-EX-STRT-SAL TO D-EXSTARTSAL.
           WRITE PRTLINE  
               FROM H-DETAILLINE
                   AFTER ADVANCING 2 LINES
                       AT END-OF-PAGE
                           PERFORM L3-HEADINGS.
           COMPUTE STUDCOUNT = STUDCOUNT + 1.
       L2-INPUT.
           READ STUDENT-MASTER
               AT END
                   MOVE 'NO' TO MORE-RECS.

       L3-HEADINGS.
           COMPUTE CPAGECOUNT = CPAGECOUNT + 1.
           MOVE CPAGECOUNT TO H-PAGE.
           WRITE PRTLINE
               FROM H-TITLELINE
                   AFTER ADVANCING PAGE.
           WRITE PRTLINE 
               FROM H-HEADING1
                   AFTER ADVANCING 2 LINES.
           WRITE PRTLINE   
               FROM H-HEADING2
                   AFTER ADVANCING 1 LINE.
       END PROGRAM CBLTAM00.