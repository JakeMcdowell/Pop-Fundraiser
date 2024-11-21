       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 CBLJRM05.
       AUTHOR.                     JAKE MCDOWELL.
       DATE-WRITTEN.               1/20/2023
       DATE-COMPILED.

      *******************************************************************
      *              MCDOWELL'S COCOA-COLA FUNDRAISER                   *
      *               POP TYPE TOTAL AND TEAM TOTALS                    *
      *******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FUNDRAISER
           ASSIGN TO "C:\COBOL\CBLPOPSL.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
           
       SELECT PRTOUT
           ASSIGN TO "C:\COBOL\CBLPOPSL.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       SELECT ERROR-FILE
           ASSIGN TO "C:\COBOL\CBLPOPER.PRT"
           ORGANIZATION IS RECORD SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       FD  FUNDRAISER
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC 
           RECORD CONTAINS 76 CHARACTERS.

       

       01  I-REC.
           05     I-LNAME            PIC X(15).
               88 VAL-LNAME    VALUE 'A' THRU 'Z'.
           05     I-FNAME            PIC X(15).
               88 VAL-FNAME    VALUE 'A' THRU 'Z'.
           05     I-ADDRESS          PIC X(15).
               88 VAL-ADDRESS  VALUE '1' THRU '9'.
               88 VAL-ADDRESS2 VALUE 'A' THRU 'Z'.
           05     I-CITY             PIC X(10).
               88 VAL-CITY     VALUE 'A' THRU 'Z'.
           05     I-STATE            PIC XX.
               88 VAL-STATE    VALUE 'IA' 'IL' 'MI' 'MO' 'NE' 'WI'.
           05     I-ZIP              PIC 9(5).
               88 VAL-ZIP      VALUE 1 THRU 9.
           05     I-ZIP2             PIC 9(4).
               88 VAL-ZIP2     VALUE 1 THRU 9.
           05     I-POP-TYPE         PIC 99.
               88 VAL-POP-TYPE VALUE 1, 2, 3, 4, 5, 6,.
           05     I-NUM-CASES        PIC 99.
               88 VAL-NUM-CASE VALUE 01 THRU 99.
           05     I-TEAM             PIC X.
               88 VAL-TEAM     VALUE 'A' 'B' 'C' 'D' 'E'.

           05     I-TEAMA            PIC X.
           05     I-TEAMB            PIC X.
           05     I-TEAMC            PIC X.
           05     I-TEAMD            PIC X.
           05     I-TEAME            PIC X.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           DATA RECORD IS PRTLINE
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                PIC X(132).

       FD  ERROR-FILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS ERRORLINE
           RECORD CONTAINS 132 CHARACTERS
           LINAGE IS 60 WITH FOOTING AT 56.

       01 ERRORLINE                  PIC X(132).

       WORKING-STORAGE SECTION.
       01 BLANK-LINE.
               05  FILLER             PIC X(132)    VALUE SPACES.

       01  WORK-AREA.
           05      ERROR-SW           PIC XXX.
           05      MORE-RECS          PIC XXX.
           05      C-PCTR             PIC 99       VALUE ZERO.
           05      C-PCTR-ER          PIC 99       VALUE ZERO.
           05      C-SCTR             PIC 999      VALUE ZERO.
           05      C-DEPOSIT-AMT      PIC 9(4)V99  VALUE ZERO.
           05      C-TOTAL-SALE       PIC 9(4)V99  VALUE ZERO.
           
           05      C-GT-COKE-QTY      PIC 9(5)9    VALUE ZERO.
           05      C-GT-DTCOKE-QTY    PIC 9(5)9    VALUE ZERO.
           05      C-GT-MELYEL-QTY    PIC 9(5)9    VALUE ZERO.
           05      C-GT-CHERCOKE-QTY  PIC 9(5)9    VALUE ZERO.
           05      C-GT-DTCHECOKE-QTY PIC 9(5)9    VALUE ZERO.
           05      C-GT-SPRITE-QTY    PIC 9(5)9    VALUE ZERO.


           05      C-GT-TEAMA-TOTAL   PIC 9(9)V99  VALUE ZERO.
           05      C-GT-TEAMB-TOTAL   PIC 9(9)V99  VALUE ZERO.
           05      C-GT-TEAMC-TOTAL   PIC 9(9)V99  VALUE ZERO.
           05      C-GT-TEAMD-TOTAL   PIC 9(9)V99  VALUE ZERO.
           05      C-GT-TEAME-TOTAL   PIC 9(9)V99  VALUE ZERO.
           05      C-ERROR-TOTAL      PIC 9(4)     VALUE ZERO.


           05      PRICE-PER-CASE     PIC 999V99   VALUE ZERO.
           


       01  CURRENT-DATE-AND-TIME.
           05      I-DATE.
                   10  I-YEAR     PIC 9(4).
                   10  I-MONTH    PIC 99.
                   10  I-DAY      PIC 99.
           


       01 COMPANY-TITLE-LINE.
           05      FILLER         PIC X(6)      VALUE "DATE: ".
           05      O-MONTH        PIC 99.
           05      FILLER         PIC X         VALUE '/'.
           05      O-DAY          PIC 99.
           05      FILLER         PIC X         VALUE '/'.
           05      O-YEAR         PIC 9(4).
           05      FILLER         PIC X(36)     VALUE SPACES.
           05      FILLER         PIC X(28)     VALUE
                                         "ALBIA SOCCER CLUB FUNDRAISER".
           05      FILLER         PIC X(44)     VALUE SPACES.
           05      FILLER         PIC X(6)      VALUE "PAGE:".
           05      O-PCTR         PIC Z9.

       01 COMPANY-TITLE-LINE2.
           05      FILLER         PIC X(8)     VALUE "CBLJRM04".
           05      FILLER         PIC X(48)     VALUE SPACES.
           05      FILLER         PIC X(18)     VALUE
                                                   "MCDOWELL'S DIVISON".
           05      FILLER         PIC X(57)     VALUE SPACES.

       01 COMPANY-TITLE-LINE3.
           05      FILLER         PIC X(60)     VALUE SPACES.
           05      FILLER         PIC X(12)      VALUE "SALES REPORT".
           05      FILLER         PIC X(60)     VALUE SPACES.

           
       01 COL-HDG1.
           05      FILLER         PIC X(3)     VALUE SPACES.
           05      FILLER         PIC X(9)     VALUE "LAST NAME".
           05      FILLER         PIC X(8)     VALUE SPACES.
           05      FILLER         PIC X(10)    VALUE "FIRST NAME".
           05      FILLER         PIC X(7)     VALUE SPACES.
           05      FILLER         PIC X(4)     VALUE "CITY".
           05      FILLER         PIC X(8)     VALUE SPACES.
           05      FILLER         PIC X(5)     VALUE "STATE".
           05      FILLER         PIC X        VALUE SPACES.
           05      FILLER         PIC X(8)     VALUE "ZIP CODE".
           05      FILLER         PIC X(4)     VALUE SPACES.
           05      FILLER         PIC X(8)     VALUE "POP TYPE".
           05      FILLER         PIC X(13)    VALUE SPACES.
           05      FILLER         PIC X(7)     VALUE "QUANTIT".
           05      FILLER         PIC X(6)     VALUE SPACES.
           05      FILLER         PIC X(11)    VALUE "DEPOSIT AMT".
           05      FILLER         PIC X(6)     VALUE SPACES.
           05      FILLER         PIC X(11)    VALUE "TOTAL SALES".
           05      FILLER         PIC XX       VALUE SPACES.
           
       01 DETAIL-LINE.
           05   FILLER         PIC XXX      VALUE SPACES.
           05   O-LNAME        PIC X(15).
           05   FILLER         PIC XX       VALUE SPACES.
           05   O-FNAME        PIC X(15).
           05   FILLER         PIC XX       VALUE SPACES.
           05   O-CITY         PIC X(10).
           05   FILLER         PIC XXX       VALUE SPACES.
           05   O-STATE        PIC XX.
           05   FILLER         PIC XXX       VALUE SPACES.
           05   O-ZIP-CODE     PIC 9(5).
           05   FILLER         PIC X         VALUE "-".
           05   O-ZIP-CODE2    PIC 9(4).
           05   FILLER         PIC XX        VALUE SPACES.
           05   O-POP-TYPE     PIC X(16).
           05   FILLER         PIC X(8)      VALUE SPACES.
           05   O-QUANTITY     PIC Z9.
           05   FILLER         PIC X(11)     VALUE SPACES.
           05   O-DEPOSIT-AMT  PIC $$$$.99.
           05   FILLER         PIC X(9)      VALUE SPACES.
           05   O-TOTAL-SALES  PIC $$,$$$$.99.
           05   FILLER         PIC XX        VALUE SPACES.
           
       01 GRANDTOTAL-TITLE-LINE.
           05  FILLER          PIC X(13)      VALUE "GRAND TOTALS:".
           05  FILLER          PIC X(119)     VALUE SPACES.

       01 GT-LINE.
           05  FILLER             PIC XXX       VALUE SPACES.
           05  O-GT-POP-TYPEA     PIC X(16).
           05  FILLER             PIC X         VALUE SPACES.
           05  O-GT-TYPE-QTYA     PIC ZZZ,ZZ9.
           05  FILLER             PIC X(6)      VALUE SPACES.
           05  O-GT-POP-TYPEB     PIC X(16).
           05  FILLER             PIC X         VALUE SPACES.
           05  O-GT-TYPE-QTYB     PIC ZZZ,ZZ9.
           05  FILLER             PIC X(6)      VALUE SPACES.
           05  O-GT-POP-TYPEC     PIC X(16).
           05  FILLER             PIC X         VALUE SPACES.
           05  O-GT-TYPE-QTYC     PIC ZZZ,ZZ9.
           05  FILLER             PIC X(45)     VALUE SPACES.

       01 TEAM-TITLE-LINE.
           05  FILLER          PIC X(12)      VALUE "TEAM TOTALS:".
           05  FILLER          PIC X(120)     VALUE SPACES.

       01 TEAM-GRANDTOTAL-LINE.
           05  FILLER             PIC XXX     VALUE SPACES.
           05  O-TEAM             PIC X       VALUE SPACES.
           05  FILLER             PIC X       VALUE SPACES.
           05  O-GT-TEAM-TOTAL    PIC $$$$,$$$,$$$.99.
           05  FILLER             PIC X(112)      VALUE SPACES.

       01 ERROR-TITLE-LINE.
           05 FILLER           PIC X(12)      VALUE "ERROR RECORD".
           05 FILLER           PIC X(60)      VALUE SPACES.
           05 FILLER           PIC X(17)      VALUE "ERROR DESCRIPTION".
           05 FILLER           PIC X(43)      VALUE SPACES.

       01 ERROR-LINE.
           05 O-ERROR-REC      PIC X(71).
           05 FILLER           PIC X          VALUE SPACES.
           05 O-ERR-MEG        PIC X(60).
       
       01 ERROR-TOTAL.
           05 FILLER           PIC X(13)          VALUE "TOTAL ERROR ".
           05 O-TOTAL-ERROR    PIC Z(4).
           05 FILLER           PIC X(115)         VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YEAR TO O-YEAR.
           MOVE I-MONTH TO O-MONTH.
           MOVE I-DAY TO O-DAY.

           OPEN INPUT FUNDRAISER.
           OPEN OUTPUT PRTOUT.
           OPEN OUTPUT ERROR-FILE.

          

           PERFORM 9000-READ.
           PERFORM 9100-HDG.
           PERFORM 9500-HDG-ERR.

          
       2000-MAINLINE.
           PERFORM 2300-VALIDATION THRU 2300-X.
           IF ERROR-SW = "NO"
               PERFORM 2200-CALCS
               PERFORM 2100-OUTPUT
           ELSE
               PERFORM 2400-ERROR.
           PERFORM 9000-READ.

       2100-OUTPUT.
           MOVE I-LNAME TO O-LNAME.
           MOVE I-FNAME TO O-FNAME.
           MOVE I-STATE TO O-STATE.
           MOVE I-CITY TO O-CITY.
           MOVE I-ZIP TO O-ZIP-CODE.
           MOVE I-ZIP2 TO O-ZIP-CODE2.
           MOVE I-NUM-CASES TO O-QUANTITY.
           MOVE C-DEPOSIT-AMT TO O-DEPOSIT-AMT.
           MOVE C-TOTAL-SALE TO O-TOTAL-SALES.
        
           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9100-HDG.
           
       2200-CALCS.
           MOVE 18.71 TO PRICE-PER-CASE.

           EVALUATE I-STATE
		      WHEN "IA"
		        COMPUTE C-DEPOSIT-AMT = 24 * .05 * I-NUM-CASES
		      WHEN "NE"
			    COMPUTE C-DEPOSIT-AMT = 24 * .05 * I-NUM-CASES
		      WHEN "WI"
			    COMPUTE C-DEPOSIT-AMT = 24 * .05 * I-NUM-CASES
		      WHEN "MI"
			    COMPUTE C-DEPOSIT-AMT = 24 * .10 * I-NUM-CASES
              WHEN OTHER
                COMPUTE C-DEPOSIT-AMT = 0.

            COMPUTE C-TOTAL-SALE = PRICE-PER-CASE * I-NUM-CASES  + 
                                                          C-DEPOSIT-AMT.

           EVALUATE I-TEAM
               WHEN "A"
                   ADD C-TOTAL-SALE TO C-GT-TEAMA-TOTAL
               WHEN "B"
                   ADD C-TOTAL-SALE TO C-GT-TEAMB-TOTAL
               WHEN "C"
                   ADD C-TOTAL-SALE TO C-GT-TEAMC-TOTAL
               WHEN "D"
                   ADD C-TOTAL-SALE TO C-GT-TEAMD-TOTAL
               WHEN "E"
                   ADD C-TOTAL-SALE TO C-GT-TEAME-TOTAL.
          
            EVALUATE I-POP-TYPE
               WHEN "01"
                   MOVE "COKE" TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-GT-COKE-QTY
               WHEN "02"
                   MOVE "DIET COKE" TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-GT-DTCOKE-QTY
               WHEN "03" 
                   MOVE "MELLO YELLO" TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-GT-MELYEL-QTY
               WHEN "04"
                   MOVE "CHERRY COKE" TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-GT-CHERCOKE-QTY
               WHEN "05"
                   MOVE "DIET CHERRY COKE" TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-GT-DTCHECOKE-QTY
               WHEN "06"
                   MOVE "SPRITE" TO O-POP-TYPE
                   ADD I-NUM-CASES TO C-GT-SPRITE-QTY.

       2300-VALIDATION.
           MOVE 'YES' TO ERROR-SW
		IF I-LNAME = SPACES
			MOVE "ERROR. LAST NAME REQUIRED," TO O-ERR-MEG
			GO TO 2300-X.
		IF I-FNAME = SPACES
			MOVE "ERROR. FIST NAME REQUIRED,"TO O-ERR-MEG
			GO TO 2300-X.	
		IF I-ADDRESS = SPACES
			MOVE "ERROR. ADDRESS REQUIRED," TO O-ERR-MEG
			GO TO 2300-X.
		IF I-CITY = SPACES
			MOVE "ERROR. CITY REQUIRED," TO O-ERR-MEG
			GO TO 2300-X.
		IF NOT VAL-STATE 
			MOVE "ERROR. STATE MOST BE IA,IL,MI,MO,NE,WI" TO O-ERR-MEG  
			GO TO 2300-X.
		IF I-ZIP  NOT NUMERIC 
			MOVE "ERROR. ZIP CODE MUST BE NUMERIC" TO O-ERR-MEG
			GO TO 2300-X.
        IF I-ZIP2 NOT NUMERIC 
			MOVE "ERROR. ZIP CODE MUST BE NUMERIC" TO O-ERR-MEG
			GO TO 2300-X.
		IF I-POP-TYPE NOT NUMERIC 
			MOVE "ERROR. POP TYPE MUST BE NUMERIC" TO O-ERR-MEG
			GO TO 2300-X.
        IF NOT VAL-POP-TYPE
            MOVE "ERROR. POP TYPE MUST BE HAVE OF COUNT OF 1 THRU 6"
                                                           TO O-ERR-MEG
			GO TO 2300-X.
		IF I-NUM-CASES NOT NUMERIC  
			MOVE "ERROR. CASES MUST BE NUMERIC" TO O-ERR-MEG
			GO TO 2300-X.
       IF  I-NUM-CASES < 1
			MOVE "ERROR. CASES MUST HAVE OF A COUNT OF 1 THRU 99"
                                                           TO O-ERR-MEG
			GO TO 2300-X.
		IF NOT VAL-TEAM 
			MOVE "ERROR. TEAM MUST BE ALPHANUMERIC A, B, C, D, E,"
                                                           TO O-ERR-MEG
			GO TO 2300-X.
		MOVE 'NO' TO ERROR-SW.

       2400-ERROR.
          
          ADD 1 TO C-ERROR-TOTAL.
         
          MOVE I-REC TO O-ERROR-REC.
          WRITE ERRORLINE FROM ERROR-LINE
		     AFTER ADVANCING 2 LINE	
			    AT EOP
                    PERFORM 9500-HDG-ERR.
       
       2300-X.
           EXIT.    
       3000-CLOSING.
           PERFORM 3100-GRANDTOTAL.
           PERFORM 3200-TEAMTOTAL.
           PERFORM 3300-ERRORTOTAL.
       
           CLOSE FUNDRAISER.
           CLOSE PRTOUT.
           CLOSE ERROR-FILE.

       3100-GRANDTOTAL.
           PERFORM 9100-HDG
           WRITE PRTLINE FROM GRANDTOTAL-TITLE-LINE
               AFTER ADVANCING 3 LINES.

           MOVE "COKE" TO O-GT-POP-TYPEA.
           MOVE C-GT-COKE-QTY TO O-GT-TYPE-QTYA.
           MOVE "DIET COKE" TO O-GT-POP-TYPEB.
           MOVE C-GT-DTCOKE-QTY TO O-GT-TYPE-QTYB.
           MOVE "MELLO YELLO" TO O-GT-POP-TYPEC.
           MOVE C-GT-MELYEL-QTY TO O-GT-TYPE-QTYC
           
           WRITE PRTLINE FROM GT-LINE
              AFTER ADVANCING 2 LINES.

           MOVE "CHERRY COKE" TO O-GT-POP-TYPEA.
           MOVE C-GT-CHERCOKE-QTY TO O-GT-TYPE-QTYA.
           MOVE "DIET CHERRY COKE" TO O-GT-POP-TYPEB.
           MOVE C-GT-DTCHECOKE-QTY TO O-GT-TYPE-QTYB.
           MOVE "SPRITE" TO O-GT-POP-TYPEC.  
           MOVE C-GT-SPRITE-QTY TO O-GT-TYPE-QTYC.


           WRITE PRTLINE FROM GT-LINE
              AFTER ADVANCING 2 LINES.

       3200-TEAMTOTAL.

           MOVE "A" TO I-TEAMA
           MOVE "B" TO I-TEAMB
           MOVE "C" TO I-TEAMC
           MOVE "D" TO I-TEAMD
           MOVE "E" TO I-TEAME

           WRITE PRTLINE FROM TEAM-TITLE-LINE
               AFTER ADVANCING 3 LINES.

           MOVE C-GT-TEAMA-TOTAL TO O-GT-TEAM-TOTAL.
           MOVE I-TEAMA TO O-TEAM.
           WRITE PRTLINE FROM TEAM-GRANDTOTAL-LINE
               AFTER ADVANCING 2 LINES.

           MOVE C-GT-TEAMB-TOTAL TO O-GT-TEAM-TOTAL.
           MOVE I-TEAMB TO O-TEAM.
           WRITE PRTLINE FROM TEAM-GRANDTOTAL-LINE
               AFTER ADVANCING 2 LINES.

           MOVE C-GT-TEAMC-TOTAL TO O-GT-TEAM-TOTAL.
           MOVE I-TEAMC TO O-TEAM.
           WRITE PRTLINE FROM TEAM-GRANDTOTAL-LINE
               AFTER ADVANCING 2 LINES.

           MOVE C-GT-TEAMD-TOTAL TO O-GT-TEAM-TOTAL.
           MOVE I-TEAMD TO I-TEAM.
           WRITE PRTLINE FROM TEAM-GRANDTOTAL-LINE
               AFTER ADVANCING 2 LINES.

           MOVE C-GT-TEAME-TOTAL TO O-GT-TEAM-TOTAL.
           MOVE I-TEAME TO O-TEAM.
           WRITE PRTLINE FROM TEAM-GRANDTOTAL-LINE
               AFTER ADVANCING 2 LINES.

       3300-ERRORTOTAL.
           MOVE C-ERROR-TOTAL TO O-TOTAL-ERROR.

           WRITE ERRORLINE FROM ERROR-TOTAL
               AFTER ADVANCING 3 LINE.


       9000-READ.
           READ FUNDRAISER
               AT END
                   MOVE "NO" TO MORE-RECS.
       9100-HDG.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR. 
           WRITE PRTLINE FROM COMPANY-TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COMPANY-TITLE-LINE2
               AFTER ADVANCING 1 LINES.
           WRITE PRTLINE FROM COMPANY-TITLE-LINE3
               AFTER ADVANCING 1 LINES.
           WRITE PRTLINE FROM COL-HDG1
               AFTER ADVANCING 2 LINES.
          
       9500-HDG-ERR.
           ADD 1 TO C-PCTR-ER.
           MOVE C-PCTR-ER TO O-PCTR.
            WRITE ERRORLINE FROM COMPANY-TITLE-LINE
              AFTER ADVANCING PAGE.
           WRITE ERRORLINE FROM COMPANY-TITLE-LINE2
               AFTER ADVANCING 1 LINES.
           WRITE ERRORLINE FROM COMPANY-TITLE-LINE3
               AFTER ADVANCING 1 LINES.
           WRITE ERRORLINE FROM ERROR-TITLE-LINE
               AFTER ADVANCING 2 LINE.
