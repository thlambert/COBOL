       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LISTEMPA.
       AUTHOR. CGL.
      ******************************************************************
      * This program uses screens to show that indexed sequential files
      * can be processed sequentially.
      *  
      *   Input file  :  C:\COBOL\ALTCOAMASTER.DAT  
      *      Account Number    : 1-4
      *      Beginning Balance : 5-11
      *      Current Balance   : 12-18
      *      Account Status    : 19
      *      Account Type      : 20
      *      Description       : 21-30
      *
      *   Output is displayed on the screen.
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALT-INDEXED-FILE
                 ASSIGN TO UT-SYS-ALT-INDEX
                 ORGANIZATION IS INDEXED
                 ACCESS IS DYNAMIC
                 RECORD KEY IS ALT-ACCTNUM
                 ALTERNATE RECORD KEY IS ALT-ACTTYPE
                     WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

       FD ALT-INDEXED-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 50 CHARACTERS.
       01 ALT-INDEXED-REC.
           05 ALT-ACCTNUM          PIC 9(4).
           05 ALT-BEGBAL           PIC S9(5)V99.
           05 ALT-CURBAL           PIC S9(5)V99.
           05 ALT-ACTSTATUS        PIC X.
           05 ALT-ACTTYPE          PIC X.
           05 ALT-ACTDESC          PIC X(30).

       WORKING-STORAGE SECTION.
       01  WS-FILENAMES.
           05  UT-SYS-ALT-INDEX                    PIC X(50)
           VALUE "C:\COBOL\ALTCOAMASTER.DAT".
                   
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'F'.
               88  OK-TO-STOP               VALUE 'Y'.
           05  WS-END-OF-FILE                      PIC X.
               88 EOF                       VALUE 'Y'.
           05 WS-CONTINUE                          PIC X.
       01 WS-ACCT-CHOICE                           PIC X.
       
       01 WS-LN-CT                                 PIC 99 VALUE 7.
       01 WS-DATE.
          05 WS-YEAR                               PIC 9(4).
          05 WS-MONTH                              PIC 99.
          05 WS-DAY                                PIC 99.
          05                                       PIC X(10).
       SCREEN SECTION.
       01 SCRMM            BLANK SCREEN
                           PROMPT
                           AUTO
                           REQUIRED
                           BACKGROUND-COLOR 7
                           FOREGROUND-COLOR 0.
           05 SCRMM-R1.
              10 LINE 1 COL 1   PIC 99 FROM WS-MONTH.
              10 LINE 1 COL 3      VALUE "/".
              10 LINE 1 COL 4   PIC 99 FROM WS-DAY.
              10 LINE 1 COL 6      VALUE "/".
              10 LINE 1 COL 7   PIC 9999 FROM WS-YEAR.
              10 LINE 1 COL 37     VALUE "VSAM 5".
           05 SCRMM-R2.
              10 LINE 2 COL 33     VALUE "GENERAL LEDGER".
           05 SCRMM-R3.
              10 LINE 3 COL 20
           VALUE "ERIC STERWALD & COLE SQUIRE & TOM LAMBERT". 
           05 SCRMM-R5.
              10 LINE 5 COL 30 VALUE "ENTER ACCOUNT TYPE:".
              10        COL 51 PIC X TO WS-ACCT-CHOICE.
           05 SCRMM-R6.
              10 LINE 6 COL 35 VALUE "E: EXPENSE".
           05 SCRMM-R7.
              10 LINE 7 COL 35 VALUE "I: INCOME".
           05 SCRMM-R8.
              10 LINE 8 COL 35 VALUE "C: CASH".
       01 SCR-REC.
           05 SCR-REC-R5.
              10 LINE 5           ERASE EOL.
              10        COL 3  VALUE "ACCOUNT".
              10        COL 12 VALUE "BEGINING".
              10        COL 24 VALUE "CURRENT".
              10        COL 33 VALUE "ACCOUNT".
              10        COL 41 VALUE "ACCOUNT".
              10        COL 49 VALUE "ACCOUNT".
           05 SCR-REC-R6.
              10 LINE 6           ERASE EOL.
              10        COL 4  VALUE "NUMBER".
              10        COL 13 VALUE "BALANCE".
              10        COL 24 VALUE "BALANCE".
              10        COL 34 VALUE "STATUS".
              10        COL 43 VALUE "TYPE".
              10        COL 49 VALUE "DESCRIPTION".
           05 SCR-REC-LN.
              10 LINE WS-LN-CT    ERASE EOL.
              10        COL 6  PIC 9(4) FROM ALT-ACCTNUM.
              10        COL 11 PIC $$$$$9.99 FROM ALT-BEGBAL.
              10        COL 22 PIC $$$$$9.99 FROM ALT-CURBAL.
              10        COL 36 PIC X FROM ALT-ACTSTATUS.
              10        COL 44 PIC X FROM ALT-ACTTYPE.
              10        COL 49 PIC X(30) FROM ALT-ACTDESC.
       01 SCR02            FOREGROUND-COLOR 4.
           05 SCR2-R6.
              10 LINE 6 COL 25
                   VALUE "NO RECORDS WITH ACCOUNT TYPE -".              
              10 LINE 6 COL 56    PIC X FROM WS-ACCT-CHOICE.
              10 LINE 7 ERASE EOL.
       01 SCR03.
           05 SCR3-LN.
              10 LINE WS-LN-CT COL 17
                 VALUE "TYPE 'Y' TO QUIT PROGRAM OR ENTER TO CONTINUE:".
              10               COL 65 PIC X TO WS-STOP-PROGRAM.
       01 SCR04.
           05 SCR4-R8.
              10 LINE 8 ERASE EOL.

       PROCEDURE DIVISION.
      *************************************************************
      *  Do startup tasks.
      *  Loop until the user decides to stop the program.
      *  Do ending tasks.
      *************************************************************
       100-MAIN.
           PERFORM 900-INITIALIZATION THRU 900-EXIT.
           PERFORM 200-PROCESS-FILE THRU 200-EXIT
                    UNTIL OK-TO-STOP.
           PERFORM 910-END-PROGRAM-RTN.
           STOP RUN.
      ***************************************************************
      *     Read a record from the indexed file move it to the
      *
      *     and write it out.
      ***************************************************************
       200-PROCESS-FILE.
           MOVE 7 TO WS-LN-CT
           DISPLAY SCRMM
           ACCEPT SCRMM
           MOVE 'N' TO WS-END-OF-FILE
           MOVE WS-ACCT-CHOICE TO ALT-ACTTYPE
           READ ALT-INDEXED-FILE KEY IS ALT-ACTTYPE
               INVALID KEY
                    DISPLAY SCR02
               NOT INVALID KEY
                  PERFORM 300-LIST-EMPS THRU 300-EXIT
                      UNTIL ALT-ACTTYPE NOT EQUAL WS-ACCT-CHOICE
                            OR  EOF
           END-READ
           IF WS-LN-CT LESS THAN 09
               DISPLAY SCR04
           END-IF
           IF NOT EOF
               COMPUTE WS-LN-CT = WS-LN-CT + 1
               DISPLAY SCR03
               ACCEPT SCR03
           END-IF.
       200-EXIT.
           EXIT.
      ***************************************************************
      *  If match is found, then employees with this same name are
      *  displayed
      ***************************************************************
       300-LIST-EMPS.
           DISPLAY SCR-REC
           READ ALT-INDEXED-FILE NEXT RECORD
              AT END MOVE 'Y' TO WS-END-OF-FILE
                     PERFORM 400-END-RTN THRU 400-EXIT
           END-READ
           COMPUTE WS-LN-CT = WS-LN-CT + 1.
       300-EXIT.
           EXIT.
      *************************************************************** 
      *   Set up line count for asking if program should continue.
      ***************************************************************
       400-END-RTN.
           COMPUTE WS-LN-CT = WS-LN-CT + 1
           DISPLAY SCR03
           ACCEPT SCR03.
       400-EXIT.
           EXIT.
      ***************************************************************
      *   Open the files.
      ***************************************************************
       900-INITIALIZATION.
           OPEN INPUT ALT-INDEXED-FILE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE.
       900-EXIT.
           EXIT.

      *****************************************************************
      *   Close files.
      *****************************************************************
       910-END-PROGRAM-RTN.
            CLOSE ALT-INDEXED-FILE.
       910-EXIT.
           EXIT.
