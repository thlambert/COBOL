       IDENTIFICATION DIVISION.
       program-id. VSAMPT3 as "lambertth_VSAM3.Program1".
       AUTHOR. THOMAS LAMBERT.

      ******************************************************************
      * This program uses screens to show that indexed sequential files
      * can be processed sequentially.
      *  
      *   Input file  :  C:\COBOL\NEWCOAMASTER.DAT  
      *
      *   Output is displayed on the screen.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INDEXED-FILE
               ASSIGN TO UT-SYS-INDFILE
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS NEW-ACCTNUM.
      *        ALTERNATE KEY IS NEW-ACTDESC.

       data division.
       FILE SECTION.

       FD INDEXED-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 50 CHARACTERS.
       01 NEW-INDEXED-REC.
           05 NEW-ACCTNUM          PIC 9(4).
           05 NEW-BEGBAL           PIC S9(5)V99.
           05 NEW-CURBAL           PIC S9(5)V99.
           05 NEW-ACTSTATUS        PIC X.
           05 NEW-ACTTYPE          PIC X.
           05 NEW-ACTDESC          PIC X(30).

       working-storage section.
       01  WS-FILENAMES.
           05  UT-SYS-INDFILE                  PIC X(50)
                                   VALUE "C:\COBOL\NEWCOAMASTER.DAT".
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'F'.
               88  OK-TO-STOP               VALUE 'Y'.
           05  WS-CONTINUE                         PIC X.
           05 WS-ACT-NOT-FOUND                      PIC X
                                            VALUE 'F'.
               88  ACCOUNT-FOUND            VALUE 'T'.
           05  WS-OK-TO-DELETE                     PIC X
                                            VALUE 'N'.
               88  OK-TO-DELETE             VALUE 'Y'.
       01 WS-DATE.
          05 WS-YEAR                               PIC 9(4).
          05 WS-MONTH                              PIC 99.
          05 WS-DAY                                PIC 99.
          05                                       PIC X(10).
       SCREEN SECTION.
       01 SCR01            BLANK SCREEN
                           PROMPT
                           AUTO
                           REQUIRED
                           BACKGROUND-COLOR 7
                           FOREGROUND-COLOR 0.
           05 SCR1-R1.
              10 LINE 1 COL 1   PIC 99 FROM WS-MONTH.
              10 LINE 1 COL 3      VALUE "/".
              10 LINE 1 COL 4   PIC 99 FROM WS-DAY.
              10 LINE 1 COL 6      VALUE "/".
              10 LINE 1 COL 7   PIC 9999 FROM WS-YEAR.
              10 LINE 1 COL 28     VALUE "CHART OF ACCOUNTS RECORDS".
           05 SCR1-R2.
              10 LINE 2 COL 33     VALUE "GENERAL LEDGER".
           05 SCR1-R3.
              10 LINE 3 COL 20     VALUE "ERIC STERWALD & COLE SQUIRE & TOM LAMBERT".
           05 SCR1-R5.
              10 LINE 5 COL 20     VALUE "ENTER ACCOUNT NUMBER:".
              10        COL 42   PIC 9(4) TO NEW-ACCTNUM.
           05 SCR1-R12.
              10 LINE 12           ERASE EOL.
           05 SCR1-R19.
              10 LINE 15           ERASE EOL.
           05 SCR1-R19.
              10 LINE 19           ERASE EOL.
       01 SCR02            FOREGROUND-COLOR 1.
           05 SCR2-R12.
              10 LINE 19 COL 30    VALUE "PRESS ENTER TO CONTINUE".
       01 SCR03.
           05 SCR3-R18.
              10 LINE 18           ERASE EOL.
           05 SCR3-R19.
              10 LINE 19 COL 30    VALUE "PRESS Y TO EXIT.        ".
              10 LINE 20 COL 30.
       01 SCR04            PROMPT
                           AUTO
                           REQUIRED
                           BACKGROUND-COLOR 7
                           FOREGROUND-COLOR 0.
           05 SCR4-R5.
              10 LINE 5 COL 20     VALUE "ACCOUNT NUMBER:               ".
              10 LINE 5 COL 36     PIC 9999 FROM NEW-ACCTNUM.
           05 SCR4-R6.
              10 LINE 6 COL 17     VALUE "BEGINNING BALANCE:".
              10 LINE 6 COL 36     PIC $ZZZZ9.99- FROM NEW-BEGBAL.
           05 SCR4-R7.
              10 LINE 7 COL 19     VALUE "CURRENT BALANCE:".
              10 LINE 7 COL 36     PIC $ZZZZ9.99- FROM NEW-CURBAL.
           05 SCR4-R8.
              10 LINE 8 COL 20     VALUE "ACCOUNT STATUS:".
              10 LINE 8 COL 36     PIC X FROM NEW-ACTSTATUS.
           05 SCR4-R9.
              10 LINE 9 COL 22     VALUE "ACCOUNT TYPE:".
              10 LINE 9 COL 36     PIC X FROM NEW-ACTTYPE.
           05 SCR4-R10.
              10 LINE 10 COL 15     VALUE "ACCOUNT DESCRIPTION:".
              10 LINE 10 COL 36    PIC X(30) FROM NEW-ACTDESC.
           05 SCR4-R12.
              10 LINE 12 COL 17       VALUE "OK TO DELETE THIS RECORD? (Y TO DELTE):".
       01 SCR05            FOREGROUND-COLOR 4.
           05 SCR5-R6.
              10 LINE 6 COL 23    VALUE "RECORD NOT FOUND -".
              10 LINE 6 COL 42    PIC X(4) FROM NEW-INDEXED-REC.
       procedure division.

      ***************************************************************
      *   Do startup tasks.
      *   Loop until out of records.
      *   Do ending tasks.
      *************************************************************** 
       100-MAIN.
           PERFORM 900-INITIALIZATION THRU 900-EXIT.
           PERFORM 200-PROCESS-FILE THRU 200-EXIT
                   UNTIL OK-TO-STOP.
           PERFORM 910-END-PROGRAM THRU 910-EXIT.
           STOP RUN.
      ***************************************************************
      *   Displays appropriate screens and reads the file.
      *   Also waits for user input at end
      ***************************************************************
       200-PROCESS-FILE.

           DISPLAY SCR01.
           ACCEPT SCR01.
           PERFORM 800-READ-ACT-MASTER THRU 800-EXIT
           IF ACCOUNT-FOUND
               DISPLAY SCR04
      *        DISPLAY "OK TO DELETE THIS RECORD? (Y TO DELETE)"
               ACCEPT WS-OK-TO-DELETE
               IF OK-TO-DELETE
                   DELETE INDEXED-FILE record
                       INVALID KEY PERFORM 850-NOT-FOUND THRU 850-EXIT
                   END-DELETE
               END-IF
           END-IF
                    
           DISPLAY SCR02.
           ACCEPT WS-CONTINUE.
           DISPLAY SCR03.
           ACCEPT WS-STOP-PROGRAM.
       200-EXIT.
           exit.

       800-READ-ACT-MASTER.
           READ INDEXED-FILE
               INVALID KEY PERFORM 850-NOT-FOUND THRU 850-EXIT
               NOT INVALID KEY MOVE 'T' TO WS-ACT-NOT-FOUND
           END-READ.
       800-EXIT.
           exit.

       850-NOT-FOUND.
      *    DISPLAY  "ERROR IN READING RECORD - ", NEW-INDEXED-REC
           DISPLAY SCR05.
           MOVE 'F' TO WS-ACT-NOT-FOUND.
       850-EXIT.
           exit.

       860-NOT-FOUND.
           DISPLAY "ERROR IN DELETING RECORD - ", NEW-INDEXED-REC.
       860-EXIT.
           exit.
      ***************************************************************
      *   Open the files and gets the current date.
      ***************************************************************
       900-INITIALIZATION.
           OPEN I-O INDEXED-FILE.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE.
       900-EXIT.
           exit.
      *****************************************************************
      *   Close files.
      *****************************************************************
       910-END-PROGRAM.
           CLOSE INDEXED-FILE.
       910-EXIT.
           exit.

