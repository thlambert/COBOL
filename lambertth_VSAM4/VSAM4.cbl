       IDENTIFICATION DIVISION.
       program-id. VSAMPT3 as "lambertth_VSAM3Program1".
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
                                   VALUE "C:\COBOL\CorrectConvertedFile.DAT".
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'F'.
               88  OK-TO-STOP               VALUE 'Y'.
           05  WS-CONTINUE                         PIC X.
           05  WS-MENU-CHOICE                      PIC X.
           05 WS-ACT-NOT-FOUND                     PIC X
                                            VALUE 'F'.
               88  ACCOUNT-FOUND            VALUE 'T'.
       01 WS-VARS.
           05  WS-ACCT-NUM                         PIC 9999.
           05  WS-OK-TO-DELETE                     PIC X
                                            VALUE 'N'.
               88  OK-TO-DELETE             VALUE 'Y'.
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
              10 LINE 1 COL 28     VALUE "CHART OF ACCOUNTS RECORDS".
           05 SCRMM-R2.
              10 LINE 2 COL 33     VALUE "GENERAL LEDGER".
           05 SCRMM-R3.
              10 LINE 3 COL 20
           VALUE "ERIC STERWALD & COLE SQUIRE & TOM LAMBERT". 
           05 SCRMM-R5.
              10 LINE 5 COL 21 VALUE "ENTER MENU OPTION:".
              10        COL 39 PIC X TO WS-MENU-CHOICE.
           05 SCRMM-R7.
              10 LINE 7 COL 31 VALUE "1. ADD ACCOUNT".
           05 SCRMM-R8.
              10 LINE 8 COL 31 VALUE "2. UPDATE ACCOUNT".
           05 SCRMM-R9.
              10 LINE 9 COL 31 VALUE "3. DELETE ACCOUNT".
           05 SCRMM-R10.
              10 LINE 10 COL 31 VALUE "4. VIEW ACCOUNT".
           05 SCRMM-R11.
              10 LINE 11 COL 31 VALUE "5. EXIT".
       01 SCR01.
           05 SCR1-R5.
              10 LINE 5           ERASE EOL.
              10 LINE 5 COL 20     VALUE "ENTER ACCOUNT NUMBER:".
              10        COL 42   PIC 9(4) TO NEW-ACCTNUM.
           05 SCR1-R7.
              10 LINE 7           ERASE EOL.
           05 SCR1-R8.
              10 LINE 8           ERASE EOL.
           05 SCR1-R9.
              10 LINE 9           ERASE EOL.
           05 SCR1-R10.
              10 LINE 10           ERASE EOL.
           05 SCR1-R11.
              10 LINE 11           ERASE EOL.
           05 SCR1-R12.
              10 LINE 12           ERASE EOL.
           05 SCR1-R19.
              10 LINE 15           ERASE EOL.
           05 SCR1-R19.
              10 LINE 19           ERASE EOL.
       01 SCR02            FOREGROUND-COLOR 1.
           05 SCR2-R12.
              10 LINE 19 COL 30    VALUE "PRESS ENTER TO CONTINUE".
       01 SCR04            PROMPT
                           AUTO
                           REQUIRED
                           BACKGROUND-COLOR 7
                           FOREGROUND-COLOR 0.
           05 SCR4-R5.
              10 LINE 5 COL 20     VALUE "ACCOUNT NUMBER:" ERASE EOL.
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
           
       01 SCR05            FOREGROUND-COLOR 4.
           05 SCR5-R6.
              10 LINE 6 COL 23    VALUE "RECORD NOT FOUND -".
              10 LINE 6 COL 42    PIC X(4) FROM NEW-INDEXED-REC.
       01 SCR06.
           05 SCR6-R12.
              10 LINE 12 COL 17        VALUE "RECORD DELETED" ERASE EOL.
       01 SCR07.
           05 SCR7-R12.
              10 LINE 12 COL 17       VALUE "OK TO DELETE THIS RECORD? (Y TO DELETE):".
       01 SCR09            PROMPT
                           AUTO
                           REQUIRED
                           BACKGROUND-COLOR 7
                           FOREGROUND-COLOR 0.
           05 SCR4-R5.
              10 LINE 5 COL 20     VALUE
                               "ACCOUNT NUMBER:               ".
              10 LINE 5 COL 36     PIC 9999 FROM NEW-ACCTNUM.
           05 SCR4-R6.
              10 LINE 6 COL 17     VALUE "BEGINNING BALANCE:".
              10 LINE 6 COL 36     PIC $ZZZZ9.99- FROM NEW-BEGBAL.
           05 SCR4-R7.
              10 LINE 7 COL 19     VALUE "CURRENT BALANCE:".
              10 LINE 7 COL 36     PIC $ZZZZ9.99- FROM NEW-CURBAL.
           05 SCR4-R8.
              10 LINE 8 COL 20     VALUE "ACCOUNT STATUS:".
              10 LINE 8 COL 36     PIC X TO NEW-ACTSTATUS.
           05 SCR4-R9.
              10 LINE 9 COL 22     VALUE "ACCOUNT TYPE:".
              10 LINE 9 COL 36     PIC X FROM NEW-ACTTYPE.
           05 SCR4-R10.
              10 LINE 10 COL 15     VALUE "ACCOUNT DESCRIPTION:".
              10 LINE 10 COL 36    PIC X(30) TO NEW-ACTDESC.
       01 SCRADD1.
           05 SCRADD-R6.
              10 LINE 6 COL 23     VALUE "BEGINNING BALANCE:" ERASE EOL.
              10 LINE 6 COL 42     PIC $ZZZZ9.99- TO NEW-BEGBAL.
           05 SCRADD-R7.
              10 LINE 7 COL 25     VALUE "CURRENT BALANCE:".
              10 LINE 7 COL 42     PIC $ZZZZ9.99- TO NEW-CURBAL.
           05 SCRADD-R8.
              10 LINE 8 COL 17     VALUE "ACCOUNT STATUS (A or D):".
              10 LINE 8 COL 42     PIC X TO NEW-ACTSTATUS.
           05 SCRADD-R9.
              10 LINE 9 COL 16     VALUE "ACCOUNT TYPE (I, E or C):".
              10 LINE 9 COL 42     PIC X TO NEW-ACTTYPE.
           05 SCRADD-R10.
              10 LINE 10 COL 21    VALUE "ACCOUNT DESCRIPTION:".
              10 LINE 10 COL 42    PIC X(30) TO NEW-ACTDESC.
       01 SCRADDERR     FOREGROUND-COLOR 4.
           05 SCRADDERR-R6.
              10 LINE 11 COL 20     VALUE "ERROR WRITING FILE".
       01 SCRADDFERR.
           05 SCRADDFERR-R6.
              10 LINE 6 COL 20     VALUE "ACCOUNT ALREADY EXISTS".
       01 SCRADDSUC.
           05 SCRADDSUC-R18.
              10 LINE 18 COL 30    VALUE "RECORD ADDED".
       01 SCRUPDSUC.
           05 SCRUPSUC-R18.
              10 LINE 18 COL 30  VALUE "RECORD UPDATED".
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
           DISPLAY SCRMM
           ACCEPT SCRMM
           IF WS-MENU-CHOICE = 1
              PERFORM 300-ADD-RECORD THRU 300-EXIT
           ELSE
              IF WS-MENU-CHOICE = 2
                 PERFORM 400-UPDATE-RECORD THRU 400-EXIT
              ELSE 
                 IF WS-MENU-CHOICE = 3
                    PERFORM 500-DELETE-RECORD THRU 500-EXIT
                 ELSE
                    IF WS-MENU-CHOICE = 4
                       PERFORM 600-VIEW THRU 600-EXIT
                    ELSE
                       IF WS-MENU-CHOICE = 5
                          MOVE 'Y' TO WS-STOP-PROGRAM
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.
       200-EXIT.
           exit.
      *****************************************************************
      *   
      *****************************************************************
       300-ADD-RECORD.
           DISPLAY SCR01
           ACCEPT SCR01
           PERFORM 550-READ-ACT-MASTER THRU 550-EXIT
           IF ACCOUNT-FOUND
               DISPLAY SCRADDFERR
               DISPLAY SCR02
               ACCEPT WS-CONTINUE
           else
               DISPLAY SCRADD1
               ACCEPT SCRADD1
               PERFORM 350-ADD-READ THRU 350-EXIT
           END-IF.
       300-EXIT.
           exit.
      *****************************************************************
      *   
      ***************************************************************** 
       350-ADD-READ.
           WRITE NEW-INDEXED-REC 
		      INVALID KEY DISPLAY SCRADDERR
                          DISPLAY SCR02
                          ACCEPT WS-CONTINUE
              NOT INVALID KEY DISPLAY SCRADDSUC
                          DISPLAY SCR02
                          ACCEPT WS-CONTINUE
           END-WRITE.

       350-EXIT.
           exit.
      *****************************************************************
      *   
      *****************************************************************
       400-UPDATE-RECORD.
            DISPLAY SCR01
            ACCEPT SCR01
            DISPLAY SCR09
            ACCEPT SCR09
			REWRITE NEW-INDEXED-REC
				INVALID KEY DISPLAY SCR05
                            DISPLAY SCR02
                NOT INVALID KEY DISPLAY SCRUPDSUC
                            DISPLAY SCR02
                            ACCEPT WS-CONTINUE
			END-REWRITE.
       400-EXIT.
           exit.
      *****************************************************************
      *   
      *****************************************************************
       500-DELETE-RECORD.
           DISPLAY SCR01.
           ACCEPT SCR01.
           PERFORM 550-READ-ACT-MASTER THRU 550-EXIT
           IF ACCOUNT-FOUND
               DISPLAY SCR04
               DISPLAY SCR07
               ACCEPT WS-OK-TO-DELETE
               IF OK-TO-DELETE
                   DISPLAY SCR06
                   DELETE INDEXED-FILE record
                       INVALID KEY PERFORM 560-NOT-FOUND THRU 560-EXIT
                   END-DELETE
               END-IF
           END-IF
           DISPLAY SCR02.
           ACCEPT WS-CONTINUE.
       500-EXIT.
           exit.
      *****************************************************************
      *   
      ***************************************************************** 
       550-READ-ACT-MASTER.
           READ INDEXED-FILE
               INVALID KEY PERFORM 560-NOT-FOUND THRU 560-EXIT
               NOT INVALID KEY MOVE 'T' TO WS-ACT-NOT-FOUND
           END-READ.
       550-EXIT.
           exit.
      *****************************************************************
      *   
      ***************************************************************** 
       560-NOT-FOUND.
           DISPLAY SCR05.
           MOVE 'F' TO WS-ACT-NOT-FOUND.
       560-EXIT.
           exit.
      *****************************************************************
      *   
      ***************************************************************** 
       600-VIEW.
           DISPLAY SCR01.
           ACCEPT SCR01.
           PERFORM 550-READ-ACT-MASTER THRU 550-EXIT.
           IF ACCOUNT-FOUND
               DISPLAY SCR04
           END-IF.
           DISPLAY SCR02.
           ACCEPT WS-CONTINUE.
       600-EXIT.
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

