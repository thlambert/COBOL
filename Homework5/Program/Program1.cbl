       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LISTEMP.
       AUTHOR.  THOMAS LAMBERT.
      *****************************************************************
      * Purpose of the program: This program uses an indexed file
      * with staff details as input
      * User will enter an id and the record with that ID will
      * be displayed if there is a record with that ID.
      *  
      *   Input file  :  C:\COBOL\NEWEMP2.DAT
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NEW-PAYROLL-FILE
                 ASSIGN TO UT-SYS-EMPFILE
                 ORGANIZATION IS INDEXED
                 ACCESS IS DYNAMIC
                 RECORD KEY IS NEW-SSNO.

       DATA DIVISION.
       FILE SECTION.

       FD NEW-PAYROLL-FILE
          LABEL RECORDS ARE STANDARD
          RECORD CONTAINS 34 CHARACTERS.
       01 NEW-PAYROLL-REC.
          05  NEW-SSNO                             PIC 9(9).
          05  NEW-NAME                             PIC X(20).
          05  NEW-SALARY                           PIC 9(5).

       WORKING-STORAGE SECTION.
       01  WS-FILENAMES.
           05  UT-SYS-EMPFILE                      PIC X(50)
                                   VALUE "C:\COBOL\NEWEMP2.DAT".
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'F'.
               88  OK-TO-STOP               VALUE 'Y'.
               88  ALSO-OK-TO-STOP          VALUE 'y'.
           05  WS-CONTINUE                         PIC X.
       01 WS-DATE.
          05 WS-YEAR                               PIC 9(4).
          05 WS-MONTH                              PIC 99.
          05 WS-DAY                                PIC 99.
          05                                       PIC X(10).

       SCREEN SECTION.
       01 SCR01                BLANK SCREEN
                               PROMPT
                               AUTO
                               REQUIRED
                               BACKGROUND-COLOR 0
                               FOREGROUND-COLOR 2.
          05 SCR1-R1.
             10         COL 1          VALUE "Thomas Lambert".
             10         COL 36         VALUE "Staff Records".
             10         COL 71     PIC 99 FROM WS-MONTH.
             10         COL 73         VALUE "/".
             10         COL 74     PIC 99 FROM WS-DAY.
             10         COL 76         VALUE "/".
             10         COL 77     PIC 9(4) FROM WS-YEAR.
          05 SCR1-R5.
             10 LINE 5  COL 10         VALUE "ENTER STAFF ID:".
             10         COL 27     PIC 9(9) TO NEW-SSNO.
       01 SCR02.
          05 SCR2-R5.
             10 LINE 5  COL 10         VALUE "      STAFF ID:".
             10         COL 27     PIC 9(9) FROM NEW-SSNO.
          05 SCR2-R6.
             10 LINE 6  COL 16         VALUE "NAME:".
             10 LINE 6  COL 27     PIC X(20) FROM NEW-NAME.
          05 SCR2-R7.
             10 LINE 7  COL 16         VALUE "SALARY:".
             10 LINE 7  COL 27     PIC 9(5) FROM NEW-SALARY.
       01 SCR03.
          05 SCR3-R9.
             10 LINE 9  COL 16         VALUE "PRESS ENTER TO CONTINUE".
             10 LINE 10 COL 16.
       01 SCR04.
          05 SCR4-R9.
             10 LINE 9  COL 16          VALUE "TYPE Y TO QUIT PROGRAM ".
             10 LINE 10 COL 16.
       01 SCR05.
          05 SCR5-R6.
             10 LINE 6 COL 10          VALUE "EMPLOYEE IS NOT ON FILE  ".

       PROCEDURE DIVISION.
      *************************************************************
      *  Do startup tasks.
      *  Loop until the user decides to stop the program.
      *  Do ending tasks.
      *************************************************************
       100-MAIN.
           PERFORM 900-INITIALIZATION THRU 900-EXIT.
           PERFORM 200-PROCESS-FILE THRU 200-EXIT
                    UNTIL OK-TO-STOP OR ALSO-OK-TO-STOP.
           PERFORM 910-END-PROGRAM-RTN.
           STOP RUN.
      ***************************************************************
      *    Accepts an ID
      *    Reads from the indexed file to see if someone has that id
      *    Displays error messages and continue messages.
      *    Accepts continue and quit input.
      ***************************************************************
       200-PROCESS-FILE.

           DISPLAY SCR01
           ACCEPT SCR01
           READ NEW-PAYROLL-FILE
               INVALID KEY
                    DISPLAY SCR05
               NOT INVALID KEY
                  PERFORM 300-DISPLAY-EMP THRU 300-EXIT
           END-READ
		   DISPLAY SCR03
	       ACCEPT WS-CONTINUE
           DISPLAY SCR04
           ACCEPT WS-STOP-PROGRAM.
       200-EXIT.
           EXIT.
      ***************************************************************
      *  If match is found, then employee with this  ID is
      *  displayed
      ***************************************************************
       300-DISPLAY-EMP.
           DISPLAY SCR02.           
       300-EXIT.
           EXIT.
      ***************************************************************
      *   Open the files and gets the current date.
      ***************************************************************
       900-INITIALIZATION.
           OPEN INPUT NEW-PAYROLL-FILE.
           MOVE FUNCTION current-date TO WS-DATE.
       900-EXIT.
           EXIT.

      *****************************************************************
      *   Close files.
      *****************************************************************
       910-END-PROGRAM-RTN.
            CLOSE NEW-PAYROLL-FILE.
       910-EXIT.
           EXIT.
