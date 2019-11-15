       IDENTIFICATION DIVISION.
       PROGRAM-ID.  LISTEMPAV2.
       AUTHOR.  LISA LANDGRAF.
      *****************************************************************
      * This program uses the alternate index for name to display
      * Records with the same alternate index.  User will enter
      * The name of someone and all records will be displayed that
      * have the same name
      *  
      *   Input file  :  C:\COBOL\NEWEMPA.DAT
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NEW-PAYROLL-FILE
                 ASSIGN TO UT-SYS-EMPFILE
                 ORGANIZATION IS INDEXED
                 ACCESS IS DYNAMIC
                 RECORD KEY IS NEW-SSNO
                 ALTERNATE RECORD KEY IS NEW-NAME
                     WITH DUPLICATES.

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
                                   VALUE "C:\COBOL\NEWEMP3.DAT".
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'F'.
               88  OK-TO-STOP               VALUE 'Y'.
           05  WS-END-OF-FILE                      PIC X.
               88 EOF                       VALUE 'Y'.
       01 WS-NAME                                  PIC X(20).

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

           DISPLAY "Enter the name of an employee"
           ACCEPT WS-NAME
           MOVE 'N' TO WS-END-OF-FILE
           MOVE WS-NAME TO NEW-NAME
           START NEW-PAYROLL-FILE KEY = NEW-NAME
               INVALID KEY
                    DISPLAY "NO EMPLOYEE WITH THAT NAME"
               NOT INVALID KEY
                  READ NEW-PAYROLL-FILE NEXT RECORD
                     AT END MOVE 'Y' TO WS-END-OF-FILE
                  END-READ
                  PERFORM 300-LIST-EMPS THRU 300-EXIT
                      UNTIL NEW-NAME NOT EQUAL WS-NAME
                            OR  EOF
           END-START.
           DISPLAY "TYPE Y TO STOP PROGRAM"
           ACCEPT WS-STOP-PROGRAM.
       200-EXIT.
           EXIT.
      ***************************************************************
      *  If match is found, then employees with this same name are
      *  displayed
      ***************************************************************
       300-LIST-EMPS.
           DISPLAY NEW-SSNO, " ", NEW-NAME, " ", NEW-SALARY
           READ NEW-PAYROLL-FILE NEXT RECORD
               AT END MOVE 'Y' TO WS-END-OF-FILE
           END-READ.
           
       300-EXIT.
           EXIT.
      ***************************************************************
      *   Open the files.
      ***************************************************************
       900-INITIALIZATION.
           OPEN INPUT NEW-PAYROLL-FILE.
       900-EXIT.
           EXIT.

      *****************************************************************
      *   Close files.
      *****************************************************************
       910-END-PROGRAM-RTN.
            CLOSE NEW-PAYROLL-FILE.
       910-EXIT.
           EXIT.
