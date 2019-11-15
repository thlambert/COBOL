       IDENTIFICATION DIVISION.
       PROGRAM-ID.  DELEMP.
       AUTHOR.  LISA LANDGRAF.
      *****************************************************************
      * This program allows records in an indexed sequential file to
      *  be deleted interactively.
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-MASTER-FILE
                 ASSIGN TO UT-SYS-EMPFILE
                 ORGANIZATION IS INDEXED
                 ACCESS IS RANDOM
                 RECORD KEY IS EMP-SSNO.

       DATA DIVISION.
       FILE SECTION.

       FD EMPLOYEE-MASTER-FILE
          LABEL RECORDS ARE STANDARD
          RECORD CONTAINS 34 CHARACTERS.
       01 EMPLOYEE-MASTER-REC.
          05  EMP-SSNO                             PIC 9(9).
          05  EMP-NAME                             PIC X(20).
          05  EMP-SALARY                           PIC 9(5).

       WORKING-STORAGE SECTION.
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'N'.
               88  OK-TO-STOP               VALUE 'Y'.
           05  WS-RECORD-NOT-FOUND                 PIC X
                                            VALUE 'F'.
               88  EMPLOYEE-FOUND           VALUE 'T'.
           05  WS-OK-TO-DELETE                     PIC X
                                            VALUE 'N'.
               88  OK-TO-DELETE             VALUE 'Y'.
       01 WS-FILES.
          05  UT-SYS-EMPFILE                       PIC X(50)
                                   VALUE "C:\COBOL\NEWEMP2.DAT".

       PROCEDURE DIVISION.
      *************************************************************
      *  Do startup tasks.
      *  Loop until the user decides to stop the program.
      *  Do ending tasks.
      *************************************************************
       100-MAIN.
           PERFORM 900-INITIALIZATION THRU 900-EXIT.
           DISPLAY "STOP NOW?(Y to stop)"
           ACCEPT WS-STOP-PROGRAM.
           PERFORM 200-PROCESS-FILE THRU 200-EXIT
                    UNTIL OK-TO-STOP.
           PERFORM 910-END-PROGRAM-RTN.
           STOP RUN.
      ***************************************************************
      *     Prompt for SS #, Display the current name and salary.
      *     Prompt once more for delete.  If yes, delete the record.
      *     Ask if it is ok to continue.
      *
      *     Errors will either be that the record is not on file
      *     or somehow the delete command does not work.
      ***************************************************************
       200-PROCESS-FILE.
           DISPLAY "ENTER SS#: "
           ACCEPT EMP-SSNO
           PERFORM 700-READ-EMP-MASTER THRU 700-EXIT.
           IF EMPLOYEE-FOUND
               PERFORM 300-GET-DATA THRU 300-EXIT
               DISPLAY "Ok to delete this record? (Y to delete)"
               ACCEPT WS-OK-TO-DELETE
               IF OK-TO-DELETE
                   DELETE EMPLOYEE-MASTER-FILE RECORD
                       INVALID KEY PERFORM 610-NOT-FOUND THRU 610-EXIT
                   END-DELETE
               END-IF
           END-IF.
           DISPLAY "STOP NOW? (Y TO STOP)"
           ACCEPT WS-STOP-PROGRAM.
       200-EXIT.
           EXIT.
      ***************************************************************
      *   Display record
      ***************************************************************
       300-GET-DATA.
           DISPLAY EMP-SSNO
           DISPLAY EMP-NAME
           DISPLAY EMP-SALARY.
       300-EXIT.
           EXIT.

      ***************************************************************
      *   Display error message when record not found in file.
      ***************************************************************
       600-NOT-FOUND.
           DISPLAY "ERROR IN READING RECORD - ", EMP-SSNO.
           DISPLAY "RECORD NOT ON FILE".
           MOVE 'F' TO WS-RECORD-NOT-FOUND.
       600-EXIT.
           EXIT.


      ***************************************************************
      *   Display error message and the record in error.
      ***************************************************************
       610-NOT-FOUND.
           DISPLAY "ERROR IN DELETING RECORD - ", EMPLOYEE-MASTER-REC.
       610-EXIT.
           EXIT.

      ***************************************************************
      *   Read a record from the employee master file.
      ***************************************************************
       700-READ-EMP-MASTER.
           READ EMPLOYEE-MASTER-FILE
              INVALID KEY PERFORM 600-NOT-FOUND THRU 600-EXIT
              NOT INVALID KEY MOVE 'T' TO WS-RECORD-NOT-FOUND
           END-READ.
       700-EXIT.
           EXIT.

      ***************************************************************
      *   Open the files.
      ***************************************************************
       900-INITIALIZATION.
           OPEN I-O  EMPLOYEE-MASTER-FILE.
       900-EXIT.
           EXIT.

      *****************************************************************
      *   Close files.
      *****************************************************************
       910-END-PROGRAM-RTN.
            CLOSE EMPLOYEE-MASTER-FILE.
       910-EXIT.
           EXIT.
