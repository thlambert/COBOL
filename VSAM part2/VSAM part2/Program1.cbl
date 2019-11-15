       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ADDBOOK
       AUTHOR.  LISA LANDGRAF.
      *****************************************************************
      * This program uses screens to capture data for a book record
      *    and then adds the book to the data file if it does not 
      *    already exist in the file.  
      *  
      *   Input file  :  C:\COBOL\BOOK.DAT  
      *
      *    Input/Output record
      *    05 BR-BOOK-ISBN          	PIC 9(13).
      *    05 BR-BOOK-TITLE             PIC X(20).
      *    05			   	            PIC X(30).
      *    05 BR-BOOK-PUBLISHER     	PIC X(15).
      *    05 BR-BOOK-PRICE         	PIC 9(5)V99.
      *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOK-FILE
                 ASSIGN TO UT-SYS-BOOKFILE
                 ORGANIZATION IS INDEXED
                 ACCESS IS DYNAMIC
                 RECORD KEY IS BR-BOOK-ISBN.

       DATA DIVISION.
       FILE SECTION.

       FD BOOK-FILE
          RECORD CONTAINS 85 CHARACTERS.
       01 BOOK-REC.
           05 BR-BOOK-ISBN          		PIC 9(13).
           05 BR-BOOK-TITLE            		PIC X(20).
           05 BR-AUTHOR-ID	  	            PIC 9(5).
           05			   	                PIC X(25).
           05 BR-BOOK-PUBLISHER     	    PIC X(15).
           05 BR-BOOK-PRICE         	    PIC 9(5)V99.


       WORKING-STORAGE SECTION.
       01  WS-FILENAMES.
           05  UT-SYS-BOOKFILE                      PIC X(50)
                                   VALUE "C:\COBOL\BOOK.DAT".
       01  WS-SWITCHES.
           05  WS-STOP-PROGRAM                     PIC X
                                            VALUE 'F'.
               88  OK-TO-STOP               VALUE 'Y'.
           05  WS-ADD                              PIC X.
               88 OK-TO-ADD                VALUE "Y".
               88 OK-TO-ADD2               VALUE "y".
       01 WS-DATE.
          05 WS-YEAR                               PIC 9(4).
          05 WS-MONTH                              PIC 99.
          05 WS-DAY                                PIC 99.
          05                                       PIC X(10).
       01  WS-VARS.
          05 ERR-MSG               PIC X(50)  VALUE SPACES.
          05 WS-ADD-MSG            PIC X(12)  VALUE "RECORD ADDED".
          05 WS-NO-ADD-MSG         PIC X(16)  VALUE "RECORD NOT ADDED".
          05 WS-ON-FILE-MSG        PIC X(16)  VALUE "RECORD EXISTS".
          05 WS-CONTINUE           PIC X.
          05 SOME-COLOR            PIC 9.
      
       SCREEN SECTION.
       01 SCR01                BLANK SCREEN
                               PROMPT
                               AUTO
                               REQUIRED
                               BACKGROUND-COLOR 0
                               FOREGROUND-COLOR 2.
          05 SCR1-R1.
             10  LINE 1 COL 1          VALUE "IDSCR".
             10         COL 36         VALUE "ADD BOOKS".
             10         COL 71     PIC 99 FROM WS-MONTH.
             10         COL 73         VALUE "/".
             10         COL 74     PIC 99 FROM WS-DAY.
             10         COL 76         VALUE "/".
             10         COL 77     PIC 9(4) FROM WS-YEAR.
          05 SCR1-R5.
             10 LINE 5  COL 10         VALUE "ENTER BOOK ID:".
             10         COL 25     PIC 9(13) TO BR-BOOK-ISBN.

       01 SCR02                FOREGROUND-COLOR 0.
          05 SCR2-R5.
             10 LINE 1 COL 1           VALUE "DATASCR".
             10 LINE 5 COL 1   ERASE EOL.
             10 LINE 5  COL 18     VALUE "ID:".
             10         COL 22     PIC 9(13) FROM BR-BOOK-ISBN.
          05 SCR2-R6.
             10 LINE 6  COL 15     VALUE "TITLE:".
             10         COL 22     PIC X(20) TO BR-BOOK-TITLE. 
          05 SCR2-R7.
             10 LINE 7  COL 11     VALUE "PUBLISHER:".
             10 LINE 7  COL 22     PIC X(20) TO BR-BOOK-PUBLISHER.
          05 SCR2-R8.
             10 LINE 8  COL 15     VALUE "PRICE:".
             10 LINE 8  COL 22     PIC 99999V99 TO BR-BOOK-PRICE.
          
          05 SCR2-R10.
             10 LINE 10 COL 15     VALUE "OK TO ADD?".
             10         COL 26     PIC X TO WS-ADD.
       01 SCR03                FOREGROUND-COLOR SOME-COLOR.
          05 LINE 23   COL 1   PIC X(16) FROM ERR-MSG.
          05           COL 18  VALUE "PRESS ENTER TO CONTINUE".
          05           COL 42  PIC X TO WS-CONTINUE.
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
      *     Enter BOOK id.  if all zeros then stop
      ***************************************************************
       200-PROCESS-FILE.
           DISPLAY SCR01
           ACCEPT SCR01
           IF BR-BOOK-ISBN NOT = ZEROS
              MOVE "N" TO WS-ADD
              PERFORM 300-ADD-BOOK THRU 300-EXIT
           ELSE
               MOVE "Y" TO WS-STOP-PROGRAM
           END-IF.
          
       200-EXIT.
           EXIT.
      
      ***************************************************************
      *     Check to see if record exists
      *************************************************************** 
       300-ADD-BOOK.
           READ BOOK-FILE
               INVALID key
                   PERFORM 400-ENTER-BOOK-DATA THRU 400-EXIT
               NOT INVALID KEY
                    MOVE WS-ON-FILE-MSG TO ERR-MSG
                    MOVE 4 TO SOME-COLOR
                    DISPLAY SCR03
                    ACCEPT  SCR03
               END-READ.
       300-EXIT.
           exit.
      
      ***************************************************************
      *   Enter book data and then decide to write or not  
      *************************************************************** 
       400-ENTER-BOOK-DATA.
           DISPLAY SCR02
           ACCEPT  SCR02
           IF OK-TO-ADD OR OK-TO-ADD2
               WRITE BOOK-REC
                  INVALID KEY 
                     MOVE 2 TO SOME-COLOR
                     MOVE WS-NO-ADD-MSG TO ERR-MSG
                  NOT INVALID KEY
                     MOVE 2 TO SOME-COLOR
                     MOVE WS-ADD-MSG TO ERR-MSG
               end-write
               DISPLAY SCR03
               ACCEPT  SCR03
           ELSE
               MOVE WS-NO-ADD-MSG TO ERR-MSG
               MOVE 2 TO SOME-COLOR
               DISPLAY SCR03
               ACCEPT SCR03
           END-IF.
       400-exit.
           exit.
              
      ***************************************************************
      *   Open the files.
      ***************************************************************
       900-INITIALIZATION.
           OPEN I-O BOOK-FILE
           MOVE FUNCTION current-date TO WS-DATE.
       900-EXIT.
           EXIT.

      *****************************************************************
      *   Close files.
      *****************************************************************
       910-END-PROGRAM-RTN.
            CLOSE BOOK-FILE.
       910-EXIT.
           EXIT.
