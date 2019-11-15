       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGLFILEPROCESSING AS "VSAM PT1".
       AUTHOR. COLE SQUIRE.
       DATE-WRITTEN. 02/19/2019.
       DATE-COMPILED. 05/08/2018.
      *******************************************************************
      * THIS FILE SECTION DEFINES THE FORMAT FOR CHART OF ACCOUNTS.
      * 
      * Input: C:\COBOL\coaFILE.DAT
      * 
      * Output: C:\COBOL\GCLPOSTFILERPT.DOC
      *
      * Date/Time Due: 02/18/2019
      * Date Assigned: 02/08/2019
      *******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CGL-COA-FILE ASSIGN TO UT-SYS-COA-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CGL-COA-TRANS-FILE ASSIGN TO UT-SYS-TRANS-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CGL-COA-SRT-FILE ASSIGN TO UT-SYS-SRT-FILE.
           SELECT CGL-NEW-COA-MASTER-FILE ASSIGN TO UT-SYS-NEW-MASTER-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CGLC-TRANS-LOG-FILE ASSIGN TO UT-SYS-TRANS-LOG-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CGL-COA-FILE
           RECORD CONTAINS 50 CHARACTERS.
       01  CGL-COA-REC.
           05  CGLC-NO                     PIC 9(4).
	       05  CGLC-BEG-BALANCE		       PIC S9(5)V99.
	       05  CGLC-CUR-BALANCE		       PIC S9(5)V99.
           05  CGLC-ACCT-ACTIVE	           PIC X.
 	       05  CGLC-ACCT-TYPE		       PIC X.
           05  CGLC-DESCRIPTION		       PIC X(30).
       FD CGL-COA-TRANS-FILE
           RECORD CONTAINS 51 CHARACTERS.
       01  CGLC-TRANS-REC.
           05  CGLC-TRANS-NO               PIC 9(4).
           05  CGLC-TRANS-BEG-BALANCE      PIC S9(5)V99.
	       05  CGLC-TRANS-CUR-BALANCE	   PIC S9(5)V99. 
           05  CGLC-TRANS-ACCT-ACTIVE	   PIC X.
 	       05  CGLC-TRANS-ACCT-TYPE		   PIC X.
           05  CGLC-TRANS-DESCRIPTION	   PIC X(30).
           05  CGLC-TRANS-TYPE             PIC X.
               88  COA-ADD                 VALUE "A".
               88  COA-UPDATE              VALUE "U".
               88  COA-DELETE              VALUE "D".
       FD  CGL-NEW-COA-MASTER-FILE
           RECORD CONTAINS 50 CHARACTERS.
       01  CGL-COA-MSTR-REC.
           05  CGLC-MSTR-NO                PIC 9(4).
	       05  CGLC-MSTR-BEG-BALANCE	   PIC S9(5)V99.
           05  CGLC-MSTR-CUR-BALANCE	   PIC S9(5)V99.
           05  CGLC-MSTR-ACCT-ACTIVE	   PIC X.
 	       05  CGLC-MSTR-ACCT-TYPE		   PIC X.
           05  CGLC-MSTR-DESCRIPTION	   PIC X(30).
       FD  CGLC-TRANS-LOG-FILE
           RECORD CONTAINS 65 CHARACTERS.
       01  CGLC-TRANS-LOG-REC.
           05  CGLC-TL-NO                  PIC 9(4).
           05  CGLC-TL-BEG-BALANCE	       PIC S9(5)V99.
	       05  CGLC-TL-CUR-BALANCE		   PIC S9(5)V99. 
	       05  CGLC-TL-ACCT-ACTIVE	       PIC X.
 	       05  CGLC-TL-ACCT-TYPE		   PIC X.
           05  CGLC-TL-DESCRIPTION		   PIC X(30).
           05  CGLC-TL-TRANS-TYPE          PIC X.
               88  COA-TL-ADD              VALUE "A".
               88  COA-TL-UPDATE           VALUE "U".
               88  COA-TL-DELETE           VALUE "D".
           05  CGLC-TL-TRANS-DESC          PIC X(14).
               88  ADD-ERROR               VALUE "ADD ERROR".
               88  DEL-ERROR               VALUE "DELETE ERROR".
               88  UPD-ERROR               VALUE "UPDATE ERROR".
               88  ADDED                   VALUE "RECORD ADDED".
               88  DELETED                 VALUE "RECORD DELETED".
               88  UPDATED                 VALUE "RECORD UPDATED".
               88  NO-CHANGE               VALUE "NO CHANGE".
       SD  CGL-COA-SRT-FILE
           RECORD CONTAINS 50 CHARACTERS.
       01 CGL-COA-SORT-REC.
           05  CGLC-SRT-NO                 PIC 9(4).
	       05  CGLC-SRT-BEG-BALANCE		   PIC S9(5)V99.
	       05  CGLC-SRT-CUR-BALANCE		   PIC S9(5)V99. 
           05  CGLC-SRT-ACCT-ACTIVE	       PIC X.
 	       05  CGLC-SRT-ACCT-TYPE		   PIC X.
           05  CGLC-SRT-DESCRIPTION		   PIC X(30).
       WORKING-STORAGE SECTION.
       01  WS-FILES.
           05  UT-SYS-COA-FILE              PIC X(43)
                                       VALUE "J:\CS3530\COAFILE.DAT".
           05  UT-SYS-TRANS-FILE           PIC X(60)
                                       VALUE "J:\CS3530\COATRANSFILE.DAT".
           05  UT-SYS-NEW-MASTER-FILE      PIC X(43)
                                       VALUE "J:\CS3530\COAFILEMASTER.DAT".
           05  UT-SYS-TRANS-LOG-FILE      PIC X(58)
                                       VALUE "J:\CS3530\COATRANSLOGFILE.DAT".
       PROCEDURE DIVISION.
      *****************************************************************
      * 
      *****************************************************************
       100-MAIN.
           PERFORM 200-SORT-COA THRU 200-SORT-COA-EXIT
           OPEN INPUT CGL-COA-FILE
                      CGL-COA-TRANS-FILE
           OPEN OUTPUT CGL-NEW-COA-MASTER-FILE
                       CGLC-TRANS-LOG-FILE
           PERFORM 900-READ-COA THRU 900-READ-COA-EXIT
           PERFORM 1000-READ-TRANS THRU 1000-READ-TRANS-EXIT
           PERFORM 300-PROCESS-TRANS THRU 300-PROCESS-TRANS-EXIT
               UNTIL CGLC-NO = 9999
               AND CGLC-TRANS-NO = 9999
           STOP RUN.

      *****************************************************************
      * 
      *****************************************************************
       200-SORT-COA.
           SORT CGL-COA-SRT-FILE
               ON ASCENDING KEY CGLC-SRT-NO
               USING CGL-COA-FILE
               GIVING CGL-COA-FILE.
       200-SORT-COA-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       300-PROCESS-TRANS.
           EVALUATE TRUE
               WHEN CGLC-NO = CGLC-TRANS-NO
                   PERFORM 500-UPDATE-TEST
               WHEN CGLC-NO > CGLC-TRANS-NO
                   PERFORM 400-ADD-ACCOUNT
               WHEN OTHER
                   PERFORM 600-WRITE-ACCOUNT
           END-EVALUATE.
       300-PROCESS-TRANS-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       400-ADD-ACCOUNT.
           IF COA-ADD
               MOVE CGLC-TRANS-NO TO CGLC-MSTR-NO
               MOVE CGLC-TRANS-BEG-BALANCE TO CGLC-MSTR-BEG-BALANCE
	           MOVE CGLC-TRANS-CUR-BALANCE TO CGLC-MSTR-CUR-BALANCE 
               MOVE CGLC-TRANS-ACCT-ACTIVE TO CGLC-MSTR-ACCT-ACTIVE
 	           MOVE CGLC-TRANS-ACCT-TYPE TO CGLC-MSTR-ACCT-TYPE
               MOVE CGLC-TRANS-DESCRIPTION TO CGLC-MSTR-DESCRIPTION
               WRITE CGL-COA-MSTR-REC
               PERFORM 1100-WRITE-TRANS-TL THRU 1100-WRITE-TRANS-TL-EXIT
           ELSE
               PERFORM 700-ERROR-RTN THRU 700-ERROR-RTN-EXIT
           END-IF
               
           PERFORM 1000-READ-TRANS THRU 1000-READ-TRANS-EXIT.
       400-ADD-CUST-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       500-UPDATE-TEST.
           EVALUATE TRUE
               WHEN COA-DELETE
                   PERFORM 1100-WRITE-TRANS-TL
                   PERFORM 900-READ-COA THRU 900-READ-COA-EXIT
                   PERFORM 1000-READ-TRANS THRU 1000-READ-TRANS-EXIT
                   CONTINUE
               WHEN COA-ADD
                   PERFORM 700-ERROR-RTN
                   PERFORM 1000-READ-TRANS THRU 1000-READ-TRANS-EXIT
               WHEN COA-UPDATE
                   PERFORM 800-UPDATE-RECORD
                   PERFORM 900-READ-COA THRU 900-READ-COA-EXIT
                   PERFORM 1000-READ-TRANS THRU 1000-READ-TRANS-EXIT
            END-EVALUATE.
       500-UPDATE-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       600-WRITE-ACCOUNT.
            MOVE CGLC-NO TO CGLC-MSTR-NO
            MOVE CGLC-BEG-BALANCE TO CGLC-MSTR-BEG-BALANCE
	        MOVE CGLC-CUR-BALANCE TO CGLC-MSTR-CUR-BALANCE 
            MOVE CGLC-ACCT-ACTIVE TO CGLC-MSTR-ACCT-ACTIVE
 	        MOVE CGLC-ACCT-TYPE TO CGLC-MSTR-ACCT-TYPE
            MOVE CGLC-DESCRIPTION TO CGLC-MSTR-DESCRIPTION
            WRITE CGL-COA-MSTR-REC
            PERFORM 1200-WRITE-COA-TL THRU 1200-WRITE-COA-TL-EXIT
            PERFORM 900-READ-COA THRU 900-READ-COA-EXIT.
       600-WRITE-ACCOUNT-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       700-ERROR-RTN.
           IF COA-ADD
                MOVE "ADD ERROR" TO CGLC-TL-TRANS-DESC
            ELSE IF COA-DELETE
                MOVE "DELETE ERROR" TO CGLC-TL-TRANS-DESC
            ELSE IF COA-UPDATE
                MOVE "UPDATE ERROR" TO CGLC-TL-TRANS-DESC
            END-IF
            MOVE CGLC-TRANS-NO TO CGLC-TL-NO
            MOVE CGLC-TRANS-BEG-BALANCE TO CGLC-TL-BEG-BALANCE
	        MOVE CGLC-TRANS-CUR-BALANCE TO CGLC-TL-CUR-BALANCE 
            MOVE CGLC-TRANS-ACCT-ACTIVE TO CGLC-TL-ACCT-ACTIVE
 	        MOVE CGLC-TRANS-ACCT-TYPE TO CGLC-TL-ACCT-TYPE
            MOVE CGLC-TRANS-DESCRIPTION TO CGLC-TL-DESCRIPTION
            MOVE CGLC-TRANS-TYPE TO CGLC-TL-TRANS-TYPE
            WRITE CGLC-TRANS-LOG-REC.

       700-ERROR-RTN-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       800-UPDATE-RECORD.
            MOVE CGLC-NO TO CGLC-MSTR-NO
            MOVE CGLC-TRANS-BEG-BALANCE TO CGLC-MSTR-BEG-BALANCE
	        MOVE CGLC-TRANS-CUR-BALANCE TO CGLC-MSTR-CUR-BALANCE 
            MOVE CGLC-TRANS-ACCT-ACTIVE TO CGLC-MSTR-ACCT-ACTIVE
 	        MOVE CGLC-TRANS-ACCT-TYPE TO CGLC-MSTR-ACCT-TYPE
            MOVE CGLC-TRANS-DESCRIPTION TO CGLC-MSTR-DESCRIPTION
            WRITE CGL-COA-MSTR-REC.
            PERFORM 1100-WRITE-TRANS-TL THRU 1100-WRITE-TRANS-TL-EXIT.
       800-UPDATE-RECORD-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       900-READ-COA.
           READ CGL-COA-FILE
               AT END MOVE 99999 TO CGLC-NO
           END-READ.
       900-READ-COA-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       1000-READ-TRANS.
           READ CGL-COA-TRANS-FILE
               AT END MOVE 99999 TO CGLC-TRANS-NO
           END-READ.
       1000-READ-TRANS-EXIT.
           EXIT.

      *****************************************************************
      * 
      *****************************************************************
       1100-WRITE-TRANS-TL.
            MOVE CGLC-TRANS-NO TO CGLC-TL-NO
            MOVE CGLC-TRANS-BEG-BALANCE TO CGLC-TL-BEG-BALANCE
	        MOVE CGLC-TRANS-CUR-BALANCE TO CGLC-TL-CUR-BALANCE 
            MOVE CGLC-TRANS-ACCT-ACTIVE TO CGLC-TL-ACCT-ACTIVE
 	        MOVE CGLC-TRANS-ACCT-TYPE TO CGLC-TL-ACCT-TYPE
            MOVE CGLC-TRANS-DESCRIPTION TO CGLC-TL-DESCRIPTION
            MOVE CGLC-TRANS-TYPE TO CGLC-TL-TRANS-TYPE
            IF COA-ADD
                MOVE "RECORD ADDED" TO CGLC-TL-TRANS-DESC
            ELSE IF COA-DELETE
                MOVE "RECORD DELETED" TO CGLC-TL-TRANS-DESC
            ELSE IF COA-UPDATE
                MOVE "RECORD UPDATED" TO CGLC-TL-TRANS-DESC
            END-IF
            WRITE CGLC-TRANS-LOG-REC.
       1100-WRITE-TRANS-TL-EXIT.
           EXIT.

      *****************************************************************  
      * 
      *****************************************************************
       1200-WRITE-COA-TL.
            MOVE CGLC-NO TO CGLC-TL-NO
            MOVE CGLC-BEG-BALANCE TO CGLC-TL-BEG-BALANCE
	        MOVE CGLC-CUR-BALANCE TO CGLC-TL-CUR-BALANCE 
            MOVE CGLC-ACCT-ACTIVE TO CGLC-TL-ACCT-ACTIVE
 	        MOVE CGLC-ACCT-TYPE TO CGLC-TL-ACCT-TYPE
            MOVE CGLC-DESCRIPTION TO CGLC-TL-DESCRIPTION
            MOVE " " TO CGLC-TL-TRANS-TYPE
            MOVE "NO CHANGE" TO CGLC-TL-TRANS-DESC
            WRITE CGLC-TRANS-LOG-REC.
       1200-WRITE-COA-TL-EXIT.
           EXIT.