       *****************************************************************
       * PROGRAM NAME : CA11G086
       * AUTHOR       : ASSESSMENT
       * DESCRIPTION  : Reads student records from sequential file,
       *                validates data, formats name, calls subprogram
       *                to calculate percentage, and writes to KSDS.
       *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CA11G086.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
               ASSIGN TO DD1
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS1.

           SELECT OUTFILE
               ASSIGN TO DD2
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS O-STID
               FILE STATUS IS WS-FS2.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE.
       01  INREC.
           05 I-STID        PIC 9(5).
           05 FILLER        PIC X.
           05 I-FNAME       PIC X(10).
           05 FILLER        PIC X.
           05 I-LNAME       PIC X(10).
           05 FILLER        PIC X.
           05 I-MARKS       PIC 99.99.
           05 FILLER        PIC X(47).

       FD  OUTFILE.
       01  OUTREC.
           05 O-STID        PIC X(6).
           05 FILLER        PIC X.
           05 O-NEWNAME     PIC X(15).
           05 FILLER        PIC X.
           05 O-MARKS       PIC 99.99.
           05 FILLER        PIC X.
           05 O-PERC        PIC 9(2).
           05 FILLER        PIC X.
           05 O-RESULT      PIC X(30).
           05 FILLER        PIC X(18).

       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUS.
          05 WS-FS1         PIC 99.
             88 FS1-OK      VALUE 00.
             88 FS1-EOF     VALUE 10.
          05 WS-FS2         PIC 99.
             88 FS2-OK      VALUE 00.

       01 WS-VARIABLES.
          05 WS-NAME        PIC X(10).
          05 WS-RECN        PIC 999 VALUE 0.
          05 WS-MARKS       PIC 99.99.
          05 WS-PERC        PIC 9(2).

       01 WS-CONSTANTS.
          05 WS-PASS-PERC   PIC 9(2) VALUE 70.

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-PROCESS
           PERFORM 9000-TERMINATE
           STOP RUN.

       1000-INIT.
           INITIALIZE WS-FILE-STATUS
                      WS-VARIABLES
                      OUTREC.

       2000-PROCESS.
           PERFORM 2100-OPEN
           PERFORM UNTIL FS1-EOF
               PERFORM 2200-READ
           END-PERFORM
           PERFORM 2300-CLOSE.

       2100-OPEN.
           OPEN INPUT INFILE
                OUTPUT OUTFILE.

           IF FS1-OK
              DISPLAY "INFILE OPEN SUCCESS"
           ELSE
              DISPLAY "INFILE OPEN ERROR " WS-FS1
           END-IF.

           IF FS2-OK
              DISPLAY "OUTFILE OPEN SUCCESS"
           ELSE
              DISPLAY "OUTFILE OPEN ERROR " WS-FS2
           END-IF.

       2200-READ.
           READ INFILE
               AT END
                   SET FS1-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-RECN
                   PERFORM 2210-VALIDATE
           END-READ.

       2210-VALIDATE.
           EVALUATE TRUE
               WHEN I-STID IS NUMERIC
                AND I-LNAME IS ALPHABETIC
                AND I-FNAME NOT = SPACES
                AND I-MARKS(1:2) IS NUMERIC
                AND I-MARKS(4:2) IS NUMERIC
                   PERFORM 2220-BUILD-RECORD
               WHEN OTHER
                   DISPLAY "INVALID RECORD : " WS-RECN
           END-EVALUATE.

       2220-BUILD-RECORD.
           STRING "S"
                  I-STID
              INTO O-STID
           END-STRING.

           MOVE I-FNAME TO WS-NAME.
           INSPECT WS-NAME
               REPLACING ALL "@"
                         BY SPACE
                         ALL "$"
                         BY SPACE
                         ALL "%"
                         BY SPACE
                         ALL "&"
                         BY SPACE.

           MOVE I-LNAME TO O-NEWNAME.
           STRING O-NEWNAME(1:1)
                  "."
                  WS-NAME
              INTO O-NEWNAME
           END-STRING.

           MOVE I-MARKS TO O-MARKS
                           WS-MARKS.

           CALL "CA21G086"
               USING WS-MARKS WS-PERC.

           MOVE WS-PERC TO O-PERC.

           IF WS-PERC >= WS-PASS-PERC
               MOVE "CONGRATULATIONS!!!"
                   TO O-RESULT
           ELSE
               MOVE "BETTER LUCK NEXT TIME!!!"
                   TO O-RESULT
           END-IF.

           WRITE OUTREC.

       2300-CLOSE.
           CLOSE INFILE
                 OUTFILE.

       9000-TERMINATE.
           DISPLAY "TOTAL RECORDS PROCESSED : " WS-RECN.
