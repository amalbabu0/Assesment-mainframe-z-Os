       IDENTIFICATION DIVISION.
       PROGRAM-ID. CA11G086.
       AUTHOR. ASSESMENT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT INFILE ASSIGN TO DD1
                   ORGANIZATION IS SEQUENTIAL
                   ACCESS MODE IS SEQUENTIAL
                   FILE STATUS WS-FS1.
            SELECT OUTFILE ASSIGN TO DD2
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS O-STID
                   FILE STATUS WS-FS2.
       DATA DIVISION.
       FILE SECTION.
       FD INFILE.
       01 INREC.
          05 I-STID                   PIC 9(5).
          05 FILLER                   PIC X.
          05 I-FNAME                  PIC X(10).
          05 FILLER                   PIC X.
          05 I-LNAME                  PIC X(10).
          05 FILLER                   PIC X.
          05 I-MARKS                  PIC 99.99.
          05 FILLER                   PIC X(47).
       FD OUTFILE.
       01 OUTREC.
          05 O-STID                   PIC X(6).
          05 FILLER                   PIC X.
          05 O-NEWNAME                PIC X(15).
          05 FILLER                   PIC X.
          05 O-MARKS                  PIC 99.99.
          05 FILLER                   PIC X.
          05 O-PERC                   PIC 9(2).
          05 FILLER                   PIC X.
          05 O-RESULT                 PIC X(30).
          05 FILLER                   PIC X(18).
       WORKING-STORAGE SECTION.
       01 WS-FS1                      PIC 99.
          88 FS1-OK                             VALUE 00.
          88 FS1-EOF                            VALUE 10.
          88 FS1-NOTFOUND                       VALUE 35.
       01 WS-FS2                      PIC 99.
          88 FS2-OK                             VALUE 00.
          88 FS2-EOF                            VALUE 10.
          88 FS2-NOTFOUND                       VALUE 35.
       01 WS-NAME                     PIC X(10).
       01 WS-RECN                     PIC 999.
       01 WS-MARKS                    PIC 99.99.
       01 WS-PERC                     PIC 9(2).
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
            PERFORM 1000-INIT-PARA
               THRU 1000-INIT-EXIT.
            PERFORM 2000-PROCESS-PARA
               THRU 2000-PROCESS-EXIT.
            PERFORM 9000-TERM-PARA
               THRU 9000-TERM-EXIT.
       0000-MAIN-EXIT.
            EXIT.
       1000-INIT-PARA.
            INITIALIZE WS-FS1 WS-FS2 WS-NAME.
       1000-INIT-EXIT.
            EXIT.
       2000-PROCESS-PARA.
            PERFORM 2100-OPEN-PARA
               THRU 2100-OPEN-EXIT.
            PERFORM 2200-READ-PARA
               THRU 2200-READ-EXIT UNTIL FS1-EOF.
            PERFORM 2300-CLOSE-PARA
               THRU 2300-CLOSE-EXIT.
       2000-PROCESS-EXIT.
            EXIT.
       9000-TERM-PARA.
            EXIT.
       9000-TERM-EXIT.
            STOP RUN.
       2100-OPEN-PARA.
            OPEN INPUT INFILE.
            EVALUATE TRUE
                WHEN FS1-OK
                     DISPLAY "OPEN FS1 SUCCES"
                WHEN OTHER
                     DISPLAY "OPEN ERROR" WS-FS1
            END-EVALUATE.
            OPEN OUTPUT OUTFILE.
            MOVE SPACES TO OUTREC.
            EVALUATE TRUE
                WHEN FS2-OK
                     DISPLAY "OPEN FS2 SUCCES"
                WHEN OTHER
                     DISPLAY "OPEN ERROR" WS-FS2
            END-EVALUATE.
       2100-OPEN-EXIT.
            EXIT.
       2200-READ-PARA.
            READ INFILE.
            EVALUATE TRUE
                WHEN FS1-OK
                     ADD 1 TO WS-RECN
                     PERFORM 2210-VALIDATE-PARA
                     THRU 2210-VALIDATE-EXIT
                     DISPLAY "READ REC  :" WS-RECN " SUCCES "
                WHEN FS1-EOF
                     DISPLAY "READ END WITH TOTAL : " WS-RECN
                WHEN OTHER
                     DISPLAY "READ ERROR" WS-FS1
            END-EVALUATE.
       2200-READ-EXIT.
            EXIT.
       2300-CLOSE-PARA.
            CLOSE INFILE.
            EVALUATE TRUE
                WHEN FS1-OK
                     DISPLAY "CLOSE FS1 SUCCES"
                WHEN OTHER
                     DISPLAY "CLOSE ERROR" WS-FS1
            END-EVALUATE.
            CLOSE OUTFILE.
            EVALUATE TRUE
                WHEN FS2-OK
                     DISPLAY "CLOSE FS2 SUCCES"
                WHEN OTHER
                     DISPLAY "CLOSE ERROR" WS-FS2
            END-EVALUATE.
       2300-CLOSE-EXIT.
            EXIT.
       2210-VALIDATE-PARA.
              EVALUATE TRUE
               WHEN (I-MARKS(1:2) IS NUMERIC
                AND I-MARKS(4:2) IS NUMERIC )
                  AND ( I-STID IS NUMERIC )
                    AND ( I-LNAME IS ALPHABETIC )
                    AND ( I-FNAME NOT = SPACE )
                           PERFORM 2211-STR-PARA
                              THRU 2211-STR-EXIT
                           PERFORM 2212-MOVE-PARA
                              THRU 2212-MOVE-EXIT
                   WHEN OTHER
                        DISPLAY "ERROR RECORD  "
                        CONTINUE
               END-EVALUATE.
       2210-VALIDATE-EXIT.
            EXIT.
       2211-STR-PARA.
            STRING "S"    DELIMITED BY SIZE
                   I-STID DELIMITED BY SIZE
              INTO O-STID
            END-STRING.
       2211-STR-EXIT.
            EXIT.
       2212-MOVE-PARA.
            MOVE I-FNAME TO WS-NAME.
            INSPECT WS-NAME
                REPLACING ALL "@" BY SPACE
                          ALL "$" BY SPACE
                          ALL "%" BY SPACE
                          ALL "&" BY SPACE.
            MOVE I-LNAME TO O-NEWNAME.
            STRING O-NEWNAME(1:1) DELIMITED BY SIZE
                   "."            DELIMITED BY SIZE
                   WS-NAME        DELIMITED BY SIZE
              INTO O-NEWNAME.
            MOVE I-MARKS TO O-MARKS.
            MOVE I-MARKS TO WS-MARKS.
            CALL "CA21G086" USING WS-MARKS WS-PERC.
            MOVE WS-PERC TO O-PERC.
            EVALUATE TRUE
                WHEN ( WS-PERC > 70 ) OR ( WS-PERC = 70 )
                     MOVE "CONGRATULATIONS!!!" TO O-RESULT
                WHEN WS-PERC < 70
                     MOVE "BETTER LUCK NEXT TIME!!!" TO O-RESULT
            END-EVALUATE.
            WRITE OUTREC.
            EVALUATE TRUE
                WHEN FS2-OK
                     DISPLAY "WRITE REC :" WS-RECN " SUCCES "
                WHEN OTHER
                     DISPLAY "WRITE ERROR" WS-FS2
            END-EVALUATE.
       2212-MOVE-EXIT.
            EXIT.
