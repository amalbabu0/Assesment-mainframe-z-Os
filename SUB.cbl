       *****************************************************************
       * PROGRAM NAME : CA21G086
       * DESCRIPTION  : Calculates percentage from marks
       *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CA21G086.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPS PIC 9(2)V9(2).

       LINKAGE SECTION.
       01 LK-MARKS PIC 99.99.
       01 LK-PERC  PIC 9(2).

       PROCEDURE DIVISION USING LK-MARKS LK-PERC.

           MOVE LK-MARKS(1:2) TO WS-OPS(1:2)
           MOVE LK-MARKS(4:2) TO WS-OPS(3:2)

           IF WS-OPS > 0
               COMPUTE LK-PERC ROUNDED =
                   (WS-OPS / 50) * 100
           ELSE
               MOVE 0 TO LK-PERC
           END-IF

           GOBACK.
