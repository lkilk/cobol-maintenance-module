       IDENTIFICATION DIVISION.
       PROGRAM-ID. main-program.
       DATA DIVISION.
       PROCEDURE DIVISION.
           DISPLAY "Day 0"
           DISPLAY "-----"
           CALL "SYSTEM" USING "cat in.dat"
           DISPLAY " "

           CALL "gilded-rose".


           DISPLAY "Day 1"
           DISPLAY "-----"
           CALL "SYSTEM" USING "cat out.dat"
           DISPLAY " "

           GOBACK.
