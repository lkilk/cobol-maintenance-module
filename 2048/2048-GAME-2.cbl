       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2048-GAME.

      * AUTHOR:    Valdis Grinbergs (vgrin)
      * BASED ON http://gabrielecirulli.github.io/2048/
      * ORIGINAL JAVASCRIPT SOURCE CODE AT:
      * https://github.com/gabrielecirulli/2048
      *
      * GAME PLAY:
      * EACH TURN SLIDE ALL TILES ON GRID IN ONE OF FOUR
      * DIRECTIONS.  IF TWO TILES WITH THE SAME VALUE
      * COLLIDE, THEY ARE REPLACED WITH ONE TILE WITH
      * DOUBLE THE VALUE.  EACH TURN A NEW TILE IS ADDED
      * TO THE GRID.
      * GAME IS WON IF CREATE A TILE WITH THE VALUE 2048
      * (2 TO THE 11TH POWER).  GAME IS LOST IF ALL SPACES
      * IN THE GRID ARE FILLED.
      *
      * PARAGRAPHS SLIDE-RIGHT SLIDE-LEFT SLIDE-UP SLIDE-DOWN
      * ARE SIMILAR AND COULD BE REPLACED BY ONE PARAGRAPH
      * CALLED FOUR DIFFERENT WAYS.  HOWEVER, THEY ARE KEPT
      * SEPARATE BECAUSE IT MIGHT MAKE THE CODE EASIER TO
      * UNDERSTAND AND IT DOES NOT CREATE TOO MUCH DUPLICATION.
      *
      * LICENSE:
      * 2048-GAME is free software: you can redistribute it and/or
      * modify it under the terms of the GNU General Public License as
      * published by the Free Software Foundation, either version 3 of
      * the License, or (at your option) any later version.
      *
      * 2048-GAME is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the GNU General Public
      * License along with 2048-GAME.
      * If not, see <http://www.gnu.org/licenses/>.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT OPTIONAL CONTROL-FILE
           ASSIGN TO '.2048-CONTROLS'
           FILE STATUS IS CONTROL-FILE-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.
      * USE A FILE NAME WITH A . PREFIX TO CREATE A HIDDEN FILE IN LINUX

       SELECT OPTIONAL HIGH-SCORE-FILE
           ASSIGN TO '.2048-HIGH-SCORES'
           FILE STATUS IS HIGH-SCORE-FILE-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.
      * USE A FILE NAME WITH A . PREFIX TO CREATE A HIDDEN FILE IN LINUX

       DATA DIVISION.
       FILE SECTION.

       FD  CONTROL-FILE.
       01  CONTROL-RECORD.
           05  CONTROL-RECORD-UP            PIC X.
           05  CONTROL-RECORD-DOWN          PIC X.
           05  CONTROL-RECORD-RIGHT         PIC X.
           05  CONTROL-RECORD-LEFT          PIC X.

       FD  HIGH-SCORE-FILE.
       01  HIGH-SCORE-RECORD                PIC X(23).

       WORKING-STORAGE SECTION.
       01  NUM-COLOURS.
           05 NUM-COLOUR OCCURS 36 TIMES   
           ASCENDING KEY IS COLOUR 
           INDEXED BY COLOUR-IDX.
               10 COLOUR PIC 9 VALUE 7.

       01  COUNTER PIC 99.



       01  GAME-STATUS                      PIC X VALUE 'P'.
           88  GAME-OVER                    VALUE 'Q' 'H'.
           88  GAME-QUIT                    VALUE 'Q'.
           88  HOLD-DISPLAY                 VALUE 'H'.

       01  EMPTY-COUNT                      PIC 99 USAGE COMP.
       01  RANDOM-NUMBER                    PIC 99 USAGE COMP.
       01  CELL-COUNT                       PIC 99 USAGE COMP.
       01  USER-ENTRY                       PIC X(10).
       01  USER-COMMAND                     PIC X.
           88  USER-CHOSE-QUIT              VALUE 'Q'.
           88  USER-CHOSE-SET-COMMANDS      VALUE 'C'.
           88  USER-CHOSE-HIGH-SCORES       VALUE 'H'.

       01  GRID-DIMENSIONS.
      * CHANGING THE DIMENSIONS OF GAME-GRID WOULD REQUIRE
      * MANY OTHER CHANGES IN THE SOURCE CODE.
      * THESE VALUES ARE USED TO TEST FOR AN INCOMPLETE
      * CHANGE TO THE SIZE OF THE GAME-GRID.  CHANGING ONLY
      * THE GRID AND THESE DIMENSIONS IS NOT SUFFICIENT.
           05  NUMBER-OF-ROWS               PIC 9 VALUE 6.
           05  NUMBER-OF-COLS               PIC 9 VALUE 6.

       01  GAME-GRID.
           05  GRID-ROW OCCURS 6 TIMES INDEXED BY ROW-INDEX.
               10  GRID-COL OCCURS 6 TIMES INDEXED BY COL-INDEX.
                   15  GRID-CELL            PIC 999999 VALUE ZERO.

      * UNSUCCESSFUL ATTEMPT TO ADD COLORS
      * SEE ERROR MESSAGE IN SCREEN SECTION
      *01  GRID-COLORS.
      *    05  COLOR-ROW OCCURS 4 TIMES.
      *        10  COLOR-COL OCCURS 4 TIMES.
      *            15  COLOR-CELL           PIC 9 VALUE 1.

       01  UPDATED-GROUP.
           05  UPDATED-SET OCCURS 6 TIMES INDEXED BY UPDATED-INDEX
                                            PIC 999999 VALUE ZERO.
       01  PRIOR-TILE                       PIC 999999 VALUE ZERO.

       01  CONTROL-FILE-STATUS              PIC 99.
           88  CONTROL-FILE-EXISTS          VALUE 00.
           88  CONTROL-FILE-OK              VALUE 00.
           88  CONTROL-FILE-OK-OR-NEW       VALUE 00 05.

       01  HIGH-SCORE-FILE-STATUS           PIC 99.
           88  HIGH-SCORE-FILE-EXISTS       VALUE 00.
           88  HIGH-SCORE-FILE-OK           VALUE 00.
           88  HIGH-SCORE-NO-MORE-RECORDS   VALUE 10.
           88  HIGH-SCORE-FILE-OK-OR-EOF    VALUE 00 10.
           88  HIGH-SCORE-FILE-OK-OR-NEW    VALUE 00 05.

       01  ARE-TILE-NOT-NEEDED              PIC X VALUE 'N'.
           88  TILE-NOT-NEEDED              VALUE 'Y'.
           88  TILE-NEEDED                  VALUE 'N'.

       01  CONTROL-VALUES.
           05  CONTROL-UP                   PIC X.
           05  CONTROL-DOWN                 PIC X.
           05  CONTROL-LEFT                 PIC X.
           05  CONTROL-RIGHT                PIC X.

       01  GRID-MESSAGES.
           05  WIN-MESSAGE                  PIC X(47)
               VALUE 'YOU REACHED 2048 AND WON! (PRESS ENTER TO EXIT)'.
           05  NO-MOVES-MESSAGE             PIC X(47)
               VALUE 'NO MOVES LEFT'.

       01  HIGH-SCORES.
           05  HS-TABLE OCCURS 1 TO 5 TIMES DEPENDING ON HS-COUNT
               INDEXED BY HS-INDEX.
               10  HS-SCORE                 PIC 9(4).
               10  FILLER                   PIC X(3) VALUE SPACES.
               10  HS-TEXT                  PIC X(16).

       01  HS-COUNT                         PIC 9.
       01  CURRENT-SCORE                    PIC 9(4).

       01  HIGH-SCORE-ENTRY.
           05  HS-NUMBER                    PIC 9(4).
           05  FILLER                       PIC X(3) VALUE SPACES.
           05  HS-YEAR                      PIC X(4).
           05  FILLER                       PIC X VALUE '-'.
           05  HS-MONTH                     PIC X(2).
           05  FILLER                       PIC X VALUE '-'.
           05  HS-DAY                       PIC X(2).
           05  FILLER                       PIC X VALUE ' '.
           05  HS-HOUR                      PIC X(2).
           05  FILLER                       PIC X VALUE ':'.
           05  HS-MINUTE                    PIC X(2).

       01  CURRENT-DATE.
           05  CD-YEAR                      PIC 9(4).
           05  CD-MONTH                     PIC 99.
           05  CD-DAY                       PIC 99.

       01  CURRENT-TIME.
           05  CT-HOUR                      PIC 99.
           05  CT-MINUTE                    PIC 99.
           05  CT-SECOND                    PIC 99.
           05  CT-HUNDREDTHS-OF-SECOND      PIC 99.

       SCREEN SECTION.

       01  GAME-GRID-SCREEN.
           05  BLANK SCREEN.
           05  GRID-SCREEN.
               10  LINE 2 COLUMN 5 VALUE '**********|' FOREGROUND-COLOR 
               IS 6.
               10  LINE 2 COLUMN 16 VALUE '2' HIGHLIGHT, 
               FOREGROUND-COLOR IS 2.
               10  LINE 2 COLUMN 17 VALUE '0' HIGHLIGHT, 
               FOREGROUND-COLOR IS 3.
               10  LINE 2 COLUMN 18 VALUE '4' HIGHLIGHT, 
               FOREGROUND-COLOR IS 4.
               10  LINE 2 COLUMN 19 VALUE '8' HIGHLIGHT, 
               FOREGROUND-COLOR IS 5.
               10  LINE 2 COLUMN 20 VALUE '|**********' FOREGROUND-COLOR 
               IS 6.
      * FOLLOWING ATTEMPT TO INCLUDE COLORS DID NOT WORK
      * ERROR MESSAGE:
      * In file included from /tmp/cob6562_0.c:61:0:
      * /tmp/cob6562_0.c.l.h: In function ‘_32048__GAME_’:
      * /tmp/cob6562_0.c.l.h:60:66: error: ‘f0’ undeclared
      * (first use in this function)
      * /tmp/cob6562_0.c.l.h:60:66: note: each undeclared identifier
      * is reported only once for each function it appears in
      *
      * COMPILE TIME ERROR USING GNUCOBOL 1.1 ON DEBIAN
      * SIMILAR ERROR USING GNUCOBOL 1.1 ON CYGWIN
      *
      *        10  GRID-DISPLAY-11 LINE 4 COLUMN 2 PIC ZZZ9
      *            FROM GRID-CELL (1, 1)
      *            FOREGROUND-COLOR COLOR-CELL (1, 1).

               10  GRID-DISPLAY-12 LINE 4 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (1, 1)
                   FOREGROUND-COLOR IS COLOUR(1).    
               10  GRID-DISPLAY-12 LINE 4 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (1, 1)
                   FOREGROUND-COLOR IS COLOUR(1).         
               10  GRID-DISPLAY-12 LINE 4 COLUMN 7 PIC ZZZ9
                   FROM GRID-CELL (1, 2)
                   FOREGROUND-COLOR IS COLOUR(2).
               10  GRID-DISPLAY-13 LINE 4 COLUMN 12 PIC ZZZ9
                   FROM GRID-CELL (1, 3)
                   FOREGROUND-COLOR IS COLOUR(3).
               10  GRID-DISPLAY-14 LINE 4 COLUMN 17 PIC ZZZ9
                   FROM GRID-CELL (1, 4)
                   FOREGROUND-COLOR IS COLOUR(4).
               10  GRID-DISPLAY-15 LINE 4 COLUMN 22 PIC ZZZ9
                   FROM GRID-CELL (1, 5)
                   FOREGROUND-COLOR IS COLOUR(5).
               10  GRID-DISPLAY-16 LINE 4 COLUMN 27 PIC ZZZ9
                   FROM GRID-CELL (1, 6)
                   FOREGROUND-COLOR IS COLOUR(6).
               10  GRID-DISPLAY-21 LINE 6 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (2, 1)
                   FOREGROUND-COLOR IS COLOUR(7).
               10  GRID-DISPLAY-22 LINE 6 COLUMN 7 PIC ZZZ9
                   FROM GRID-CELL (2, 2)
                   FOREGROUND-COLOR IS COLOUR(8).
               10  GRID-DISPLAY-23 LINE 6 COLUMN 12 PIC ZZZ9
                   FROM GRID-CELL (2, 3)
                   FOREGROUND-COLOR IS COLOUR(9).
               10  GRID-DISPLAY-24 LINE 6 COLUMN 17 PIC ZZZ9
                   FROM GRID-CELL (2, 4)
                   FOREGROUND-COLOR IS COLOUR(10).
               10  GRID-DISPLAY-25 LINE 6 COLUMN 22 PIC ZZZ9
                   FROM GRID-CELL (2, 5)
                   FOREGROUND-COLOR IS COLOUR(11).
               10  GRID-DISPLAY-26 LINE 6 COLUMN 27 PIC ZZZ9
                   FROM GRID-CELL (2, 6)
                   FOREGROUND-COLOR IS COLOUR(12).
               10  GRID-DISPLAY-31 LINE 8 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (3, 1)
                   FOREGROUND-COLOR IS COLOUR(13).
               10  GRID-DISPLAY-32 LINE 8 COLUMN 7 PIC ZZZ9
                   FROM GRID-CELL (3, 2)
                   FOREGROUND-COLOR IS COLOUR(14).
               10  GRID-DISPLAY-33 LINE 8 COLUMN 12 PIC ZZZ9
                   FROM GRID-CELL (3, 3)
                   FOREGROUND-COLOR IS COLOUR(15).
               10  GRID-DISPLAY-34 LINE 8 COLUMN 17 PIC ZZZ9
                   FROM GRID-CELL (3, 4)
                   FOREGROUND-COLOR IS COLOUR(16).
               10  GRID-DISPLAY-35 LINE 8 COLUMN 22 PIC ZZZ9
                   FROM GRID-CELL (3, 5)
                   FOREGROUND-COLOR IS COLOUR(17).
               10  GRID-DISPLAY-36 LINE 8 COLUMN 27 PIC ZZZ9
                   FROM GRID-CELL (3, 6)
                   FOREGROUND-COLOR IS COLOUR(18).
               10  GRID-DISPLAY-41 LINE 10 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (4, 1)
                   FOREGROUND-COLOR IS COLOUR(19).
               10  GRID-DISPLAY-42 LINE 10 COLUMN 7 PIC ZZZ9
                   FROM GRID-CELL (4, 2)
                   FOREGROUND-COLOR IS COLOUR(20).
               10  GRID-DISPLAY-43 LINE 10 COLUMN 12 PIC ZZZ9
                   FROM GRID-CELL (4, 3)
                   FOREGROUND-COLOR IS COLOUR(21).
               10  GRID-DISPLAY-44 LINE 10 COLUMN 17 PIC ZZZ9
                   FROM GRID-CELL (4, 4)
                   FOREGROUND-COLOR IS COLOUR(22).
               10  GRID-DISPLAY-45 LINE 10 COLUMN 22 PIC ZZZ9
                   FROM GRID-CELL (4, 5)
                   FOREGROUND-COLOR IS COLOUR(23).
               10  GRID-DISPLAY-46 LINE 10 COLUMN 27 PIC ZZZ9
                   FROM GRID-CELL (4, 6)
                   FOREGROUND-COLOR IS COLOUR(24).
               10  GRID-DISPLAY-51 LINE 12 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (5, 1)
                   FOREGROUND-COLOR IS COLOUR(25).
               10  GRID-DISPLAY-52 LINE 12 COLUMN 7 PIC ZZZ9
                   FROM GRID-CELL (5, 2)
                   FOREGROUND-COLOR IS COLOUR(26).
               10  GRID-DISPLAY-53 LINE 12 COLUMN 12 PIC ZZZ9
                   FROM GRID-CELL (5, 3)
                   FOREGROUND-COLOR IS COLOUR(27).
               10  GRID-DISPLAY-54 LINE 12 COLUMN 17 PIC ZZZ9
                   FROM GRID-CELL (5, 4)
                   FOREGROUND-COLOR IS COLOUR(28).
               10  GRID-DISPLAY-55 LINE 12 COLUMN 22 PIC ZZZ9
                   FROM GRID-CELL (5, 5)
                   FOREGROUND-COLOR IS COLOUR(29).
               10  GRID-DISPLAY-56 LINE 12 COLUMN 27 PIC ZZZ9
                   FROM GRID-CELL (5, 6)
                   FOREGROUND-COLOR IS COLOUR(30).
               10  GRID-DISPLAY-61 LINE 14 COLUMN 2 PIC ZZZ9
                   FROM GRID-CELL (6, 1)
                   FOREGROUND-COLOR IS COLOUR(31).
               10  GRID-DISPLAY-62 LINE 14 COLUMN 7 PIC ZZZ9
                   FROM GRID-CELL (6, 2)
                   FOREGROUND-COLOR IS COLOUR(32).
               10  GRID-DISPLAY-63 LINE 14 COLUMN 12 PIC ZZZ9
                   FROM GRID-CELL (6, 3)
                   FOREGROUND-COLOR IS COLOUR(33).
               10  GRID-DISPLAY-64 LINE 14 COLUMN 17 PIC ZZZ9
                   FROM GRID-CELL (6, 4)
                   FOREGROUND-COLOR IS COLOUR(34).
               10  GRID-DISPLAY-65 LINE 14 COLUMN 22 PIC ZZZ9
                   FROM GRID-CELL (6, 5)
                   FOREGROUND-COLOR IS COLOUR(35).
               10  GRID-DISPLAY-66 LINE 14 COLUMN 27 PIC ZZZ9
                   FROM GRID-CELL (6, 6)       
                   FOREGROUND-COLOR IS COLOUR(36).  
                   10  LINE 16 COLUMN  4 VALUE 'CHOICE:' 
                   FOREGROUND-COLOR IS 6.
                   10  DISPLAY-UP-COMMAND    LINE 18 COLUMN 12 PIC X(9) 
                   HIGHLIGHT, FOREGROUND-COLOR IS 3.
                   10  DISPLAY-DOWN-COMMAND  LINE 19 COLUMN 12 PIC X(9)
                   HIGHLIGHT, FOREGROUND-COLOR IS 3.
                   10  DISPLAY-LEFT-COMMAND  LINE 20 COLUMN 12 PIC X(9)
                   HIGHLIGHT, FOREGROUND-COLOR IS 3.
                   10  DISPLAY-RIGHT-COMMAND LINE 21 COLUMN 12 PIC X(9)
                   HIGHLIGHT, FOREGROUND-COLOR IS 3.
                   10  LINE 22 COLUMN 12 VALUE 'C - CHANGE CONTROLS'
                       HIGHLIGHT, FOREGROUND-COLOR IS 5.
                   10  LINE 23 COLUMN 12 VALUE 'H - HIGH SCORES'
                       HIGHLIGHT, FOREGROUND-COLOR IS 2.
                   10  LINE 24 COLUMN 12 VALUE 'Q - QUIT'
                       HIGHLIGHT, FOREGROUND-COLOR IS 4.
                   10  GRID-MESSAGE LINE 26 COLUMN 4     PIC X(47).
               05  GRID-INPUT.
                   10  USER-INPUT LINE 16 COLUMN 12    PIC X(10)
                       USING USER-ENTRY FOREGROUND-COLOR IS 6.

       01  CONTROLS-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COLUMN 5 VALUE '**********|' FOREGROUND-COLOR 
               IS 6.
           05  LINE 2 COLUMN 16 VALUE '2' HIGHLIGHT, 
               FOREGROUND-COLOR IS 2.
           05  LINE 2 COLUMN 17 VALUE '0' HIGHLIGHT, 
               FOREGROUND-COLOR IS 3.
           05  LINE 2 COLUMN 18 VALUE '4' HIGHLIGHT, 
               FOREGROUND-COLOR IS 4.
           05  LINE 2 COLUMN 19 VALUE '8' HIGHLIGHT, 
               FOREGROUND-COLOR IS 5.
           05  LINE 2 COLUMN 20 VALUE '|**********' FOREGROUND-COLOR 
               IS 6.
           05  LINE 4 COLUMN 4 VALUE 'CUSTOMIZE CONTROLS BELOW' 
               HIGHLIGHT, FOREGROUND-COLOR IS 4.
           05  LINE 6 COLUMN 4 VALUE 'SLIDE UP:'.
           05  SET-CONTROL-UP LINE 6 COLUMN 20 PIC X
               USING CONTROL-UP FOREGROUND-COLOR IS 6.
           05  LINE 7 COLUMN 4 VALUE 'SLIDE DOWN:'.
           05  SET-CONTROL-DOWN LINE 7 COLUMN 20 PIC X
               USING CONTROL-DOWN FOREGROUND-COLOR IS 6.
           05  LINE 8 COLUMN 4 VALUE 'SLIDE LEFT:'.
           05  SET-CONTROL-LEFT LINE 8 COLUMN 20 PIC X
               USING CONTROL-LEFT FOREGROUND-COLOR IS 6.
           05  LINE 9 COLUMN 4 VALUE 'SLIDE RIGHT:'.
           05  SET-CONTROL-RIGHT LINE 9 COLUMN 20 PIC X
               USING CONTROL-RIGHT FOREGROUND-COLOR IS 6.

       01  HIGH-SCORE-SCREEN.
           05  BLANK SCREEN.
           05  LINE 2 COLUMN 5 VALUE '**********|' FOREGROUND-COLOR 
               IS 6.
           05  LINE 2 COLUMN 16 VALUE '2' HIGHLIGHT, 
               FOREGROUND-COLOR IS 2.
           05  LINE 2 COLUMN 17 VALUE '0' HIGHLIGHT, 
               FOREGROUND-COLOR IS 3.
           05  LINE 2 COLUMN 18 VALUE '4' HIGHLIGHT, 
               FOREGROUND-COLOR IS 4.
           05  LINE 2 COLUMN 19 VALUE '8' HIGHLIGHT, 
               FOREGROUND-COLOR IS 5.
           05  LINE 2 COLUMN 20 VALUE '|**********' FOREGROUND-COLOR 
               IS 6.
           05  LINE 4 COLUMN 4 VALUE 'HIGH SCORES' HIGHLIGHT, 
               FOREGROUND-COLOR IS 2.
           05  LINE 6 COLUMN 4 VALUE 'SCORE  YEAR-MO-DY HR-MM' HIGHLIGHT 
               FOREGROUND-COLOR IS 4.
           05  HIGH-SCORE-1 LINE  7 COLUMN 4 PIC X(23).
           05  HIGH-SCORE-2 LINE  8 COLUMN 4 PIC X(23).
           05  HIGH-SCORE-3 LINE  9 COLUMN 4 PIC X(23).
           05  HIGH-SCORE-4 LINE 10 COLUMN 4 PIC X(23).
           05  HIGH-SCORE-5 LINE 11 COLUMN 4 PIC X(23).
           05  LINE 13 COLUMN 4 VALUE 'PRESS ENTER TO EXIT' 
               FOREGROUND-COLOR IS 6.
           05  HS-INPUT LINE 13 COLUMN 24    PIC X(10)
               USING USER-ENTRY FOREGROUND-COLOR IS 6.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM TEST-GRID-SIZE
           PERFORM LOAD-CONTROLS
           MOVE SPACES TO GRID-MESSAGE
           PERFORM PLACE-NEW-TILE
           PERFORM GAME-TURN
               UNTIL GAME-OVER
           PERFORM SAVE-HIGH-SCORES
           IF HOLD-DISPLAY
               ACCEPT USER-INPUT
           END-IF
           STOP RUN
           .

       TEST-GRID-SIZE.
           IF (LENGTH OF GAME-GRID / LENGTH OF GRID-ROW)
           NOT = NUMBER-OF-ROWS
               DISPLAY 'INCOMPLETE CHANGE TO NUMBER OF ROWS '
                       'IN GAME GRID.  EXITING...'
               STOP RUN
           END-IF
           IF LENGTH OF GRID-COL
           NOT = NUMBER-OF-COLS
               DISPLAY 'INCOMPLETE CHANGE TO NUMBER OF COLUMNS '
                       'IN GAME GRID.  EXITING...'
               STOP RUN
           END-IF
           .

       LOAD-CONTROLS.
           OPEN INPUT CONTROL-FILE
           IF CONTROL-FILE-EXISTS
               READ CONTROL-FILE
               IF CONTROL-FILE-OK
                   MOVE CONTROL-RECORD-UP    TO CONTROL-UP
                   MOVE CONTROL-RECORD-DOWN  TO CONTROL-DOWN
                   MOVE CONTROL-RECORD-LEFT  TO CONTROL-LEFT
                   MOVE CONTROL-RECORD-RIGHT TO CONTROL-RIGHT
               ELSE
                   MOVE 'U' TO CONTROL-UP
                   MOVE 'D' TO CONTROL-DOWN
                   MOVE 'L' TO CONTROL-LEFT
                   MOVE 'R' TO CONTROL-RIGHT
               END-IF
           ELSE
               MOVE 'U' TO CONTROL-UP
               MOVE 'D' TO CONTROL-DOWN
               MOVE 'L' TO CONTROL-LEFT
               MOVE 'R' TO CONTROL-RIGHT
           END-IF
           PERFORM UPDATE-COMMAND-DISPLAY
           CLOSE CONTROL-FILE
           .

       UPDATE-COMMAND-DISPLAY.
           STRING CONTROL-UP    ' - UP'    INTO DISPLAY-UP-COMMAND
           STRING CONTROL-DOWN  ' - DOWN'  INTO DISPLAY-DOWN-COMMAND
           STRING CONTROL-LEFT  ' - LEFT'  INTO DISPLAY-LEFT-COMMAND
           STRING CONTROL-RIGHT ' - RIGHT' INTO DISPLAY-RIGHT-COMMAND
           .

       GAME-TURN.
           IF TILE-NOT-NEEDED
               SET TILE-NEEDED TO TRUE
           ELSE
               PERFORM PLACE-NEW-TILE
           END-IF

           PERFORM COLOUR-CHECK
           DISPLAY GAME-GRID-SCREEN
           PERFORM HANDLE-USER-ENTRY
           PERFORM CHECK-IF-WIN
           .

       COLOUR-CHECK.
           MOVE 0 TO COUNTER. 
           SET ROW-INDEX TO 0
           PERFORM 6 TIMES
               SET ROW-INDEX UP BY 1
               SET COL-INDEX TO 0
               PERFORM 6 TIMES
                   SET COL-INDEX UP BY 1
                   ADD 1 TO COUNTER
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) = 2
                       MOVE 7 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 4
                       MOVE 3 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 8
                       MOVE 4 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 16
                       MOVE 5 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 32
                       MOVE 6 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 64
                       MOVE 1 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 128
                       MOVE 2 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 256
                       MOVE 3 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 512
                       MOVE 4 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 1024
                       MOVE 5 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 2048
                       MOVE 6 TO COLOUR(COUNTER)
                   ELSE IF GRID-CELL (ROW-INDEX, COL-INDEX) = 0
                       MOVE 2 TO COLOUR(COUNTER)
                   END-IF 
               END-PERFORM
           END-PERFORM
           .

       PLACE-NEW-TILE.
      * CALLED TWICE BEFORE PLAY BEGINS AND ONCE EVERY TURN
           PERFORM COUNT-EMPTY
           IF EMPTY-COUNT = ZERO
               SET GAME-OVER TO TRUE
               MOVE NO-MOVES-MESSAGE TO GRID-MESSAGE
               PERFORM COLOUR-CHECK
               DISPLAY GAME-GRID-SCREEN
           END-IF
           COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * EMPTY-COUNT + 1
           MOVE ZERO TO CELL-COUNT
           PERFORM VARYING ROW-INDEX FROM 1 BY 1
               UNTIL ROW-INDEX > 6 OR CELL-COUNT >= RANDOM-NUMBER
               PERFORM VARYING COL-INDEX FROM 1 BY 1
               UNTIL COL-INDEX > 6 OR CELL-COUNT >= RANDOM-NUMBER
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) = ZERO
                       ADD 1 TO CELL-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM
           SET ROW-INDEX DOWN BY 1
           SET COL-INDEX DOWN BY 1
           COMPUTE RANDOM-NUMBER = FUNCTION RANDOM * 10 + 1
           IF RANDOM-NUMBER > 8
               MOVE 4 TO GRID-CELL (ROW-INDEX, COL-INDEX)
           ELSE
               MOVE 2 TO GRID-CELL (ROW-INDEX, COL-INDEX)
           END-IF
           .

       COUNT-EMPTY.
      * THE COUNT OF EMPTY SPACES DECREASES BY ONE EACH TURN
      * AS A RESULT OF PARAGRAPH PLACE-NEW-TILE
      * THE COUNT DECREASES BY ONE FOR EACH COLLISION OF
      * TWO TILES WITH THE SAME NUMBER (IN THE SLIDE- PARAGRAPHS).
      * RATHER THAN UPDATING ONE GLOBAL VARIABLE IN SEVERAL
      * PARAGRAPHS, THE COUNT IS CALCULATED WHEN NEEDED IN THIS
      * PARAGRAPH.
           MOVE ZERO TO EMPTY-COUNT

           SET ROW-INDEX TO ZERO
           PERFORM 6 TIMES
               SET ROW-INDEX UP BY 1
               SET COL-INDEX TO ZERO
               PERFORM 6 TIMES
               SET COL-INDEX UP BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) = ZERO
                       ADD 1 TO EMPTY-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       HANDLE-USER-ENTRY.
           MOVE SPACES TO USER-ENTRY
           ACCEPT USER-INPUT
           MOVE FUNCTION UPPER-CASE (USER-ENTRY(1:1)) TO USER-COMMAND
           EVALUATE TRUE
               WHEN USER-CHOSE-QUIT
                   SET GAME-QUIT TO TRUE
               WHEN USER-CHOSE-SET-COMMANDS
                   PERFORM CHANGE-CONTROLS
               WHEN USER-CHOSE-HIGH-SCORES
                   PERFORM SHOW-HIGH-SCORES
               WHEN USER-COMMAND = CONTROL-UP
                   PERFORM SLIDE-UP
               WHEN USER-COMMAND = CONTROL-DOWN
                   PERFORM SLIDE-DOWN
               WHEN USER-COMMAND = CONTROL-LEFT
                   PERFORM SLIDE-LEFT
               WHEN USER-COMMAND = CONTROL-RIGHT
                   PERFORM SLIDE-RIGHT
               WHEN OTHER
                   SET TILE-NOT-NEEDED TO TRUE
           END-EVALUATE
           MOVE SPACES TO USER-ENTRY
           .

       CHANGE-CONTROLS.
           ACCEPT CONTROLS-SCREEN
           MOVE FUNCTION UPPER-CASE(CONTROL-UP)    TO CONTROL-UP
           MOVE FUNCTION UPPER-CASE(CONTROL-DOWN)  TO CONTROL-DOWN
           MOVE FUNCTION UPPER-CASE(CONTROL-LEFT)  TO CONTROL-LEFT
           MOVE FUNCTION UPPER-CASE(CONTROL-RIGHT) TO CONTROL-RIGHT

      * STOP CHANGE-CONTROLS IF ASSIGNED KEYS CHOSEN
      * OR CONTROLS NOT UNIQUE
           IF CONTROL-UP    = 'Q'
           OR CONTROL-DOWN  = 'Q'
           OR CONTROL-LEFT  = 'Q'
           OR CONTROL-RIGHT = 'Q'
           OR CONTROL-UP    = 'C'
           OR CONTROL-DOWN  = 'C'
           OR CONTROL-LEFT  = 'C'
           OR CONTROL-RIGHT = 'C'
           OR CONTROL-UP    = 'H'
           OR CONTROL-DOWN  = 'H'
           OR CONTROL-LEFT  = 'H'
           OR CONTROL-RIGHT = 'H'
           OR CONTROL-UP = CONTROL-DOWN
           OR CONTROL-UP = CONTROL-LEFT
           OR CONTROL-UP = CONTROL-RIGHT
           OR CONTROL-DOWN = CONTROL-LEFT
           OR CONTROL-DOWN = CONTROL-RIGHT
           OR CONTROL-LEFT = CONTROL-RIGHT
      * RESET CONTROLS TO DEFAULT VALUES
               MOVE 'U' TO CONTROL-UP
               MOVE 'D' TO CONTROL-DOWN
               MOVE 'L' TO CONTROL-LEFT
               MOVE 'R' TO CONTROL-RIGHT
           ELSE
               OPEN OUTPUT CONTROL-FILE

               IF NOT CONTROL-FILE-OK-OR-NEW
                   DISPLAY 'UNABLE TO WRITE TO CONTROL-FILE'
                   CLOSE CONTROL-FILE
                   STOP RUN
               END-IF

               MOVE CONTROL-UP    TO CONTROL-RECORD-UP
               MOVE CONTROL-DOWN  TO CONTROL-RECORD-DOWN
               MOVE CONTROL-LEFT  TO CONTROL-RECORD-LEFT
               MOVE CONTROL-RIGHT TO CONTROL-RECORD-RIGHT
               WRITE CONTROL-RECORD
               CLOSE CONTROL-FILE
           END-IF
           PERFORM UPDATE-COMMAND-DISPLAY
           SET TILE-NOT-NEEDED TO TRUE
           .

       SHOW-HIGH-SCORES.
           PERFORM GET-HIGH-SCORES
           MOVE HS-TABLE (1) TO HIGH-SCORE-1
           IF HS-COUNT > 1
               MOVE HS-TABLE (2) TO HIGH-SCORE-2
           ELSE
               MOVE SPACES TO HIGH-SCORE-2
           END-IF
           IF HS-COUNT > 2
               MOVE HS-TABLE (3) TO HIGH-SCORE-3
           ELSE
               MOVE SPACES TO HIGH-SCORE-3
           END-IF
           IF HS-COUNT > 3
               MOVE HS-TABLE (4) TO HIGH-SCORE-4
           ELSE
               MOVE SPACES TO HIGH-SCORE-4
           END-IF
           IF HS-COUNT > 4
               MOVE HS-TABLE (5) TO HIGH-SCORE-5
           ELSE
               MOVE SPACES TO HIGH-SCORE-5
           END-IF
           MOVE SPACES TO USER-ENTRY
           ACCEPT HIGH-SCORE-SCREEN
           SET TILE-NOT-NEEDED TO TRUE
           .

       GET-HIGH-SCORES.
           MOVE ZERO TO HS-COUNT
           OPEN INPUT HIGH-SCORE-FILE
           IF HIGH-SCORE-FILE-EXISTS
               READ HIGH-SCORE-FILE
               PERFORM TEST-HIGH-SCORE-FILE
               SET HS-INDEX TO 1
               PERFORM UNTIL HIGH-SCORE-NO-MORE-RECORDS OR HS-INDEX > 5
                   MOVE HIGH-SCORE-RECORD TO HS-TABLE (HS-INDEX)
                   ADD 1 TO HS-COUNT
                   READ HIGH-SCORE-FILE
                   PERFORM TEST-HIGH-SCORE-FILE
                   SET HS-INDEX UP BY 1
               END-PERFORM
           END-IF
           CLOSE HIGH-SCORE-FILE
           PERFORM GET-CURRENT-SCORE
           IF HS-INDEX < 6
               MOVE CURRENT-SCORE TO HS-SCORE (HS-INDEX)
               MOVE 'CURRENT SCORE' TO HS-TEXT (HS-INDEX)
               ADD 1 TO HS-COUNT
           ELSE
               IF CURRENT-SCORE > HS-SCORE(5)
                   MOVE CURRENT-SCORE TO HS-SCORE (5)
                   MOVE 'CURRENT SCORE' TO HS-TEXT (5)
                   ADD 1 TO HS-COUNT
               END-IF
           END-IF
           SORT HS-TABLE ON DESCENDING KEY HS-SCORE
                             ASCENDING KEY  HS-TEXT
           .

       TEST-HIGH-SCORE-FILE.
           IF NOT HIGH-SCORE-FILE-OK-OR-EOF
               DISPLAY 'UNABLE TO READ HIGH SCORE FILE'
               CLOSE HIGH-SCORE-FILE
               STOP RUN
           END-IF
           .

       GET-CURRENT-SCORE.
           MOVE ZERO TO CURRENT-SCORE
           SET ROW-INDEX TO ZERO
           PERFORM 6 TIMES
               SET ROW-INDEX UP BY 1
               SET COL-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET COL-INDEX UP BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) > CURRENT-SCORE
                       MOVE GRID-CELL (ROW-INDEX, COL-INDEX)
                         TO CURRENT-SCORE
                   END-IF
               END-PERFORM
           END-PERFORM
           .

       SAVE-HIGH-SCORES.
           PERFORM GET-HIGH-SCORES
           PERFORM VARYING HS-INDEX FROM 1 BY 1
               UNTIL HS-INDEX > HS-COUNT
               IF HS-TEXT (HS-INDEX) = 'CURRENT SCORE'
                   ACCEPT CURRENT-DATE FROM DATE YYYYMMDD
                   ACCEPT CURRENT-TIME FROM TIME
                   MOVE HS-SCORE (HS-INDEX) TO HS-NUMBER
                   MOVE CD-YEAR TO HS-YEAR
                   MOVE CD-MONTH TO HS-MONTH
                   MOVE CD-DAY   TO HS-DAY
                   MOVE CT-HOUR  TO HS-HOUR
                   MOVE CT-MINUTE TO HS-MINUTE
                   MOVE HIGH-SCORE-ENTRY TO HS-TABLE (HS-INDEX)
               END-IF
           END-PERFORM
           OPEN OUTPUT HIGH-SCORE-FILE
           IF NOT HIGH-SCORE-FILE-OK-OR-NEW
               DISPLAY 'UNABLE TO WRITE TO HIGH SCORE FILE'
               CLOSE HIGH-SCORE-FILE
               STOP RUN
           END-IF
           PERFORM VARYING HS-INDEX FROM 1 BY 1
               UNTIL HS-INDEX > HS-COUNT
               WRITE HIGH-SCORE-RECORD FROM HS-TABLE (HS-INDEX)
               IF NOT HIGH-SCORE-FILE-OK
                   DISPLAY 'UNABLE TO WRITE TO HIGH SCORE FILE'
                   CLOSE HIGH-SCORE-FILE
                   STOP RUN
               END-IF
           END-PERFORM
           CLOSE HIGH-SCORE-FILE
           .

       SLIDE-UP.
           SET COL-INDEX TO ZERO
           PERFORM 6 TIMES
               SET COL-INDEX UP BY 1
               MOVE ZERO TO PRIOR-TILE

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE ZERO TO UPDATED-SET (UPDATED-INDEX)
               END-PERFORM

               SET UPDATED-INDEX TO 1
               SET ROW-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET ROW-INDEX UP BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) NOT = 0
                       IF GRID-CELL (ROW-INDEX, COL-INDEX) = PRIOR-TILE
                           COMPUTE UPDATED-SET(UPDATED-INDEX - 1)
                               = UPDATED-SET(UPDATED-INDEX - 1) * 2
                           MOVE ZERO TO PRIOR-TILE
                       ELSE
                           MOVE GRID-CELL (ROW-INDEX, COL-INDEX)
                             TO UPDATED-SET(UPDATED-INDEX)
                                PRIOR-TILE
                           SET UPDATED-INDEX UP BY 1
                       END-IF
                   END-IF
               END-PERFORM

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE UPDATED-SET (UPDATED-INDEX)
                     TO GRID-CELL (UPDATED-INDEX, COL-INDEX)
               END-PERFORM

           END-PERFORM
           .

       SLIDE-DOWN.
           SET COL-INDEX TO ZERO
           PERFORM 6 TIMES
               SET COL-INDEX UP BY 1
               MOVE ZERO TO PRIOR-TILE

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE ZERO TO UPDATED-SET (UPDATED-INDEX)
               END-PERFORM

               SET UPDATED-INDEX TO 6
               SET ROW-INDEX TO 7
               PERFORM 6 TIMES
                   SET ROW-INDEX DOWN BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) NOT = 0
                       IF GRID-CELL (ROW-INDEX, COL-INDEX) = PRIOR-TILE
                           COMPUTE UPDATED-SET(UPDATED-INDEX + 1)
                               = UPDATED-SET(UPDATED-INDEX + 1) * 2
                           MOVE ZERO TO PRIOR-TILE
                       ELSE
                           MOVE GRID-CELL (ROW-INDEX, COL-INDEX)
                             TO UPDATED-SET(UPDATED-INDEX)
                                PRIOR-TILE
                           SET UPDATED-INDEX DOWN BY 1
                       END-IF
                   END-IF
               END-PERFORM

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE UPDATED-SET (UPDATED-INDEX)
                     TO GRID-CELL (UPDATED-INDEX, COL-INDEX)
               END-PERFORM

           END-PERFORM
           .

       SLIDE-LEFT.
           SET ROW-INDEX TO ZERO
           PERFORM 6 TIMES
               SET ROW-INDEX UP BY 1
               MOVE ZERO TO PRIOR-TILE

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE ZERO TO UPDATED-SET (UPDATED-INDEX)
               END-PERFORM

               SET UPDATED-INDEX TO 1
               SET COL-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET COL-INDEX UP BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) NOT = 0
                       IF GRID-CELL (ROW-INDEX, COL-INDEX) = PRIOR-TILE
                           COMPUTE UPDATED-SET(UPDATED-INDEX - 1)
                               = UPDATED-SET(UPDATED-INDEX - 1) * 2
                           MOVE ZERO TO PRIOR-TILE
                       ELSE
                           MOVE GRID-CELL (ROW-INDEX, COL-INDEX)
                             TO UPDATED-SET(UPDATED-INDEX)
                                PRIOR-TILE
                           SET UPDATED-INDEX UP BY 1
                       END-IF
                   END-IF
               END-PERFORM

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE UPDATED-SET (UPDATED-INDEX)
                     TO GRID-CELL (ROW-INDEX, UPDATED-INDEX)
               END-PERFORM

           END-PERFORM
           .

       SLIDE-RIGHT.
           SET ROW-INDEX TO ZERO
           PERFORM 6 TIMES
               SET ROW-INDEX UP BY 1
               MOVE ZERO TO PRIOR-TILE

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE ZERO TO UPDATED-SET (UPDATED-INDEX)
               END-PERFORM

               SET UPDATED-INDEX TO 6
               SET COL-INDEX TO 7
               PERFORM 6 TIMES
                   SET COL-INDEX DOWN BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) NOT = 0
                       IF GRID-CELL (ROW-INDEX, COL-INDEX) = PRIOR-TILE
                           COMPUTE UPDATED-SET(UPDATED-INDEX + 1)
                               = UPDATED-SET(UPDATED-INDEX + 1) * 2
                           MOVE ZERO TO PRIOR-TILE
                       ELSE
                           MOVE GRID-CELL (ROW-INDEX, COL-INDEX)
                             TO UPDATED-SET(UPDATED-INDEX)
                                PRIOR-TILE
                           SET UPDATED-INDEX DOWN BY 1
                       END-IF
                   END-IF
               END-PERFORM

               SET UPDATED-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET UPDATED-INDEX UP BY 1
                   MOVE UPDATED-SET (UPDATED-INDEX)
                     TO GRID-CELL (ROW-INDEX, UPDATED-INDEX)
               END-PERFORM

           END-PERFORM
           .

       CHECK-IF-WIN.
           SET ROW-INDEX TO ZERO
           PERFORM 6 TIMES
               SET ROW-INDEX UP BY 1
               SET COL-INDEX TO ZERO
               PERFORM 6 TIMES
                   SET COL-INDEX UP BY 1
                   IF GRID-CELL (ROW-INDEX, COL-INDEX) = 2048
                       MOVE WIN-MESSAGE TO GRID-MESSAGE
                       PERFORM COLOUR-CHECK
                       DISPLAY GAME-GRID-SCREEN
                       SET GAME-OVER TO TRUE
                   END-IF
               END-PERFORM
           END-PERFORM
           .
