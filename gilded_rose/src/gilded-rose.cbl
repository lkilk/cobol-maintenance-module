       IDENTIFICATION DIVISION.
       PROGRAM-ID. "gilded-rose".

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
             SELECT FI-IN-ITEMS ASSIGN "in.dat"
               ORGANISATION IS LINE SEQUENTIAL.
             SELECT FI-OUT-ITEMS ASSIGN "out.dat"
               ORGANISATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD FI-IN-ITEMS.
           01 FS-IN-ITEM PIC X(60).
           FD FI-OUT-ITEMS.
           01 FS-OUT-ITEM.
             05 SELL-IN PIC S9(3) SIGN IS LEADING SEPARATE CHARACTER.
             05 FILLER PIC X VALUE SPACE.
             05 QUALITY PIC S9(3) SIGN IS LEADING SEPARATE CHARACTER.
             05 FILLER PIC X VALUE SPACE.
             05 ITEM-NAME PIC X(50).

       PROCEDURE DIVISION.
           OPEN INPUT FI-IN-ITEMS OUTPUT FI-OUT-ITEMS.

       0100-start SECTION.
           READ FI-IN-ITEMS END GO TO 0200-end.
             MOVE FS-IN-ITEM TO FS-OUT-ITEM.
             IF ITEM-NAME NOT EQUAL "Aged Brie" AND ITEM-NAME NOT
             EQUAL "Backstage passes to a TAFKAL80ETC concert"
               IF QUALITY GREATER THAN 0
                 IF ITEM-NAME NOT EQUAL TO "Sulfuras, Hand of Ragnaros"
                   SUBTRACT 1 FROM QUALITY
                 END-IF
               END-IF
             ELSE
               IF QUALITY IS LESS THAN 50
                 ADD 1 TO QUALITY
                 IF ITEM-NAME EQUALS
                 "Backstage passes to a TAFKAL80ETC concert"
                   IF SELL-IN LESS THAN 11
                     IF QUALITY LESS THAN 50
                       ADD 1 TO QUALITY
                     END-IF
                   END-IF
                   IF SELL-IN LESS THAN 6
                     IF QUALITY LESS THAN 50
                       ADD 1 TO QUALITY
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
             IF ITEM-NAME NOT EQUAL "Sulfuras, Hand of Ragnaros"
               SUBTRACT 1 FROM SELL-IN
             END-IF
             IF SELL-IN IS LESS THAN 0
               IF ITEM-NAME IS NOT EQUAL TO "Aged Brie"
                 IF ITEM-NAME IS NOT EQUAL TO
                 "Backstage passes to a TAFKAL80ETC concert"
                   IF QUALITY IS GREATER THAN 0
                     IF ITEM-NAME IS NOT EQUAL TO
                     "Sulfuras, Hand of Ragnaros"
                       SUBTRACT 1 FROM QUALITY
                     END-IF
                   END-IF
                 ELSE
                   SUBTRACT QUALITY FROM QUALITY
                 END-IF
               ELSE
                 IF QUALITY IS LESS THAN 50
                   ADD 1 TO QUALITY
                 END-IF
               END-IF
             END-IF
             WRITE FS-OUT-ITEM.
           GO TO 0100-start.

       0200-end SECTION.
           CLOSE FI-IN-ITEMS.
           CLOSE FI-OUT-ITEMS.

       0300-return SECTION.
           GOBACK.



