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

       WORKING-STORAGE SECTION.
           01 DEDUCTION PIC 9 VALUE 0.
           01 NUM PIC 9.

       PROCEDURE DIVISION.
           OPEN INPUT FI-IN-ITEMS OUTPUT FI-OUT-ITEMS.

       0100-START.
           READ FI-IN-ITEMS END GO TO 0200-END.
             MOVE FS-IN-ITEM TO FS-OUT-ITEM.

               IF ITEM-NAME NOT EQUAL "Sulfuras, Hand of Ragnaros"
                   SUBTRACT 1 FROM SELL-IN
               END-IF.

               IF ITEM-NAME NOT EQUAL "Aged Brie" AND ITEM-NAME NOT
               EQUAL "Backstage passes to a TAFKAL80ETC concert"
                   PERFORM 0400-DECREASE-QUALITY
               ELSE
                   PERFORM 0500-INCREASE-QUALITY
               END-IF. 
      
             WRITE FS-OUT-ITEM.
           GO TO 0100-START.

       0200-END.
           CLOSE FI-IN-ITEMS.
           CLOSE FI-OUT-ITEMS.

       0300-RETURN.
           GOBACK.

       0400-DECREASE-QUALITY.
           IF ITEM-NAME IS EQUAL TO "Sulfuras, Hand of Ragnaros" 
               MOVE 0 TO DEDUCTION 
           ELSE IF ITEM-NAME IS EQUAL TO "Conjured Mana Cake"
               MOVE 2 TO DEDUCTION
           ELSE
               MOVE 1 TO DEDUCTION
           END-IF. 
           IF QUALITY IS GREATER THAN 0 
               IF SELL-IN IS GREATER THAN 0 
                   COMPUTE NUM = 1 * DEDUCTION
                   SUBTRACT NUM FROM QUALITY
               ELSE 
                   COMPUTE NUM = 2 * DEDUCTION
                   SUBTRACT NUM FROM QUALITY
           END-IF. 
           IF QUALITY IS LESS THAN 0 
               MOVE 0 TO QUALITY
           END-IF. 

       0500-INCREASE-QUALITY.
           IF QUALITY IS LESS THAN 50 
               IF ITEM-NAME IS EQUAL TO "Aged Brie" 
                   ADD 1 TO QUALITY 
               ELSE IF SELL-IN IS LESS THAN 1 
                   MOVE 0 TO QUALITY 
               ELSE IF SELL-IN IS LESS THAN 6 
                   ADD 3 TO QUALITY 
               ELSE IF SELL-IN IS LESS THAN 11 
                   ADD 2 TO QUALITY
               ELSE 
                   ADD 1 TO QUALITY
               END-IF
           END-IF.
           IF QUALITY IS GREATER THAN 50 
               MOVE 50 TO QUALITY
           END-IF. 

           
               
                
           

