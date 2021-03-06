       IDENTIFICATION DIVISION.
       PROGRAM-ID. "new-gilded-rose".

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

       0100-START.
           READ FI-IN-ITEMS END GO TO 0200-END.
             MOVE FS-IN-ITEM TO FS-OUT-ITEM.

               IF ITEM-NAME(1:8) NOT EQUAL "Sulfuras"
                   SUBTRACT 1 FROM SELL-IN
               END-IF.

               IF ITEM-NAME = "Aged Brie" 
                   PERFORM 0400-AGED-BRIE 
               ELSE IF ITEM-NAME(1:16) = "Backstage passes"
                   PERFORM 0500-BACKSTAGE-PASSES
               ELSE IF ITEM-NAME(1:8) = "Sulfuras"
                   ADD 0 TO QUALITY
               ELSE IF ITEM-NAME(1:8) = "Conjured"
                   PERFORM 0600-CONJURED
               ELSE 
                   PERFORM 0300-GENERAL-ITEM
               END-IF. 
      
             WRITE FS-OUT-ITEM.
           GO TO 0100-START.

       0200-END.
           CLOSE FI-IN-ITEMS.
           CLOSE FI-OUT-ITEMS.

       0300-GENERAL-ITEM.
           SUBTRACT 1 FROM QUALITY
           IF SELL-IN IS LESS THAN 0 
               SUBTRACT 1 FROM QUALITY
           END-IF. 
           PERFORM 0700-MIN-QUALITY. 

       0400-AGED-BRIE.
           IF QUALITY IS LESS THAN 50 
                   ADD 1 TO QUALITY  
           END-IF.

       0500-BACKSTAGE-PASSES.
           ADD 1 TO QUALITY. 
           IF SELL-IN IS LESS THAN 1 
               MOVE 0 TO QUALITY 
           ELSE IF SELL-IN IS LESS THAN 6 
               ADD 2 TO QUALITY 
           ELSE IF SELL-IN IS LESS THAN 11 
               ADD 1 TO QUALITY
           END-IF.
           IF QUALITY IS GREATER THAN 50 
               MOVE 50 TO QUALITY
           END-IF. 
       
       0600-CONJURED.
           SUBTRACT 2 FROM QUALITY
           IF SELL-IN IS LESS THAN 0
               SUBTRACT 2 FROM QUALITY
           END-IF. 
           PERFORM 0700-MIN-QUALITY.

       0700-MIN-QUALITY.
           IF QUALITY IS LESS THAN 0 
               MOVE 0 TO QUALITY
           END-IF. 
           