       IDENTIFICATION DIVISION.
       PROGRAM-ID. "test-new-gilded-rose".
       PROCEDURE DIVISION.

      * Trivial test that it keeps the name the same
           SET ENVIRONMENT "in_dat" TO "in.dat".
           SET ENVIRONMENT "out_dat" TO "out.dat".
           CALL "new-gilded-rose".
           
