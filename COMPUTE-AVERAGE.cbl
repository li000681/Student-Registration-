       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTE-AVERAGE AS "COMPUTE-AVERAGE".
       
       DATA DIVISION.
           LINKAGE SECTION.
           01 LS-AVERAGE-OUT PIC 9(3).
           01 LS-AVERAGE-1 PIC 9(3).
           01 LS-AVERAGE-2 PIC 9(3).
           01 LS-AVERAGE-3 PIC 9(3).
           01 LS-AVERAGE-4 PIC 9(3).
           01 LS-AVERAGE-5 PIC 9(3).
               
       PROCEDURE DIVISION USING
           LS-AVERAGE-OUT, LS-AVERAGE-1, LS-AVERAGE-2,
           LS-AVERAGE-3,LS-AVERAGE-4, LS-AVERAGE-5.                   
           COMPUTE LS-AVERAGE-OUT ROUNDED = (LS-AVERAGE-1 + 
           LS-AVERAGE-2 + LS-AVERAGE-3 + LS-AVERAGE-4 + 
           LS-AVERAGE-5) / 5.
                         
       EXIT PROGRAM.