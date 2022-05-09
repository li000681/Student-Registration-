      ******************************************************************
      * This program is to read student records and course records from 
      * external files and then produce student report for each student.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT3-PROGRAM3 AS "PROJECT3-PROGRAM3".
       AUTHOR.  SHURONG HAN and YING YANG.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  STUDENT-FILE-IN
                ASSIGN TO "C:\COBOL\INDEXED-STUFILE"
                ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS STUDENT-NUMBER
                   FILE STATUS IS STATUS-FIELD.
           SELECT  PROGRAM-FILE-IN
                ASSIGN TO "C:\COBOL\PROGRAM.TXT"
                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT   STUDENT-REPORT-OUT
                 ASSIGN TO "C:\COBOL\REPORT.TXT"
                 ORGANIZATION IS LINE SEQUENTIAL.
                 
       DATA DIVISION.
       FILE  SECTION.
       FD  STUDENT-FILE-IN.
       01 STUDENT-RECORD.
           05   STUDENT-NUMBER    PIC 9(6).                             
           05   TUITION-OWED      PIC 9(4)V99.
           05   STUDENT-NAME      PIC X(40).
           05   PROGRAM-OF-STUDY  PIC X(5).
           05   COURSE-CODE-1     PIC X(7).
           05   COURSE-AVERAGE-1  PIC 9(3).
           05   COURSE-CODE-2     PIC X(7).
           05   COURSE-AVERAGE-2  PIC 9(3).
           05   COURSE-CODE-3     PIC X(7).
           05   COURSE-AVERAGE-3  PIC 9(3).
           05   COURSE-CODE-4     PIC X(7).
           05   COURSE-AVERAGE-4  PIC 9(3).
           05   COURSE-CODE-5     PIC X(7).
           05   COURSE-AVERAGE-5  PIC 9(3).

       FD  PROGRAM-FILE-IN.
       01 PROGRAM-RECORD.
           05   PROGRAM-CODE      PIC X(5).
           05   PROGRAM-NAME      PIC X(20).
           
       FD  STUDENT-REPORT-OUT.
       01  STUDENT-REPORT.
           05   REPORT-LINE       PIC X(100).
           
       WORKING-STORAGE SECTION.
        01  CONTROL-FIELDS.
           05   EOF-FLAG-STUDENT    PIC X(3).
           05   EOF-FLAG-PROGRAM    PIC X(3).
           05   FOUND-FLAG          PIC X(3).
           05   SUB-1               PIC 9(2).
           05   SUB-2               PIC 9(2).
           05   STATUS-FIELD        PIC X(2).
           
       01  STUDENT-READ-COUNT    PIC 9(2)  VALUE 0.
       01  STUDENT-WRITE-COUNT   PIC 9(2)  VALUE 0.
      ******************************************************************
      *Copy the structure of the Program Table from external  
      ******************************************************************
       COPY "PROG-TBL.cpy".
       
       01  STUDENT-REPORT-LINE.
           05  REPORT-STUDENT-NAME PIC X(40).
           05  FILLER              PIC X(2)  VALUE   SPACES.
           05  AVERAGE             PIC 9(3).
           05  FILLER              PIC X(4)  VALUE   SPACES.
           05  SEARCHED-NAME       PIC X(20).
           05  FILLER              PIC X(4)  VALUE   SPACES.
           05  TUITION-OWED-DEC    PIC Z,ZZ9.99.
       01  STUDENT-REPORT-HEADER.
           05  FILLER   PIC X(4)   VALUE   "NAME".
           05  FILLER   PIC X(37)  VALUE   SPACES.
           05  FILLER   PIC X(7)   VALUE   "AVERAGE".
           05  FILLER   PIC X(6)   VALUE   SPACES.
           05  FILLER   PIC X(7)   VALUE   "PROGRAM".
           05  FILLER   PIC X(10)   VALUE   SPACES.
           05  FILLER   PIC X(12)   VALUE   "TUITION OWED".

       PROCEDURE DIVISION.
      
       100-PRODUCE-STUDENT-REPORT.
           PERFORM 200-INITIATE-STUDENT-REPORT.
           PERFORM 201-WRITE-STUDENT-REPORT
               UNTIL EOF-FLAG-STUDENT = "YES".
           PERFORM  202-TERMINATE-STUDENT-REPORT.
           STOP RUN.
           
       200-INITIATE-STUDENT-REPORT.
           PERFORM  300-OPEN-FILES.
           PERFORM  301-READ-STUDENT-RECORD.
           PERFORM  302-LOAD-PROGRAM-TABLE.
           PERFORM  303-WRITE-HEADER.
           
       201-WRITE-STUDENT-REPORT.    
           PERFORM  304-SEARCH-PROGRAM-NAME.
           PERFORM  305-COMPUTE-AVERAGE.
           PERFORM  306-WRITE-STUDENT-RECORD.
           PERFORM  301-READ-STUDENT-RECORD.
           
       202-TERMINATE-STUDENT-REPORT.
           PERFORM 307-CLOSE-FILES.
           PERFORM 308-DISPLAY-AUDIT-COUNTERS.
           
       300-OPEN-FILES.
           OPEN  I-O  STUDENT-FILE-IN.
           OPEN  INPUT  PROGRAM-FILE-IN.
           OPEN  OUTPUT  STUDENT-REPORT-OUT.
       
       301-READ-STUDENT-RECORD.
           READ  STUDENT-FILE-IN
             AT END  MOVE "YES"  TO EOF-FLAG-STUDENT
               NOT AT END  ADD 1 TO STUDENT-READ-COUNT.
           
       302-LOAD-PROGRAM-TABLE.
           PERFORM 400-LOAD-PROGRAM 
		     VARYING SUB-1 FROM 1 BY 1 
               UNTIL SUB-1 > 20 OR EOF-FLAG-PROGRAM = "YES".
       
       303-WRITE-HEADER.
           WRITE STUDENT-REPORT FROM STUDENT-REPORT-HEADER.
           
       304-SEARCH-PROGRAM-NAME.
           MOVE  "NO"  TO FOUND-FLAG.
           PERFORM 401-SEARCH-PROGRAM
             VARYING SUB-2 FROM 1 BY 1
               UNTIL FOUND-FLAG = "YES" OR SUB-2 > 20.

       305-COMPUTE-AVERAGE.
      ******************************************************************
      *CALL externally executed program COMPUTE-AVERAGE
      ******************************************************************
       CALL "COMPUTE-AVERAGE"
           USING                                                               
           AVERAGE, COURSE-AVERAGE-1, COURSE-AVERAGE-2,
           COURSE-AVERAGE-3, COURSE-AVERAGE-4, COURSE-AVERAGE-5.
           
       306-WRITE-STUDENT-RECORD.
           MOVE STUDENT-NAME TO REPORT-STUDENT-NAME.
           MOVE TUITION-OWED TO TUITION-OWED-DEC.
           WRITE STUDENT-REPORT FROM STUDENT-REPORT-LINE.
           ADD 1 TO STUDENT-WRITE-COUNT.
           
       307-CLOSE-FILES.
           CLOSE   STUDENT-FILE-IN.
           CLOSE   PROGRAM-FILE-IN.
           CLOSE   STUDENT-REPORT-OUT.
           
       308-DISPLAY-AUDIT-COUNTERS.
           DISPLAY "Student Records read: " STUDENT-READ-COUNT.
           DISPLAY "Student Report records written: " 
             STUDENT-WRITE-COUNT.
           
       400-LOAD-PROGRAM.
	       READ PROGRAM-FILE-IN  
		     AT END  MOVE "YES" TO EOF-FLAG-PROGRAM
               NOT AT END  MOVE PROGRAM-RECORD TO PROGRAM-TABLE (SUB-1).
           
        401-SEARCH-PROGRAM.
           IF PROGRAM-OF-STUDY  =  PROG-CODE (SUB-2)
             MOVE "YES" TO FOUND-FLAG
               MOVE PROG-NAME (SUB-2) TO SEARCHED-NAME.
           
       END PROGRAM PROJECT3-PROGRAM3.