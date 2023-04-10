      ******************************************************************
      * Author:JEFFERSON MOTA SILVA
      * Date:12/02/2023
      * Purpose:PRACTICE COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISTTLIST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT CAD ASSIGN TO
             "C:\Users\PC\Desktop\ProjetosJavaScript\COBOL5\CAD.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL
             RECORD KEY IS FD-ID
             FILE STATUS WS-FS.

             SELECT RELATO ASSIGN TO
             "C:\Users\PC\Desktop\ProjetosJavaScript\COBOL5\CAD.DAT"
             FILE STATUS WS-FS-RELATO.



       DATA DIVISION.
       FILE SECTION.
       FD CAD.
       01 REGISTRO.
           03 FD-ID                               PIC 9(4).
           03 FD-NM                               PIC X(25).
           03 FD-MATERIA                          PIC X(11).
           03 FD-STATUS                           PIC X(11).
           03 FD-MEDIA                            PIC 9(1)V9.

       FD RELATO.
       01 REG-RELATO                              PIC X(60).


       WORKING-STORAGE SECTION.
       77 WS-FS-RELATO                        PIC 99.
       77 WS-FS                               PIC 99.
         88 FS-OK                             VALUE 0.
       77 WS-EOF                              PIC X.
         88 EOF-OK                            VALUE "V" FALSE "F".
       77 WS-COUNT                            PIC 9(4) VALUE 0.
       01 CADASTRO.
           03 WS-ID                           PIC 9(4).
           03 WS-NM                           PIC X(25).
           03 WS-MATERIA                      PIC X(11).
           03 WS-STATUS                       PIC X(11).
           03 WS-MEDIA                        PIC 9(1)V9.
           03 NT1                             PIC 9(1)V9.
           03 NT2                             PIC 9(1)V9.
           03 NT3                             PIC 9(1)V9.
           03 NT4                             PIC 9(1)V9.

       01 WS-DATA-HORA-SYS.
         05 WS-DATE-SYS.
           10 WS-ANO-SYS                      PIC 9(4).
           10 WS-MES-SYS                      PIC 9(2).
           10 WS-DIA-SYS                      PIC 9(2).
         05 WS-TIME-SYS.
           10 WS-HORA-SYS                     PIC 9(2).
           10 WS-MINUTO-SYS                   PIC 9(2).
         05 FILLER                            PIC X(9).

       77 ACUM-LINHAS                         PIC 9(2).
       77 ACUM-PAG                            PIC 9(5).

       01 CAB001.
           05 FILLER                          PIC X(40) VALUE
           'BOLETIM ESCOLAR DE ALUNOS'.
           05 FILLER                          PIC X(6) VALUE 'PAG.:'.
           05 CAB001-PAG                      PIC Z.ZZ9.

       01 CAB002.
           05 FILLER                          PIC X(35) VALUE
           'PROGRAMA = SISTTCAD'.
           05 CAB002-HORA                     PIC 99.
           05 FILLER                          PIC X(1) VALUE ":".
           05 CAB002-MINUTO                   PIC 99.
           05 FILLER                          PIC X(6) VALUE SPACES.
           05 CAB002-DIA                      PIC 99/.
           05 CAB002-MES                      PIC 99/.
           05 CAB002-ANO                      PIC 9999.

       01 CAB003.
           05 FILLER                          PIC X(20) VALUE SPACES.
           05 FILLER                          PIC X(40) VALUE
           'RELATORIO DO CADASTRO DE ALUNOS'.

       01 CAB004.
           05 FILLER                          PIC X(6) VALUE 'CODIGO'.
           05 FILLER                          PIC X(21) VALUE
           'NOME DO ALUNO'.
           05 FILLER                          PIC X(7) VALUE 'MATERIA'.
           05 FILLER                          PIC X(5) VALUE 'NOTA1'.
           05 FILLER                          PIC X(5) VALUE 'NOTA2'.
           05 FILLER                          PIC X(5) VALUE 'NOTA3'.
           05 FILLER                          PIC X(5) VALUE 'NOTA4'.
           05 FILLER                          PIC X(6) VALUE 'STATUS'.

       01 DET001.
           05 FILLER                          PIC X(1) VALUE' '.
           05 DET001-CODIGO                   PIC 9(4).
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-NOME                     PIC X(25).
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-MATERIA                  PIC X(11).
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-NOTA1                    PIC Z.ZZ9.
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-NOTA2                    PIC Z.ZZ9.
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-NOTA3                    PIC Z.ZZ9.
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-NOTA4                    PIC Z.ZZ9.
           05 FILLER                          PIC X(3) VALUE SPACES.
           05 DET001-STATUS                   PIC X(11).



       PROCEDURE DIVISION.

            SET EOF-OK     TO FALSE.
            SET WS-COUNT   TO   0.

           OPEN INPUT CAD

             PERFORM UNTIL EOF-OK
                  IF FS-OK THEN
                  READ CAD INTO CADASTRO
                    AT END
                    SET EOF-OK   TO  TRUE
                NOT AT END
                ADD 1 TO WS-COUNT


            DISPLAY "**************************************************"
                DISPLAY "REGISTRO:"WS-ID
                DISPLAY "NOME:"WS-NM
                DISPLAY "MATERIA:"WS-MATERIA
                DISPLAY "MEDIA:"WS-MEDIA
                DISPLAY "STATUS:"WS-STATUS

           DISPLAY "***************************************************"

                ELSE
                DISPLAY "NAO FOI POSSIVEL LISTAR, WS-FS "WS-FS

               END-IF
               END-PERFORM
               CLOSE CAD.


            STOP RUN.
       END PROGRAM SISTTLIST.
