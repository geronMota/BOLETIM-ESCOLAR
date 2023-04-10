      ******************************************************************
      * Author:JEFFERSON MOTA
      * Date:27/03/23
      * Purpose:LER ARQUIVOS DE ALUNOS APROVRADOS E REPROVADOS
      *         E MOVER APROVADOR PARA O ARQUIVO MOVEAPRO.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESSA.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT CAD ASSIGN TO
             "C:\Users\PC\Desktop\ProjetosJavaScript\COBOL5\CAD.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS FD-ID
             FILE STATUS WS-FS.

             SELECT MOVEAPRO ASSIGN TO
           "C:\Users\PC\Desktop\ProjetosJavaScript\COBOL5\MOVEAPRO.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS FD-ID-APRO
             FILE STATUS WS-FS-MOVEAPRO.

             SELECT MOVEREPRO ASSIGN TO
           "C:\Users\PC\Desktop\ProjetosJavaScript\COBOL5\MOVEREPRO.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC
             RECORD KEY IS FD-ID-REPRO
             FILE STATUS WS-FS-MOVEREPRO.

       DATA DIVISION.
       FILE SECTION.
       FD CAD.
       01 REGISTRO.
           03 FD-ID                               PIC 9(4).
           03 FD-NM                               PIC X(25).
           03 FD-MATERIA                          PIC X(11).
           03 FD-STATUS                           PIC X(11).
           03 FD-MEDIA                            PIC 9(1)V9.

       FD MOVEAPRO.
       01 REGISTRO-APRO.
           03 FD-ID-APRO                          PIC 9(4).
           03 FD-NM-APRO                          PIC X(25).
           03 FD-MATERIA-APRO                     PIC X(11).
           03 FD-STATUS-APRO                      PIC X(11).
           03 FD-MEDIA-APRO                       PIC 9(2)V9.

       FD MOVEREPRO.
       01 REGISTRO-REPRO.
           03 FD-ID-REPRO                          PIC 9(4).
           03 FD-NM-REPRO                          PIC X(25).
           03 FD-MATERIA-REPRO                     PIC X(11).
           03 FD-STATUS-REPRO                      PIC X(11).
           03 FD-MEDIA-REPRO                       PIC 9(2)V9.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS.
           03 WS-APROVADOS                        PIC 9(6).
           03 WS-REPROVADOS                       PIC 9(6).
           03 WS-LIDOS                            PIC 9(6).

       77 WS-FS-MOVEREPRO                          PIC 99.
         88 FS-MOVEREPRO-OK                        VALUE 0.
       77 WS-FS-MOVEAPRO                          PIC 99.
         88 FS-MOVEAPRO-OK                        VALUE 0.
       77 WS-FS                                   PIC 99.
         88 FS-OK                                 VALUE 0.
       77 WS-EOF                                  PIC X.
         88 EOF-OK                                VALUE "V" FALSE "F".

       77 WS-OPCAO                                PIC X.

       PROCEDURE DIVISION.
       P100-LER-E-GRAVAR.



           OPEN I-O CAD MOVEAPRO MOVEREPRO.

           IF WS-FS-MOVEAPRO AND WS-FS-MOVEREPRO EQUAL 35
               OPEN OUTPUT MOVEAPRO MOVEREPRO
           END-IF.


           LER-ARQUIVO.

           READ CAD
           AT END
           GO TO FINALIZAR
           NOT AT END
           ADD 1 TO WS-LIDOS


           IF FD-STATUS EQUAL "APROVADO"

           MOVE   FD-ID        TO FD-ID-APRO
           MOVE   FD-NM        TO FD-NM-APRO
           MOVE   FD-MATERIA   TO FD-MATERIA-APRO
           MOVE   FD-STATUS    TO FD-STATUS-APRO
           MOVE   FD-MEDIA     TO FD-MEDIA-APRO

           WRITE REGISTRO-APRO

           ADD 1 TO WS-APROVADOS

           ELSE
           IF FD-STATUS EQUAL "REPROVADO"

           MOVE   FD-ID        TO FD-ID-REPRO
           MOVE   FD-NM        TO FD-NM-REPRO
           MOVE   FD-MATERIA   TO FD-MATERIA-REPRO
           MOVE   FD-STATUS    TO FD-STATUS-REPRO
           MOVE   FD-MEDIA     TO FD-MEDIA-REPRO


           WRITE REGISTRO-REPRO

           ADD 1 TO WS-REPROVADOS

           END-IF.

               GO TO LER-ARQUIVO.

           FINALIZAR.
            CLOSE CAD  MOVEAPRO MOVEREPRO
            DISPLAY "REGISTROS LIDOS............:"WS-LIDOS
            DISPLAY "REGISTROS ALUNOS APROVADOS.:"WS-APROVADOS
            DISPLAY "REGISTROS ALUNOS REPROVADOS:"WS-REPROVADOS

           DISPLAY "OBRIGADO POR USAR ESSE PROGRAMA. FIM".
            STOP RUN.
       END PROGRAM PROCESSA.
