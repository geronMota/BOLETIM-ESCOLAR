      ******************************************************************
      * Author:JEFFERSON MOTA SILVA
      * Date:12/02/2023
      * Purpose:PRACTICE COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SISTTCAD.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT CAD ASSIGN TO
             "C:\Users\PC\Desktop\ProjetosJavaScript\COBOL5\CAD.DAT"
             ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM
             RECORD KEY IS FD-ID
             FILE STATUS WS-FS.



       DATA DIVISION.
       FILE SECTION.
       FD CAD.
       01 REGISTRO.
           03 FD-ID                           PIC 9(4).
           03 FD-NM                           PIC X(25).
           03 FD-MATERIA                      PIC X(11).
           03 FD-STATUS                       PIC X(11).
           03 FD-MEDIA                        PIC 9(1)V9.




       WORKING-STORAGE SECTION.

       77 WS-FS                               PIC 99.
         88 FS-OK                             VALUE 0.
       77 WS-EOF                              PIC X.
         88 EOF-OK                            VALUE "V" FALSE "F".
       77 WS-OPCAO                            PIC X.
       77 WS-COUNT                            PIC 9(4).
       77 WS-CONTAINER                        PIC 9(2).

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

       PROCEDURE DIVISION.
           INICIO.
              OPEN I-O CAD

           DISPLAY "==================================================="
           DISPLAY "***************CADASTRO DE ALUNOS******************"
           DISPLAY "==================================================="

           DISPLAY "DIGITE UM ID PARA O ALUNO"
           ACCEPT WS-ID

           DISPLAY "==================================================="
           DISPLAY "DIGITE O NOME PARA O ALUNO"
           ACCEPT WS-NM
           DISPLAY "==================================================="
           DISPLAY "DIGITE A MATERIA PARA O ALUNO"
           ACCEPT WS-MATERIA
           DISPLAY "==================================================="
           DISPLAY "DIGITE A NOTA1 PARA O ALUNO"
           ACCEPT NT1
           DISPLAY "==================================================="
           DISPLAY "DIGITE A NOTA2 PARA O ALUNO"
           ACCEPT NT2
           DISPLAY "==================================================="
           DISPLAY "DIGITE A NOTA3 PARA O ALUNO"
           ACCEPT NT3
           DISPLAY "==================================================="
           DISPLAY "DIGITE A NOTA4 PARA O ALUNO"
           ACCEPT NT4
           DISPLAY "==================================================="


           IF WS-FS EQUAL 35 THEN
               OPEN OUTPUT CAD
           END-IF.

           COMPUTE WS-MEDIA = (NT1 + NT2 + NT3 + NT4)/4

           IF WS-MEDIA >= 7
               MOVE "APROVADO"    TO WS-STATUS
               DISPLAY "STATUS:"WS-STATUS
               DISPLAY "MEDIA:"WS-MEDIA
           ELSE
           IF WS-MEDIA < 7
               MOVE  "REPROVADO"  TO WS-STATUS
               DISPLAY "STATUS:"WS-STATUS
               DISPLAY "MEDIA:"WS-MEDIA
           END-IF.
                  IF FS-OK THEN

               MOVE WS-ID            TO   FD-ID
               MOVE WS-NM            TO   FD-NM
               MOVE WS-MATERIA       TO   FD-MATERIA
               MOVE WS-STATUS        TO   FD-STATUS
               MOVE WS-MEDIA         TO   FD-MEDIA

               WRITE REGISTRO
               INVALID KEY
               DISPLAY "DUBBLE KEY"
               DISPLAY "NAO FOI POSSIVEL GRAVAR,WS-FS: "WS-FS
           NOT INVALID KEY
               DISPLAY "GRAVADO COM SUCESSO"
               END-WRITE
               END-IF.

               CLOSE CAD.
           DISPLAY "=================================================".
            DISPLAY "***********O RESULTADO DO PROCESSAMENTO**********".
            DISPLAY "NOME DO ALUNO           :"WS-NM.
            DISPLAY "MATERIA                 :"WS-MATERIA.
            DISPLAY "MEDIA                   :"WS-MEDIA.
            DISPLAY "STATUS                  :"WS-STATUS.
            DISPLAY "*************************************************".
            DISPLAY "SE DESEJA CONTINUAR CALCULANDO TECLE 'S' , SE NAO,"
            "TECLE 'N' PARA TERMINAR O PROGRAMA".
            ACCEPT WS-OPCAO.
            IF WS-OPCAO = "S" THEN
                GO TO INICIO.
            IF WS-OPCAO = "N" THEN
                GO TO FINALIZAR.


           FINALIZAR.
            STOP RUN.
       END PROGRAM SISTTCAD.
