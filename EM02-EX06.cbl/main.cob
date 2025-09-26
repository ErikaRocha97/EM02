       IDENTIFICATION DIVISION.
       PROGRAM-ID.   IMPRIME.
       AUTHOR.       ERIKA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 16-09-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      LE UM REGISTRO E IMPRIME UM RELATORIO.
      
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADOK  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELOCOR  ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       
       FD CADCLI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADCLI.DAT".
           
       01 REGCLI.
           02 CODIGO-CLI PIC 9(03).
           02 CPF-CLI    PIC 9(11).
           
       FD CADOK
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADOK.DAT".

       01 REGOK.
           02 CPF-OK  PIC 9(11).     
           
       FD RELOCOR
           LABEL RECORD IS OMITTED.
           
       01 REG-REL PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       
       77 I           PIC 9(02).
       77 PESO        PIC 9(02).
       77 SOMA        PIC 9(10).
       77 AUX         PIC 9(10).
       77 DIGITO1     PIC 9(01).
       77 DIGITO2     PIC 9(01).
       77 QUOCIENT    PIC 9(10).
       77 RESTO       PIC 9(10).
       77 CPF-VALIDO  PIC X(03).
       
       01 DETALHE.
           02 FILLER        PIC X(10) VALUE SPACES.
           02 CODIGO-REL    PIC 9(03).
           02 FILLER        PIC X(10) VALUE SPACES.
           02 MENSAGEM-ERRO PIC X(30).
       
       01 CPF-DIGITOS   PIC 9 OCCURS 11 TIMES.

       PROCEDURE DIVISION.

       EXEMPLO-IMPRESSAO.
           PERFORM INICIO. 
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.
           
       INICIO.
           OPEN INPUT CADCLI
           OPEN OUTPUT CADOK
           OPEN OUTPUT RELOCOR.
           PERFORM LEITURA.
       
       LEITURA.
           READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           PERFORM VERIFICA-CPF
               IF CPF-VALIDO = "SIM"
               THEN 
               PERFORM GRAVACAO
           ELSE 
               PERFORM IMPRESSAO
           END-IF
           PERFORM LEITURA. 
           
       GRAVACAO.
           MOVE CPF-CLI TO CPF-OK
           WRITE REGOK.
           
       IMPRESSAO.
           PERFORM IMPDET.
           
       IMPDET.
           MOVE CODIGO-CLI TO CODIGO-REL
           MOVE "CPF INVALIDO" TO MENSAGEM-ERRO
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.

       FIM.
           CLOSE CADCLI CADOK RELOCOR.
       
       VERIFICA-CPF.
           PERFORM CALCULA-CPF
           IF CPF-DIGITOS(10) = DIGITO1 
               AND CPF-DIGITOS(11) = DIGITO2 THEN
               MOVE "SIM" TO CPF-VALIDO
           ELSE 
               MOVE "NAO" TO CPF-VALIDO
           END-IF.
           
       CALCULA-CPF.
      *SEPARAR DIGITOS EM 1 ARRAY 
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
                MOVE CPF-CLI(I:1) TO CPF-DIGITOS(I)
           END-PERFORM
           
      *SOMAR DIGITOS
           MOVE 0 TO SOMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
                SUBTRACT I FROM 11 GIVING PESO
                MULTIPLY CPF-DIGITOS(I) BY PESO GIVING AUX
                ADD AUX TO SOMA
           END-PERFORM
           
      *CALCULO DO DIGITO1      
           DIVIDE SOMA BY 11 GIVING QUOCIENT REMAINDER RESTO
           IF RESTO < 2
                MOVE 0 TO DIGITO1
           ELSE
                SUBTRACT RESTO FROM 11 GIVING DIGITO1
           END-IF.
           
      *SOMAR DIGITOS
           MOVE 0 TO SOMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                SUBTRACT I FROM 12 GIVING PESO
                MULTIPLY CPF-DIGITOS(I) BY PESO GIVING AUX
                ADD AUX TO SOMA
           END-PERFORM
           
      *CALCULO DO DIGITO2  
           DIVIDE SOMA BY 11 GIVING QUOCIENT REMAINDER RESTO
           IF RESTO < 2
                MOVE 0 TO DIGITO2
           ELSE
                SUBTRACT RESTO FROM 11 GIVING DIGITO2
           END-IF.
           
       
