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
           02 CODIGO-CLI      PIC x(03).
           02 CPF-CLI         PIC X(11).
           02 CPF-R REDEFINES 
              CPF-CLI         PIC 9(11).
           02 NOME-CLI        PIC X(30).
           02 ESTADO-CLI      PIC X(02).
               88 ESTADO-VALIDO       
                              VALUE "AC" "AL" "AP" "AM" "BA"
                                    "CE" "DF" "ES" "GO" "MT"
                                    "MA" "MS" "MG" "PA" "PB"
                                    "PR" "PE" "PI" "RJ" "RN"
                                    "RS" "RO" "RR" "SC" "SP"
                                    "SE" "TO".
           02 CIDADE-CLI      PIC X(30).
           02 EMAIL-CLI       PIC X(30).
           02 TEL-CLI         PIC 9(10).
           
       FD CADOK
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADOK.DAT".

       01 REGOK.
           02 CPF-OK    PIC 9(11).     
           
       FD RELOCOR
           LABEL RECORD IS OMITTED.
           
       01 REG-REL PIC X(80).
       
       WORKING-STORAGE SECTION.

       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 PRIMEIRO-ERRO PIC X(03) VALUE "SIM".
       
       77 I             PIC 9(02).
       77 PESO          PIC 9(02).
       77 SOMA          PIC 9(10).
       77 AUX           PIC 9(10).
       77 DIGITO1       PIC 9(01).
       77 DIGITO2       PIC 9(01).
       77 QUOCIENT      PIC 9(10).
       77 RESTO         PIC 9(10).
       
       77 CPF-VALIDO    PIC X(03).
       77 NOME-VALIDO   PIC X(03).
       77 CIDADE-VALIDO PIC X(03).
       77 EMAIL-VALIDO  PIC X(03).
       77 TEL-VALIDO    PIC X(03).
       
       01 DETALHE.
           02 FILLER        PIC X(15) VALUE SPACES.
           02 CODIGO-REL    PIC x(03).
           02 FILLER        PIC X(17) VALUE SPACES.
           02 MENSAGEM-ERRO PIC X(45).
       
       01 CPF-DIGITOS   PIC 9 OCCURS 11 TIMES.
       
      *Cabeçalho com o título     
       01 CAB-01.
           02 FILLER  PIC X(31) VALUE SPACES.
           02 FILLER  PIC X(17) VALUE "TURISMAR TURISMOS".
           02 FILLER  PIC X(32) VALUE SPACES.
           
       01 CAB-02.
           02 FILLER  PIC X(17) VALUE SPACES.
           02 FILLER  PIC X(22) VALUE "RELATORIO DE DADOS DE ".
           02 FILLER  PIC X(23) VALUE "CLIENTES INCONSISTENTES".
           02 FILLER  PIC X(18) VALUE SPACES.
           
      *Cabeçalho da tabela do relatório     
       01 CAB-03.
           02 FILLER  PIC X(08) VALUE SPACES.
           02 FILLER  PIC X(17) VALUE "CODIGO DO CLIENTE".
           02 FILLER  PIC X(10) VALUE SPACES.
           02 FILLER  PIC X(15) VALUE "DADOS INVALIDOS". 
           02 FILLER  PIC X(30) VALUE SPACES.

      *Cabeçalho da tabela do relatório     
       01 SEPARADOR.
           02 FILLER  PIC X(80) VALUE ALL "-".

       PROCEDURE DIVISION.

       EXEMPLO-IMPRESSAO.
           PERFORM INICIO. 
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.
           
       INICIO.
           OPEN INPUT CADCLI
                OUTPUT CADOK
                OUTPUT RELOCOR
           PERFORM CABECALHO
           PERFORM LEITURA.
       
       LEITURA.
           READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           PERFORM VERIFICA-CAMPOS
           IF  CPF-VALIDO    = "SIM"  AND
               NOME-VALIDO   = "SIM"  AND
               ESTADO-VALIDO          AND
               CIDADE-VALIDO = "SIM"  AND 
               EMAIL-VALIDO  = "SIM"   
               THEN
               PERFORM GRAVACAO
           END-IF
           PERFORM LEITURA. 
           
       GRAVACAO.
           MOVE CPF-CLI TO CPF-OK
           WRITE REGOK.

       CABECALHO.
           WRITE REG-REL FROM CAB-01    AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-02    AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-03    AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM SEPARADOR AFTER ADVANCING 1 LINES.
              
       FIM.
           CLOSE CADCLI CADOK RELOCOR.
       
       VERIFICA-CAMPOS.
           MOVE "SIM" TO PRIMEIRO-ERRO
           PERFORM VERIFICA-CPF
           PERFORM VERIFICA-NOME
           PERFORM VERIFICA-ESTADO
           PERFORM VERIFICA-CIDADE
           PERFORM VERIFICA-EMAIL.
           
       VERIFICA-OCORRENCIA.
           IF PRIMEIRO-ERRO = "SIM"
               PERFORM PRIMEIRA-OCORRENCIA
           ELSE
               PERFORM OUTRA-OCORRENCIA
           END-IF.
       
       PRIMEIRA-OCORRENCIA.
           MOVE CODIGO-CLI TO CODIGO-REL
           MOVE "NAO" TO PRIMEIRO-ERRO
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 2 LINE.
           
       OUTRA-OCORRENCIA.
           MOVE SPACES TO CODIGO-REL
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
       
       VERIFICA-NOME.
           IF NOME-CLI = SPACES THEN 
               MOVE "NÃO" TO NOME-VALIDO
               MOVE "NOME NAO INFORMADO" TO MENSAGEM-ERRO
               PERFORM VERIFICA-OCORRENCIA
           ELSE 
               MOVE "SIM" TO NOME-VALIDO
           END-IF.
       
       VERIFICA-ESTADO.
           IF NOT ESTADO-VALIDO
               MOVE "ESTADO INVALIDO" TO MENSAGEM-ERRO
               PERFORM VERIFICA-OCORRENCIA
           END-IF.

       VERIFICA-CIDADE.
           IF CIDADE-CLI = SPACES
               MOVE "NAO" TO CIDADE-VALIDO
               MOVE "CIDADE NAO INFORMADA" TO MENSAGEM-ERRO
               PERFORM VERIFICA-OCORRENCIA
           ELSE
               MOVE "SIM" TO CIDADE-VALIDO
           END-IF.
       
       VERIFICA-EMAIL.
           IF EMAIL-CLI = SPACES
               MOVE "NAO" TO EMAIL-VALIDO
               MOVE "EMAIL NAO INFORMADO" TO MENSAGEM-ERRO
               PERFORM VERIFICA-OCORRENCIA
           ELSE
               MOVE "SIM" TO EMAIL-VALIDO
           END-IF.

       VERIFICA-CPF.
           IF  CPF-CLI IS NUMERIC
               PERFORM CALCULA-CPF
               IF  CPF-DIGITOS(10) = DIGITO1 AND 
                   CPF-DIGITOS(11) = DIGITO2 THEN
                   MOVE "SIM" TO CPF-VALIDO
               ELSE 
                   MOVE "NAO" TO CPF-VALIDO
                   MOVE "CPF INVALIDO" TO MENSAGEM-ERRO
                   PERFORM VERIFICA-OCORRENCIA
               END-IF    
           ELSE 
               MOVE "NAO" TO CPF-VALIDO
               MOVE "CPF INVALIDO" TO MENSAGEM-ERRO
               PERFORM VERIFICA-OCORRENCIA
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
           
       
