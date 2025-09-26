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
           SELECT CADSOC1  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CADSOC2  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELSOCIO  ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       
       FD CADSOC1
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADSOC1.DAT".
           
       01 REG-SOC1.
           02 COD-PAG1     PIC 9(02).
           02 NUM-SOCIO1   PIC 9(06).
           02 NOME-SOCIO1  PIC X(30).
           02 VALOR-PAG1   PIC 9(09)V9(02).
           
       FD CADSOC2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADSOC2.DAT".

       01 REG-SOC2.
           02 NUM-SOCIO2   PIC 9(06).
           02 NOME-SOCIO2  PIC X(30).
           02 VALOR-PAG2   PIC 9(09)V9(02).       
           
       FD RELSOCIO
           LABEL RECORD IS OMITTED.
           
       01 REG-REL PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       77 CT-LIN      PIC 9(02) VALUE 30. 
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       
      *Cabeçalho com o numero da página
       01 CAB-01.
           02 FILLER  PIC X(70) VALUE SPACES.
           02 FILLER  PIC X(05) VALUE "PAG.".
           02 VAR-PAG PIC Z9.
           02 FILLER  PIC X(03) VALUE SPACES.
           
      *Cabeçalho com o título     
       01 CAB-02.
           02 FILLER  PIC X(25) VALUE SPACES.
           02 FILLER  PIC X(19) VALUE "RELATORIO DE SOCIOS".
           02 FILLER  PIC X(01) VALUE SPACES.
           02 FILLER  PIC X(09) VALUE "ATRASADOS".
           02 FILLER  PIC X(26) VALUE SPACES.
           
      *Cabeçalho da tabela do relatório     
       01 CAB-03.
           02 FILLER  PIC X(15) VALUE "NUMERO DO SOCIO".
           02 FILLER  PIC X(17) VALUE SPACES.
           02 FILLER  PIC X(13) VALUE "NOME DO SOCIO". 
           02 FILLER  PIC X(17) VALUE SPACES.
           02 FILLER  PIC X(18) VALUE "VALOR DO PAGAMENTO".
           
      *Dados da tabela do relatório     
       01 DETALHE.
           02 FILLER          PIC X(4) VALUE SPACES.
           02 NUM-SOCIO3      PIC 9(06).
           02 FILLER          PIC X(15) VALUE SPACES.
           02 NOME-SOCIO3     PIC X(30).
           02 FILLER          PIC X(5) VALUE SPACES.
           02 VALOR-PAG3      PIC 9(09)V9(02).
           
       01 RODAPE-01.
           02 FILLER        PIC X(25)         
                            VALUE "TOTAL DE SOCIOS ATRASADOS".
           02 FILLER        PIC X(05)        VALUE SPACES.
           02 TOTAL-ATRASO  PIC 9(06)        VALUE 0.
           02 FILLER        PIC X(44)        VALUE SPACES.
           
           
       01 RODAPE-02.
           02 FILLER        PIC X(20)  
                            VALUE "VALOR TOTAL ATRASADO".
           02 FILLER        PIC X(10)       VALUE SPACES.
           02 VALOR-ATRASO  PIC 9(09)V9(02) VALUE 0.
           02 FILLER        PIC X(39)       VALUE SPACES.

       PROCEDURE DIVISION.
       
       EXEMPLO-IMPRESSAO.
           PERFORM INICIO. 
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.
           
       INICIO.
           OPEN INPUT CADSOC1
           OPEN OUTPUT CADSOC2
           OPEN OUTPUT RELSOCIO.
       PERFORM LEITURA.
       
       LEITURA.
           READ CADSOC1 AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           IF COD-PAG1 = 1 THEN 
               PERFORM GRAVACAO
           ELSE IF COD-PAG1 = 2 THEN PERFORM IMPRESSAO
           END-IF
           END-IF.
           PERFORM LEITURA. 
           
       GRAVACAO.
           MOVE NUM-SOCIO1  TO NUM-SOCIO2
           MOVE NOME-SOCIO1 TO NOME-SOCIO2
           MOVE VALOR-PAG1  TO VALOR-PAG2
           WRITE REG-SOC2.
           
       IMPRESSAO.
      *IMPRIME CABEÇALHO QUANDO ATINGE 30 LINHAS
           IF CT-LIN GREATER THAN 29
               PERFORM CABECALHO
           END-IF.
           PERFORM IMPDET.
           
       IMPDET.
           MOVE NUM-SOCIO1  TO NUM-SOCIO3.
           MOVE NOME-SOCIO1 TO NOME-SOCIO3.
           MOVE VALOR-PAG1  TO VALOR-PAG3.
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
      *SOMA 1 LINHA NO TOTAL     
           ADD 1 TO CT-LIN.
           IF COD-PAG1 = 2 THEN 
               ADD 1 TO TOTAL-ATRASO
               ADD VALOR-PAG1 TO VALOR-ATRASO
           END-IF.

       CABECALHO.
           ADD 1       TO CT-PAG.
           MOVE CT-PAG TO VAR-PAG.
           MOVE SPACES TO REG-REL.
           WRITE REG-REL AFTER ADVANCING PAGE.
           WRITE REG-REL FROM CAB-01 AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-02 AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-03 AFTER ADVANCING 3 LINES.
      *REL-REL SERVE PARA ADICIONAR LINHA EM BRANCO     
           MOVE SPACES TO REG-REL
           WRITE REG-REL AFTER ADVANCING 1 LINE.
      *ZERA O CONTADOR DE LINHA
           MOVE ZEROES TO CT-LIN.
           
       RODAPE. 
           WRITE REG-REL FROM RODAPE-01 AFTER ADVANCING 3 LINE.
           WRITE REG-REL FROM RODAPE-02 AFTER ADVANCING 1 LINE.
           MOVE 0 TO TOTAL-ATRASO.
           MOVE 0 TO VALOR-ATRASO.
           
       FIM.
           PERFORM RODAPE.
           CLOSE CADSOC1 CADSOC2 RELSOCIO.

         