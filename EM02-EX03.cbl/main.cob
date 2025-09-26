       IDENTIFICATION DIVISION.
       PROGRAM-ID.   XXXXX.
       AUTHOR.       ERIKA.
       INSTALLATION. FATEC-SP.
       DATE-WRITTEN. 24-09-2025.
       DATE-COMPILED.
       SECURITY.     APENAS O AUTOR PODE MODIFICAR.
      *REMARKS.      XXXXX.
      
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADENT   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELSAI   ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       
       FD CADENT
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADENT.DAT".
           
       01 REG-ENT.
           02 COD-CLI      PIC 9(07).
           02 NOME-CLI     PIC X(30).
           02 SALARIO-CLI  PIC 9(05)V9(02).
           02 SEXO-CLI     PIC X(01).
           

       FD RELSAI
           LABEL RECORD IS OMITTED.
           
       01 REG-SAI PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       77 CT-LIN      PIC 9(02) VALUE 20. 
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       77 CT-CLI      PIC 9(04) VALUE 0.
       77 CT-SAL      PIC 9(05)V9(02).
       
      *Cabeçalho com o numero da página
       01 CAB-01.
           02 FILLER  PIC X(67) VALUE SPACES.
           02 FILLER  PIC X(05) VALUE "PAG. ".
           02 VAR-PAG PIC ZZ9.
           02 FILLER  PIC X(05) VALUE SPACES.
           
      *Cabeçalho com o título     
       01 CAB-02.
           02 FILLER  PIC X(21) VALUE SPACES.
           02 FILLER  PIC X(21) VALUE "LISTAGEM DE CLIENTES ".
           02 FILLER  PIC X(01) VALUE SPACES.
           02 FILLER  PIC X(16) VALUE "DO SEXO FEMININO".
           02 FILLER  PIC X(21) VALUE SPACES.
       
       01 CAB-03.
           02 FILLER  PIC X(22) VALUE SPACES.
           02 FILLER  PIC X(24) VALUE "COM SALARIOS SUPERIORES ".
           02 FILLER  PIC X(01) VALUE SPACES.
           02 FILLER  PIC X(10) VALUE "A 5.000,00".
           02 FILLER  PIC X(23) VALUE SPACES.
           
      *Cabeçalho da tabela do relatório     
       01 CAB-04.
           02 FILLER  PIC X(4) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "CODIGO".
           02 FILLER  PIC X(14) VALUE SPACES.
           02 FILLER  PIC X(04) VALUE "NOME". 
           02 FILLER  PIC X(40) VALUE SPACES.
           02 FILLER  PIC X(07) VALUE "SALARIO".
           02 FILLER  PIC X(5) VALUE SPACES.

      *Dados da tabela do relatório     
       01 DETALHE.
           02 FILLER          PIC X(04) VALUE SPACES.
           02 COD-REL         PIC 9(07).
           02 FILLER          PIC X(13) VALUE SPACES.
           02 NOME-REL        PIC X(30).
           02 FILLER          PIC X(13) VALUE SPACES.
           02 SALARIO-REL     PIC ZZ.999,99.
           02 FILLER          PIC X(06) VALUE SPACES.

       01 RODAPE-01.
           02 FILLER        PIC X(04)        VALUE SPACES.
           02 FILLER        PIC X(29)         
                            VALUE "TOTAL DE CLIENTES IMPRESSOS: ".
           02 FILLER        PIC X(3)        VALUE SPACES.
           02 CLI-FMT       PIC ZZ.999.
           02 FILLER        PIC X(30)        VALUE SPACES.
       
       01 RODAPE-02.
           02 FILLER        PIC X(04)        VALUE SPACES.
           02 FILLER        PIC X(18)         
                            VALUE "TOTAL DE SALARIO: ".
           02 FILLER        PIC X(6)        VALUE SPACES.
           02 SAL-FMT       PIC ZZZ.ZZZ.ZZ9,99.
           02 FILLER        PIC X(38)        VALUE SPACES.
           
       PROCEDURE DIVISION.
       
       EXEMPLO-IMPRESSAO.
           PERFORM INICIO. 
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.
           
       INICIO.
           OPEN INPUT CADENT
           OPEN OUTPUT RELSAI
           PERFORM LEITURA.
       
       LEITURA.
           READ CADENT AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           PERFORM IMPRESSAO
           PERFORM LEITURA. 

       IMPRESSAO.
           IF CT-LIN GREATER THAN 19
               PERFORM CABECALHO
           END-IF.
           IF SEXO-CLI = "F" AND SALARIO-CLI >= 5000 
               PERFORM IMPDET
           END-IF.

       IMPDET.
           MOVE COD-CLI  TO COD-REL
           MOVE NOME-CLI TO NOME-REL
           MOVE SALARIO-CLI TO SALARIO-REL
           WRITE REG-SAI FROM DETALHE AFTER ADVANCING 1 LINE
           ADD SALARIO-CLI TO CT-SAL
           ADD 1 TO CT-LIN
           ADD 1 TO CT-CLI.

       CABECALHO.
           ADD 1       TO CT-PAG.
           MOVE CT-PAG TO VAR-PAG.
           MOVE SPACES TO REG-SAI.
           WRITE REG-SAI AFTER ADVANCING PAGE.
           WRITE REG-SAI FROM CAB-01    AFTER ADVANCING 1 LINE.
           WRITE REG-SAI FROM CAB-02    AFTER ADVANCING 1 LINES.
           WRITE REG-SAI FROM CAB-03    AFTER ADVANCING 1 LINES.
           WRITE REG-SAI FROM CAB-04    AFTER ADVANCING 3 LINES.
           MOVE SPACES TO REG-SAI
           WRITE REG-SAI AFTER ADVANCING 1 LINE.
           MOVE ZEROES TO CT-LIN.
           
       RODAPE. 
           MOVE CT-CLI TO CLI-FMT
           MOVE CT-SAL TO SAL-FMT
           WRITE REG-SAI FROM RODAPE-01 AFTER ADVANCING 4 LINE.
           WRITE REG-SAI FROM RODAPE-02 AFTER ADVANCING 1 LINE.

       FIM.
           PERFORM RODAPE.
           CLOSE   CADENT RELSAI.

         