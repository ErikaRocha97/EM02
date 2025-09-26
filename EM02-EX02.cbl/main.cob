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
           SELECT CADCLI   ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELCLI   ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       
       FD CADCLI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADCLI.DAT".
           
       01 REG-CLI.
           02 CPF-CLI      PIC 9(11).
           02 NOME-CLI     PIC X(30).
           02 ESTADO-CLI   PIC X(02).
           02 CIDADE-CLI   PIC X(30).
           02 TEL-CLI.
               03 TEL-CLI01   PIC 9(04).
               03 TEL-CLI02   PIC 9(04).
           02 DATA-CLI.
               03 DATA-DIA PIC 9(02).
               03 DATA-MES PIC 9(02).
               03 DATA-ANO PIC 9(04).

       FD RELCLI
           LABEL RECORD IS OMITTED.
           
       01 REG-REL PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       77 CT-LIN      PIC 9(02) VALUE 25. 
       77 CT-PAG      PIC 9(02) VALUE ZEROES.
       
      *Cabeçalho com o numero da página
       01 CAB-01.
           02 FILLER  PIC X(68) VALUE SPACES.
           02 FILLER  PIC X(05) VALUE "PAG. ".
           02 VAR-PAG PIC 99.
           02 FILLER  PIC X(05) VALUE SPACES.
           
      *Cabeçalho com o título     
       01 CAB-02.
           02 FILLER  PIC X(22) VALUE SPACES.
           02 FILLER  PIC X(24) VALUE "CLIENTES DO PERIODO DE: ".
           02 FILLER  PIC X(01) VALUE SPACES.
           02 FILLER  PIC X(11) VALUE "2010 A 2011".
           02 FILLER  PIC X(22) VALUE SPACES.
           
      *Cabeçalho da tabela do relatório     
       01 CAB-03.
           02 FILLER  PIC X(18) VALUE SPACES.
           02 FILLER  PIC X(04) VALUE "NOME".
           02 FILLER  PIC X(17) VALUE SPACES.
           02 FILLER  PIC X(06) VALUE "ESTADO". 
           02 FILLER  PIC X(21) VALUE SPACES.
           02 FILLER  PIC X(08) VALUE "TELEFONE".
           02 FILLER  PIC X(05) VALUE SPACES.

           
      *Cabeçalho da tabela do relatório     
       01 SEPARADOR.
           02 FILLER  PIC X(80) VALUE ALL "-".
           
      *Dados da tabela do relatório     
       01 DETALHE.
           02 FILLER          PIC X(05) VALUE SPACES.
           02 NOME-REL        PIC X(30).
           02 FILLER          PIC X(06) VALUE SPACES.
           02 ESTADO-REL      PIC X(02).
           02 FILLER          PIC X(23) VALUE SPACES.
           02 TEL-REL         PIC X(09).
           02 FILLER          PIC X(05) VALUE SPACES.
       
       01 TEL-FMT.
           05 TEL-REL01   PIC 9(04).
           05 FILLER    PIC X(01) VALUE "-".
           05 TEL-REL02   PIC 9(04).
           
       01 RODAPE-01.
           02 FILLER        PIC X(05)        VALUE SPACES.
           02 FILLER        PIC X(19)         
                            VALUE "TOTAL DE CLIENTES: ".
           02 FILLER        PIC X(05)        VALUE SPACES.
           02 TOTAL-CLI     PIC 9(04)        VALUE 0.
           02 FILLER        PIC X(45)        VALUE SPACES.

       PROCEDURE DIVISION.
       
       EXEMPLO-IMPRESSAO.
           PERFORM INICIO. 
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.
           
       INICIO.
           OPEN INPUT CADCLI
           OPEN OUTPUT RELCLI
           PERFORM LEITURA.
       
       LEITURA.
           READ CADCLI AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           PERFORM IMPRESSAO
           PERFORM LEITURA. 

       IMPRESSAO.
      * imprime cabeçalho quando atinge 25 linhas.
           IF CT-LIN GREATER THAN 24
               PERFORM CABECALHO
           END-IF.
      *    IF DATA-ANO > 2009 AND DATA-ANO < 2012
               PERFORM IMPDET.
      *    END-IF.
           
       IMPDET.
           MOVE NOME-CLI  TO NOME-REL
           MOVE ESTADO-CLI TO ESTADO-REL
      * formata telefone
           MOVE TEL-CLI01 TO TEL-REL01
           MOVE TEL-CLI02 TO TEL-REL02
           MOVE TEL-FMT TO TEL-REL
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE
      * soma 1 linha e 1 cliente
           ADD 1 TO CT-LIN
           ADD 1 TO TOTAL-CLI.

       CABECALHO.
           ADD 1       TO CT-PAG.
           MOVE CT-PAG TO VAR-PAG.
           MOVE SPACES TO REG-REL.
           WRITE REG-REL AFTER ADVANCING PAGE.
           WRITE REG-REL FROM CAB-01    AFTER ADVANCING 1 LINE.
           WRITE REG-REL FROM CAB-02    AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM CAB-03    AFTER ADVANCING 3 LINES.
           WRITE REG-REL FROM SEPARADOR AFTER ADVANCING 1 LINES.
      *REL-REL SERVE PARA ADICIONAR LINHA EM BRANCO     
           MOVE SPACES TO REG-REL
           WRITE REG-REL AFTER ADVANCING 1 LINE.
      *ZERA O CONTADOR DE LINHA
           MOVE ZEROES TO CT-LIN.
           
       RODAPE. 
           WRITE REG-REL FROM RODAPE-01 AFTER ADVANCING 3 LINE.

       FIM.
           PERFORM RODAPE.
           CLOSE   CADCLI RELCLI.

         