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
           SELECT ARQALU  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQREP  ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELAPROV  ASSIGN TO DISK.

       DATA DIVISION.

       FILE SECTION.
       
       FD ARQALU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "ARQALU.DAT".
           
       01 REG-ALU.
           02 MATRICULA-ALU. 
              03 MATRICULA-01 PIC 9(06).
              03 MATRICULA-02 PIC 9(01).
        
           02 NOME-ALU      PIC X(30).
           02 NOTA-ALU1     PIC 9(02)V9(02).
           02 NOTA-ALU2     PIC 9(02)V9(02).
           02 NOTA-ALU3     PIC 9(02)V9(02).
           02 FALTAS-ALU    PIC 9(02).
           02 SEXO-ALU      PIC X(01).
       
       FD ARQREP
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "ARQREP.DAT".
           
       01 REG-REP.
           02 MATRICULA-REP PIC 9(07).
           02 NOME-REP      PIC X(30).
           02 NOTA-REP1     PIC 9(02)V9(02).
           02 NOTA-REP2     PIC 9(02)V9(02).
           02 NOTA-REP3     PIC 9(02)V9(02).
           02 FALTAS-REP    PIC 9(02).
           02 SEXO-REP      PIC X(01).
           
       FD RELAPROV
           LABEL RECORD IS OMITTED.
           
       01 REG-REL PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ       PIC X(03) VALUE "NAO".
       77 CT-LIN        PIC 9(02) VALUE 30. 
       77 CT-PAG        PIC 9(02) VALUE ZEROES.
       
       77 SOMA          PIC 9(02)V99 VALUE 0.
       77 MEDIA-CALC    PIC 9(02)V99 VALUE 0.
       
       77 MEDIA-FMT     PIC Z9,99.
       77 FALTAS-FMT    PIC Z(2).
       
       01 MATRICULA-FMT.
           05 MAT1   PIC 9(06).
           05 FILLER PIC X(01) VALUE "-".
           05 MAT2   PIC 9(01).
       
      *Cabeçalho com o numero da página
       01 CAB-01.
           02 FILLER    PIC X(70) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "PAG.".
           02 VAR-PAG   PIC Z9.
           02 FILLER    PIC X(03) VALUE SPACES.
           
      *Cabeçalho com o título     
       01 CAB-02.
           02 FILLER    PIC X(26) VALUE SPACES.
           02 FILLER    PIC X(17) VALUE "RELACAO DE ALUNOS".
           02 FILLER    PIC X(01) VALUE SPACES.
           02 FILLER    PIC X(09) VALUE "APROVADOS".
           02 FILLER    PIC X(27) VALUE SPACES.
           
      *Cabeçalho da tabela do relatório     
       01 CAB-03.
           02 FILLER    PIC X(01) VALUE SPACES.
           02 FILLER    PIC X(06) VALUE "NUMERO".
           02 FILLER    PIC X(15) VALUE SPACES.
           02 FILLER    PIC X(13) VALUE "NOME DO ALUNO". 
           02 FILLER    PIC X(16) VALUE SPACES.
           02 FILLER    PIC X(05) VALUE "MEDIA".
           02 FILLER    PIC X(17) VALUE SPACES.
           02 FILLER    PIC X(06) VALUE "FALTAS".
           
      *Dados da tabela do relatório     
       01 DETALHE.
           02 MATRICULA-REL PIC X(08).
           02 FILLER        PIC X(08) VALUE SPACES.
           02 NOME-REL      PIC X(30).
           02 FILLER        PIC X(05) VALUE SPACES.
           02 MEDIA-REL     PIC X(05).
           02 FILLER        PIC X(19) VALUE SPACES.
           02 FALTAS-REL    PIC X(02).
           02 FILLER        PIC X(03) VALUE SPACES.

       PROCEDURE DIVISION.
       
       EXEMPLO-IMPRESSAO.
           PERFORM INICIO. 
           PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM FIM.
           STOP RUN.
       
       INICIO.
           OPEN INPUT ARQALU
           OPEN OUTPUT ARQREP
           OPEN OUTPUT RELAPROV.
       PERFORM LEITURA.
       
       LEITURA.
           READ ARQALU AT END MOVE "SIM" TO FIM-ARQ.
           
       PRINCIPAL.
           PERFORM CALCULAMEDIA.
           IF MEDIA-CALC >= 7 AND FALTAS-ALU < 16 THEN 
               PERFORM IMPRESSAO
           ELSE PERFORM GRAVACAO
           END-IF.
           PERFORM LEITURA. 
           PERFORM LIMPAVARIAVEIS.
           
       CALCULAMEDIA.
           ADD NOTA-ALU1, NOTA-ALU2, NOTA-ALU3 TO SOMA.
           DIVIDE SOMA BY 3 GIVING MEDIA-CALC.
           
       LIMPAVARIAVEIS.
               MOVE 0 TO SOMA
               MOVE 0 TO MEDIA-CALC.
       
       GRAVACAO.
           MOVE MATRICULA-ALU TO MATRICULA-REP
           MOVE NOME-ALU      TO NOME-REP
           MOVE NOTA-ALU1     TO NOTA-REP1
           MOVE NOTA-ALU2     TO NOTA-REP2
           MOVE NOTA-ALU3     TO NOTA-REP3
           MOVE FALTAS-ALU    TO FALTAS-REP
           MOVE SEXO-ALU      TO SEXO-REP
           WRITE REG-REP.

       IMPRESSAO.
      *IMPRIME CABEÇALHO QUANDO ATINGE 30 LINHAS
      *CT-LIN INICIA VALENDO 30
           IF CT-LIN GREATER THAN 29
               PERFORM CABECALHO
           END-IF.
           PERFORM IMPDET.
           
       IMPDET.
           MOVE NOME-ALU TO NOME-REL.
      *FORMATA MATRICULA
           MOVE MATRICULA-01 TO MAT1.
           MOVE MATRICULA-02 TO MAT2.
           MOVE MATRICULA-FMT TO MATRICULA-REL.
      *FORMATA MEDIA     
           MOVE MEDIA-CALC TO MEDIA-FMT.
           MOVE MEDIA-FMT TO MEDIA-REL.
      *FORMATA FALTAS
           MOVE FALTAS-ALU TO FALTAS-FMT.
           MOVE FALTAS-FMT TO FALTAS-REL.
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.
           ADD 1 TO CT-LIN.
 
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

       FIM.
           CLOSE ARQALU ARQREP RELAPROV.