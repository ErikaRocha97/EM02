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
           02 CODIGO-CLI    PIC 9(03).
           02 CPF-CLI       PIC 9(11).
           02 NOME-CLI      PIC X(30).
           02 ESTADO-CLI    PIC X(02).
               88 ESTADO-VALIDO       VALUE "AC" "AL" "AP" "AM" "BA"
                                            "CE" "DF" "ES" "GO" "MT"
                                            "MA" "MS" "MG" "PA" "PB"
                                            "PR" "PE" "PI" "RJ" "RN"
                                            "RS" "RO" "RR" "SC" "SP"
                                            "SE" "TO".
           02 CIDADE-CLI    PIC X(30).
           02 EMAIL-CLI     PIC X(30).
           02 TELEFONE-CLI  PIC 9(10).
       
       FD CADOK
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS  "CADOK.DAT".
           
       01 REGOK.
           02 CPF-OK        PIC 9(11).
           02 NOME-OK       PIC X(30).
           02 ESTADO-OK     PIC X(02).
           02 CIDADE-OK     PIC X(30).
           02 EMAIL-OK      PIC X(30).
           02 TELEFONE-OK   PIC 9(10).
           
       FD RELOCOR
           LABEL RECORD IS OMITTED.
           
       01 REG-REL PIC X(80).
       
       WORKING-STORAGE SECTION.
       
       77 FIM-ARQ       PIC X(03) VALUE "NAO".

      *Verificar CPF 
       77 SOMA          PIC 9(05).
       77 RESTO         PIC 9(02).
       77 DIGITO1       PIC 9(01).
       77 DIGITO2       PIC 9(01).
       77 I             PIC 9(02).
       77 VALIDO        PIC X(03).
       77 QUOCIENT      PIC 9(05).
       77 PESO          PIC 9(02).
       77 AUX           PIC 9(04).
       
       01 CPF-DIGITOS  PIC 9 OCCURS 11 TIMES.

      *Imprimir cpf invalido no relatorio    
       01 DETALHE.
           02 CODIGO-REG     PIC 9(03).
           02 CPF-REG        PIC X(12).
           02 NOME-REG       PIC X(18).
           02 ESTADO-REG     PIC X(15).
           02 CIDADE-REG     PIC X(20).
           02 EMAIL-REG      PIC X(19).

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
           IF VALIDO = "SIM"
                PERFORM GRAVACAO
           ELSE
                MOVE CODIGO-CLI TO CODIGO-REG
                MOVE CPF-CLI    TO CPF-REG
                PERFORM IMPRESSAO
           END-IF
           PERFORM LEITURA. 

       GRAVACAO.
           MOVE CPF-CLI      TO CPF-OK.
           MOVE NOME-CLI     TO NOME-OK.
           MOVE ESTADO-CLI   TO ESTADO-OK.
           MOVE CIDADE-CLI   TO CIDADE-OK.
           MOVE EMAIL-CLI    TO EMAIL-OK.
           MOVE TELEFONE-CLI TO TELEFONE-OK.
           WRITE REGOK.

       IMPRESSAO.
           PERFORM IMPDET.
           
       IMPDET.
           WRITE REG-REL FROM DETALHE AFTER ADVANCING 1 LINE.

       FIM.
           CLOSE CADCLI CADOK RELOCOR.

       VERIFICA-CPF.
           IF FIM-ARQ = "SIM"
              EXIT PARAGRAPH
           END-IF

      *----SEPARAR CPF EM ARRAY
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 11
                MOVE CPF-CLI(I:1) TO CPF-DIGITOS(I)
           END-PERFORM

      *----CALCULO DO DIGITO1
           MOVE 0 TO SOMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
                SUBTRACT I FROM 11 GIVING PESO
                MULTIPLY CPF-DIGITOS(I) BY PESO GIVING AUX
                ADD AUX TO SOMA
           END-PERFORM

           DIVIDE SOMA BY 11 GIVING QUOCIENT REMAINDER RESTO
           
           IF RESTO < 2
                MOVE 0 TO DIGITO1
           ELSE
                SUBTRACT RESTO FROM 11 GIVING DIGITO1
           END-IF

      *----CALCULO DO DIGITO2
           MOVE 0 TO SOMA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 9
                SUBTRACT I FROM 12 GIVING PESO
                MULTIPLY CPF-DIGITOS(I) BY PESO GIVING AUX
                ADD AUX TO SOMA
           END-PERFORM
           
           MULTIPLY DIGITO1 BY 2 GIVING AUX 
           ADD AUX TO SOMA
           
           DIVIDE SOMA BY 11 GIVING QUOCIENT REMAINDER RESTO
           
           IF RESTO < 2
                MOVE 0 TO DIGITO2
           ELSE
                SUBTRACT RESTO FROM 11 GIVING DIGITO2
           END-IF
           
      *----VALIDAÇÃO FINAL
           IF DIGITO1 = CPF-DIGITOS(10) AND DIGITO2 = CPF-DIGITOS(11)
                MOVE "SIM" TO VALIDO
           ELSE
                MOVE "NAO" TO VALIDO
           END-IF.

       
           