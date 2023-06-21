CREATE OR REPLACE PACKAGE BODY SIOCDI.OC_IRIS_PORTAL AS
/******************************************************************************

OBJETO: SIOCDI.OC_IRIS_PORTAL
TIPO: PACKAGE BODY
AREA SOLICITANTE: PAULO CANAVEZI
OS: N/A
DATA DE CRIACAO: 01/03/2023
RESPONSAVEL PELA CRIACAO: MARCOS COSTA
OBJETIVO: AGRUPAR AS PROCEDURES REFERENTES A API DO SYNAPSE
RECURSOS RELACIONADOS: N/A
RELATORIO: N/A
HISTORICO DE ALTERACOES: N/A

******************************************************************************/
    WC_NULL VARCHAR(1) := '?';
    APP_USER VARCHAR(45) := 'portal';

    LBL_NOT_PROVIDED VARCHAR(3) := 'N/A';
    LBL_VS_BODYWEIGHT VARCHAR(15) := 'BODYWEIGHT';
    LBL_VS_BODYWEIGHT_UNIT VARCHAR(15) := 'Kg';
    LBL_VS_BODYHEIGHT VARCHAR(15) := 'BODYHEIGHT';
    LBL_VS_BODYHEIGHT_UNIT VARCHAR(15) := 'cm';

    FUNCTION NWC(TXT VARCHAR2) RETURN VARCHAR2 IS

    T_TXT VARCHAR2(250);

    BEGIN

    T_TXT := TXT;

    IF LENGTH(TXT)=1 THEN
        T_TXT := REPLACE(T_TXT,WC_NULL,'');
    END IF;

    RETURN T_TXT;

    END NWC;

    FUNCTION VPARAM(PKEY IN VARCHAR2, PXML IN VARCHAR2) RETURN VARCHAR2 IS

        P XMLPARSER.PARSER;
        XMLDOC XMLDOM.DOMDOCUMENT;
        NL XMLDOM.DOMNODELIST;
        LEN NUMBER;
        N XMLDOM.DOMNODE;
        E XMLDOM.DOMELEMENT;
        PVAL VARCHAR2(10000);

    BEGIN

        P := XMLPARSER.NEWPARSER;
        XMLPARSER.PARSEBUFFER(P,PXML);
        XMLDOC := XMLPARSER.GETDOCUMENT(P);
        NL := XMLDOM.GETELEMENTSBYTAGNAME(XMLDOC, PKEY);
        IF XMLDOM.GETLENGTH(NL) > 0 THEN

            N := XMLDOM.ITEM(NL,0);
            N := XMLDOM.GETFIRSTCHILD(N);
            PVAL := XMLDOM.GETNODEVALUE(N);

        END IF;

        RETURN PVAL;

    EXCEPTION
        WHEN OTHERS THEN
            RAISE;

    END VPARAM;

    FUNCTION NPARAM(PKEY IN VARCHAR2, PXML IN VARCHAR2) RETURN NUMBER IS

        P XMLPARSER.PARSER;
        XMLDOC XMLDOM.DOMDOCUMENT;
        NL XMLDOM.DOMNODELIST;
        LEN NUMBER;
        N XMLDOM.DOMNODE;
        E XMLDOM.DOMELEMENT;
        PVAL VARCHAR(250);
        PNUM NUMBER;

    BEGIN

        P := XMLPARSER.NEWPARSER;
        XMLPARSER.PARSEBUFFER(P,PXML);
        XMLDOC := XMLPARSER.GETDOCUMENT(P);
        NL := XMLDOM.GETELEMENTSBYTAGNAME(XMLDOC, PKEY);
        IF XMLDOM.GETLENGTH(NL) > 0 THEN

            N := XMLDOM.ITEM(NL,0);
            N := XMLDOM.GETFIRSTCHILD(N);
            PVAL := XMLDOM.GETNODEVALUE(N);
            PNUM := PVAL;

        END IF;

        RETURN PNUM;

    EXCEPTION
        WHEN OTHERS THEN
            RAISE;

    END NPARAM;

    FUNCTION LPARAM(PKEY IN VARCHAR2, PXML IN VARCHAR2) RETURN OC_PARAM_ARR IS

        P XMLPARSER.PARSER;
        XMLDOC XMLDOM.DOMDOCUMENT;
        NL XMLDOM.DOMNODELIST;
        LEN NUMBER;
        N XMLDOM.DOMNODE;
        E XMLDOM.DOMELEMENT;
        PVAL VARCHAR(1500);
        PLIST APEX_APPLICATION_GLOBAL.VC_ARR2;
        P_ARR OC_PARAM_ARR;

    BEGIN

        P := XMLPARSER.NEWPARSER;
        XMLPARSER.PARSEBUFFER(P,PXML);
        XMLDOC := XMLPARSER.GETDOCUMENT(P);
        NL := XMLDOM.GETELEMENTSBYTAGNAME(XMLDOC, PKEY);
        IF XMLDOM.GETLENGTH(NL) > 0 THEN

            N := XMLDOM.ITEM(NL,0);
            N := XMLDOM.GETFIRSTCHILD(N);
            PVAL := XMLDOM.GETNODEVALUE(N);
            PLIST := APEX_UTIL.STRING_TO_TABLE(PVAL,',');

            P_ARR := OC_PARAM_ARR();
            FOR i IN PLIST.FIRST .. PLIST.LAST
            LOOP
                P_ARR.EXTEND(1);
                P_ARR(P_ARR.COUNT) := PLIST(i);
            END LOOP;

        END IF;

        RETURN P_ARR;

    EXCEPTION
        WHEN OTHERS THEN
            RAISE;

    END LPARAM;

    FUNCTION LPARAMSPLIT(PESTAB IN VARCHAR2, PSETOR IN VARCHAR2, PKEY IN VARCHAR2, PXML IN VARCHAR2) RETURN VARCHAR2 IS

        P XMLPARSER.PARSER;
        XMLDOC XMLDOM.DOMDOCUMENT;
        NL XMLDOM.DOMNODELIST;
        LEN NUMBER;
        N XMLDOM.DOMNODE;
        E XMLDOM.DOMELEMENT;
        PVAL VARCHAR(250);
        PLIST APEX_APPLICATION_GLOBAL.VC_ARR2;
        PRETURN VARCHAR2(50);

    BEGIN

        P := XMLPARSER.NEWPARSER;
        XMLPARSER.PARSEBUFFER(P,PXML);
        XMLDOC := XMLPARSER.GETDOCUMENT(P);
        NL := XMLDOM.GETELEMENTSBYTAGNAME(XMLDOC, PKEY);
        IF XMLDOM.GETLENGTH(NL) > 0 THEN

            N := XMLDOM.ITEM(NL,0);
            N := XMLDOM.GETFIRSTCHILD(N);
            PVAL := XMLDOM.GETNODEVALUE(N);
            PLIST := APEX_UTIL.STRING_TO_TABLE(PVAL,',');
            PRETURN := null;

            FOR i IN PLIST.FIRST .. PLIST.LAST
            LOOP
                IF TRIM(REGEXP_SUBSTR(PLIST(i),'[^.]+',1,1)) = PESTAB AND TRIM(REGEXP_SUBSTR(PLIST(i),'[^.]+',1,2)) = PSETOR THEN
                    PRETURN := TRIM(REGEXP_SUBSTR(PLIST(i),'[^.]+',1,3));
                    EXIT;
                END IF;
            END LOOP;

        END IF;

        RETURN PRETURN;

    EXCEPTION
        WHEN OTHERS THEN
            RAISE;

    END LPARAMSPLIT;

    FUNCTION EPOCH_D(DT DATE) RETURN NUMBER IS

    BEGIN

    RETURN ROUND(TO_NUMBER(DT-TO_DATE('1970-01-01 00:00:00','YYYY-MM-DD HH24:MI:SS'))*60*60*24);

    END EPOCH_D;
    
    FUNCTION GETLONG( P_TNAME IN VARCHAR2, P_CNAME IN VARCHAR2, P_IDCOL IN VARCHAR2, P_ID IN NUMBER ) RETURN VARCHAR2 IS
        L_CURSOR INTEGER DEFAULT DBMS_SQL.OPEN_CURSOR;
        L_N NUMBER;
        L_LONG_VAL VARCHAR2(4000);
        L_LONG_LEN NUMBER;
        L_BUFLEN NUMBER := 4000;
        L_CURPOS NUMBER := 0;
    BEGIN
        DBMS_SQL.PARSE( L_CURSOR,'SELECT ' || P_CNAME || ' FROM ' || P_TNAME || ' WHERE ' || P_IDCOL || ' = :X',DBMS_SQL.NATIVE );
        DBMS_SQL.BIND_VARIABLE( L_CURSOR, ':X', P_ID );
        DBMS_SQL.DEFINE_COLUMN_LONG(L_CURSOR, 1);
        L_N := DBMS_SQL.EXECUTE(L_CURSOR);
        IF (DBMS_SQL.FETCH_ROWS(L_CURSOR)>0)
        THEN
        DBMS_SQL.COLUMN_VALUE_LONG(L_CURSOR, 1, L_BUFLEN, L_CURPOS ,
        L_LONG_VAL, L_LONG_LEN );
        END IF;
        DBMS_SQL.CLOSE_CURSOR(L_CURSOR);
        RETURN L_LONG_VAL;
    END GETLONG;

    PROCEDURE GET_PROFILE(IDENTIFIER_P IN VARCHAR2,IETIPOUSUARIO_P IN VARCHAR2,IDTYPE_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

      OPEN P_RECORDSET FOR
        SELECT
            PF.CD_PESSOA_FISICA,
            PF.NM_PESSOA_FISICA,
            MAX(CPF.DS_EMAIL) DS_EMAIL,
            REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') NR_CPF,
            OBTER_DADOS_PF(PF.CD_PESSOA_FISICA, 'TCD') NR_TELEFONE_CELULAR,
            M.NR_CRM,
            TO_CHAR(PF.DT_CADASTRO_ORIGINAL,'YYYY-MM-DD HH24:MI:SS') DT_CADASTRO_ORIGINAL
        FROM
            PESSOA_FISICA PF
            INNER JOIN MEDICO M
                ON (M.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND M.IE_SITUACAO = 'A')
            LEFT JOIN COMPL_PESSOA_FISICA CPF
                ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND CPF.DS_EMAIL IS NOT NULL)
            INNER JOIN CONSELHO_PROFISSIONAL CP
                ON (PF.NR_SEQ_CONSELHO = CP.NR_SEQUENCIA AND CP.SG_CONSELHO = 'CRM')
        WHERE
            (
                ( IDTYPE_P = 'CRM' AND M.UF_CRM||M.NR_CRM = IDENTIFIER_P )
                OR
                ( IDTYPE_P = 'CPF' AND REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') = IDENTIFIER_P )
            )
            AND ( IETIPOUSUARIO_P = 'physician' OR IETIPOUSUARIO_P IS NULL )
        GROUP BY
            PF.NM_PESSOA_FISICA,
            PF.NR_CPF,
            PF.CD_PESSOA_FISICA,
            M.NR_CRM,
            PF.DT_CADASTRO_ORIGINAL
        UNION ALL
        SELECT
            PF.CD_PESSOA_FISICA,
            PF.NM_PESSOA_FISICA,
            MAX(CPF.DS_EMAIL) DS_EMAIL,
            REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') NR_CPF,
            OBTER_DADOS_PF(PF.CD_PESSOA_FISICA, 'TCD') NR_TELEFONE_CELULAR,
            '' NR_CRM,
            TO_CHAR(PF.DT_CADASTRO_ORIGINAL,'YYYY-MM-DD HH24:MI:SS') DT_CADASTRO_ORIGINAL
        FROM
            PESSOA_FISICA PF
            LEFT JOIN COMPL_PESSOA_FISICA CPF
                ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND CPF.DS_EMAIL IS NOT NULL)
        WHERE
            ( IDTYPE_P = 'CPF' AND REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') = IDENTIFIER_P )
            AND ( IETIPOUSUARIO_P = 'patient' OR IETIPOUSUARIO_P IS NULL )
        GROUP BY
            PF.NM_PESSOA_FISICA,
            PF.NR_CPF,
            PF.CD_PESSOA_FISICA,
            PF.DT_CADASTRO_ORIGINAL;

    END GET_PROFILE;

    PROCEDURE GET_PROFILE_UPDATES(REF_DATE_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            PF.CD_PESSOA_FISICA,
            PF.NM_PESSOA_FISICA,
            M.NR_CRM,
            M.UF_CRM,
            GREATEST(MAX(PF.DT_ATUALIZACAO),MAX(CPF.DT_ATUALIZACAO)) DT_ATUALIZACAO
        FROM
            PESSOA_FISICA PF
            INNER JOIN COMPL_PESSOA_FISICA CPF
                ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
            LEFT JOIN MEDICO M
                ON (M.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
        WHERE
            (((PF.DT_ATUALIZACAO > TO_DATE(REF_DATE_P,'YYYY-MM-DD HH24:MI:SS')) AND (PF.NM_USUARIO <> APP_USER))
            OR ((CPF.DT_ATUALIZACAO > TO_DATE(REF_DATE_P,'YYYY-MM-DD HH24:MI:SS')) AND (CPF.NM_USUARIO <> APP_USER)))
        GROUP BY
            PF.CD_PESSOA_FISICA,
            PF.NM_PESSOA_FISICA,
            M.NR_CRM,
            M.UF_CRM
        ORDER BY
            DT_ATUALIZACAO ASC;

    END GET_PROFILE_UPDATES;

    PROCEDURE GET_PATIENT_INSURANCE(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR

        SELECT
            PTC.NR_SEQUENCIA "ID",
            PF.NM_PESSOA_FISICA,
            C.CD_CONVENIO,
            C.DS_CONVENIO,
            CC.CD_CATEGORIA,
            CC.DS_CATEGORIA,
            CP.CD_PLANO,
            CP.DS_PLANO,
            PTC.CD_USUARIO_CONVENIO NR_DOC_CONVENIO,
            PTC.CD_USUARIO_CONVENIO_TIT NR_CON_CARD,
            TO_CHAR(PTC.DT_VALIDADE_CARTEIRA,'YYYY-MM-DD') DT_VALIDADE_CARTEIRA
        FROM
            PESSOA_FISICA PF
            INNER JOIN PESSOA_TITULAR_CONVENIO PTC
                ON (PTC.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
            INNER JOIN CONVENIO C
                ON (C.CD_CONVENIO = PTC.CD_CONVENIO)
            LEFT JOIN CATEGORIA_CONVENIO CC
                ON (CC.CD_CATEGORIA = PTC.CD_CATEGORIA AND CC.CD_CONVENIO = PTC.CD_CONVENIO)
            LEFT JOIN CONVENIO_PLANO CP
                ON (CP.CD_CONVENIO = C.CD_CONVENIO AND CP.CD_PLANO = PTC.CD_PLANO_CONVENIO)
        WHERE
            PF.CD_PESSOA_FISICA = IDENTIFIER_P;

    END GET_PATIENT_INSURANCE;

    PROCEDURE GET_PATIENT_CONTACTS(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            CD_PESSOA_FISICA||'.0.mob' "ID",
            OBTER_DADOS_PF(CD_PESSOA_FISICA, 'TCD') DS_CONTATO,
            'mob' IE_TIPO
        FROM
            PESSOA_FISICA
        WHERE
            CD_PESSOA_FISICA = IDENTIFIER_P
            AND NR_TELEFONE_CELULAR IS NOT NULL
        UNION
        SELECT
            PF.CD_PESSOA_FISICA||'.'||CPF.NR_SEQUENCIA||'.mail' "ID",
            CPF.DS_EMAIL DS_CONTATO,
            'mail' IE_TIPO
        FROM
            PESSOA_FISICA PF
            INNER JOIN COMPL_PESSOA_FISICA CPF
                ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND CPF.DS_EMAIL IS NOT NULL)
        WHERE
            PF.CD_PESSOA_FISICA = IDENTIFIER_P;

    END GET_PATIENT_CONTACTS;

    PROCEDURE GET_PATIENT_ADDRESS(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            PF.CD_PESSOA_FISICA||'.'||CPF.NR_SEQUENCIA "ID",
            TO_NUMBER(CPF.CD_TIPO_LOGRADOURO) CD_TIPO_LOGRADOURO,
            OBTER_TIPO_LOGRADOURO_VGS(CPF.CD_TIPO_LOGRADOURO) SG_TIPO_LOGRADOURO,
            CPF.DS_ENDERECO,
            CPF.DS_BAIRRO,
            CPF.DS_COMPLEMENTO,
            CPF.NR_ENDERECO,
            CPF.DS_MUNICIPIO,
            CPF.SG_ESTADO,
            CPF.CD_CEP,
            CPF.IE_TIPO_COMPLEMENTO
        FROM
            PESSOA_FISICA PF
            INNER JOIN COMPL_PESSOA_FISICA CPF
                ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
        WHERE
            PF.CD_PESSOA_FISICA = IDENTIFIER_P
            AND CPF.IE_TIPO_COMPLEMENTO IN (1,2)
        ORDER BY
            TO_NUMBER(CPF.IE_TIPO_COMPLEMENTO) ASC;

    END GET_PATIENT_ADDRESS;

    PROCEDURE GET_PATIENT_RELATIVES(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            PF.CD_PESSOA_FISICA||'.'||CPF.NR_SEQUENCIA "ID",
            CPF.NR_CPF,
            CPF.NM_CONTATO,
            CPF.IE_TIPO_COMPLEMENTO,
            (CPF.NR_DDI_CELULAR||CPF.NR_DDD_CELULAR||CPF.NR_TELEFONE_CELULAR) NR_TELEFONE_CELULAR,
            CPF.CD_TIPO_LOGRADOURO,
            OBTER_TIPO_LOGRADOURO_VGS(CPF.CD_TIPO_LOGRADOURO) SG_TIPO_LOGRADOURO,
            CPF.DS_ENDERECO,
            CPF.DS_BAIRRO,
            CPF.DS_COMPLEMENTO,
            CPF.NR_ENDERECO,
            CPF.DS_MUNICIPIO,
            CPF.SG_ESTADO,
            CPF.CD_CEP
        FROM
            PESSOA_FISICA PF
            INNER JOIN COMPL_PESSOA_FISICA CPF
                ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
        WHERE
            PF.CD_PESSOA_FISICA = IDENTIFIER_P
            AND CPF.IE_TIPO_COMPLEMENTO IN (3,4,5,6)
        ORDER BY
            TO_NUMBER(CPF.NR_SEQUENCIA) ASC;

    END GET_PATIENT_RELATIVES;

    PROCEDURE GET_PATIENT_PHYSICIANS(
        IDENTIFIER_P IN VARCHAR2,
        HISTORY_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            *
        FROM
        (
            SELECT
                PF.CD_PESSOA_FISICA,
                PF.NM_PESSOA_FISICA,
                PF.NR_CPF,
                M.NR_CRM,
                PF.NR_IDENTIDADE,
                TO_CHAR(PF.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                PF.IE_SEXO,
                CASE
                WHEN PF.CD_PESSOA_FISICA = (
                    SELECT
                        CD_MEDICO_RESP
                    FROM
                        ATENDIMENTO_PACIENTE
                    WHERE
                        NR_ATENDIMENTO = (
                            SELECT
                                MAX(NR_ATENDIMENTO)
                            FROM
                                ATENDIMENTO_PACIENTE
                            WHERE
                                CD_PESSOA_FISICA = IDENTIFIER_P
                        )
                    ) THEN 'S'
                ELSE 'N'
                END IE_ULTIMO,
                COUNT(
                    CASE
                    WHEN A.IE_APRES_AGENDA_EXTERNO = 'S' THEN 1
                    ELSE NULL
                    END
                ) QT_AGENDA_LIB
            FROM
                MEDICO M
                INNER JOIN PESSOA_FISICA PF
                    ON (PF.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                LEFT JOIN AGENDA A
                    ON (A.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)))
            WHERE
                PF.CD_PESSOA_FISICA IN (
                    SELECT
                        DISTINCT
                        CD_MEDICO_RESP
                    FROM
                        ATENDIMENTO_PACIENTE
                    WHERE
                        CD_PESSOA_FISICA = IDENTIFIER_P
                        --AND CD_ESTABELECIMENTO = 31
                )
            GROUP BY
                PF.CD_PESSOA_FISICA,
                PF.NM_PESSOA_FISICA,
                PF.NR_CPF,
                M.NR_CRM,
                PF.NR_IDENTIDADE,
                PF.DT_NASCIMENTO,
                PF.IE_SEXO
        )
        WHERE
            (HISTORY_P = 'N' AND IE_ULTIMO = 'S')
            OR HISTORY_P = 'S';

    END GET_PATIENT_PHYSICIANS;

    PROCEDURE GET_PATIENT_APPOINTMENTS(PARAMS_P IN VARCHAR2,IDENTIFIER_P IN VARCHAR2,HISTORY_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            *
        FROM
        (
            SELECT
                *
            FROM
            (
                SELECT
                    AC.NR_SEQUENCIA,
                    A.CD_AGENDA,
                    A.DS_AGENDA,
                    M.CD_PESSOA_FISICA CD_MEDICO,
                    M.NM_PESSOA_FISICA NM_MEDICO,
                    CASE
                        WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
                        ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
                      END CD_ESTABELECIMENTO,
                      PJ.NM_FANTASIA NM_FANTASIA_ESTAB,
                    EM.CD_ESPECIALIDADE,
                    EM.DS_ESPECIALIDADE,
                    P.CD_PESSOA_FISICA CD_PACIENTE,
                    P.NM_PESSOA_FISICA NM_PACIENTE,
                    C.CD_CONVENIO,
                    C.DS_CONVENIO,
                    CC.CD_CATEGORIA,
                    CC.DS_CATEGORIA,
                    CP.CD_PLANO,
                    CP.DS_PLANO,
                    TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                    AC.DT_AGENDA DT_AGENDA_TS,
                    TO_CHAR(AC.DT_AGENDAMENTO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                    AC.IE_STATUS_AGENDA,
                    OBTER_VALOR_DOMINIO(83,AC.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                    AP.IE_TIPO_ATENDIMENTO,
                    OBTER_VALOR_DOMINIO(12,AP.IE_TIPO_ATENDIMENTO) NM_TIPO_ATENDIMENTO,
                    A.IE_TIPO_AGENDA_CONSULTA IE_TIPO_AGENDA,
                    DECODE(A.IE_TIPO_AGENDA_CONSULTA,
                            'C','Consulta',
                            'A','Consulta',
                            'S','Servi?o',NULL) NM_TIPO_AGENDA,
                    AC.IE_CLASSIF_AGENDA,
                    ACL.DS_CLASSIFICACAO NM_CLASSIF_AGENDA,
                    A.DS_COMPLEMENTO DS_OBSERVACAO,
                    AMC.CD_MOTIVO CD_MOTIVO_CANCEL,
                    AMC.DS_MOTIVO NM_MOTIVO_CANCEL
                FROM
                    AGENDA A
                    INNER JOIN AGENDA_CONSULTA AC
                        ON (AC.CD_AGENDA = A.CD_AGENDA AND AC.DT_AGENDA IN (
                            SELECT
                            MIN(AC1.DT_AGENDA)
                            FROM
                            AGENDA_CONSULTA AC1
                            WHERE
                            AC1.CD_AGENDA = A.CD_AGENDA
                            AND AC1.IE_STATUS_AGENDA NOT IN ('L')
                            AND AC1.CD_PESSOA_FISICA = IDENTIFIER_P
                            AND TRUNC(AC1.DT_AGENDA) = TRUNC(AC.DT_AGENDA)
                            GROUP BY
                            AC1.DT_AGENDAMENTO
                        ))
                    INNER JOIN PESSOA_FISICA P
                        ON (P.CD_PESSOA_FISICA = AC.CD_PESSOA_FISICA)
                    INNER JOIN PESSOA_FISICA M
                        ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                    INNER JOIN ESTABELECIMENTO E
                        ON (E.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO)
                    INNER JOIN ESPECIALIDADE_MEDICA EM
                        ON (EM.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE)
                    LEFT JOIN AGENDA_CLASSIF ACL
                        ON (ACL.CD_CLASSIFICACAO = AC.IE_CLASSIF_AGENDA AND ACL.IE_SITUACAO = 'A')
                    LEFT JOIN AGENDA_MOTIVO_CANCELAMENTO AMC
                        ON (AMC.CD_MOTIVO = AC.CD_MOTIVO_CANCELAMENTO
                                AND (AMC.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO OR AMC.CD_ESTABELECIMENTO IS NULL)
                                AND AMC.IE_AGENDA = 'C' AND AMC.IE_SITUACAO = 'A')
                    LEFT JOIN ATENDIMENTO_PACIENTE AP
                        ON (AP.NR_ATENDIMENTO = AC.NR_ATENDIMENTO)
                    LEFT JOIN CONVENIO C
                        ON (C.CD_CONVENIO = AC.CD_CONVENIO)
                    LEFT JOIN CATEGORIA_CONVENIO CC
                        ON (CC.CD_CATEGORIA = AC.CD_CATEGORIA AND CC.CD_CONVENIO = C.CD_CONVENIO)
                    LEFT JOIN CONVENIO_PLANO CP
                        ON (CP.CD_PLANO = AC.CD_PLANO AND CP.CD_CONVENIO = C.CD_CONVENIO)
                      INNER JOIN TASY.ESTABELECIMENTO ES
                        ON (ES.CD_ESTABELECIMENTO = CASE
                                                      WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                                                      ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                                                    END
                                                    )
                      INNER JOIN TASY.PESSOA_JURIDICA PJ
                        ON (PJ.CD_CGC = ES.CD_CGC)
                WHERE
                    AC.CD_PESSOA_FISICA = IDENTIFIER_P
                    AND AC.IE_STATUS_AGENDA NOT IN ('L')
                    AND A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
                UNION ALL
                SELECT
                    AQ.NR_SEQUENCIA,
                    NULL CD_AGENDA,
                    NULL DS_AGENDA,
                    M.CD_PESSOA_FISICA CD_MEDICO,
                    M.NM_PESSOA_FISICA NM_MEDICO,
                    TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000' CD_ESTABELECIMENTO,
                    E.NM_FANTASIA_ESTAB,
                    EM.CD_ESPECIALIDADE,
                    EM.DS_ESPECIALIDADE,
                    P.CD_PESSOA_FISICA CD_PACIENTE,
                    P.NM_PESSOA_FISICA NM_PACIENTE,
                    NULL CD_CONVENIO,
                    NULL DS_CONVENIO,
                    NULL CD_CATEGORIA,
                    NULL DS_CATEGORIA,
                    NULL CD_PLANO,
                    NULL DS_PLANO,
                    TO_CHAR(AQ.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                    AQ.DT_AGENDA DT_AGENDA_TS,
                    TO_CHAR(AQ.DT_ATUALIZACAO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                    AQ.IE_STATUS_AGENDA,
                    TASY.OBTER_VALOR_DOMINIO(83,AQ.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                    NULL IE_TIPO_ATENDIMENTO,
                    NULL NM_TIPO_ATENDIMENTO,
                    AQ.IE_TIPO_PEND_AGENDA IE_TIPO_AGENDA,
                    'Tratamento' NM_TIPO_AGENDA,
                    'Q0' IE_CLASSIF_AGENDA,
                    'Quimioterapia' NM_CLASSIF_AGENDA,
                    AQM.DS_OBSERVACAO,
                    TO_CHAR(QMC.NR_SEQUENCIA) CD_MOTIVO_CANCEL,
                    QMC.DS_MOTIVO NM_MOTIVO_CANCEL
                FROM
                    TASY.AGENDA_QUIMIO AQ
                    LEFT JOIN TASY.AGENDA_QUIMIO_MARCACAO AQM
                        ON (AQM.NR_SEQ_ATENDIMENTO = AQ.NR_SEQ_ATENDIMENTO)
                    INNER JOIN TASY.PESSOA_FISICA P
                        ON (P.CD_PESSOA_FISICA = AQ.CD_PESSOA_FISICA)
                    INNER JOIN TASY.ESTABELECIMENTO E
                        ON (E.CD_ESTABELECIMENTO = AQ.CD_ESTABELECIMENTO)
                    INNER JOIN TASY.PESSOA_JURIDICA PJ
                        ON (PJ.CD_CGC = E.CD_CGC)
                    INNER JOIN TASY.PESSOA_FISICA M
                        ON (M.CD_PESSOA_FISICA = AQ.CD_MEDICO_RESP)
                    LEFT JOIN TASY.MEDICO_ESPECIALIDADE ME
                        ON (ME.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                    LEFT JOIN TASY.ESPECIALIDADE_MEDICA EM
                        ON (EM.CD_ESPECIALIDADE = ME.CD_ESPECIALIDADE)
                    LEFT JOIN TASY.QT_MOTIVO_CANCELAMENTO QMC
                        ON (QMC.NR_SEQUENCIA = AQ.NR_SEQ_MOT_CANCELAMENTO AND QMC.IE_SITUACAO = 'A')                   
                WHERE
                    AQ.CD_PESSOA_FISICA = IDENTIFIER_P    
                    AND AQ.IE_STATUS_AGENDA NOT IN ('L')
                    AND AQ.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
            ) S
            ORDER BY
                DT_AGENDA_TS DESC
        )
        WHERE
            (HISTORY_P = 'N' AND ROWNUM=1) OR HISTORY_P = 'S';

    END GET_PATIENT_APPOINTMENTS;

    PROCEDURE GET_PATIENT_ENCOUNTERS(
        IDENTIFIER_P IN VARCHAR2,
        PATIENT_P IN VARCHAR2,
        HISTORY_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            AP.NR_ATENDIMENTO,
            TO_CHAR(AP.DT_ENTRADA,'YYYY-MM-DD HH24:MI:SS') DT_ENTRADA,
            M.CD_PESSOA_FISICA CD_MEDICO,
            M.NM_PESSOA_FISICA NM_MEDICO,
            OBTER_DADOS_PF(M.CD_PESSOA_FISICA, 'TCD') NR_CONTATO_MED,
            OBTER_ESPECIALIDADE_MEDICO(M.CD_PESSOA_FISICA,'C') CD_ESPECIALIDADE_MED,
            OBTER_ESPECIALIDADE_MEDICO(M.CD_PESSOA_FISICA,'D') DS_ESPECIALIDADE_MED,
            C.CD_CONVENIO,
            C.DS_CONVENIO,
            CC.CD_CATEGORIA,
            CC.DS_CATEGORIA,
            CP.CD_PLANO,
            CP.DS_PLANO,
            ACC.NR_DOC_CONVENIO,
            ACC.NR_CON_CARD,
            TO_CHAR(ACC.DT_VALIDADE_CARTEIRA,'YYYY-MM-DD') DT_VALIDADE_CARTEIRA,
            AP.NR_SEQ_CLASSIFICACAO,
            AP.CD_ESTABELECIMENTO,
            COUNT(
                CASE
                WHEN A.IE_APRES_AGENDA_EXTERNO = 'S' THEN 1
                ELSE NULL
                END
            ) QT_AGENDA_LIB
        FROM
            ATENDIMENTO_PACIENTE AP
            INNER JOIN PESSOA_FISICA M
                ON (M.CD_PESSOA_FISICA = AP.CD_MEDICO_REFERIDO)
            LEFT JOIN AGENDA A
                ON (A.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA AND A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)))
            LEFT JOIN ATEND_CATEGORIA_CONVENIO ACC
                ON (ACC.NR_ATENDIMENTO = AP.NR_ATENDIMENTO)
            LEFT JOIN CATEGORIA_CONVENIO CC
                ON (CC.CD_CATEGORIA = ACC.CD_CATEGORIA AND CC.CD_CONVENIO = ACC.CD_CONVENIO)
            LEFT JOIN CONVENIO C
                ON (C.CD_CONVENIO = CC.CD_CONVENIO)
            LEFT JOIN CONVENIO_PLANO CP
                ON (CP.CD_CONVENIO = C.CD_CONVENIO AND CP.CD_PLANO = ACC.CD_PLANO_CONVENIO)
        WHERE
            (AP.NR_ATENDIMENTO = IDENTIFIER_P OR IDENTIFIER_P IS NULL)
            AND (AP.CD_PESSOA_FISICA = PATIENT_P OR (PATIENT_P IS NULL AND IDENTIFIER_P IS NOT NULL))
            AND (HISTORY_P = 'S' OR AP.NR_ATENDIMENTO = (
                SELECT MAX(NR_ATENDIMENTO) FROM ATENDIMENTO_PACIENTE WHERE CD_PESSOA_FISICA = AP.CD_PESSOA_FISICA
            ))
        GROUP BY
            AP.NR_ATENDIMENTO,
            AP.DT_ENTRADA,
            M.CD_PESSOA_FISICA,
            M.NM_PESSOA_FISICA,
            C.CD_CONVENIO,
            C.DS_CONVENIO,
            CC.CD_CATEGORIA,
            CC.DS_CATEGORIA,
            CP.CD_PLANO,
            CP.DS_PLANO,
            ACC.NR_DOC_CONVENIO,
            ACC.NR_CON_CARD,
            ACC.DT_VALIDADE_CARTEIRA,
            AP.NR_SEQ_CLASSIFICACAO,
            AP.CD_ESTABELECIMENTO
        ORDER BY
            AP.DT_ENTRADA DESC;

    END GET_PATIENT_ENCOUNTERS;

    PROCEDURE GET_PHYSICIAN_SPECIALTIES(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            ME.CD_PESSOA_FISICA,
            EM.CD_ESPECIALIDADE,
            EM.DS_ESPECIALIDADE,
            ME.NR_SEQ_PRIORIDADE,
            CASE
            WHEN ME.NR_SEQ_PRIORIDADE = (
                    SELECT
                    MIN(NR_SEQ_PRIORIDADE)
                    FROM
                    MEDICO_ESPECIALIDADE
                    WHERE
                    CD_PESSOA_FISICA = ME.CD_PESSOA_FISICA
                ) THEN 'S'
            ELSE 'N'
            END IE_PRINCIPAL
        FROM
            MEDICO_ESPECIALIDADE ME
            INNER JOIN ESPECIALIDADE_MEDICA EM
                ON (EM.CD_ESPECIALIDADE = ME.CD_ESPECIALIDADE)
        WHERE
            ME.CD_PESSOA_FISICA = IDENTIFIER_P
        ORDER BY
            ME.NR_SEQ_PRIORIDADE ASC;

    END GET_PHYSICIAN_SPECIALTIES;

    PROCEDURE GET_SCHEDULE_CANCEL_CAUSES(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            CD_MOTIVO,
            DS_MOTIVO,
            IE_AGENDA
        FROM
            AGENDA_MOTIVO_CANCELAMENTO
        WHERE
            CD_ESTABELECIMENTO IS NULL
            AND IE_SITUACAO = 'A'
        ORDER BY
            DS_MOTIVO ASC;

    END GET_SCHEDULE_CANCEL_CAUSES;

    PROCEDURE GET_SPECIALTIES(NAME_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            CD_ESPECIALIDADE,
            DS_ESPECIALIDADE,
            'N' IE_PRINCIPAL
        FROM
            ESPECIALIDADE_MEDICA
        WHERE
            (UPPER(DS_ESPECIALIDADE) LIKE REPLACE(UPPER(NAME_P),'~','%') OR NAME_P IS NULL)
            AND IE_SITUACAO = 'A'
        ORDER BY
            DS_ESPECIALIDADE ASC;

    END GET_SPECIALTIES;

    PROCEDURE GET_SCHEDULE(
        IDENTIFIER_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            CD_AGENDA,
            DS_AGENDA,
            CD_PESSOA_FISICA,
            CD_ESTABELECIMENTO,
            CD_ESPECIALIDADE
        FROM
            AGENDA A
        WHERE
            CD_AGENDA = IDENTIFIER_P
            AND CD_PESSOA_FISICA IS NOT NULL
            AND IE_SITUACAO = 'A'
            AND CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR));

    END GET_SCHEDULE;

    PROCEDURE GET_OCCUPATION(NAME_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            C.CD_CARGO,
            C.DS_CARGO
        FROM
            CARGO C
        WHERE
            (UPPER(C.DS_CARGO) LIKE REPLACE(UPPER(NAME_P),'~','%') OR NAME_P IS NULL)
            AND C.IE_SITUACAO = 'A'
        ORDER BY
            C.DS_CARGO ASC;

    END GET_OCCUPATION;

    PROCEDURE GET_HOLIDAY(
        CDESTABELECIMENTO_P IN VARCHAR2,
        DTINI_P IN VARCHAR2,
        DTFIM_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            TO_CHAR(DT_FERIADO,'YYYY-MM-DD') DT_FERIADO,
            TO_CHAR(DT_FERIADO,'YYYYMMDD') DT_FERIADO_TS,
            TO_CHAR(DT_FERIADO,'DY') DT_FERIADO_WD,
            MIN(DS_MOTIVO_FERIADO) DS_MOTIVO_FERIADO
        FROM
            FERIADO
        WHERE
            TRUNC(DT_FERIADO) BETWEEN TO_DATE(NVL(DTINI_P,TRUNC(SYSDATE)),'YYYY-MM-DD') AND TO_DATE(NVL(DTFIM_P,TRUNC(SYSDATE)),'YYYY-MM-DD')
            AND IE_TIPO_FERIADO NOT IN (3)
            AND
            (
                CD_ESTABELECIMENTO = CDESTABELECIMENTO_P
                OR
                (CDESTABELECIMENTO_P IS NULL AND CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)))
            )
        GROUP BY
            DT_FERIADO
        ORDER BY
            DT_FERIADO ASC;

    END GET_HOLIDAY;

    PROCEDURE GET_SCHEDULE_SLOTS(
        CD_MEDICO_P IN VARCHAR2,
        CD_PESSOA_FISICA_P IN VARCHAR2,
        CD_CONVENIO_P IN VARCHAR2,
        CD_CATEGORIA_P IN VARCHAR2,
        CD_PLANO_P IN VARCHAR2,
        CD_ESTABELECIMENTO_P IN VARCHAR2,
        NR_SEQ_AGENDAMENTO_P IN VARCHAR2,
        CD_ESPECIALIDADE_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    CD_ESTAB_T VARCHAR2(250);
    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHSSECTORS_ARR OC_PARAM_ARR;
    T_UNIDADE VARCHAR2(50);
    T_SETOR VARCHAR2(50);
    T_QTD_SLOTS_P NUMBER(1);

    BEGIN

        SET TRANSACTION ISOLATION LEVEL READ COMMITTED;

        T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);
        T_BRANCHSSECTORS_ARR := LPARAM('BRANCHSSECTORS',PARAMS_P);

        IF CD_ESTABELECIMENTO_P IS NULL THEN

            SELECT
                MAX(CD_ESTABELECIMENTO) INTO CD_ESTAB_T
            FROM
                AGENDA
            WHERE
            (
                NR_SEQ_AGENDAMENTO_P IS NOT NULL
                AND
                CD_AGENDA = (SELECT NVL(CD_AGENDA,0) FROM AGENDA_CONSULTA WHERE NR_SEQUENCIA = NR_SEQ_AGENDAMENTO_P)
            )
            OR
            (
                NR_SEQ_AGENDAMENTO_P IS NULL
                AND
                (
                    CD_PESSOA_FISICA = CD_MEDICO_P
                    AND IE_SITUACAO = 'A'
                    AND CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
                )
            );

        ELSE

            CD_ESTAB_T := CD_ESTABELECIMENTO_P;

        END IF;

        T_UNIDADE := TRIM(REGEXP_SUBSTR(CD_ESTAB_T,'[^.]+',1,1));
        T_SETOR := TRIM(REGEXP_SUBSTR(CD_ESTAB_T,'[^.]+',1,2));

        DELETE FROM OC_TEMP_SCHED_SLOTS;

        T_QTD_SLOTS_P := NVL(NPARAM('SLOTSQTD',PARAMS_P),1);

        INSERT INTO OC_TEMP_SCHED_SLOTS_A
        SELECT
            S.*,
            (TRUNC(SYSDATE)-TRUNC(ACU.DT_AGENDA)) QT_DIAS_ULT_CONSULTA,
            ACU.IE_CLASSIF_AGENDA IE_CLASSIF_ULT_CONSULTA,
            (TRUNC(S.DT_AGENDA_T)-TRUNC(ACUA.DT_AGENDA)) QT_DIAS_ULT_AGENDADA,
            CASE
            WHEN S.NR_SEQ_ULTIMA_CONSULTA IS NULL THEN 'S'
            ELSE 'N'
            END IE_PRIMEIRA,
            CASE
            WHEN CD_DOENCA_PAC IS NULL THEN 'OTHER'
            ELSE 'ONCO'
            END IE_TIPO_CLINICO
        FROM
        (
            SELECT
                AC.NR_SEQUENCIA,
                A.CD_AGENDA,
                A.DS_AGENDA,
                A.IE_SEXO_AGENDA,
                M.CD_PESSOA_FISICA CD_MEDICO,
                OBTER_NOME_MEDICO(M.CD_PESSOA_FISICA,'g') NM_MEDICO,
                CASE
                  WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
                  ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
                END CD_ESTABELECIMENTO,
                PJ.NM_FANTASIA NM_FANTASIA_ESTAB,
                EM.CD_ESPECIALIDADE,
                EM.DS_ESPECIALIDADE,
                AC.DT_AGENDA DT_AGENDA_T,
                TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                (TRUNC(AC.DT_AGENDA)-TRUNC(SYSDATE)) QT_DIAS_AGENDA,
                (
                    SELECT
                        MAX(CD_DOENCA_CID)
                    FROM
                        CAN_LOCO_REGIONAL
                    WHERE
                        IE_SITUACAO = 'A'
                        AND DT_LIBERACAO IS NOT NULL
                        AND CD_DOENCA_CID LIKE 'C%'
                        AND CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                ) CD_DOENCA_PAC,
                TO_CHAR(AC.DT_AGENDA,'DY') DT_AGENDA_WD,
                TO_CHAR(AC.DT_AGENDA,'YYYYMMDDHH24MI') DT_AGENDA_TS,
                (
                    SELECT
                        MAX(NR_SEQUENCIA)
                    FROM
                        AGENDA_CONSULTA
                    WHERE
                        CD_AGENDA = A.CD_AGENDA
                        AND IE_STATUS_AGENDA IN ('E','AD')
                        AND CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                ) NR_SEQ_ULTIMA,
                (
                    SELECT
                        MAX(ACM.NR_SEQUENCIA)
                    FROM
                        AGENDA_CONSULTA ACM
                    LEFT JOIN AGENDA AM
                        ON (ACM.CD_AGENDA = AM.CD_AGENDA)
                    WHERE
                        AM.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA
                        AND ACM.IE_STATUS_AGENDA IN ('E','AD')
                        AND ACM.CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                ) NR_SEQ_ULTIMA_CONSULTA,
                (
                    SELECT
                        COUNT(AC1.CD_PESSOA_FISICA)
                    FROM
                        AGENDA_CONSULTA AC1
                    WHERE
                        AC1.CD_AGENDA = AC.CD_AGENDA
                        AND TRUNC(AC1.DT_AGENDA) = TRUNC(AC.DT_AGENDA)
                        AND AC1.IE_STATUS_AGENDA = 'N'
                        AND (
                                SELECT
                                    COUNT(AC2.NR_SEQUENCIA)
                                FROM
                                    AGENDA_CONSULTA AC2
                                WHERE
                                    AC2.CD_AGENDA = AC1.CD_AGENDA
                                    AND AC2.CD_PESSOA_FISICA = AC1.CD_PESSOA_FISICA
                                    AND AC2.IE_STATUS_AGENDA IN ('E','AD')
                        ) = 0
                ) QTD_PRIM_CONS_DIA,
                (
                    SELECT
                        COUNT(AC1.NR_SEQUENCIA)
                    FROM
                        AGENDA_CONSULTA AC1
                    WHERE
                        AC1.CD_AGENDA = AC.CD_AGENDA
                        AND TRUNC(AC1.DT_AGENDA) = TRUNC(AC.DT_AGENDA)
                        AND AC1.CD_CONVENIO = CD_CONVENIO_P
                        AND AC1.IE_STATUS_AGENDA NOT IN ('C')
                ) QTD_CONS_CONVENIO_DIA,
                (
                    SELECT
                        MAX(NR_SEQUENCIA)
                    FROM
                        AGENDA_CONSULTA
                    WHERE
                        CD_AGENDA = A.CD_AGENDA
                        AND IE_STATUS_AGENDA NOT IN ('C')
                        AND CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                ) NR_SEQ_ULTIMA_AGENDADA,
                (
                    SELECT
                        MAX(AT.NR_SEQUENCIA)
                    FROM
                        AGENDA_TURNO AT
                    WHERE
                        AT.CD_AGENDA = AC.CD_AGENDA
                        AND (AC.DT_AGENDA >= AT.DT_INICIO_VIGENCIA AND (AC.DT_AGENDA <= AT.DT_FINAL_VIGENCIA OR AT.DT_FINAL_VIGENCIA IS NULL))
                        AND (TO_CHAR(AC.DT_AGENDA,'D') = AT.IE_DIA_SEMANA OR (AT.IE_DIA_SEMANA=9 AND TO_CHAR(AC.DT_AGENDA,'D') BETWEEN 2 AND 6))
                        AND AC.DT_AGENDA BETWEEN TO_DATE(TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD')||' '||TO_CHAR(AT.HR_INICIAL,'HH24:MI'),'YYYY-MM-DD HH24:MI')
                                                    AND TO_DATE(TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD')||' '||TO_CHAR(AT.HR_FINAL,'HH24:MI'),'YYYY-MM-DD HH24:MI')
                ) NR_SEQ_REG_TURNO,
                TRUNC(MONTHS_BETWEEN(SYSDATE,PF.DT_NASCIMENTO) / 12) QT_IDADE_PAC,
                PF.IE_SEXO IE_SEXO_PAC,
                M.NR_CRM||M.UF_CRM NM_CRM,
                (
                    SELECT
                    LISTAGG(CD_ESTABELECIMENTO,',') WITHIN GROUP (ORDER BY CD_ESTABELECIMENTO)
                    FROM
                    CONVENIO_ESTABELECIMENTO
                    WHERE
                    CD_CONVENIO = CD_CONVENIO_P
                ) LIST_CONV_ESTAB,
                AC.NR_MINUTO_DURACAO
            FROM
                AGENDA A
                INNER JOIN MEDICO M
                    ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                INNER JOIN AGENDA_CONSULTA AC
                    ON (A.CD_AGENDA = AC.CD_AGENDA)
                INNER JOIN PESSOA_FISICA M
                    ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                INNER JOIN PESSOA_FISICA PF
                    ON (PF.CD_PESSOA_FISICA = CD_PESSOA_FISICA_P)
                INNER JOIN ESTABELECIMENTO E
                    ON (E.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO)
                INNER JOIN ESPECIALIDADE_MEDICA EM
                    ON (EM.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE)
                INNER JOIN TASY.ESTABELECIMENTO ES
                    ON (ES.CD_ESTABELECIMENTO = CASE
                                                  WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                                                  ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                                                END
                                                )
                INNER JOIN TASY.PESSOA_JURIDICA PJ
                    ON (PJ.CD_CGC = ES.CD_CGC)
            WHERE
                AC.NR_SEQUENCIA = NR_SEQ_AGENDAMENTO_P
                OR
                (
                    NR_SEQ_AGENDAMENTO_P IS NULL
                    AND
                    (
                        M.CD_PESSOA_FISICA = CD_MEDICO_P
                        AND TRUNC(AC.DT_AGENDA) BETWEEN TRUNC(SYSDATE) AND TRUNC(SYSDATE+180)
                        AND AC.IE_STATUS_AGENDA IN ('L')
                        AND A.IE_APRES_AGENDA_EXTERNO = 'S'
                        AND
                        (
                            CD_CONVENIO_P IN (SELECT CD_CONVENIO FROM REGRA_LIB_CONV_AGENDA WHERE CD_AGENDA = A.CD_AGENDA)
                            OR
                            (SELECT COUNT(CD_CONVENIO) FROM REGRA_LIB_CONV_AGENDA WHERE CD_AGENDA = A.CD_AGENDA) = 0
                        )
                        AND A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
                        AND
                        (
                              (A.CD_ESTABELECIMENTO = TO_NUMBER(T_UNIDADE) AND A.CD_SETOR_AGENDA = TO_NUMBER(T_SETOR) AND TO_NUMBER(T_UNIDADE) IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))
                               OR
                              (A.CD_ESTABELECIMENTO = TO_NUMBER(T_UNIDADE) AND T_SETOR = '0000' AND TO_NUMBER(T_UNIDADE) NOT IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))
                        )
                        AND (A.CD_ESPECIALIDADE = CD_ESPECIALIDADE_P OR CD_ESPECIALIDADE_P IS NULL)
                    )
                )
        ) S LEFT JOIN AGENDA_CONSULTA ACU
                ON (ACU.NR_SEQUENCIA = S.NR_SEQ_ULTIMA)
            LEFT JOIN AGENDA_CONSULTA ACUA
                ON (ACUA.NR_SEQUENCIA = S.NR_SEQ_ULTIMA_AGENDADA)
            LEFT JOIN AGENDA_TURNO AT
                ON (AT.NR_SEQUENCIA = S.NR_SEQ_REG_TURNO)
        WHERE
            (((S.QT_IDADE_PAC >= AT.QT_IDADE_MIN OR AT.QT_IDADE_MIN IS NULL)
            AND (S.QT_IDADE_PAC <= AT.QT_IDADE_MAX OR AT.QT_IDADE_MAX IS NULL))
            OR S.NR_SEQ_REG_TURNO IS NULL)
            AND ((S.IE_SEXO_AGENDA = S.IE_SEXO_PAC) OR S.IE_SEXO_AGENDA = 'A');

        INSERT INTO OC_TEMP_SCHED_SLOTS
        SELECT
            *
        FROM
            OC_TEMP_SCHED_SLOTS_A S
        WHERE
            (T_QTD_SLOTS_P = 1 OR T_QTD_SLOTS_P <= (
                SELECT
                    COUNT(AC.NR_SEQUENCIA)
                FROM
                    OC_TEMP_SCHED_SLOTS_A AC
                WHERE
                    AC.CD_AGENDA = S.CD_AGENDA
                    AND AC.DT_AGENDA_T >= S.DT_AGENDA_T
                    AND AC.DT_AGENDA_T <= S.DT_AGENDA_T+((T_QTD_SLOTS_P-1)*((1/24/60)*AC.NR_MINUTO_DURACAO))
            ));

        OPEN P_RECORDSET FOR

            SELECT
                ROWNUM IDX,
                S.*,
                CASE
                WHEN (
                    SELECT
                    MIN(NR_SEQUENCIA)
                    FROM
                    OC_TEMP_SCHED_SLOTS SS
                    WHERE
                    TRUNC(SS.DT_AGENDA_T) = TRUNC(S.DT_AGENDA_T)
                ) = S.NR_SEQUENCIA THEN 'S'
                ELSE 'N'
                END IE_PRIM_HORARIO_DIA
            FROM
                OC_TEMP_SCHED_SLOTS S
            ORDER BY
                DT_AGENDA_T ASC;

    END GET_SCHEDULE_SLOTS;

    PROCEDURE GET_COMPANY_BRANCH(BRANCH_P IN VARCHAR2,PHYSICIAN_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            *
        FROM
        (
            SELECT
                DISTINCT
                PJ.CD_CGC,
                E.CD_ESTABELECIMENTO,
                E.NM_FANTASIA_ESTAB,
                PJ.DS_ENDERECO,
                PJ.DS_BAIRRO,
                PJ.DS_COMPLEMENTO,
                PJ.NR_ENDERECO,
                PJ.DS_MUNICIPIO,
                PJ.SG_ESTADO,
                PJ.CD_CEP
            FROM
                ESTABELECIMENTO E
                INNER JOIN PESSOA_JURIDICA PJ
                    ON (PJ.CD_CGC = E.CD_CGC)
                INNER JOIN AGENDA A
                    ON (A.CD_ESTABELECIMENTO = E.CD_ESTABELECIMENTO)
            WHERE
                (A.CD_PESSOA_FISICA = PHYSICIAN_P OR (PHYSICIAN_P IS NULL AND BRANCH_P IS NULL))
                AND E.NM_FANTASIA_ESTAB IS NOT NULL
                --AND E.CD_ESTABELECIMENTO = 31
            UNION ALL
            SELECT
                DISTINCT
                PJ.CD_CGC,
                E.CD_ESTABELECIMENTO,
                E.NM_FANTASIA_ESTAB,
                PJ.DS_ENDERECO,
                PJ.DS_BAIRRO,
                PJ.DS_COMPLEMENTO,
                PJ.NR_ENDERECO,
                PJ.DS_MUNICIPIO,
                PJ.SG_ESTADO,
                PJ.CD_CEP
            FROM
                ESTABELECIMENTO E
                INNER JOIN PESSOA_JURIDICA PJ
                    ON (PJ.CD_CGC = E.CD_CGC)
            WHERE
                E.CD_ESTABELECIMENTO = BRANCH_P
                AND E.NM_FANTASIA_ESTAB IS NOT NULL
                --AND E.CD_ESTABELECIMENTO = 31
        )
        ORDER BY
            NM_FANTASIA_ESTAB ASC;

    END GET_COMPANY_BRANCH;

    PROCEDURE GET_PHYSICIANS(
        IDENTIFIER_P IN VARCHAR2,
        NAME_P IN VARCHAR2,
        SPECIALTY_P IN VARCHAR2,
        CDESTABELECIMENTO_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

        T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

        IF IDENTIFIER_P IS NULL THEN

            OPEN P_RECORDSET FOR
                SELECT
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    (M.UF_CRM||M.NR_CRM) NR_CRM,
                    PF.NR_IDENTIDADE,
                    TO_CHAR(PF.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                    PF.IE_SEXO,
                    'N' IE_ULTIMO,
                    COUNT(
                        CASE
                        WHEN (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)) AND A.IE_APRES_AGENDA_EXTERNO = 'S') THEN 1
                        ELSE NULL
                        END
                    ) QT_AGENDA_LIB
                FROM
                    MEDICO M
                    INNER JOIN PESSOA_FISICA PF
                        ON (PF.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                    INNER JOIN AGENDA A
                        ON (A.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND A.IE_SITUACAO = 'A')
                    LEFT JOIN MEDICO_ESPECIALIDADE ME
                        ON (ME.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
                    INNER JOIN CONSELHO_PROFISSIONAL CP
                        ON (PF.NR_SEQ_CONSELHO = CP.NR_SEQUENCIA AND CP.SG_CONSELHO = 'CRM')
                WHERE
                    (UPPER(PF.NM_PESSOA_FISICA) LIKE REPLACE(UPPER(NAME_P),'~','%') OR NAME_P IS NULL)
                    AND (ME.CD_ESPECIALIDADE = SPECIALTY_P OR SPECIALTY_P IS NULL)
                    AND (M.CD_PESSOA_FISICA = IDENTIFIER_P OR IDENTIFIER_P IS NULL)
                    AND ( A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)) OR IDENTIFIER_P IS NOT NULL )
                    AND ( A.CD_AGENDA IS NOT NULL OR IDENTIFIER_P IS NOT NULL )
                    AND ( ( A.CD_ESTABELECIMENTO = CDESTABELECIMENTO_P OR CDESTABELECIMENTO_P IS NULL ) OR IDENTIFIER_P IS NOT NULL )
                    AND M.IE_SITUACAO = 'A'
                GROUP BY
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    M.UF_CRM,
                    M.NR_CRM,
                    PF.NR_IDENTIDADE,
                    PF.DT_NASCIMENTO,
                    PF.IE_SEXO
                ORDER BY
                    PF.NM_PESSOA_FISICA ASC;
        ELSE

            OPEN P_RECORDSET FOR
                SELECT
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    (M.UF_CRM||M.NR_CRM) NR_CRM,
                    PF.NR_IDENTIDADE,
                    TO_CHAR(PF.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                    PF.IE_SEXO,
                    'N' IE_ULTIMO,
                    COUNT(
                        CASE
                        WHEN (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)) AND A.IE_APRES_AGENDA_EXTERNO = 'S') THEN 1
                        ELSE NULL
                        END
                    ) QT_AGENDA_LIB
                FROM
                    MEDICO M
                    INNER JOIN PESSOA_FISICA PF
                        ON (PF.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                    LEFT JOIN AGENDA A
                        ON (A.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND A.IE_SITUACAO = 'A')
                    INNER JOIN CONSELHO_PROFISSIONAL CP
                        ON (PF.NR_SEQ_CONSELHO = CP.NR_SEQUENCIA AND CP.SG_CONSELHO = 'CRM')
                WHERE
                    M.CD_PESSOA_FISICA = IDENTIFIER_P
                    AND M.IE_SITUACAO = 'A'
                GROUP BY
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    M.UF_CRM,
                    M.NR_CRM,
                    PF.NR_IDENTIDADE,
                    PF.DT_NASCIMENTO,
                    PF.IE_SEXO;

        END IF;

    END GET_PHYSICIANS;

    PROCEDURE GET_PHYSICIAN_REF_PATIENTS(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            S.*,
            TO_CHAR(AP.DT_ENTRADA,'YYYY-MM-DD HH24:MI:SS') DT_REFERENCIA
        FROM
        (
            SELECT
                AP.CD_PESSOA_FISICA,
                MAX(AP.NR_ATENDIMENTO) NR_ATENDIMENTO
            FROM
                ATENDIMENTO_PACIENTE AP
                INNER JOIN ATENDIMENTO_INDICACAO AI
                    ON (AI.NR_ATENDIMENTO = AP.NR_ATENDIMENTO)
            WHERE
                AI.CD_MEDICO = IDENTIFIER_P
            GROUP BY
                AP.CD_PESSOA_FISICA
        ) S INNER JOIN ATENDIMENTO_PACIENTE AP
                ON (AP.NR_ATENDIMENTO = S.NR_ATENDIMENTO)
            INNER JOIN PESSOA_FISICA PF
                ON (PF.CD_PESSOA_FISICA = AP.CD_PESSOA_FISICA)
        ORDER BY
            AP.DT_ENTRADA DESC;

    END GET_PHYSICIAN_REF_PATIENTS;

    PROCEDURE GET_PATIENT(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            PF.CD_PESSOA_FISICA,
            PF.NM_PESSOA_FISICA,
            PF.NR_CPF,
            PF.NR_IDENTIDADE,
            TO_CHAR(PF.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
            PF.IE_SEXO,
            PF.CD_CARGO,
            C.DS_CARGO,
            PF.IE_GRAU_INSTRUCAO,
            PF.NM_SOCIAL,
            TO_CHAR(PF.DT_CADASTRO_ORIGINAL,'YYYY-MM-DD HH24:MI:SS') DT_CADASTRO_ORIGINAL
        FROM
            PESSOA_FISICA PF
            LEFT JOIN CARGO C
                ON (C.CD_CARGO = PF.CD_CARGO)
        WHERE
            PF.CD_PESSOA_FISICA = IDENTIFIER_P;

    END GET_PATIENT;



    PROCEDURE GET_INSURANCE_TYPE(IDENTIFIER_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS

    T_ID_1 VARCHAR(45);
    T_ID_2 VARCHAR(45);
    T_ID_3 VARCHAR(45);

    BEGIN

        -- CD_CONVENIO
        T_ID_1 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,1) );
        -- CD_CATEGORIA
        T_ID_2 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,2) );
        -- CD_PLANO
        T_ID_3 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,3) );

        OPEN P_RECORDSET FOR

        SELECT
            C.CD_CONVENIO||'.'||NVL(CC.CD_CATEGORIA,'0')||'.'||NVL(CP.CD_PLANO,'0') "ID",
            C.CD_CONVENIO,
            C.DS_CONVENIO,
            CC.CD_CATEGORIA,
            CC.DS_CATEGORIA,
            CP.CD_PLANO,
            CP.DS_PLANO
        FROM
            CONVENIO C
            LEFT JOIN CATEGORIA_CONVENIO CC
                ON (CC.CD_CONVENIO = C.CD_CONVENIO)
            LEFT JOIN CONVENIO_PLANO CP
                ON (CP.CD_CONVENIO = C.CD_CONVENIO)
        WHERE
            C.IE_SITUACAO = 'A'
            AND CC.IE_SITUACAO = 'A'
            AND CP.IE_SITUACAO = 'A'
            AND C.CD_CONVENIO = T_ID_1
            AND CC.CD_CATEGORIA = T_ID_2
            AND CP.CD_PLANO = T_ID_3
        ORDER BY
            C.DS_CONVENIO ASC,
            CC.DS_CATEGORIA ASC,
            CP.DS_PLANO ASC;

    END GET_INSURANCE_TYPE;

    PROCEDURE GET_INSURANCE_OPERATORS(IDENTIFIER_P IN VARCHAR2,NAME_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            CD_CONVENIO,
            DS_CONVENIO
        FROM
            CONVENIO
        WHERE
            IE_SITUACAO = 'A'
            AND IE_TIPO_CONVENIO = 2
            AND (CD_CONVENIO = IDENTIFIER_P OR IDENTIFIER_P IS NULL)
            AND (UPPER(DS_CONVENIO) LIKE UPPER(NAME_P) OR NAME_P IS NULL)
        ORDER BY
            TRIM(DS_CONVENIO) ASC;

    END GET_INSURANCE_OPERATORS;

    PROCEDURE GET_INSURANCE_CATEGORIES(OPERATOR_P IN VARCHAR2,NAME_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            CD_CATEGORIA "ID",
            CD_CONVENIO,
            CD_CATEGORIA,
            DS_CATEGORIA
        FROM
            CATEGORIA_CONVENIO
        WHERE
            IE_SITUACAO = 'A'
            AND CD_CONVENIO = OPERATOR_P
            AND (UPPER(DS_CATEGORIA) LIKE UPPER(NAME_P) OR NAME_P IS NULL)
        ORDER BY
            TRIM(DS_CATEGORIA) ASC;

    END GET_INSURANCE_CATEGORIES;

    PROCEDURE GET_INSURANCE_PLANS(OPERATOR_P IN VARCHAR2,NAME_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN
      OPEN P_RECORDSET FOR
        SELECT
            CD_PLANO "ID",
            CD_CONVENIO,
            CD_PLANO,
            DS_PLANO
        FROM
            CONVENIO_PLANO
        WHERE
            IE_SITUACAO = 'A'
            AND CD_CONVENIO = OPERATOR_P
            AND (UPPER(DS_PLANO) LIKE UPPER(NAME_P) OR NAME_P IS NULL)
        ORDER BY
            TRIM(DS_PLANO) ASC;

    END GET_INSURANCE_PLANS;

    PROCEDURE CREATE_APPOINTMENT(
        NRSEQCONSULTA_P IN VARCHAR2,
        CDAGENDA_P IN VARCHAR2,
        CDPESSOAFISICA_P IN VARCHAR2,
        CDCONVENIO_P IN VARCHAR2,
        CDCATEGORIA_P IN VARCHAR2,
        CDPLANO_P IN VARCHAR2,
        IECLASSIFAGENDA_P IN VARCHAR2,
        CDUSUARIOCONVENIO_P IN VARCHAR2,
        DTVALIDADECARTEIRA_P IN VARCHAR2,
        IEFORMAAGENDAMENTO_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_QTD NUMBER := 0;
    NR_IDADE_P NUMBER;
    QTD_SLOTS_P NUMBER := 0;
    DT_NASCIMENTO_P VARCHAR2(20);
    NR_TELEFONE_P VARCHAR2(30);
    DT_AGENDA_REF_P DATE;
    SCHEDULE_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( SCHEDULE_NOT_FOUND, -20001 );
    NO_SLOTS_AVAILABLE EXCEPTION;
    PRAGMA EXCEPTION_INIT( NO_SLOTS_AVAILABLE, -20002 );

    T_CD_AGENDA NUMBER(10);
    T_DT_AGENDA DATE;
    T_CD_ESTABELECIMENTO NUMBER(10);

    BEGIN

        SELECT
            COUNT(NR_SEQUENCIA) INTO T_QTD
        FROM
            AGENDA_CONSULTA
        WHERE
            NR_SEQUENCIA = NRSEQCONSULTA_P
            AND CD_AGENDA = CDAGENDA_P
            AND IE_STATUS_AGENDA = 'L';

        IF T_QTD=0 THEN
            RAISE SCHEDULE_NOT_FOUND;
        END IF;

        SELECT
            TRUNC(TRUNC(TRUNC(SYSDATE) - TRUNC(DT_NASCIMENTO)) / 365),
            TO_CHAR(DT_NASCIMENTO,'YYYY-MM-DD'),
            NR_DDD_CELULAR||'-'||NR_TELEFONE_CELULAR
            INTO
            NR_IDADE_P,
            DT_NASCIMENTO_P,
            NR_TELEFONE_P
        FROM
            PESSOA_FISICA
        WHERE
            CD_PESSOA_FISICA = CDPESSOAFISICA_P;

        QTD_SLOTS_P := NVL(NPARAM('SLOTSQTD',PARAMS_P),1);

        IF QTD_SLOTS_P > 1 THEN

            SELECT
                DT_AGENDA INTO DT_AGENDA_REF_P
            FROM
                AGENDA_CONSULTA
            WHERE
                NR_SEQUENCIA = NRSEQCONSULTA_P
                AND CD_AGENDA = CDAGENDA_P;

            DECLARE

            CURSOR C_SLOTS IS
                SELECT
                    *
                FROM
                (
                    SELECT
                        NR_SEQUENCIA
                    FROM
                        AGENDA_CONSULTA
                    WHERE
                        CD_AGENDA = CDAGENDA_P
                        AND IE_STATUS_AGENDA = 'L'
                        AND TRUNC(DT_AGENDA) = TRUNC(DT_AGENDA_REF_P)
                        AND ( NR_SEQUENCIA = NRSEQCONSULTA_P OR DT_AGENDA > DT_AGENDA_REF_P )
                        AND DT_AGENDA <= DT_AGENDA_REF_P+((QTD_SLOTS_P-1)*((1/24/60)*NR_MINUTO_DURACAO))
                    ORDER BY
                        DT_AGENDA ASC
                )
                WHERE
                    ROWNUM <= QTD_SLOTS_P;

            R_SLOT C_SLOTS%ROWTYPE;
            T_CURR_DATE DATE;

            BEGIN

                T_CURR_DATE := SYSDATE;

                OPEN C_SLOTS;

                LOOP
                FETCH C_SLOTS INTO R_SLOT;
                EXIT WHEN C_SLOTS%NOTFOUND;

                    UPDATE
                        AGENDA_CONSULTA
                    SET
                        NR_SEQ_AGENDAMENTO = NRSEQCONSULTA_P,
                        IE_STATUS_AGENDA = 'N',
                        IE_CLASSIF_AGENDA = IECLASSIFAGENDA_P, --'N' = CONSULTA; 'Pr' = RETORNO
                        DT_AGENDAMENTO = T_CURR_DATE,
                        NM_USUARIO = APP_USER,
                        NM_USUARIO_ORIGEM = APP_USER,
                        CD_PESSOA_FISICA = CDPESSOAFISICA_P,
                        NR_TELEFONE = NR_TELEFONE_P,
                        DT_NASCIMENTO_PAC = TO_DATE(DT_NASCIMENTO_P, 'YYYY-MM-DD'),
                        QT_IDADE_PAC = NR_IDADE_P,
                        CD_CONVENIO = CDCONVENIO_P,
                        CD_CATEGORIA  = CDCATEGORIA_P,
                        CD_PLANO = CDPLANO_P,
                        CD_USUARIO_CONVENIO = CDUSUARIOCONVENIO_P,
                        DT_VALIDADE_CARTEIRA = TO_DATE(DTVALIDADECARTEIRA_P,'YYYY-MM-DD')
                    WHERE
                        NR_SEQUENCIA = R_SLOT.NR_SEQUENCIA;

                END LOOP;

                IF C_SLOTS%ROWCOUNT <> QTD_SLOTS_P THEN
                    ROLLBACK;
                    RAISE NO_SLOTS_AVAILABLE;
                END IF;

                CLOSE C_SLOTS;

            END;

        ELSE

            UPDATE
                AGENDA_CONSULTA
            SET
                IE_STATUS_AGENDA = 'N',
                IE_CLASSIF_AGENDA = IECLASSIFAGENDA_P, --'N' = CONSULTA; 'Pr' = RETORNO
                DT_AGENDAMENTO = SYSDATE,
                NM_USUARIO = APP_USER,
                NM_USUARIO_ORIGEM = APP_USER,
                CD_PESSOA_FISICA = CDPESSOAFISICA_P,
                NR_TELEFONE = NR_TELEFONE_P,
                DT_NASCIMENTO_PAC = TO_DATE(DT_NASCIMENTO_P, 'YYYY-MM-DD'),
                QT_IDADE_PAC = NR_IDADE_P,
                CD_CONVENIO = CDCONVENIO_P,
                CD_CATEGORIA  = CDCATEGORIA_P,
                CD_PLANO = CDPLANO_P,
                CD_USUARIO_CONVENIO = CDUSUARIOCONVENIO_P,
                DT_VALIDADE_CARTEIRA = TO_DATE(DTVALIDADECARTEIRA_P,'YYYY-MM-DD')
            WHERE
                NR_SEQUENCIA = NRSEQCONSULTA_P;

        END IF;

        SELECT
            AC.CD_AGENDA,
            AC.DT_AGENDA,
            A.CD_ESTABELECIMENTO
            INTO
            T_CD_AGENDA,
            T_DT_AGENDA,
            T_CD_ESTABELECIMENTO
        FROM
            AGENDA_CONSULTA AC
            INNER JOIN AGENDA A
                ON (A.CD_AGENDA = AC.CD_AGENDA)
        WHERE
            AC.NR_SEQUENCIA = NRSEQCONSULTA_P
            AND A.CD_AGENDA = CDAGENDA_P;

        GERAR_HORARIO_AGENDA_CONSULTA(APP_USER,T_CD_ESTABELECIMENTO,T_CD_AGENDA,TRUNC(T_DT_AGENDA),TRUNC(T_DT_AGENDA),NULL,0);

    EXCEPTION
        WHEN SCHEDULE_NOT_FOUND THEN
            P_ERR := 'SCHEDULE_NOT_FOUND';
        WHEN NO_SLOTS_AVAILABLE THEN
            P_ERR := 'NO_SLOTS_AVAILABLE';
        WHEN OTHERS THEN
            RAISE;

    END CREATE_APPOINTMENT;

    PROCEDURE CREATE_PATIENT_INSURANCE(
        CDPESSOAFISICA_P IN VARCHAR2,
        CDCONVENIO_P IN VARCHAR2,
        CDCATEGORIA_P IN VARCHAR2,
        CDPLANO_P IN VARCHAR2,
        CDUSUARIOCONVENIO_P IN VARCHAR2,
        DTVALIDADECARTEIRA_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_QTD_PF NUMBER := 0;
    T_QTD_INS NUMBER := 0;
    T_QTD_CAT NUMBER := 0;
    PERSON_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( PERSON_NOT_FOUND, -20001 );
    INSURANCE_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( INSURANCE_NOT_FOUND, -20002 );
    DUPLICATED_INSURANCE EXCEPTION;
    PRAGMA EXCEPTION_INIT( DUPLICATED_INSURANCE, -20003 );

    BEGIN

        SELECT
        COUNT(CD_PESSOA_FISICA) INTO T_QTD_PF
        FROM
        PESSOA_FISICA
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P;

        IF T_QTD_PF=0 THEN

            RAISE PERSON_NOT_FOUND;

        END IF;

        SELECT
        COUNT(CD_CATEGORIA) INTO T_QTD_CAT
        FROM
        CATEGORIA_CONVENIO
        WHERE
        CD_CONVENIO = CDCONVENIO_P
        AND CD_CATEGORIA = CDCATEGORIA_P;

        IF T_QTD_CAT=0 THEN

            RAISE INSURANCE_NOT_FOUND;

        END IF;

        SELECT
        COUNT(NR_SEQUENCIA) INTO T_QTD_INS
        FROM
        PESSOA_TITULAR_CONVENIO
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P
        AND CD_CONVENIO = CDCONVENIO_P
        AND CD_CATEGORIA = CDCATEGORIA_P
        AND CD_USUARIO_CONVENIO = CDUSUARIOCONVENIO_P;

        IF T_QTD_INS>0 THEN

            RAISE DUPLICATED_INSURANCE;

        END IF;

        SELECT PESSOA_TITULAR_CONVENIO_SEQ.NEXTVAL INTO T_ID FROM DUAL;

        INSERT INTO
        PESSOA_TITULAR_CONVENIO
        (NR_SEQUENCIA,CD_PESSOA_FISICA,CD_CONVENIO,CD_CATEGORIA,CD_PLANO_CONVENIO,CD_USUARIO_CONVENIO,DT_VALIDADE_CARTEIRA,DT_ATUALIZACAO,NM_USUARIO)
        VALUES
        (T_ID,CDPESSOAFISICA_P,CDCONVENIO_P,CDCATEGORIA_P,CDPLANO_P,CDUSUARIOCONVENIO_P,TO_DATE(DTVALIDADECARTEIRA_P,'YYYY-MM-DD'),SYSDATE,'portal');

        P_ID := T_ID;

    EXCEPTION
        WHEN PERSON_NOT_FOUND THEN
            P_ERR := 'PERSON_NOT_FOUND';
        WHEN INSURANCE_NOT_FOUND THEN
            P_ERR := 'INSURANCE_NOT_FOUND';
        WHEN DUPLICATED_INSURANCE THEN
            P_ERR := 'DUPLICATED_INSURANCE';
        WHEN OTHERS THEN
            RAISE;

    END CREATE_PATIENT_INSURANCE;

    PROCEDURE DELETE_PATIENT_INSURANCE(
        NRSEQUENCIA_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_CATEGORIA VARCHAR(45);
    T_PLANO VARCHAR(45);

    BEGIN

        SELECT
        NR_SEQUENCIA INTO T_ID
        FROM
        PESSOA_TITULAR_CONVENIO
        WHERE
        NR_SEQUENCIA = NRSEQUENCIA_P;

        DELETE FROM
        PESSOA_TITULAR_CONVENIO
        WHERE
        NR_SEQUENCIA = T_ID;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END DELETE_PATIENT_INSURANCE;

    PROCEDURE UPDATE_APPOINTMENT_STATUS(
        NRSEQCONSULTA_P IN VARCHAR2,
        TYPE_P IN VARCHAR2,
        CDMOTIVOCANCELAMENTO_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_CD_PACIENTE VARCHAR2(10);
    T_CD_AGENDA NUMBER(10);
    T_DT_AGENDA DATE;
    T_DT_AGENDAMENTO DATE;
    T_CD_ESTABELECIMENTO NUMBER(10);

    BEGIN

        SELECT
            AC.NR_SEQUENCIA,
            AC.CD_PESSOA_FISICA,
            AC.CD_AGENDA,
            AC.DT_AGENDA,
            AC.DT_AGENDAMENTO,
            A.CD_ESTABELECIMENTO
            INTO
            T_ID,
            T_CD_PACIENTE,
            T_CD_AGENDA,
            T_DT_AGENDA,
            T_DT_AGENDAMENTO,
            T_CD_ESTABELECIMENTO
        FROM
            AGENDA_CONSULTA AC
            INNER JOIN AGENDA A
                ON (A.CD_AGENDA = AC.CD_AGENDA)
        WHERE
            AC.NR_SEQUENCIA = NRSEQCONSULTA_P
            AND AC.IE_STATUS_AGENDA IN ('N');

        IF TYPE_P = 'CONFIRM' THEN

            UPDATE
                AGENDA_CONSULTA
            SET
                IE_STATUS_AGENDA = 'CN',
                NM_USUARIO = 'portal',
                NM_USUARIO_STATUS = 'portal',
                NM_USUARIO_CONFIRM = 'portal',
                DT_CONFIRMACAO = SYSDATE,
                DT_STATUS = SYSDATE
            WHERE
                NR_SEQUENCIA = NRSEQCONSULTA_P
                OR NR_SEQ_AGENDAMENTO = NRSEQCONSULTA_P
                OR (
                    CD_AGENDA = T_CD_AGENDA
                    AND CD_PESSOA_FISICA = T_CD_PACIENTE
                    AND TRUNC(DT_AGENDA) = TRUNC(T_DT_AGENDA)
                    AND DT_AGENDAMENTO = T_DT_AGENDAMENTO
                );

            GERAR_HORARIO_AGENDA_CONSULTA(APP_USER,T_CD_ESTABELECIMENTO,T_CD_AGENDA,TRUNC(T_DT_AGENDA),TRUNC(T_DT_AGENDA),NULL,0);

        END IF;

        IF TYPE_P = 'CANCEL' THEN

            UPDATE
                AGENDA_CONSULTA
            SET
                IE_STATUS_AGENDA = 'C',
                NM_USUARIO = 'portal',
                NM_USUARIO_STATUS = 'portal',
                NM_USUARIO_CANCELAMENTO = 'portal',
                DT_CANCELAMENTO = SYSDATE,
                DT_STATUS = SYSDATE,
                CD_MOTIVO_CANCELAMENTO = CDMOTIVOCANCELAMENTO_P
            WHERE
                NR_SEQUENCIA = NRSEQCONSULTA_P
                OR NR_SEQ_AGENDAMENTO = NRSEQCONSULTA_P
                OR (
                    CD_AGENDA = T_CD_AGENDA
                    AND CD_PESSOA_FISICA = T_CD_PACIENTE
                    AND TRUNC(DT_AGENDA) = TRUNC(T_DT_AGENDA)
                    AND DT_AGENDAMENTO = T_DT_AGENDAMENTO
                );

            GERAR_HORARIO_AGENDA_CONSULTA(APP_USER,T_CD_ESTABELECIMENTO,T_CD_AGENDA,TRUNC(T_DT_AGENDA),TRUNC(T_DT_AGENDA),NULL,0);

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END UPDATE_APPOINTMENT_STATUS;

    PROCEDURE UPDATE_PATIENT(
        IDENTIFIER_P IN VARCHAR2,
        NMPESSOAFISICA_P IN VARCHAR2,
        NRIDENTIDADE_P IN VARCHAR2,
        DTNASCIMENTO_P IN VARCHAR2,
        IESEXO_P IN VARCHAR2,
        CDCARGO_P IN VARCHAR2,
        IEGRAUINSTRUCAO_P IN VARCHAR2,
        NMSOCIAL_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);

    BEGIN

        SELECT CD_PESSOA_FISICA INTO T_ID FROM PESSOA_FISICA WHERE CD_PESSOA_FISICA = IDENTIFIER_P;

        UPDATE
            PESSOA_FISICA
        SET
            NM_PESSOA_FISICA = NVL(NMPESSOAFISICA_P,NM_PESSOA_FISICA),
            NR_IDENTIDADE = NWC(NVL(NRIDENTIDADE_P,NR_IDENTIDADE)),
            DT_NASCIMENTO = NVL(TO_DATE(DTNASCIMENTO_P,'YYYY-MM-DD'),DT_NASCIMENTO),
            IE_SEXO = NVL(IESEXO_P,IE_SEXO),
            CD_CARGO = NWC(NVL(CDCARGO_P,CD_CARGO)),
            IE_GRAU_INSTRUCAO = NWC(NVL(IEGRAUINSTRUCAO_P,IE_GRAU_INSTRUCAO)),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal',
            NM_SOCIAL = NVL(NMSOCIAL_P,NM_SOCIAL)
        WHERE
            CD_PESSOA_FISICA = IDENTIFIER_P;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END UPDATE_PATIENT;

    PROCEDURE CREATE_PATIENT(
        NMPESSOAFISICA_P IN VARCHAR2,
        NRIDENTIDADE_P IN VARCHAR2,
        NRCPF_P IN VARCHAR2,
        DTNASCIMENTO_P IN VARCHAR2,
        IESEXO_P IN VARCHAR2,
        CDCARGO_P IN VARCHAR2,
        IEGRAUINSTRUCAO_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_QTD NUMBER := 0;

    BEGIN

        SELECT
        COUNT(CD_PESSOA_FISICA) INTO T_QTD
        FROM
        PESSOA_FISICA
        WHERE
        REPLACE(REPLACE(NR_CPF,'.',''),'-','') = REPLACE(REPLACE(NRCPF_P,'.',''),'-','');

        IF T_QTD>0 THEN

            P_ERR := 'CPF_ALREADY_EXISTS';

        ELSE

            SELECT PESSOA_FISICA_SEQ.NEXTVAL INTO T_ID FROM DUAL;

            INSERT INTO
                PESSOA_FISICA
                (CD_PESSOA_FISICA,
                NR_PRONTUARIO,
                NM_PESSOA_FISICA,
                NR_IDENTIDADE,
                NR_CPF,
                DT_NASCIMENTO,
                IE_SEXO,
                CD_CARGO,
                IE_GRAU_INSTRUCAO,
                DT_ATUALIZACAO,
                DT_CADASTRO_ORIGINAL,
                NM_USUARIO,
                IE_TIPO_PESSOA)
            VALUES
                (T_ID,
                T_ID,
                NMPESSOAFISICA_P,
                NRIDENTIDADE_P,
                NRCPF_P,
                TO_DATE(DTNASCIMENTO_P,'YYYY-MM-DD'),
                IESEXO_P,
                CDCARGO_P,
                IEGRAUINSTRUCAO_P,
                SYSDATE,
                SYSDATE,
                'portal',
                '2');

            P_ID := T_ID;

        END IF;

    EXCEPTION
        WHEN OTHERS THEN
            RAISE;

    END CREATE_PATIENT;

    PROCEDURE UPDATE_PATIENT_RELATIVE(
        IDENTIFIER_P IN VARCHAR2,
        NRCPF_P IN VARCHAR2,
        NMCONTATO_P IN VARCHAR2,
        IETIPOCOMPLEMENTO_P IN VARCHAR2,
        NRTELEFONECELULAR_P IN VARCHAR2,
        CDTIPOLOGRADOURO_P IN VARCHAR2,
        DSENDERECO_P IN VARCHAR2,
        DSBAIRRO_P IN VARCHAR2,
        DSCOMPLEMENTO_P IN VARCHAR2,
        NRENDERECO_P IN VARCHAR2,
        DSMUNICIPIO_P IN VARCHAR2,
        SGESTADO_P IN VARCHAR2,
        CDCEP_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_ID_1 VARCHAR(45);
    T_ID_2 VARCHAR(45);
    T_REF_PF VARCHAR(45);

    BEGIN

        -- CD_PESSOA_FISICA
        T_ID_1 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,1) );
        -- NR_SEQUENCIA COMPL
        T_ID_2 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,2) );

        SELECT
            CD_PESSOA_FISICA INTO T_ID
        FROM
            COMPL_PESSOA_FISICA
        WHERE
            CD_PESSOA_FISICA = T_ID_1
            AND NR_SEQUENCIA = T_ID_2;

        UPDATE
            COMPL_PESSOA_FISICA
        SET
            NM_CONTATO = NWC(NVL(NMCONTATO_P,NM_CONTATO)),
            NR_CPF = NWC(NVL(NRCPF_P,NR_CPF)),
            IE_TIPO_COMPLEMENTO = NVL(IETIPOCOMPLEMENTO_P,IE_TIPO_COMPLEMENTO),
            NR_TELEFONE_CELULAR = NWC(NVL(NRTELEFONECELULAR_P,NR_TELEFONE_CELULAR)),
            CD_TIPO_LOGRADOURO = NVL(CDTIPOLOGRADOURO_P,CD_TIPO_LOGRADOURO),
            DS_ENDERECO = NWC(NVL(DSENDERECO_P,DS_ENDERECO)),
            DS_BAIRRO = NWC(NVL(DSBAIRRO_P,DS_BAIRRO)),
            DS_COMPLEMENTO = NWC(NVL(DSCOMPLEMENTO_P,DS_COMPLEMENTO)),
            NR_ENDERECO = NWC(NVL(NRENDERECO_P,NR_ENDERECO)),
            DS_MUNICIPIO = NWC(NVL(DSMUNICIPIO_P,DS_MUNICIPIO)),
            SG_ESTADO = NVL(SGESTADO_P,SG_ESTADO),
            CD_CEP = NWC(NVL(CDCEP_P,CD_CEP)),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal'
        WHERE
            CD_PESSOA_FISICA = T_ID_1
            AND NR_SEQUENCIA = T_ID_2;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END UPDATE_PATIENT_RELATIVE;

    PROCEDURE UPDATE_PATIENT_ADDRESS(
        IDENTIFIER_P IN VARCHAR2,
        IETIPOCOMPLEMENTO_P IN VARCHAR2,
        CDTIPOLOGRADOURO_P IN VARCHAR2,
        DSENDERECO_P IN VARCHAR2,
        DSBAIRRO_P IN VARCHAR2,
        DSCOMPLEMENTO_P IN VARCHAR2,
        NRENDERECO_P IN VARCHAR2,
        DSMUNICIPIO_P IN VARCHAR2,
        SGESTADO_P IN VARCHAR2,
        CDCEP_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_ID_1 VARCHAR(45);
    T_ID_2 VARCHAR(45);

    BEGIN

        -- CD_PESSOA_FISICA
        T_ID_1 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,1) );
        -- NR_SEQUENCIA COMPL
        T_ID_2 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,2) );

        SELECT
            CD_PESSOA_FISICA INTO T_ID
        FROM
            COMPL_PESSOA_FISICA
        WHERE
            CD_PESSOA_FISICA = T_ID_1
            AND NR_SEQUENCIA = T_ID_2;

        UPDATE
            COMPL_PESSOA_FISICA
        SET
            IE_TIPO_COMPLEMENTO = NWC(NVL(IETIPOCOMPLEMENTO_P,IE_TIPO_COMPLEMENTO)),
            CD_TIPO_LOGRADOURO = NWC(NVL(LPAD(CDTIPOLOGRADOURO_P,3,'0'),CD_TIPO_LOGRADOURO)),
            DS_ENDERECO = NWC(NVL(DSENDERECO_P,DS_ENDERECO)),
            DS_BAIRRO = NWC(NVL(DSBAIRRO_P,DS_BAIRRO)),
            DS_COMPLEMENTO = NWC(NVL(DSCOMPLEMENTO_P,DS_COMPLEMENTO)),
            NR_ENDERECO = NWC(NVL(NRENDERECO_P,NR_ENDERECO)),
            DS_MUNICIPIO = NWC(NVL(DSMUNICIPIO_P,DS_MUNICIPIO)),
            SG_ESTADO = NWC(NVL(SGESTADO_P,SG_ESTADO)),
            CD_CEP = NWC(NVL(CDCEP_P,CD_CEP)),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal'
        WHERE
            CD_PESSOA_FISICA = T_ID_1
            AND NR_SEQUENCIA = T_ID_2;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END UPDATE_PATIENT_ADDRESS;

    PROCEDURE CREATE_PATIENT_ADDRESS(
        CDPESSOAFISICA_P IN VARCHAR2,
        IETIPOCOMPLEMENTO_P IN VARCHAR2,
        CDTIPOLOGRADOURO_P IN VARCHAR2,
        DSENDERECO_P IN VARCHAR2,
        DSBAIRRO_P IN VARCHAR2,
        DSCOMPLEMENTO_P IN VARCHAR2,
        NRENDERECO_P IN VARCHAR2,
        DSMUNICIPIO_P IN VARCHAR2,
        SGESTADO_P IN VARCHAR2,
        CDCEP_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    PERSON_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( PERSON_NOT_FOUND, -20001 );
    ADDRESS_ALREADY_EXISTS EXCEPTION;
    PRAGMA EXCEPTION_INIT( ADDRESS_ALREADY_EXISTS, -20002 );
    T_ID VARCHAR(45);
    T_QTD_PF NUMBER := 0;
    T_QTD_ADDR NUMBER := 0;
    T_LAST_SEQ NUMBER := 0;

    BEGIN

        SELECT
        COUNT(CD_PESSOA_FISICA) INTO T_QTD_PF
        FROM
        PESSOA_FISICA
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P;

        IF T_QTD_PF=0 THEN

            RAISE PERSON_NOT_FOUND;

        END IF;

        SELECT
        COUNT(CD_PESSOA_FISICA) INTO T_QTD_ADDR
        FROM
        COMPL_PESSOA_FISICA
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P
        AND IE_TIPO_COMPLEMENTO = IETIPOCOMPLEMENTO_P
        AND DS_ENDERECO IS NOT NULL;

        IF T_QTD_ADDR>0 THEN

            RAISE ADDRESS_ALREADY_EXISTS;

        END IF;

        SELECT
        NVL(MAX(NR_SEQUENCIA),0) INTO T_LAST_SEQ
        FROM
        COMPL_PESSOA_FISICA
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P;

        INSERT INTO
        COMPL_PESSOA_FISICA
        (CD_PESSOA_FISICA,
        NR_SEQUENCIA,
        IE_TIPO_COMPLEMENTO,
        CD_TIPO_LOGRADOURO,
        DS_ENDERECO,
        DS_BAIRRO,
        DS_COMPLEMENTO,
        NR_ENDERECO,
        DS_MUNICIPIO,
        SG_ESTADO,
        CD_CEP,
        DT_ATUALIZACAO,
        NM_USUARIO)
        VALUES
        (CDPESSOAFISICA_P,
        T_LAST_SEQ+1,
        IETIPOCOMPLEMENTO_P,
        LPAD(CDTIPOLOGRADOURO_P,3,'0'),
        DSENDERECO_P,
        DSBAIRRO_P,
        DSCOMPLEMENTO_P,
        NRENDERECO_P,
        DSMUNICIPIO_P,
        SGESTADO_P,
        CDCEP_P,
        SYSDATE,
        'portal');

        P_ID := CDPESSOAFISICA_P||'.'||(T_LAST_SEQ+1);

    EXCEPTION
        WHEN PERSON_NOT_FOUND THEN
            P_ERR := 'PERSON_NOT_FOUND';
        WHEN ADDRESS_ALREADY_EXISTS THEN
            P_ERR := 'ADDRESS_ALREADY_EXISTS';
        WHEN OTHERS THEN
            RAISE;

    END CREATE_PATIENT_ADDRESS;

    PROCEDURE UPDATE_PATIENT_INSURANCE(
        IDENTIFIER_P IN VARCHAR2,
        CDCONVENIO_P IN VARCHAR2,
        CDCATEGORIA_P IN VARCHAR2,
        CDPLANOCONVENIO_P IN VARCHAR2,
        CDUSUARIOCONVENIO_P IN VARCHAR2,
        DTVALIDADECARTEIRA_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);

    BEGIN

        SELECT
            NR_SEQUENCIA INTO T_ID
        FROM
            PESSOA_TITULAR_CONVENIO
        WHERE
            NR_SEQUENCIA = IDENTIFIER_P;

        UPDATE
            PESSOA_TITULAR_CONVENIO
        SET
            CD_CONVENIO = NWC(NVL(CDCONVENIO_P,CD_CONVENIO)),
            CD_CATEGORIA = NWC(NVL(CDCATEGORIA_P,CD_CATEGORIA)),
            CD_PLANO_CONVENIO = NWC(NVL(CDPLANOCONVENIO_P,CD_PLANO_CONVENIO)),
            CD_USUARIO_CONVENIO = NWC(NVL(CDUSUARIOCONVENIO_P,CD_USUARIO_CONVENIO)),
            DT_VALIDADE_CARTEIRA = NWC(NVL(TO_DATE(DTVALIDADECARTEIRA_P,'YYYY-MM-DD'),DT_VALIDADE_CARTEIRA)),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal'
        WHERE
            NR_SEQUENCIA = IDENTIFIER_P;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END UPDATE_PATIENT_INSURANCE;

    PROCEDURE UPDATE_PATIENT_CONTACT(
        IDENTIFIER_P IN VARCHAR2,
        DSCONTATO_P IN VARCHAR2,
        IETIPO_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_ID_1 VARCHAR(45);
    T_ID_2 VARCHAR(45);
    T_ID_3 VARCHAR(45);

    BEGIN

        -- CD_PESSOA_FISICA
        T_ID_1 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,1) );
        -- NR_SEQUENCIA COMPL
        T_ID_2 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,2) );
        -- TIPO
        T_ID_3 := TRIM(REGEXP_SUBSTR(IDENTIFIER_P,'[^.]+',1,3) );

        IF T_ID_3 = 'mob' THEN

            SELECT
                CD_PESSOA_FISICA INTO T_ID
            FROM
                PESSOA_FISICA
            WHERE
                CD_PESSOA_FISICA = T_ID_1;

            UPDATE
            PESSOA_FISICA
            SET
            NR_DDI_CELULAR = NVL(SUBSTR(DSCONTATO_P,-13,2),NR_DDI_CELULAR),
            NR_DDD_CELULAR = NVL(SUBSTR(DSCONTATO_P,-11,2),NR_DDD_CELULAR),
            NR_TELEFONE_CELULAR = NVL(SUBSTR(DSCONTATO_P,-9),NR_TELEFONE_CELULAR),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal'
            WHERE
            CD_PESSOA_FISICA = T_ID_1;

        END IF;

        IF T_ID_3 = 'mail' THEN

            SELECT
                CD_PESSOA_FISICA INTO T_ID
            FROM
                COMPL_PESSOA_FISICA
            WHERE
                CD_PESSOA_FISICA = T_ID_1
                AND NR_SEQUENCIA = T_ID_2;

            UPDATE
            COMPL_PESSOA_FISICA
            SET
            DS_EMAIL = NVL(DSCONTATO_P,DS_EMAIL),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal'
            WHERE
            CD_PESSOA_FISICA = T_ID_1
            AND NR_SEQUENCIA = T_ID_2;

        END IF;

        IF T_ID_3 IS NULL OR T_ID_3 NOT IN ('mob','mail') THEN
            P_ERR := 'NO_DATA_FOUND';
        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END UPDATE_PATIENT_CONTACT;

    PROCEDURE CREATE_PATIENT_CONTACT(
        CDPESSOAFISICA_P IN VARCHAR2,
        DSCONTATO_P IN VARCHAR2,
        IETIPO_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    PERSON_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( PERSON_NOT_FOUND, -20001 );
    MOBILE_ALREADY_EXISTS EXCEPTION;
    PRAGMA EXCEPTION_INIT( MOBILE_ALREADY_EXISTS, -20002 );
    MAIL_ALREADY_EXISTS EXCEPTION;
    PRAGMA EXCEPTION_INIT( MAIL_ALREADY_EXISTS, -20003 );
    T_ID VARCHAR(45);
    T_QTD_PF NUMBER := 0;
    T_QTD_MOB NUMBER := 0;
    T_QTD_MAIL NUMBER := 0;
    T_MAIL_ID NUMBER := 0;

    BEGIN

        SELECT
        COUNT(CD_PESSOA_FISICA) INTO T_QTD_PF
        FROM
        PESSOA_FISICA
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P;

        IF T_QTD_PF=0 THEN

            RAISE PERSON_NOT_FOUND;

        END IF;

        IF IETIPO_P = 'mob' THEN

            SELECT
            COUNT(CD_PESSOA_FISICA) INTO T_QTD_MOB
            FROM
            PESSOA_FISICA
            WHERE
            CD_PESSOA_FISICA = CDPESSOAFISICA_P
            AND NR_TELEFONE_CELULAR IS NOT NULL;

            IF T_QTD_MOB>0 THEN

                RAISE MOBILE_ALREADY_EXISTS;

            END IF;

            UPDATE
            PESSOA_FISICA
            SET
            NR_DDI_CELULAR = NVL(SUBSTR(DSCONTATO_P,-13,2),NR_DDI_CELULAR),
            NR_DDD_CELULAR = NVL(SUBSTR(DSCONTATO_P,-11,2),NR_DDD_CELULAR),
            NR_TELEFONE_CELULAR = NVL(SUBSTR(DSCONTATO_P,-9),NR_TELEFONE_CELULAR),
            DT_ATUALIZACAO = SYSDATE,
            NM_USUARIO = 'portal'
            WHERE
            CD_PESSOA_FISICA = CDPESSOAFISICA_P;

            P_ID := CDPESSOAFISICA_P||'.0.mob';

        END IF;

        IF IETIPO_P = 'mail' THEN

            SELECT
            COUNT(DS_EMAIL)
            INTO
            T_QTD_MAIL
            FROM
            COMPL_PESSOA_FISICA
            WHERE
            CD_PESSOA_FISICA = CDPESSOAFISICA_P;

            SELECT
            NVL(MIN(NR_SEQUENCIA),0)
            INTO
            T_MAIL_ID
            FROM
            COMPL_PESSOA_FISICA
            WHERE
            CD_PESSOA_FISICA = CDPESSOAFISICA_P;

            IF T_QTD_MAIL>0 THEN

                RAISE MAIL_ALREADY_EXISTS;

            END IF;

            IF T_MAIL_ID=0 THEN

                INSERT INTO
                COMPL_PESSOA_FISICA
                (CD_PESSOA_FISICA,NR_SEQUENCIA,IE_TIPO_COMPLEMENTO,DS_EMAIL,DT_ATUALIZACAO,NM_USUARIO)
                VALUES
                (CDPESSOAFISICA_P,1,1,DSCONTATO_P,SYSDATE,'portal');

                P_ID := CDPESSOAFISICA_P||'.1.mail';

            ELSE

                UPDATE
                COMPL_PESSOA_FISICA
                SET
                DS_EMAIL = DSCONTATO_P,
                NM_USUARIO = 'portal'
                WHERE
                CD_PESSOA_FISICA = CDPESSOAFISICA_P
                AND NR_SEQUENCIA = T_MAIL_ID;

                P_ID := CDPESSOAFISICA_P||'.'||T_MAIL_ID||'.mail';

            END IF;

        END IF;

        IF IETIPO_P IS NULL OR IETIPO_P NOT IN ('mob','mail') THEN
            P_ERR := 'NO_DATA_FOUND';
        END IF;

    EXCEPTION
        WHEN PERSON_NOT_FOUND THEN
            P_ERR := 'PERSON_NOT_FOUND';
        WHEN MOBILE_ALREADY_EXISTS THEN
            P_ERR := 'MOBILE_ALREADY_EXISTS';
        WHEN MAIL_ALREADY_EXISTS THEN
            P_ERR := 'MAIL_ALREADY_EXISTS';
        WHEN OTHERS THEN
            RAISE;

    END CREATE_PATIENT_CONTACT;


    PROCEDURE GET_ALLERGIES(
        PATIENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                PA.NR_SEQUENCIA,
                PA.CD_PESSOA_FISICA,
                PA.NR_ATENDIMENTO,
                NVL(TA.DS_TIPO_ALERGIA,LBL_NOT_PROVIDED)||NVL(
                (CASE
                WHEN M.DS_REDUZIDA IS NOT NULL THEN ': '||M.DS_REDUZIDA
                ELSE ''
                END),
                (CASE
                WHEN PA.DS_MEDIC_NAO_CAD IS NOT NULL THEN ': '||PA.DS_MEDIC_NAO_CAD
                ELSE ''
                END)) DS_TIPO_ALERGIA,
                NVL(MRA.DS_REACAO,LBL_NOT_PROVIDED)||
                (CASE
                WHEN PA.DS_OBSERVACAO IS NOT NULL THEN ': '||PA.DS_OBSERVACAO
                ELSE ''
                END) DS_REACAO,
                PA.DT_LIBERACAO
            FROM
                PACIENTE_ALERGIA PA
                LEFT JOIN TIPO_ALERGIA TA
                    ON (TA.NR_SEQUENCIA = PA.NR_SEQ_TIPO)
                LEFT JOIN MED_REACAO_ALERGICA MRA
                    ON (MRA.NR_SEQUENCIA = PA.NR_SEQ_REACAO)
                LEFT JOIN MATERIAL M
                    ON (M.CD_MATERIAL = PA.CD_MATERIAL)
            WHERE
                PA.CD_PESSOA_FISICA = PATIENT_ID_P
                AND PA.IE_CONFIRMACAO = 'C'
                AND PA.IE_NEGA_ALERGIAS = 'N'
                AND PA.DT_LIBERACAO IS NOT NULL;

    END GET_ALLERGIES;

    PROCEDURE GET_VITAL_SIGNS(
        PATIENT_ID_P IN VARCHAR2,
        TYPE_P IN VARCHAR2,
        LAST_P IN VARCHAR2,
        START_DATE_P IN VARCHAR2,
        END_DATE_P IN VARCHAR2,
        OFFSET_P IN NUMBER,
        LIMIT_P IN NUMBER,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                *
            FROM
            (
                SELECT
                    SS.*,
                    ROWNUM RNUM
                FROM
                (
                    SELECT
                        S.NR_SEQUENCIA,
                        S.CD_PACIENTE,
                        S.NR_ATENDIMENTO,
                        S.VS_VALUE,
                        S.VS_UNIT,
                        S.VS_TYPE,
                        S.DT_SINAL_VITAL
                    FROM
                    (
                        SELECT
                            ASV.*,
                            ASV.QT_PESO VS_VALUE,
                            NVL(ASV.IE_UNID_MED_PESO,LBL_VS_BODYWEIGHT_UNIT) VS_UNIT,
                            LBL_VS_BODYWEIGHT VS_TYPE
                        FROM
                            ATENDIMENTO_SINAL_VITAL ASV
                        WHERE
                            (TYPE_P = LBL_VS_BODYWEIGHT OR TYPE_P IS NULL)
                        UNION ALL
                        SELECT
                            ASV.*,
                            ASV.QT_ALTURA_CM VS_VALUE,
                            NVL(ASV.IE_UNID_MED_ALTURA,LBL_VS_BODYHEIGHT_UNIT) VS_UNIT,
                            LBL_VS_BODYHEIGHT VS_TYPE
                        FROM
                            ATENDIMENTO_SINAL_VITAL ASV
                        WHERE
                            (TYPE_P = LBL_VS_BODYHEIGHT OR TYPE_P IS NULL)
                    ) S
                    WHERE
                        S.DT_LIBERACAO IS NOT NULL
                        AND S.VS_VALUE IS NOT NULL
                        AND S.CD_PACIENTE = PATIENT_ID_P
                        AND S.DT_SINAL_VITAL BETWEEN CAST(TO_TIMESTAMP(START_DATE_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) AND CAST(TO_TIMESTAMP(END_DATE_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE)
                    ORDER BY
                        S.DT_SINAL_VITAL DESC
                ) SS
            )
            WHERE
                (LAST_P = 'N' AND (RNUM > OFFSET_P AND RNUM <= OFFSET_P+LIMIT_P))
                OR (LAST_P = 'S' AND RNUM = 1)
            ORDER BY
                RNUM ASC;

    END GET_VITAL_SIGNS;

    PROCEDURE GET_MEDICATIONS(
        PATIENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                PMU.NR_SEQUENCIA,
                PMU.CD_MATERIAL,
                NVL(M.DS_REDUZIDA,PMU.DS_MEDICAMENTO) DS_MEDICAMENTO,
                PMU.QT_DOSE,
                NVL(PMU.CD_UNID_MED,M.CD_UNIDADE_MEDIDA_CONSUMO) CD_UNIDADE_MEDIDA,
                PMU.CD_PESSOA_FISICA,
                PMU.NR_ATENDIMENTO,
                PMU.DT_REGISTRO
            FROM
                PACIENTE_MEDIC_USO PMU
                LEFT JOIN MATERIAL M
                    ON (M.CD_MATERIAL = PMU.CD_MATERIAL)
            WHERE
                PMU.IE_NEGA_MEDICAMENTOS = 'N'
                AND PMU.CD_PESSOA_FISICA = PATIENT_ID_P
                AND PMU.DT_LIBERACAO IS NOT NULL
            ORDER BY
                PMU.DT_REGISTRO DESC;

    END GET_MEDICATIONS;

    PROCEDURE GET_TUMORAL_DIAGNOSIS(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                CLR.NR_SEQUENCIA,
                CLR.CD_DOENCA_CID,
                CD.DS_DOENCA_CID,
                CLR.CD_TOPOGRAFIA,
                CT.DS_TOPOGRAFIA,
                CLR.CD_MORFOLOGIA,
                NVL(CMDA.DS_MORFOLOGIA,CM.DS_MORFOLOGIA) DS_MORFOLOGIA,
                CLR.CD_TUMOR_PRIMARIO,
                CLR.CD_LINFONODO_REGIONAL,
                CLR.CD_METASTASE_DISTANCIA,
                CLR.CD_ESTADIAMENTO,
                CLR.CD_TUMOR_PRIM_PAT,
                CLR.CD_LINFONODO_REG_PAT,
                CLR.CD_METASTASE_DIST_PAT,
                CLR.CD_ESTADIO_OUTRO,
                CLR.DT_AVALIACAO
            FROM
                CAN_LOCO_REGIONAL CLR
                LEFT JOIN CID_DOENCA CD
                    ON (CD.CD_DOENCA_CID = CLR.CD_DOENCA_CID)
                LEFT JOIN CIDO_TOPOGRAFIA CT
                    ON (CT.CD_TOPOGRAFIA = CLR.CD_TOPOGRAFIA)
                LEFT JOIN CIDO_MORFOLOGIA CM
                    ON (CM.CD_MORFOLOGIA = CLR.CD_MORFOLOGIA)
                LEFT JOIN CIDO_MORFOLOGIA_DESC_ADIC CMDA
                    ON (CMDA.NR_SEQUENCIA = CLR.NR_SEQ_MORF_DESC_ADIC AND CMDA.CD_MORFOLOGIA = CLR.CD_MORFOLOGIA)
            WHERE
                CLR.CD_PESSOA_FISICA = PATIENT_ID_P
                AND ( CLR.CD_MEDICO = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL )
                AND CLR.DT_LIBERACAO IS NOT NULL
                AND CLR.IE_SITUACAO = 'A'
            ORDER BY
                CLR.NR_SEQUENCIA DESC;

    END GET_TUMORAL_DIAGNOSIS;

    PROCEDURE GET_TREATMENT_PLAN(
        PATIENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                PS.NR_SEQ_PACIENTE,
                PM.CD_PROTOCOLO,
                PS.CD_PESSOA_FISICA,
                PS.CD_MEDICO_RESP,
                PM.NM_MEDICACAO,
                P.NM_PROTOCOLO,
                (
                    SELECT
                        MIN(PA.DT_PREVISTA)
                    FROM
                        PACIENTE_ATENDIMENTO PA
                    WHERE
                        PA.NR_SEQ_PACIENTE = PS.NR_SEQ_PACIENTE
                ) DT_PRIMEIRA,
                (
                    SELECT
                        MAX(PA.DT_REAL)
                    FROM
                        PACIENTE_ATENDIMENTO PA
                    WHERE
                        PA.NR_SEQ_PACIENTE = PS.NR_SEQ_PACIENTE
                        AND PA.DT_INICIO_ADM IS NOT NULL
                ) DT_ULTIMA
            FROM
                PACIENTE_SETOR PS
                INNER JOIN PROTOCOLO_MEDICACAO PM
                    ON (PM.CD_PROTOCOLO = PS.CD_PROTOCOLO AND PM.NR_SEQUENCIA = PS.NR_SEQ_MEDICACAO)
                INNER JOIN PROTOCOLO P
                    ON (P.CD_PROTOCOLO = PM.CD_PROTOCOLO)
            WHERE
                PS.CD_PESSOA_FISICA = PATIENT_ID_P
            ORDER BY
                PS.NR_SEQ_PACIENTE DESC;

    END GET_TREATMENT_PLAN;

    PROCEDURE GET_TREATMENT_SESSIONS(
        PATIENT_ID_P IN VARCHAR2,
        PLAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                PA.NR_SEQ_ATENDIMENTO,
                PA.DS_DIA_CICLO,
                PA.NR_CICLO,
                PA.NR_PRESCRICAO,
                PA.DT_PREVISTA,
                PA.DT_REAL,
                PA.DT_FALTA, /*absent-patient*/
                PA.IE_EXIGE_LIBERACAO, /*release-required*/
                NVL(OBTER_SE_PRESCRICAO_LIBERADA(PA.NR_PRESCRICAO,'E'),'N') IE_PRESCRICAO_LIBERADA, /* pending-prescription,released-prescription */
                NVL(OBTER_PRESCRICAO_SUSPENCA(PA.NR_PRESCRICAO),'N') IE_PRESCRICAO_SUSPENSA, /* suspended-prescription */
                OBTER_PENDENCIA_ASSINATURA(WHEB_USUARIO_PCK.GET_NM_USUARIO,PA.NR_SEQ_ATENDIMENTO,'ONC') IE_PENDENTE_ASSINATURA, /* pending-prescription-signature */
                OBTER_SE_DIA_CICLO_AUTORIZADO(PA.NR_SEQ_ATENDIMENTO,PA.NR_CICLO) IE_CICLO_AUTORIZADO, /* authorized-cycle */
                OBTER_SE_MEDIC_QUIMIO_ADM(PA.NR_SEQ_ATENDIMENTO) IE_MEDIC_ADM, /* attended-day */
                OBTER_STATUS_PACIENTE_QT(PA.NR_SEQ_ATENDIMENTO,PA.DT_INICIO_ADM,PA.DT_FIM_ADM,PA.NR_SEQ_LOCAL,PA.IE_EXIGE_LIBERACAO,PA.DT_CHEGADA,'C') IE_STATUS_PAC, /* 83 - Alta na quimio */
                PA.DT_SUSPENSAO DT_SUSPENSAO_DIA /* suspended-day */
            FROM
                PACIENTE_ATENDIMENTO PA
                INNER JOIN PACIENTE_SETOR PS
                    ON (PS.NR_SEQ_PACIENTE = PA.NR_SEQ_PACIENTE)
            WHERE
                PS.CD_PESSOA_FISICA = PATIENT_ID_P
                AND PA.NR_SEQ_PACIENTE = PLAN_ID_P
            ORDER BY
                PA.NR_CICLO ASC,
                PA.DT_PREVISTA DESC;

    END GET_TREATMENT_SESSIONS;

    PROCEDURE GET_PHYSICIAN_APPOINTMENTS(
        PARAMS_P IN VARCHAR2,
        START_DATE_P IN VARCHAR2,
        END_DATE_P IN VARCHAR2,
        OFFSET_P IN NUMBER,
        LIMIT_P IN NUMBER,
        TYPE_SCHEDULE_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_PHYSICIANS_ARR OC_PARAM_ARR;
    T_CLASSIF_ARR OC_PARAM_ARR;
    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHS_SECTORS_ARR OC_PARAM_ARR;

    BEGIN

    T_PHYSICIANS_ARR        := LPARAM('PHYSICIANS',PARAMS_P);    
    T_BRANCHS_ARR           := LPARAM('BRANCHS',PARAMS_P);
    T_BRANCHS_SECTORS_ARR   := LPARAM('BRANCHSSECTORS',PARAMS_P);

    IF UPPER(TYPE_SCHEDULE_P) = 'APPOINTMENT' THEN

        OPEN P_RECORDSET FOR
            SELECT
                ROWNUM RNUM,
                AC.NR_SEQUENCIA,
                A.CD_AGENDA,
                A.DS_AGENDA,
                M.CD_PESSOA_FISICA CD_MEDICO,
                M.NM_PESSOA_FISICA NM_MEDICO,
                PJ.CD_CGC,
                CASE
                WHEN (A.CD_SETOR_AGENDA IS NULL OR E.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHS_SECTORS_ARR))) THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
                ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
                END CD_ESTABELECIMENTO,
                PJ.NM_FANTASIA NM_FANTASIA_ESTAB,
                PJ.DS_ENDERECO,
                PJ.DS_BAIRRO,
                PJ.DS_COMPLEMENTO,
                PJ.NR_ENDERECO,
                PJ.DS_MUNICIPIO,
                PJ.SG_ESTADO,
                PJ.CD_CEP,
                EM.CD_ESPECIALIDADE,
                EM.DS_ESPECIALIDADE,
                P.CD_PESSOA_FISICA CD_PACIENTE,
                P.NM_PESSOA_FISICA NM_PACIENTE,
                C.CD_CONVENIO,
                C.DS_CONVENIO,
                CC.CD_CATEGORIA,
                CC.DS_CATEGORIA,
                CP.CD_PLANO,
                CP.DS_PLANO,
                TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                TO_CHAR(AC.DT_AGENDAMENTO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                AC.IE_STATUS_AGENDA,
                OBTER_VALOR_DOMINIO(83,AC.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                AP.IE_TIPO_ATENDIMENTO,
                OBTER_VALOR_DOMINIO(12,AP.IE_TIPO_ATENDIMENTO) NM_TIPO_ATENDIMENTO,
                A.IE_TIPO_AGENDA_CONSULTA IE_TIPO_AGENDA,
                DECODE(A.IE_TIPO_AGENDA_CONSULTA,
                        'C','Consulta',
                        'A','Consulta',
                        'S','Servico',NULL) NM_TIPO_AGENDA,
                AC.IE_CLASSIF_AGENDA,
                ACL.DS_CLASSIFICACAO NM_CLASSIF_AGENDA,
                A.DS_COMPLEMENTO DS_OBSERVACAO,
                AMC.CD_MOTIVO CD_MOTIVO_CANCEL,
                AMC.DS_MOTIVO NM_MOTIVO_CANCEL,
                AC.NR_MINUTO_DURACAO
            FROM
                AGENDA A
                INNER JOIN AGENDA_CONSULTA AC
                    ON (A.CD_AGENDA = AC.CD_AGENDA)
                INNER JOIN PESSOA_FISICA P
                    ON (P.CD_PESSOA_FISICA = AC.CD_PESSOA_FISICA)
                INNER JOIN PESSOA_FISICA M
                    ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                INNER JOIN ESTABELECIMENTO E
                    ON (E.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO)
                INNER JOIN PESSOA_JURIDICA PJ
                    ON (PJ.CD_CGC = E.CD_CGC)
                LEFT JOIN ESPECIALIDADE_MEDICA EM
                    ON (EM.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE)
                LEFT JOIN AGENDA_CLASSIF ACL
                    ON (ACL.CD_CLASSIFICACAO = AC.IE_CLASSIF_AGENDA AND ACL.IE_SITUACAO = 'A')
                LEFT JOIN AGENDA_MOTIVO_CANCELAMENTO AMC
                    ON (AMC.CD_MOTIVO = AC.CD_MOTIVO_CANCELAMENTO
                            AND (AMC.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO OR AMC.CD_ESTABELECIMENTO IS NULL)
                            AND AMC.IE_AGENDA = 'C' AND AMC.IE_SITUACAO = 'A')
                LEFT JOIN ATENDIMENTO_PACIENTE AP
                    ON (AP.NR_ATENDIMENTO = AC.NR_ATENDIMENTO)
                LEFT JOIN CONVENIO C
                    ON (C.CD_CONVENIO = AC.CD_CONVENIO)
                LEFT JOIN CATEGORIA_CONVENIO CC
                    ON (CC.CD_CATEGORIA = AC.CD_CATEGORIA AND CC.CD_CONVENIO = C.CD_CONVENIO)
                LEFT JOIN CONVENIO_PLANO CP
                    ON (CP.CD_PLANO = AC.CD_PLANO AND CP.CD_CONVENIO = C.CD_CONVENIO)
                INNER JOIN TASY.ESTABELECIMENTO ES
                    ON (ES.CD_ESTABELECIMENTO = CASE
                                                    WHEN (A.CD_SETOR_AGENDA IS NULL OR E.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHS_SECTORS_ARR))) THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                                                    ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                                                END)
                INNER JOIN TASY.PESSOA_JURIDICA PJ
                    ON (PJ.CD_CGC = ES.CD_CGC)
            WHERE
                A.CD_PESSOA_FISICA IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                AND AC.DT_AGENDA BETWEEN CAST(TO_TIMESTAMP(START_DATE_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) AND CAST(TO_TIMESTAMP(END_DATE_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE)                
                AND (
                    EM.CD_ESPECIALIDADE IS NOT NULL
                    OR
                    ( SELECT COUNT(ME.CD_ESPECIALIDADE) FROM MEDICO_ESPECIALIDADE ME WHERE ME.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA ) > 0                    
                )
                AND AC.IE_STATUS_AGENDA NOT IN ('L','C')
            ORDER BY
                AC.DT_AGENDA DESC;

    ELSE

        OPEN P_RECORDSET FOR
            SELECT
                ROWNUM RNUM,
                AQ.NR_SEQUENCIA,
                '' CD_AGENDA,
                '' DS_AGENDA,
                M.CD_PESSOA_FISICA CD_MEDICO,
                M.NM_PESSOA_FISICA NM_MEDICO,
                PJ.CD_CGC,
                TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000' CD_ESTABELECIMENTO,
                E.NM_FANTASIA_ESTAB,
                PJ.DS_ENDERECO,
                PJ.DS_BAIRRO,
                PJ.DS_COMPLEMENTO,
                PJ.NR_ENDERECO,
                PJ.DS_MUNICIPIO,
                PJ.SG_ESTADO,
                PJ.CD_CEP,
                EM.CD_ESPECIALIDADE,
                EM.DS_ESPECIALIDADE,
                P.CD_PESSOA_FISICA CD_PACIENTE,
                P.NM_PESSOA_FISICA NM_PACIENTE,
                C.CD_CONVENIO,
                C.DS_CONVENIO,
                CC.CD_CATEGORIA,
                CC.DS_CATEGORIA,
                CP.CD_PLANO,
                CP.DS_PLANO,
                TO_CHAR(AQ.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                TO_CHAR(AQ.DT_ATUALIZACAO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                AQ.IE_STATUS_AGENDA,
                TASY.OBTER_VALOR_DOMINIO(83,AQ.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                AP.IE_TIPO_ATENDIMENTO,
                TASY.OBTER_VALOR_DOMINIO(12,AP.IE_TIPO_ATENDIMENTO) NM_TIPO_ATENDIMENTO,
                AQ.IE_TIPO_PEND_AGENDA IE_TIPO_AGENDA,
                'Tratamento' NM_TIPO_AGENDA,
                'Q5' IE_CLASSIF_AGENDA,
                'Quimioterapia' NM_CLASSIF_AGENDA,
                AQM.DS_OBSERVACAO,
                QMC.NR_SEQUENCIA CD_MOTIVO_CANCEL,
                QMC.DS_MOTIVO NM_MOTIVO_CANCEL,
                AQ.NR_MINUTO_DURACAO
            FROM
                TASY.AGENDA_QUIMIO AQ
                LEFT JOIN TASY.AGENDA_QUIMIO_MARCACAO AQM
                    ON (AQM.NR_SEQ_ATENDIMENTO = AQ.NR_SEQ_ATENDIMENTO)
                INNER JOIN TASY.PESSOA_FISICA P
                    ON (P.CD_PESSOA_FISICA = AQ.CD_PESSOA_FISICA)
                INNER JOIN TASY.ESTABELECIMENTO E
                    ON (E.CD_ESTABELECIMENTO = AQ.CD_ESTABELECIMENTO)
                INNER JOIN TASY.PESSOA_JURIDICA PJ
                    ON (PJ.CD_CGC = E.CD_CGC)
                INNER JOIN TASY.PESSOA_FISICA M
                    ON (M.CD_PESSOA_FISICA = AQ.CD_MEDICO_RESP)
                LEFT JOIN TASY.MEDICO_ESPECIALIDADE ME
                    ON (ME.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                LEFT JOIN TASY.ESPECIALIDADE_MEDICA EM
                    ON (EM.CD_ESPECIALIDADE = ME.CD_ESPECIALIDADE)
                LEFT JOIN TASY.QT_MOTIVO_CANCELAMENTO QMC
                    ON (QMC.NR_SEQUENCIA = AQ.NR_SEQ_MOT_CANCELAMENTO AND QMC.IE_SITUACAO = 'A')
                LEFT JOIN TASY.PACIENTE_ATENDIMENTO PA
                    ON (PA.NR_SEQ_ATENDIMENTO = AQ.NR_SEQ_ATENDIMENTO)
                LEFT JOIN TASY.ATENDIMENTO_PACIENTE AP
                    ON (AP.NR_ATENDIMENTO = AQ.NR_ATENDIMENTO)
                LEFT JOIN TASY.PESSOA_TITULAR_CONVENIO PTC
                    ON (PTC.CD_PESSOA_FISICA = P.CD_PESSOA_FISICA)
                LEFT JOIN TASY.CONVENIO C
                    ON (C.CD_CONVENIO = PTC.CD_CONVENIO)
                LEFT JOIN TASY.CATEGORIA_CONVENIO CC
                    ON (CC.CD_CATEGORIA = PTC.CD_CATEGORIA AND CC.CD_CONVENIO = PTC.CD_CONVENIO)
                LEFT JOIN TASY.CONVENIO_PLANO CP
                    ON (CP.CD_CONVENIO = C.CD_CONVENIO AND CP.CD_PLANO = PTC.CD_PLANO_CONVENIO)
            WHERE
                AQ.CD_PESSOA_FISICA IN (SELECT DISTINCT CD_PESSOA_FISICA FROM TASY.ATENDIMENTO_PACIENTE WHERE CD_MEDICO_REFERIDO IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR)))
                AND AQ.DT_AGENDA BETWEEN CAST(TO_TIMESTAMP(START_DATE_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) AND CAST(TO_TIMESTAMP(END_DATE_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE)
                AND AQ.IE_STATUS_AGENDA NOT IN ('L','C')
                AND AQ.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
            ORDER BY
                AQ.DT_AGENDA DESC;

    END IF;

    END GET_PHYSICIAN_APPOINTMENTS;

    PROCEDURE GET_PHYSICIAN_PATIENTS(
        PARAMS_P IN VARCHAR2,
        PATIENT_P IN VARCHAR2,
        OFFSET_P IN NUMBER,
        LIMIT_P IN NUMBER,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_PHYSICIANS_ARR OC_PARAM_ARR;

    BEGIN

        T_PHYSICIANS_ARR := LPARAM('PHYSICIANS',PARAMS_P);

        OPEN P_RECORDSET FOR
            SELECT
                SS.*
            FROM
            (
                SELECT
                    S.*,
                    ROWNUM RNUM
                FROM
                (
                    SELECT
                        P.CD_PESSOA_FISICA,
                        P.NM_PESSOA_FISICA,
                        TO_CHAR(P.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                        MAX(AP.DT_ENTRADA) DT_ULT_CONSULTA
                    FROM
                        AGENDA A
                        INNER JOIN AGENDA_CONSULTA AC
                            ON (A.CD_AGENDA = AC.CD_AGENDA)
                        INNER JOIN PESSOA_FISICA P
                            ON (P.CD_PESSOA_FISICA = AC.CD_PESSOA_FISICA)
                        INNER JOIN ATENDIMENTO_PACIENTE AP
                            ON (AP.NR_ATENDIMENTO = AC.NR_ATENDIMENTO)
                    WHERE
                        ( A.CD_PESSOA_FISICA IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                          OR AP.CD_MEDICO_RESP IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                          OR AP.CD_MEDICO_REFERIDO IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR)) )
                        AND (UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(P.NM_PESSOA_FISICA, 'NLS_SORT=BINARY_AI')) LIKE UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(REPLACE(PATIENT_P,'~','%'), 'NLS_SORT=BINARY_AI')) OR PATIENT_P IS NULL)
                        AND AC.IE_STATUS_AGENDA NOT IN ('L','N')
                        AND AP.DT_ENTRADA IS NOT NULL
                    GROUP BY
                        P.CD_PESSOA_FISICA,
                        P.NM_PESSOA_FISICA,
                        P.DT_NASCIMENTO
                    ORDER BY
                        DT_ULT_CONSULTA DESC
                ) S
            ) SS
            WHERE
                RNUM > OFFSET_P AND RNUM <= OFFSET_P+LIMIT_P;

    END GET_PHYSICIAN_PATIENTS;

    PROCEDURE GET_PATIENT_EVOLUTION(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ERE1.DS_RESULTADO DS_RESULTADO_EVOL,
                ERE2.DS_RESULTADO DS_RESULTADO_COMP,
                ERE3.DS_RESULTADO DS_RESULTADO_CONDUCT,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ER.NR_SEQUENCIA = ERT.NR_SEQ_REG
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO IN (293865,322489) /*Evolucao clinica*/ )
                INNER JOIN EHR_REG_ELEMENTO ERE2
                    ON ( ERE2.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE2.NR_SEQ_TEMP_CONTEUDO IN (316389,322485) /*Observacao*/ )
                INNER JOIN EHR_REG_ELEMENTO ERE3
                    ON ( ERE3.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE3.NR_SEQ_TEMP_CONTEUDO IN (318314,322486) /* 7 - Conduta */ )
            WHERE
                ERT.NR_SEQ_TEMPLATE IN (100780,101068) /*Evolucao*/
                AND ER.CD_PACIENTE = PATIENT_ID_P
                AND ( ER.CD_PROFISSIONAL = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL )
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_PATIENT_EVOLUTION;

    PROCEDURE GET_PATIENT_DIAG_IMP(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ERE1.DS_RESULTADO DS_RESULTADO,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ER.NR_SEQUENCIA = ERT.NR_SEQ_REG
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO = 293838 /*Impressao diagnostica*/ )
            WHERE
                ERT.NR_SEQ_TEMPLATE = 100778 /*Impressao diagnostica*/
                AND ER.CD_PACIENTE = PATIENT_ID_P
                AND ( ER.CD_PROFISSIONAL = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL )
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_PATIENT_DIAG_IMP;

    PROCEDURE GET_PATIENT_CONDUCT(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ERE1.DS_RESULTADO DS_RESULTADO,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ER.NR_SEQUENCIA = ERT.NR_SEQ_REG
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO IN (293839,322686) /*Conduta*/ )
            WHERE
                ERT.NR_SEQ_TEMPLATE IN (100779,101077) /*Conduta*/
                AND ER.CD_PACIENTE = PATIENT_ID_P
                AND ( ER.CD_PROFISSIONAL = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL )
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_PATIENT_CONDUCT;

    PROCEDURE SET_PHYSICIAN_SPECIALTY(
        IDENTIFIER_P IN VARCHAR2,
        CDESPECIALIDADE_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_QTD NUMBER := 0;

    BEGIN

        SELECT CD_PESSOA_FISICA INTO T_ID FROM PESSOA_FISICA WHERE CD_PESSOA_FISICA = IDENTIFIER_P;

        SELECT
        COUNT(*) INTO T_QTD
        FROM
        MEDICO_ESPECIALIDADE
        WHERE
        CD_PESSOA_FISICA = T_ID
        AND CD_ESPECIALIDADE = CDESPECIALIDADE_P;

        IF T_QTD=0 THEN

            INSERT INTO
            MEDICO_ESPECIALIDADE
            (CD_PESSOA_FISICA,CD_ESPECIALIDADE,DT_ATUALIZACAO,NM_USUARIO,NR_SEQ_PRIORIDADE)
            VALUES
            (T_ID,CDESPECIALIDADE_P,SYSDATE,APP_USER,100);

        END IF;

    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            P_ERR := 'NO_DATA_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END SET_PHYSICIAN_SPECIALTY;

    PROCEDURE GET_PATIENT_ANAMNESIS(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ER.NR_ATENDIMENTO,
                ERE1.DS_RESULTADO DS_RESULT_QUEIXA,
                ERE2.DS_RESULTADO DS_RESULT_HPMA,
                ERE3.DS_RESULTADO DS_RESULT_ISDA,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ( ER.NR_SEQUENCIA = ERT.NR_SEQ_REG )
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO IN (293833,322577) /*Queixa principal*/ )
                LEFT JOIN EHR_REG_ELEMENTO ERE2
                    ON ( ERE2.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE2.NR_SEQ_TEMP_CONTEUDO IN (293834,322578) /*HPMA*/ )
                LEFT JOIN EHR_REG_ELEMENTO ERE3
                    ON ( ERE3.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE3.NR_SEQ_TEMP_CONTEUDO IN (293836,322579) /*ISDA*/ )
            WHERE
                ERT.NR_SEQ_TEMPLATE IN (100777,101071) /*Evolucao ANAMNESIS*/
                AND ER.CD_PACIENTE = PATIENT_ID_P
                AND ( ER.CD_PROFISSIONAL = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL )
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_PATIENT_ANAMNESIS;

    PROCEDURE GET_STORAGE_PARAMS(NM_STORAGE_P IN VARCHAR2,CD_UUID_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

      OPEN P_RECORDSET FOR
        SELECT
            FSP.*
        FROM
            FILE_STORAGE FS
            INNER JOIN FILE_STORAGE_PATH FSP
                ON (FSP.NM_STORAGE = FS.NM_STORAGE)
        WHERE
            (UPPER(FS.NM_STORAGE) = UPPER(NM_STORAGE_P) OR (FS.NM_STORAGE = 'DEFAULT' AND NM_STORAGE_P IS NULL))
            AND (FSP.CD_UUID = CD_UUID_P OR CD_UUID_P IS NULL);

    END GET_STORAGE_PARAMS;

    PROCEDURE CREATE_DIGITALIZED_DOCUMENT(
        CDPESSOAFISICA_P IN VARCHAR2,
        NRSEQTIPOARQUIVO_P IN VARCHAR2,
        DSTITULO_P IN VARCHAR2,
        DSARQUIVO_P IN VARCHAR2,
        CDPROFISSIONAL_P IN VARCHAR2,
        DTEXPEDICAO_P IN VARCHAR2,
        EXTERNALID_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45);
    T_PF_ESTAB NUMBER;
    PERSON_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( PERSON_NOT_FOUND, -20001 );

    BEGIN

        SELECT
        MAX(CD_ESTABELECIMENTO) INTO T_PF_ESTAB
        FROM
        PESSOA_FISICA
        WHERE
        CD_PESSOA_FISICA = CDPESSOAFISICA_P;

        IF T_PF_ESTAB IS NULL THEN

            RAISE PERSON_NOT_FOUND;

        END IF;

        SELECT GED_ATENDIMENTO_SEQ.NEXTVAL INTO T_ID FROM DUAL;

        INSERT INTO
        GED_ATENDIMENTO
        (NR_SEQUENCIA,
        NR_SEQ_TIPO_ARQUIVO,
        CD_ESTABELECIMENTO,
        CD_PESSOA_FISICA,
        CD_PROFISSIONAL,
        DS_TITULO,
        DS_ARQUIVO,
        DT_EXPEDICAO,
        DT_REGISTRO,
        DT_LIBERACAO,
        DT_ATUALIZACAO,
        NM_USUARIO,
        DS_OBSERVACAO)
        VALUES
        (T_ID,
        NRSEQTIPOARQUIVO_P,
        T_PF_ESTAB,
        CDPESSOAFISICA_P,
        CDPROFISSIONAL_P,
        DSTITULO_P,
        DSARQUIVO_P,
        TO_DATE(DTEXPEDICAO_P,'YYYY-MM-DD HH24:MI:SS'),
        SYSDATE,
        SYSDATE,
        SYSDATE,
        'marcos.costa',
        EXTERNALID_P);

        P_ID := T_ID;

    EXCEPTION
        WHEN PERSON_NOT_FOUND THEN
            P_ERR := 'PERSON_NOT_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END CREATE_DIGITALIZED_DOCUMENT;

    PROCEDURE DELETE_DIGITALIZED_DOCUMENT(
        CDPESSOAFISICA_P IN VARCHAR2,
        EXTERNALID_P IN VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_ID VARCHAR(45) := NULL;
    DOCUMENT_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( DOCUMENT_NOT_FOUND, -20001 );

    BEGIN

        SELECT
        MAX(NR_SEQUENCIA) INTO T_ID
        FROM
        GED_ATENDIMENTO
        WHERE
        DS_OBSERVACAO = EXTERNALID_P
        AND (CD_PESSOA_FISICA = CDPESSOAFISICA_P OR CDPESSOAFISICA_P IS NULL);

        IF T_ID IS NULL THEN

            RAISE DOCUMENT_NOT_FOUND;

        END IF;

        UPDATE
        GED_ATENDIMENTO
        SET
        IE_SITUACAO = 'I'
        WHERE
        NR_SEQUENCIA = T_ID;

    EXCEPTION
        WHEN DOCUMENT_NOT_FOUND THEN
            P_ERR := 'DOCUMENT_NOT_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END DELETE_DIGITALIZED_DOCUMENT;

    PROCEDURE GET_LAB_EXAMS(
        PATIENT_P IN VARCHAR2,
        ENCOUNTER_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        DATE_START_P IN VARCHAR2,
        DATE_END_P IN VARCHAR2,
        FILTER_P IN VARCHAR2,
        PAGE_P IN NUMBER,
        LIMIT_P IN NUMBER,
        SORT_COL_P IN VARCHAR2,
        SORT_ORDER_P IN VARCHAR2,
        CRM_P IN VARCHAR2,
        P_TOTAL OUT NUMBER,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_PHYSICIANS_ARR OC_PARAM_ARR;
    T_ORIGINS_ARR OC_PARAM_ARR;
    T_EXAMBL_ARR OC_PARAM_ARR;

    BEGIN

    P_TOTAL := 0;
    T_PHYSICIANS_ARR := LPARAM('PHYSICIANS',PARAMS_P);
    T_ORIGINS_ARR := LPARAM('ORIGINS',PARAMS_P);
    T_EXAMBL_ARR := LPARAM('EXAMBL',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            S.*
        FROM
        (
            SELECT
                ROWNUM RNUM,
                S1.*
            FROM
            (
                /* [START] CORE RESULTSET */
                SELECT
                    DISTINCT
                    S2.CD_PACIENTE,
                    S2.NM_PACIENTE,
                    S2.NR_CPF_PACIENTE,
                    S2.CD_PROCEDENCIA,
                    S2.DS_PROCEDENCIA,
                    S2.NR_PRESCRICAO,
                    S2.NR_ATENDIMENTO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN 'N'
                    ELSE 'S'
                    END IE_COMBO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.NR_SEQ_PRESCR_PROC
                    ELSE (
                        SELECT
                            MIN(NR_SEQUENCIA)
                        FROM
                            PRESCR_PROCEDIMENTO
                        WHERE
                            NR_PRESCRICAO = S2.NR_PRESCRICAO
                            AND NR_SEQ_EXAME = S2.NR_SEQ_EXAME_COMBO
                    )
                    END NR_SEQ_PRESCRICAO,
                    S2.DT_ENTRADA DT_ATENDIMENTO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.NR_SEQ_PROC_INTERNO
                    ELSE NULL
                    END NR_SEQ_PROC,
                    NVL(S2.NR_SEQ_EXAME_COMBO,S2.NR_SEQ_EXAME) NR_SEQ_EXAME,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.NM_EXAME
                    ELSE S2.NM_EXAME_COMBO
                    END NM_EXAME,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.DT_LIBERACAO
                    ELSE NULL
                    END DT_EXAME,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.DT_LAUDO
                    ELSE NULL
                    END DT_LAUDO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.DT_RESULTADO
                    ELSE NULL
                    END DT_PREV_ENTREGA,
                    S2.CD_MEDICO,
                    S2.NM_MEDICO_RESP,
                    S2.NR_CRM,            
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.IE_STATUS_LAUDO
                    ELSE NULL
                    END IE_STATUS_LAUDO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN OBTER_VALOR_DOMINIO(1214,S2.IE_STATUS_LAUDO)
                    ELSE NULL
                    END NM_STATUS_LAUDO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.DS_ARQUIVO
                    ELSE NULL
                    END DS_ARQUIVO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.NR_SEQ_LAUDO
                    ELSE NULL
                    END NR_SEQ_LAUDO,
                    CASE
                    WHEN S2.NR_SEQ_EXAME_COMBO IS NULL THEN S2.IE_STATUS_EXECUCAO
                    ELSE NULL
                    END IE_STATUS_EXECUCAO
                FROM
                (
                    SELECT
                        PF.CD_PESSOA_FISICA CD_PACIENTE,
                        PF.NM_PESSOA_FISICA NM_PACIENTE,
                        PF.NR_CPF NR_CPF_PACIENTE,
                        PCD.CD_PROCEDENCIA,
                        PCD.DS_PROCEDENCIA,
                        PPR.NR_PRESCRICAO,
                        PM.NR_ATENDIMENTO,
                        EL.NR_SEQ_EXAME,
                        EL.NM_EXAME,
                        ELC.NR_SEQ_EXAME NR_SEQ_EXAME_COMBO,
                        ELC.NM_EXAME NM_EXAME_COMBO,
                        PPR.NR_SEQUENCIA NR_SEQ_PRESCR_PROC,
                        AP.DT_ENTRADA,
                        PPR.NR_SEQ_PROC_INTERNO,
                        PM.DT_LIBERACAO,
                        LP.DT_LAUDO,
                        PPR.DT_RESULTADO,
                        PFM.CD_PESSOA_FISICA CD_MEDICO,
                        PFM.NM_PESSOA_FISICA NM_MEDICO_RESP,
                        (MDC.UF_CRM||MDC.NR_CRM) NR_CRM,
                        LP.IE_STATUS_LAUDO,
                        OBTER_VALOR_DOMINIO(1214,LP.IE_STATUS_LAUDO) NM_STATUS_LAUDO,
                        LP.DS_ARQUIVO,
                        LP.NR_SEQUENCIA NR_SEQ_LAUDO,
                        PPR.IE_STATUS_EXECUCAO
                    FROM
                        PRESCR_MEDICA PM
                        INNER JOIN PRESCR_PROCEDIMENTO PPR
                            ON (PPR.NR_PRESCRICAO = PM.NR_PRESCRICAO)
                        INNER JOIN EXAME_LABORATORIO EL
                            ON (EL.CD_PROCEDIMENTO = PPR.CD_PROCEDIMENTO AND EL.NR_SEQ_PROC_INTERNO = PPR.NR_SEQ_PROC_INTERNO AND PM.CD_ESTABELECIMENTO = EL.CD_ESTABELECIMENTO)                                     
                        INNER JOIN PESSOA_FISICA PF
                            ON (PF.CD_PESSOA_FISICA = PM.CD_PESSOA_FISICA)
                        INNER JOIN PESSOA_FISICA PFM
                            ON (PFM.CD_PESSOA_FISICA = PM.CD_MEDICO)
                        INNER JOIN MEDICO MDC
                            ON (MDC.CD_PESSOA_FISICA = PM.CD_MEDICO)
                        INNER JOIN ATENDIMENTO_PACIENTE AP
                            ON (AP.NR_ATENDIMENTO = PM.NR_ATENDIMENTO)
                        LEFT JOIN PROCEDENCIA PCD
                            ON (PCD.CD_PROCEDENCIA = AP.CD_PROCEDENCIA)
                        LEFT JOIN LAUDO_PACIENTE LP
                            ON (LP.NR_PRESCRICAO = PPR.NR_PRESCRICAO
                                AND LP.NR_SEQ_PRESCRICAO = PPR.NR_SEQUENCIA
                                AND LP.IE_STATUS_LAUDO = 'LL')
                        LEFT JOIN EXAME_LABORATORIO ELC
                            ON (ELC.NR_SEQ_EXAME = (
                                SELECT
                                    DISTINCT ELD1.NR_SEQ_EXAME
                                FROM
                                    EXAME_LAB_DEPENDENTE ELD1
                                    INNER JOIN EXAME_LABORATORIO EL1
                                        ON (EL1.NR_SEQ_EXAME = ELD1.NR_SEQ_EXAME)
                                WHERE
                                    (
                                        (ELD1.NR_SEQ_EXAME_DEP = EL.NR_SEQ_EXAME
                                        AND ELD1.NR_SEQ_EXAME IN (
                                            SELECT
                                                PP1.NR_SEQ_EXAME
                                            FROM
                                                PRESCR_PROCEDIMENTO PP1
                                            WHERE
                                                PP1.NR_PRESCRICAO = PPR.NR_PRESCRICAO
                                        ))
                                        OR
                                        ELD1.NR_SEQ_EXAME = EL.NR_SEQ_EXAME
                                    )
                                    AND EL1.NR_SEQ_GRUPO = 48
                            ))
                    WHERE
                        1=1
                        AND PM.DT_LIBERACAO IS NOT NULL
                        AND PM.DT_SUSPENSAO IS NULL
                        AND PPR.IE_STATUS_EXECUCAO <> '910' /*Descartado*/
                        AND PPR.IE_SUSPENSO = 'N'
                        AND ( PPR.CD_CGC_LABORATORIO IS NOT NULL OR ELC.NR_SEQ_EXAME = EL.NR_SEQ_EXAME )
                        AND ( AP.DT_ENTRADA >= CAST(TO_TIMESTAMP(DATE_START_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) OR DATE_START_P IS NULL )
                        AND ( AP.DT_ENTRADA <= CAST(TO_TIMESTAMP(DATE_END_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) OR DATE_END_P IS NULL )
                        AND
                        (
                            FILTER_P IS NULL
                            OR UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(PF.NM_PESSOA_FISICA, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%')
                            OR UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(PFM.NM_PESSOA_FISICA, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%')
                            OR UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(EL.NM_EXAME, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%')
                            OR UPPER(AP.NR_ATENDIMENTO) LIKE REPLACE(UPPER(FILTER_P),'~','%')
                            OR UPPER(PF.NR_CPF) LIKE REPLACE(UPPER(FILTER_P),'~','%')
                            OR AP.NR_ATENDIMENTO IN (
                                SELECT
                                    AI1.NR_ATENDIMENTO
                                FROM
                                    ATENDIMENTO_INDICACAO AI1
                                    INNER JOIN PESSOA_FISICA PF1
                                        ON (PF1.CD_PESSOA_FISICA = AI1.CD_MEDICO)
                                WHERE
                                    AI1.NR_ATENDIMENTO = AP.NR_ATENDIMENTO
                                    AND AI1.NR_SEQ_INDICACAO = '150'
                                    AND UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(PF1.NM_PESSOA_FISICA, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%')
                            )
                        )
                        AND ( MDC.UF_CRM||MDC.NR_CRM = CRM_P OR CRM_P IS NULL)
                        AND (
                            PM.CD_MEDICO IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                            OR AP.NR_ATENDIMENTO IN (
                                SELECT
                                    AI1.NR_ATENDIMENTO
                                FROM
                                    ATENDIMENTO_INDICACAO AI1
                                    INNER JOIN PESSOA_FISICA PF1
                                        ON (PF1.CD_PESSOA_FISICA = AI1.CD_MEDICO)
                                WHERE
                                    AI1.NR_ATENDIMENTO = AP.NR_ATENDIMENTO
                                    AND AI1.NR_SEQ_INDICACAO = '150'
                                    AND AI1.CD_MEDICO IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                            )
                            OR (SELECT COUNT(*) FROM TABLE(T_PHYSICIANS_ARR)) = 0
                        )
                        AND ( AP.CD_PROCEDENCIA IN (SELECT * FROM TABLE(T_ORIGINS_ARR)) OR (SELECT COUNT(*) FROM TABLE(T_ORIGINS_ARR)) = 0 )
                        AND ( EL.NR_SEQ_EXAME NOT IN (SELECT * FROM TABLE(T_EXAMBL_ARR)) OR EL.NR_SEQ_EXAME IS NULL )
                        AND ( ELC.NR_SEQ_EXAME NOT IN (SELECT * FROM TABLE(T_EXAMBL_ARR)) OR ELC.NR_SEQ_EXAME IS NULL )
                        AND ( AP.CD_PESSOA_FISICA = PATIENT_P OR PATIENT_P IS NULL )
                        AND ( AP.NR_ATENDIMENTO = ENCOUNTER_P OR ENCOUNTER_P IS NULL )
                        AND (
                            LP.NR_SEQUENCIA = (
                                SELECT
                                MAX(NR_SEQUENCIA)
                                FROM
                                LAUDO_PACIENTE
                                WHERE
                                NR_PRESCRICAO = PPR.NR_PRESCRICAO
                                AND NR_SEQ_PRESCRICAO = PPR.NR_SEQUENCIA
                                AND IE_STATUS_LAUDO = 'LL'
                            )
                            OR LP.NR_SEQUENCIA IS NULL
                        )
                ) S2
                /* [END] CORE RESULTSET */
                ORDER BY
                    CASE
                    WHEN SORT_ORDER_P = 'asc' THEN DECODE(SORT_COL_P,
                        'examName',NM_EXAME,
                        'patient.name',NM_PACIENTE,
                        'physician.name',NM_MEDICO_RESP,
                        'origin.name',DS_PROCEDENCIA,
                        NULL)
                    ELSE '1'
                    END ASC,
                    CASE
                    WHEN SORT_ORDER_P = 'asc' THEN DECODE(SORT_COL_P,
                        'examDate',DT_EXAME,
                        'forecastDate',DT_PREV_ENTREGA,
                        'attendanceDate',DT_ATENDIMENTO,
                        NULL)
                    ELSE TO_DATE('01/01/1970','DD/MM/YYYY')
                    END ASC NULLS LAST,
                    CASE
                    WHEN SORT_ORDER_P = 'desc' THEN DECODE(SORT_COL_P,
                        'examName',NM_EXAME,
                        'patient.name',NM_PACIENTE,
                        'physician.name',NM_MEDICO_RESP,
                        'origin.name',DS_PROCEDENCIA,
                        NULL)
                    ELSE '1'
                    END DESC,
                    CASE
                    WHEN SORT_ORDER_P = 'desc' THEN DECODE(SORT_COL_P,
                        'examDate',DT_EXAME,
                        'forecastDate',DT_PREV_ENTREGA,
                        'attendanceDate',DT_ATENDIMENTO,
                        NULL)
                    ELSE TO_DATE('01/01/1970','DD/MM/YYYY')
                    END DESC NULLS LAST,
                    CASE
                    WHEN SORT_ORDER_P = 'asc' THEN DT_ATENDIMENTO
                    ELSE TO_DATE('01/01/1970','DD/MM/YYYY')
                    END ASC,
                    CASE
                    WHEN SORT_ORDER_P = 'desc' THEN DT_ATENDIMENTO
                    ELSE TO_DATE('01/01/1970','DD/MM/YYYY')
                    END DESC
            ) S1
            WHERE
                ROWNUM <= NVL(PAGE_P,1)*NVL(LIMIT_P,10)
        ) S
        WHERE
            RNUM > (NVL(PAGE_P,1)-1)*NVL(LIMIT_P,10);

    END GET_LAB_EXAMS;

    PROCEDURE GET_COMBO_EXAM_ITENS(
        NRPRESCRICAO_P IN VARCHAR2,
        NRSEQEXAME_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        DATE_START_P IN VARCHAR2,
        DATE_END_P IN VARCHAR2,
        FILTER_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_EXAMBL_ARR OC_PARAM_ARR;

    BEGIN

    T_EXAMBL_ARR := LPARAM('EXAMBL',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            PF.CD_PESSOA_FISICA CD_PACIENTE,
            PF.NM_PESSOA_FISICA NM_PACIENTE,
            PF.NR_CPF NR_CPF_PACIENTE,
            PCD.CD_PROCEDENCIA,
            PCD.DS_PROCEDENCIA,
            PM.NR_PRESCRICAO,
            PM.NR_ATENDIMENTO,
            'S' IE_COMBO,
            PPR.NR_SEQUENCIA NR_SEQ_PRESCRICAO,
            LP.NR_SEQ_PROC,
            PPR.NR_SEQ_EXAME,
            EL.NM_EXAME,
            PM.DT_LIBERACAO DT_EXAME,
            LP.DT_LAUDO,
            PPR.DT_RESULTADO DT_PREV_ENTREGA,
            PFM.CD_PESSOA_FISICA CD_MEDICO,
            PFM.NM_PESSOA_FISICA NM_MEDICO_RESP,
            (MDC.UF_CRM||MDC.NR_CRM) NR_CRM,
            AP.DT_ENTRADA DT_ATENDIMENTO,
            LP.IE_STATUS_LAUDO,
            OBTER_VALOR_DOMINIO(1214,LP.IE_STATUS_LAUDO) NM_STATUS_LAUDO,
            LP.DS_ARQUIVO,
            LP.NR_SEQUENCIA NR_SEQ_LAUDO
        FROM
            PRESCR_PROCEDIMENTO PPR
            INNER JOIN EXAME_LABORATORIO EL
                ON (EL.NR_SEQ_EXAME = PPR.NR_SEQ_EXAME)
            INNER JOIN PRESCR_MEDICA PM
                ON (PM.NR_PRESCRICAO = PPR.NR_PRESCRICAO)
            INNER JOIN PESSOA_FISICA PF
                ON (PF.CD_PESSOA_FISICA = PM.CD_PESSOA_FISICA)
            INNER JOIN PESSOA_FISICA PFM
                ON (PFM.CD_PESSOA_FISICA = PM.CD_MEDICO)
            INNER JOIN MEDICO MDC
                ON (MDC.CD_PESSOA_FISICA = PM.CD_MEDICO)    
            INNER JOIN ATENDIMENTO_PACIENTE AP
                ON (AP.NR_ATENDIMENTO = PM.NR_ATENDIMENTO)
            LEFT JOIN PROCEDENCIA PCD
                ON (PCD.CD_PROCEDENCIA = AP.CD_PROCEDENCIA)
            INNER JOIN EXAME_LAB_DEPENDENTE ELD
                ON (ELD.NR_SEQ_EXAME_DEP = PPR.NR_SEQ_EXAME AND ELD.NR_SEQ_EXAME = NRSEQEXAME_P)
            LEFT JOIN LAUDO_PACIENTE LP
                ON (LP.NR_PRESCRICAO = PPR.NR_PRESCRICAO
                    AND LP.NR_SEQ_PRESCRICAO = PPR.NR_SEQUENCIA
                    AND LP.IE_STATUS_LAUDO = 'LL')
        WHERE
            PM.NR_PRESCRICAO = NRPRESCRICAO_P
            AND PM.DT_LIBERACAO IS NOT NULL
            AND PM.DT_SUSPENSAO IS NULL
            AND PPR.IE_STATUS_EXECUCAO <> '910' /*Descartado*/
            AND PPR.IE_SUSPENSO = 'N'
            AND PPR.CD_CGC_LABORATORIO IS NOT NULL
            AND ( AP.DT_ENTRADA >= CAST(TO_TIMESTAMP(DATE_START_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) OR DATE_START_P IS NULL )
            AND ( AP.DT_ENTRADA <= CAST(TO_TIMESTAMP(DATE_END_P,'YYYY-MM-DD"T"HH24:MI:SS.FF3"Z"') AS DATE) OR DATE_END_P IS NULL )
            AND ( EL.NR_SEQ_EXAME NOT IN (SELECT * FROM TABLE(T_EXAMBL_ARR)) )
            AND (
                LP.NR_SEQUENCIA = (
                    SELECT
                    MAX(NR_SEQUENCIA)
                    FROM
                    LAUDO_PACIENTE
                    WHERE
                    NR_PRESCRICAO = PPR.NR_PRESCRICAO
                    AND NR_SEQ_PRESCRICAO = PPR.NR_SEQUENCIA
                    AND IE_STATUS_LAUDO = 'LL'
                )
                OR LP.NR_SEQUENCIA IS NULL
            )
            AND
            (
                (UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(PF.NM_PESSOA_FISICA, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%') OR FILTER_P IS NULL)
                OR (UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(PFM.NM_PESSOA_FISICA, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%') OR FILTER_P IS NULL)
                OR (UPPER(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(EL.NM_EXAME, 'nls_sort=binary_ai'))) LIKE REPLACE(UPPER(FILTER_P),'~','%') OR FILTER_P IS NULL)
                OR (UPPER(AP.NR_ATENDIMENTO) LIKE REPLACE(UPPER(FILTER_P),'~','%') OR FILTER_P IS NULL)
                OR (UPPER(PF.NR_CPF) LIKE REPLACE(UPPER(FILTER_P),'~','%') OR FILTER_P IS NULL)
            )
        ORDER BY
            NR_SEQ_PRESCRICAO ASC;

    END GET_COMBO_EXAM_ITENS;

    PROCEDURE GET_LAB_EXAM_ITEM(
        NRPRESCRICAO_P IN VARCHAR2,
        NRSEQITEM_P IN VARCHAR2,
        NRSEQLAUDO_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

    OPEN P_RECORDSET FOR
        SELECT
            LP.NR_PRESCRICAO,
            LP.NR_SEQ_PRESCRICAO,
            LP.NR_SEQ_PROC,
            LP.DT_EXAME,
            LP.DT_LAUDO,
            LP.DT_PREV_ENTREGA,
            LP.IE_STATUS_LAUDO,
            LP.DS_ARQUIVO
        FROM
            LAUDO_PACIENTE LP
        WHERE
            LP.NR_PRESCRICAO = NRPRESCRICAO_P
            AND LP.NR_SEQ_PRESCRICAO = NRSEQITEM_P
            AND LP.NR_SEQUENCIA = NRSEQLAUDO_P;

    END GET_LAB_EXAM_ITEM;

    PROCEDURE GET_CARE_ORIGIN(
        CNPJ_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

    OPEN P_RECORDSET FOR
        SELECT
            PM.CD_PROCEDENCIA CD_PROCEDENCIA_MAT,
            PM.NM_LOGIN_WEB NM_LOGIN_WEB_MAT,
            PM.DS_SENHA_WEB DS_SENHA_WEB_MAT,
            PM.DS_PROCEDENCIA DS_PROCEDENCIA_MAT,
            P.CD_PROCEDENCIA,
            P.NM_LOGIN_WEB,
            P.DS_SENHA_WEB,
            P.DS_PROCEDENCIA
        FROM
            PROCEDENCIA PM
            LEFT JOIN PROCEDENCIA P
                ON (P.CD_CGC_PROCEDENCIA = PM.NM_LOGIN_WEB
                    AND P.CD_PROCEDENCIA <> PM.CD_PROCEDENCIA
                    AND P.IE_SITUACAO = 'A')
        WHERE
            PM.IE_SITUACAO = 'A'
            AND ( PM.NM_LOGIN_WEB = CNPJ_P OR P.CD_CGC_PROCEDENCIA = CNPJ_P )
        ORDER BY
            PM.DS_PROCEDENCIA ASC,
            P.DS_PROCEDENCIA ASC;

    END GET_CARE_ORIGIN;

    PROCEDURE GET_RELEASED_EXAMS(
        REFID_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

    T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            EPOCH_D(PM.DT_LIBERACAO) REFID,
            TO_CHAR(PM.DT_LIBERACAO,'YYYY-MM-DD HH24:MI:SS') DT_LIBERACAO,
            PM.NR_PRESCRICAO,
            AP.NR_ATENDIMENTO,
            AP.CD_PROCEDENCIA,
            AP.CD_ESTABELECIMENTO,
            OBTER_NOME_ESTAB(AP.CD_ESTABELECIMENTO) NM_ESTABELECIMENTO,
            AP.CD_PESSOA_FISICA CD_PACIENTE,
            OBTER_NOME_PF(AP.CD_PESSOA_FISICA) NM_PACIENTE,
            PM.CD_MEDICO CD_MEDICO,
            OBTER_NOME_PF(PM.CD_MEDICO) NM_MEDICO,
            TRIM(OBTER_EMAIL_PF(AP.CD_PESSOA_FISICA)) DS_EMAIL_PACIENTE
        FROM
            PRESCR_MEDICA PM
            INNER JOIN ATENDIMENTO_PACIENTE AP
                ON (AP.NR_ATENDIMENTO = PM.NR_ATENDIMENTO)
            INNER JOIN PRESCR_PROCEDIMENTO PPR
                ON (PPR.NR_PRESCRICAO = PM.NR_PRESCRICAO)
            INNER JOIN EXAME_LABORATORIO EL
                ON (EL.NR_SEQ_EXAME = PPR.NR_SEQ_EXAME)
        WHERE
            PM.DT_LIBERACAO IS NOT NULL
            AND PM.DT_SUSPENSAO IS NULL
            AND PPR.CD_CGC_LABORATORIO IS NOT NULL
            AND AP.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
            AND TO_NUMBER(EPOCH_D(PM.DT_LIBERACAO)) > REFID_P
        GROUP BY
            PM.DT_LIBERACAO,
            PM.NR_PRESCRICAO,
            AP.NR_ATENDIMENTO,
            AP.CD_PROCEDENCIA,
            AP.CD_ESTABELECIMENTO,
            AP.CD_PESSOA_FISICA,
            PM.CD_MEDICO
        ORDER BY
            PM.DT_LIBERACAO ASC;

    END GET_RELEASED_EXAMS;

    PROCEDURE GET_RELEASED_LAB_RESULTS(
        REFID_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

    T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            (EPOCH_D(LP.DT_LIBERACAO)||LPAD(LP.NR_SEQ_PRESCRICAO,2,'0')) REFID,
            LP.NR_SEQUENCIA NR_SEQ_LAUDO,
            TO_CHAR(LP.DT_LIBERACAO,'YYYY-MM-DD HH24:MI:SS') DT_LAUDO,
            PM.NR_PRESCRICAO,
            PPR.NR_SEQUENCIA,
            EL.NM_EXAME,
            AP.NR_ATENDIMENTO,
            AP.CD_ESTABELECIMENTO,
            OBTER_NOME_ESTAB(AP.CD_ESTABELECIMENTO) NM_ESTABELECIMENTO,
            AP.CD_PESSOA_FISICA CD_PACIENTE,
            OBTER_NOME_PF(AP.CD_PESSOA_FISICA) NM_PACIENTE,
            PM.CD_MEDICO CD_MEDICO,
            OBTER_NOME_PF(PM.CD_MEDICO) NM_MEDICO,
            (
                SELECT LISTAGG(TRIM(DS_EMAIL),',') WITHIN GROUP (ORDER BY CD_PESSOA_FISICA)
                FROM    COMPL_PESSOA_FISICA
                WHERE   CD_PESSOA_FISICA = PM.CD_MEDICO
            ) DS_EMAIL_MEDICO
        FROM
            PRESCR_MEDICA PM
            INNER JOIN PRESCR_PROCEDIMENTO PPR
                ON (PPR.NR_PRESCRICAO = PM.NR_PRESCRICAO)
            INNER JOIN EXAME_LABORATORIO EL
                ON (EL.NR_SEQ_EXAME = PPR.NR_SEQ_EXAME)
            INNER JOIN ATENDIMENTO_PACIENTE AP
                ON (AP.NR_ATENDIMENTO = PM.NR_ATENDIMENTO)
            INNER JOIN LAUDO_PACIENTE LP
                ON (LP.NR_PRESCRICAO = PPR.NR_PRESCRICAO
                    AND LP.NR_SEQ_PRESCRICAO = PPR.NR_SEQUENCIA
                    AND LP.IE_STATUS_LAUDO = 'LL')
            INNER JOIN MEDICO M
                ON (M.CD_PESSOA_FISICA = PM.CD_MEDICO AND M.IE_SITUACAO = 'A')
        WHERE
            PM.DT_LIBERACAO IS NOT NULL
            AND PM.DT_SUSPENSAO IS NULL
            AND AP.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
            AND TO_NUMBER(EPOCH_D(LP.DT_LIBERACAO)||LPAD(LP.NR_SEQ_PRESCRICAO,2,'0')) > REFID_P
            AND M.IE_EMAIL_LAUDO = 'S'
            AND LP.NR_SEQUENCIA = (
                    SELECT
                    MAX(NR_SEQUENCIA)
                    FROM
                    LAUDO_PACIENTE
                    WHERE
                    NR_PRESCRICAO = PPR.NR_PRESCRICAO
                    AND NR_SEQ_PRESCRICAO = PPR.NR_SEQUENCIA
                    AND IE_STATUS_LAUDO = 'LL'
                )
        ORDER BY
            LP.DT_LIBERACAO ASC,
            LP.NR_SEQ_PRESCRICAO ASC;

    END GET_RELEASED_LAB_RESULTS;

    PROCEDURE GET_CA_TREATMENT_DIAGNOSIS(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    BEGIN

        OPEN P_RECORDSET FOR
            SELECT
                CLR.NR_SEQ_ATEND_CONS_PEPA||'.'||CLR.NR_SEQUENCIA "ID",
                CLR.NR_SEQUENCIA,
                CLR.NR_SEQ_ATEND_CONS_PEPA,
                CLR.CD_DOENCA_CID,
                CD.DS_DOENCA_CID,
                CLR.CD_TOPOGRAFIA,
                CT.DS_TOPOGRAFIA,
                CLR.CD_MORFOLOGIA,
                NVL(CMDA.DS_MORFOLOGIA,CM.DS_MORFOLOGIA) DS_MORFOLOGIA,
                CLR.CD_TUMOR_PRIMARIO,
                CLR.CD_LINFONODO_REGIONAL,
                CLR.CD_METASTASE_DISTANCIA,
                CLR.CD_ESTADIAMENTO,
                CLR.CD_TUMOR_PRIM_PAT,
                CLR.CD_LINFONODO_REG_PAT,
                CLR.CD_METASTASE_DIST_PAT,
                CLR.CD_ESTADIO_OUTRO,
                CLR.CD_PESSOA_FISICA CD_PACIENTE,
                OBTER_NOME_PF(CLR.CD_PESSOA_FISICA) NM_PACIENTE,
                CLR.CD_MEDICO,
                OBTER_NOME_PF(CLR.CD_MEDICO) NM_MEDICO,
                CLR.CD_ESTABELECIMENTO,
                OBTER_NOME_ESTAB(CLR.CD_ESTABELECIMENTO) NM_ESTABELECIMENTO,
                CLR.DT_AVALIACAO
            FROM
                CAN_LOCO_REGIONAL CLR
                LEFT JOIN CID_DOENCA CD
                    ON (CD.CD_DOENCA_CID = CLR.CD_DOENCA_CID)
                LEFT JOIN CIDO_TOPOGRAFIA CT
                    ON (CT.CD_TOPOGRAFIA = CLR.CD_TOPOGRAFIA)
                LEFT JOIN CIDO_MORFOLOGIA CM
                    ON (CM.CD_MORFOLOGIA = CLR.CD_MORFOLOGIA)
                LEFT JOIN CIDO_MORFOLOGIA_DESC_ADIC CMDA
                    ON (CMDA.NR_SEQUENCIA = CLR.NR_SEQ_MORF_DESC_ADIC AND CMDA.CD_MORFOLOGIA = CLR.CD_MORFOLOGIA)
            WHERE
                CLR.CD_PESSOA_FISICA = PATIENT_ID_P
                AND
                (
                    CLR.CD_MEDICO = PHYSICIAN_ID_P
                    OR CLR.CD_PESSOA_FISICA IN (
                        SELECT
                            P.CD_PESSOA_FISICA
                        FROM
                            AGENDA A
                            INNER JOIN AGENDA_CONSULTA AC
                                ON (A.CD_AGENDA = AC.CD_AGENDA)
                            INNER JOIN PESSOA_FISICA P
                                ON (P.CD_PESSOA_FISICA = AC.CD_PESSOA_FISICA)
                            INNER JOIN ATENDIMENTO_PACIENTE AP
                                ON (AP.NR_ATENDIMENTO = AC.NR_ATENDIMENTO)
                        WHERE
                            ( A.CD_PESSOA_FISICA = PHYSICIAN_ID_P OR AP.CD_MEDICO_RESP = PHYSICIAN_ID_P OR AP.CD_MEDICO_REFERIDO = PHYSICIAN_ID_P )
                            AND P.CD_PESSOA_FISICA = P.CD_PESSOA_FISICA
                            AND AC.IE_STATUS_AGENDA NOT IN ('L','N')
                            AND AP.DT_ENTRADA IS NOT NULL
                    )
                    OR PHYSICIAN_ID_P IS NULL
                )
                AND CLR.DT_LIBERACAO IS NOT NULL
                AND CLR.IE_SITUACAO = 'A'
            ORDER BY
                CLR.NR_SEQUENCIA DESC;

    END GET_CA_TREATMENT_DIAGNOSIS;

    PROCEDURE GET_CA_TREATMENT_PLANS(
        CA_TREATMENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_ID VARCHAR2(45);

    BEGIN

        T_ID := TRIM(REGEXP_SUBSTR(CA_TREATMENT_ID_P,'[^.]+',1,2) );

        OPEN P_RECORDSET FOR
            SELECT
                PS.NR_SEQ_PACIENTE,
                PM.CD_PROTOCOLO,
                PS.CD_PESSOA_FISICA,
                PS.CD_MEDICO_RESP,
                PM.NM_MEDICACAO,
                P.NM_PROTOCOLO,
                (
                    SELECT
                        MIN(PA.DT_PREVISTA)
                    FROM
                        PACIENTE_ATENDIMENTO PA
                    WHERE
                        PA.NR_SEQ_PACIENTE = PS.NR_SEQ_PACIENTE
                ) DT_PRIMEIRA,
                (
                    SELECT
                        MAX(PA.DT_REAL)
                    FROM
                        PACIENTE_ATENDIMENTO PA
                    WHERE
                        PA.NR_SEQ_PACIENTE = PS.NR_SEQ_PACIENTE
                        AND PA.DT_INICIO_ADM IS NOT NULL
                ) DT_ULTIMA
            FROM
                PACIENTE_SETOR PS
                INNER JOIN PROTOCOLO_MEDICACAO PM
                    ON (PM.CD_PROTOCOLO = PS.CD_PROTOCOLO AND PM.NR_SEQUENCIA = PS.NR_SEQ_MEDICACAO)
                INNER JOIN PROTOCOLO P
                    ON (P.CD_PROTOCOLO = PM.CD_PROTOCOLO)
            WHERE
                PS.NR_SEQ_LOCO_REGIONAL = T_ID
                AND PS.IE_STATUS = 'A'
            ORDER BY
                PS.NR_SEQ_PACIENTE DESC;

    END GET_CA_TREATMENT_PLANS;

    PROCEDURE GET_CA_TREATMENT_EVOLUTION(
        CA_TREATMENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_ID VARCHAR2(45);

    BEGIN

        T_ID := TRIM(REGEXP_SUBSTR(CA_TREATMENT_ID_P,'[^.]+',1,1) );

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ERE1.DS_RESULTADO DS_RESULTADO_EVOL,
                ERE2.DS_RESULTADO DS_RESULTADO_COMP,
                ERE3.DS_RESULTADO DS_RESULTADO_CONDUCT,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ER.NR_SEQUENCIA = ERT.NR_SEQ_REG
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO IN (293865,322489) /*Evolucao clinica*/ )
                LEFT JOIN EHR_REG_ELEMENTO ERE2
                    ON ( ERE2.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE2.NR_SEQ_TEMP_CONTEUDO IN (316389,322485) /*Observacao*/ )
                LEFT JOIN EHR_REG_ELEMENTO ERE3
                    ON ( ERE3.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE3.NR_SEQ_TEMP_CONTEUDO IN (318314,322486) /* 7 - Conduta */ )
            WHERE
                ERT.NR_SEQ_TEMPLATE IN (100780,101068) /*Evolucao*/
                AND ER.NR_SEQ_ATEND_CONS_PEPA = T_ID
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_CA_TREATMENT_EVOLUTION;

    PROCEDURE GET_CA_TREATMENT_ANAMNESIS(
        CA_TREATMENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_ID VARCHAR2(45);

    BEGIN

        T_ID := TRIM(REGEXP_SUBSTR(CA_TREATMENT_ID_P,'[^.]+',1,1) );

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ER.NR_ATENDIMENTO,
                ERE1.DS_RESULTADO DS_RESULT_QUEIXA,
                ERE2.DS_RESULTADO DS_RESULT_HPMA,
                ERE3.DS_RESULTADO DS_RESULT_ISDA,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ( ER.NR_SEQUENCIA = ERT.NR_SEQ_REG )
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO IN (293833,322577) /*Queixa principal*/ )
                LEFT JOIN EHR_REG_ELEMENTO ERE2
                    ON ( ERE2.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE2.NR_SEQ_TEMP_CONTEUDO IN (293834,322578) /*HPMA*/ )
                LEFT JOIN EHR_REG_ELEMENTO ERE3
                    ON ( ERE3.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE3.NR_SEQ_TEMP_CONTEUDO IN (293836,322579) /*ISDA*/ )
            WHERE
                ERT.NR_SEQ_TEMPLATE IN (100777,101071) /*Evolucao ANAMNESIS*/
                AND ER.NR_SEQ_ATEND_CONS_PEPA = T_ID
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_CA_TREATMENT_ANAMNESIS;

    PROCEDURE GET_CA_TREATMENT_DIAG_IMP(
        CA_TREATMENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_ID VARCHAR2(45);

    BEGIN

        T_ID := TRIM(REGEXP_SUBSTR(CA_TREATMENT_ID_P,'[^.]+',1,1) );

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ERE1.DS_RESULTADO DS_RESULTADO,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ER.NR_SEQUENCIA = ERT.NR_SEQ_REG
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO = 293838 /*Impressao diagnostica*/ )
            WHERE
                ERT.NR_SEQ_TEMPLATE = 100778 /*Impressao diagnostica*/
                AND ER.NR_SEQ_ATEND_CONS_PEPA = T_ID
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_CA_TREATMENT_DIAG_IMP;

    PROCEDURE GET_CA_TREATMENT_CONDUCT(
        CA_TREATMENT_ID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_ID VARCHAR2(45);

    BEGIN

        T_ID := TRIM(REGEXP_SUBSTR(CA_TREATMENT_ID_P,'[^.]+',1,1) );

        OPEN P_RECORDSET FOR
            SELECT
                ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                ERE1.DS_RESULTADO DS_RESULTADO,
                ER.CD_PACIENTE,
                ER.DT_REGISTRO
            FROM
                EHR_REG_TEMPLATE ERT
                INNER JOIN EHR_REGISTRO ER
                    ON ER.NR_SEQUENCIA = ERT.NR_SEQ_REG
                INNER JOIN EHR_REG_ELEMENTO ERE1
                    ON ( ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA AND ERE1.NR_SEQ_TEMP_CONTEUDO IN (293839,322686) /*Conduta*/ )
            WHERE
                ERT.NR_SEQ_TEMPLATE IN (100779,101077) /*Conduta*/
                AND ER.NR_SEQ_ATEND_CONS_PEPA = T_ID
                AND ER.DT_LIBERACAO IS NOT NULL
                AND ER.DT_INATIVACAO IS NULL
            ORDER BY
                ERT.NR_SEQUENCIA DESC;

    END GET_CA_TREATMENT_CONDUCT;

    PROCEDURE SET_TEMPLATE_RECORD(
        ENCOUNTER_ID_P IN VARCHAR2,
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        APPOINTMENT_ID_P IN VARCHAR2,
        TEMPLATE_ID_P IN VARCHAR2,
        CONTENT_ID_P IN VARCHAR2,
        VALUE_P IN VARCHAR2,
        P_ID OUT VARCHAR2,
        P_ERR OUT VARCHAR2,
        P_MSG OUT VARCHAR2) AS

    T_NR_SEQ_REG_TEMPLATE NUMBER(10) := NULL;
    T_NR_SEQ_REG NUMBER(10) := NULL;
    T_NR_SEQ_REG_ELEMENTO NUMBER(10) := NULL;
    T_APPOINTMENT NUMBER(10) := 0;
    APPOINTMENT_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( APPOINTMENT_NOT_FOUND, -20001 );
    ENCOUNTER_NOT_FOUND EXCEPTION;
    PRAGMA EXCEPTION_INIT( ENCOUNTER_NOT_FOUND, -20002 );

    BEGIN

        -- CHECK IF APPOINTMENT EXISTS
        SELECT
            MAX(NR_SEQUENCIA)
            INTO
            T_APPOINTMENT
        FROM
            ATEND_CONSULTA_PEPA
        WHERE
            NR_SEQUENCIA = APPOINTMENT_ID_P;

        IF T_APPOINTMENT IS NULL THEN
            RAISE APPOINTMENT_NOT_FOUND;
        END IF;

        -- GET TEMPLATE DETAILS OF AN EXISTING RECORD
        SELECT
            MAX(ERT.NR_SEQUENCIA) NR_SEQ_REG_TEMPLATE,
            MAX(ER.NR_SEQUENCIA) NR_SEQ_REG
            INTO
            T_NR_SEQ_REG_TEMPLATE,
            T_NR_SEQ_REG
        FROM
            EHR_REG_TEMPLATE ERT
            INNER JOIN EHR_REGISTRO ER
                ON ( ER.NR_SEQUENCIA = ERT.NR_SEQ_REG )
        WHERE
            ERT.NR_SEQ_TEMPLATE = TEMPLATE_ID_P
            AND ER.NR_SEQ_ATEND_CONS_PEPA = APPOINTMENT_ID_P
            AND ( ER.CD_PROFISSIONAL = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL )
            AND ER.DT_LIBERACAO IS NOT NULL
            AND ER.DT_INATIVACAO IS NULL;

        -- GET TEMPLATE RELATED DETAILS
        SELECT
            NR_SEQ_ELEMENTO
            INTO
            T_NR_SEQ_REG_ELEMENTO
        FROM
            EHR_TEMPLATE_CONTEUDO
        WHERE
            NR_SEQUENCIA = CONTENT_ID_P
            AND NR_SEQ_TEMPLATE = TEMPLATE_ID_P;

        -- NO EXISTING RECORD FOUND
        IF T_NR_SEQ_REG_ELEMENTO IS NULL THEN

            -- INSERT TEMPLATE EHR_REGISTRO RECORD
            INSERT INTO
            EHR_REGISTRO
            (NR_SEQUENCIA,
            CD_PACIENTE,
            DT_ATUALIZACAO,
            DT_ATUALIZACAO_NREC,
            NM_USUARIO,
            NM_USUARIO_NREC,
            DT_REGISTRO,
            CD_PROFISSIONAL,
            NR_ATENDIMENTO,
            DT_LIBERACAO,
            NM_USUARIO_LIBERACAO,
            NR_SEQ_TEMPL,
            NR_SEQ_ATEND_CONS_PEPA)
            VALUES
            (EHR_REGISTRO_SEQ.NEXTVAL,
            PATIENT_ID_P,
            SYSDATE,
            SYSDATE,
            APP_USER,
            APP_USER,
            SYSDATE,
            PHYSICIAN_ID_P,
            ENCOUNTER_ID_P,
            SYSDATE,
            APP_USER,
            TEMPLATE_ID_P,
            APPOINTMENT_ID_P);

            -- INSERT TEMPLATE EHR_REG_TEMPLATE RECORD
            INSERT INTO
            EHR_REG_TEMPLATE
            (NR_SEQUENCIA,NR_SEQ_REG,DT_ATUALIZACAO,DT_ATUALIZACAO_NREC,NM_USUARIO,NM_USUARIO_NREC,DT_REGISTRO,NR_SEQ_TEMPLATE,DT_LIBERACAO)
            VALUES
            (EHR_REG_TEMPLATE_SEQ.NEXTVAL,EHR_REGISTRO_SEQ.CURRVAL,SYSDATE,SYSDATE,APP_USER,APP_USER,SYSDATE,TEMPLATE_ID_P,SYSDATE);

            -- INSERT TEMPLATE EHR_REG_ELEMENTO RECORD
            INSERT INTO
            EHR_REG_ELEMENTO
            (NR_SEQUENCIA,NR_SEQ_REG_TEMPLATE,DT_ATUALIZACAO,DT_ATUALIZACAO_NREC,NM_USUARIO,NM_USUARIO_NREC,NR_SEQ_ELEMENTO,DS_RESULTADO,DT_RESULTADO,NR_SEQ_TEMP_CONTEUDO,DT_LIBERACAO)
            VALUES
            (EHR_REG_ELEMENTO_SEQ.NEXTVAL,EHR_REG_TEMPLATE_SEQ.CURRVAL,SYSDATE,SYSDATE,APP_USER,APP_USER,T_NR_SEQ_REG_ELEMENTO,VALUE_P,SYSDATE,CONTENT_ID_P,SYSDATE);

            COMMIT;

            P_ID := EHR_REGISTRO_SEQ.CURRVAL;

        ELSE

            -- INSERT TEMPLATE EHR_REG_ELEMENTO ON A RECORD THAT ALREADY EXISTS
            INSERT INTO
            EHR_REG_ELEMENTO
            (NR_SEQUENCIA,NR_SEQ_REG_TEMPLATE,DT_ATUALIZACAO,DT_ATUALIZACAO_NREC,NM_USUARIO,NM_USUARIO_NREC,NR_SEQ_ELEMENTO,DS_RESULTADO,DT_RESULTADO,NR_SEQ_TEMP_CONTEUDO,DT_LIBERACAO)
            VALUES
            (EHR_REG_ELEMENTO_SEQ.NEXTVAL,T_NR_SEQ_REG_TEMPLATE,SYSDATE,SYSDATE,APP_USER,APP_USER,T_NR_SEQ_REG_ELEMENTO,VALUE_P,SYSDATE,CONTENT_ID_P,SYSDATE);

            COMMIT;

            P_ID := T_NR_SEQ_REG;

        END IF;

    EXCEPTION
        WHEN APPOINTMENT_NOT_FOUND THEN
            P_ERR := 'APPOINTMENT_NOT_FOUND';
        WHEN OTHERS THEN
            RAISE;

    END SET_TEMPLATE_RECORD;

    PROCEDURE GET_USER_PROFILE(IDENTIFIER_P IN VARCHAR2,IETIPOUSUARIO_P IN VARCHAR2,IDTYPE_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    BEGIN

      OPEN P_RECORDSET FOR
          SELECT * FROM(
            SELECT
                PF.CD_PESSOA_FISICA,
                PF.NM_PESSOA_FISICA,
                MAX(CPF.DS_EMAIL) DS_EMAIL,
                REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') NR_CPF,
                OBTER_DADOS_PF(PF.CD_PESSOA_FISICA, 'TCD') NR_TELEFONE_CELULAR,
                M.NR_CRM,
                M.UF_CRM
            FROM
                PESSOA_FISICA PF
                INNER JOIN MEDICO M
                    ON (M.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND M.IE_SITUACAO = 'A')
                LEFT JOIN COMPL_PESSOA_FISICA CPF
                    ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND CPF.DS_EMAIL IS NOT NULL)
                INNER JOIN CONSELHO_PROFISSIONAL CP
                    ON (PF.NR_SEQ_CONSELHO = CP.NR_SEQUENCIA AND CP.SG_CONSELHO = 'CRM')
            WHERE
                (
                    ( IDTYPE_P = 'CRM' AND M.UF_CRM||M.NR_CRM = IDENTIFIER_P )
                    OR
                    ( IDTYPE_P = 'CPF' AND REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') = IDENTIFIER_P )
                )
                AND ( IETIPOUSUARIO_P = 'physician' OR IETIPOUSUARIO_P IS NULL )
            GROUP BY
                PF.NM_PESSOA_FISICA,
                PF.NR_CPF,
                PF.CD_PESSOA_FISICA,
                M.NR_CRM,
                M.UF_CRM
            UNION ALL
            SELECT
                PF.CD_PESSOA_FISICA,
                PF.NM_PESSOA_FISICA,
                MAX(CPF.DS_EMAIL) DS_EMAIL,
                REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') NR_CPF,
                OBTER_DADOS_PF(PF.CD_PESSOA_FISICA, 'TCD') NR_TELEFONE_CELULAR,
                '' NR_CRM,
                '' UF_CRM
            FROM
                PESSOA_FISICA PF
                LEFT JOIN COMPL_PESSOA_FISICA CPF
                    ON (CPF.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND CPF.DS_EMAIL IS NOT NULL)
            WHERE
                ( IDTYPE_P = 'CPF' AND REPLACE(REPLACE(PF.NR_CPF,'.',''),'-','') = IDENTIFIER_P )
                AND ( IETIPOUSUARIO_P = 'patient' OR IETIPOUSUARIO_P IS NULL )
            GROUP BY
                PF.NM_PESSOA_FISICA,
                PF.NR_CPF,
                PF.CD_PESSOA_FISICA
        ) S
        ORDER BY
            TO_NUMBER(S.CD_PESSOA_FISICA);

    END GET_USER_PROFILE;

    PROCEDURE GET_ALL_PHYSICIAN_PATIENTS(
        PARAMS_P IN VARCHAR2,
        CARE_UNIT_P IN VARCHAR2,
        NAME_P IN VARCHAR2,
        DOB_P IN VARCHAR2,
        STATE_P IN VARCHAR2,
        MRI_P IN VARCHAR2,
        CPF_P IN VARCHAR2,
        RG_P IN VARCHAR2,
        ZIPCODE_P IN VARCHAR2,
        SORT_COL_P IN VARCHAR2,
        SORT_ORDER_P IN VARCHAR2,
        OFFSET_P IN NUMBER,
        LIMIT_P IN NUMBER,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_PHYSICIANS_ARR OC_PARAM_ARR;

    BEGIN

        T_PHYSICIANS_ARR := LPARAM('PHYSICIANS',PARAMS_P);

        OPEN P_RECORDSET FOR
            SELECT
                SS.*
            FROM
            (
                SELECT
                    S.*,
                    ROWNUM RNUM
                FROM
                (
                    SELECT
                        P.CD_PESSOA_FISICA,
                        P.NM_PESSOA_FISICA,
                        P.NR_CPF,
                        P.NR_IDENTIDADE,
                        P.DT_NASCIMENTO DT_NASCIMENTO_TS,
                        P.NR_PRONTUARIO,
                        P.IE_SEXO,
                        TO_CHAR(P.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                        P.CD_PESSOA_FISICA||'.'||CP.NR_SEQUENCIA "ID_ENDERECO",
                        CP.CD_CEP,
                        CP.DS_ENDERECO,
                        CP.NR_ENDERECO,
                        CP.DS_BAIRRO,
                        CP.DS_MUNICIPIO,
                        CP.SG_ESTADO,
                        OBTER_VALOR_DOMINIO(50,CP.SG_ESTADO) NM_ESTADO,
                        '' DT_ULT_CONSULTA
                    FROM
                        PESSOA_FISICA P
                        LEFT JOIN COMPL_PESSOA_FISICA CP
                            ON (CP.CD_PESSOA_FISICA = P.CD_PESSOA_FISICA AND CP.IE_TIPO_COMPLEMENTO = 1)
                        INNER JOIN ATENDIMENTO_PACIENTE AP
                            ON (AP.CD_PESSOA_FISICA = P.CD_PESSOA_FISICA)
                    WHERE
                        1=1
                        AND (
                            (
                                AP.CD_MEDICO_RESP IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                                OR AP.CD_MEDICO_REFERIDO IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                            )
                            OR (SELECT COUNT(*) FROM TABLE(T_PHYSICIANS_ARR)) = 0
                        )
                        AND (LOWER(P.NM_PESSOA_FISICA) LIKE REPLACE(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(REPLACE(NAME_P,'~','%'), 'NLS_SORT=BINARY_AI')),CHR(0)) OR NAME_P IS NULL)
                        AND (TO_CHAR(P.DT_NASCIMENTO,'YYYY-MM-DD') = DOB_P OR DOB_P IS NULL)
                        AND (CP.SG_ESTADO = STATE_P OR STATE_P IS NULL)
                        AND (P.NR_PRONTUARIO = MRI_P OR MRI_P IS NULL)
                        AND (P.NR_CPF = CPF_P OR CPF_P IS NULL)
                        AND (P.NR_IDENTIDADE = RG_P OR RG_P IS NULL)
                        AND (CP.CD_CEP = ZIPCODE_P OR ZIPCODE_P IS NULL)
                        AND (AP.CD_ESTABELECIMENTO = CARE_UNIT_P OR CARE_UNIT_P IS NULL)
                    GROUP BY
                        P.CD_PESSOA_FISICA,
                        P.NM_PESSOA_FISICA,
                        P.NR_CPF,
                        P.NR_IDENTIDADE,
                        P.NR_PRONTUARIO,
                        P.IE_SEXO,
                        P.DT_NASCIMENTO,
                        CP.NR_SEQUENCIA,
                        CP.CD_CEP,
                        CP.DS_ENDERECO,
                        CP.NR_ENDERECO,
                        CP.DS_BAIRRO,
                        CP.DS_MUNICIPIO,
                        CP.SG_ESTADO
                    ORDER BY
                        CASE
                        WHEN SORT_ORDER_P = 'asc' THEN DECODE(SORT_COL_P,
                            'name',UPPER(P.NM_PESSOA_FISICA),
                            'state',CP.SG_ESTADO,
                            NULL)
                        ELSE '1'
                        END ASC,
                        CASE
                        WHEN SORT_ORDER_P = 'desc' THEN DECODE(SORT_COL_P,
                            'name',UPPER(P.NM_PESSOA_FISICA),
                            'state',SG_ESTADO,
                            NULL)
                        ELSE '1'
                        END DESC,
                        CASE
                        WHEN SORT_ORDER_P = 'asc' THEN DECODE(SORT_COL_P,
                            'dob',DT_NASCIMENTO_TS,
                            NULL)
                        ELSE TO_DATE('01/01/1970','DD/MM/YYYY')
                        END ASC NULLS LAST,
                        CASE
                        WHEN SORT_ORDER_P = 'desc' THEN DECODE(SORT_COL_P,
                            'dob',DT_NASCIMENTO_TS,
                            NULL)
                        ELSE TO_DATE('01/01/1970','DD/MM/YYYY')
                        END DESC NULLS LAST,
                        DT_ULT_CONSULTA DESC
                ) S
            ) SS
            WHERE
                RNUM > OFFSET_P AND RNUM <= OFFSET_P+LIMIT_P;

    END GET_ALL_PHYSICIAN_PATIENTS;
    PROCEDURE GET_TREATMENT_APPOINTMENTS(
        PATIENT_ID_P IN VARCHAR2,
        PHYSICIAN_ID_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS
        
    XML_CLOB CLOB;
        
    BEGIN
    
        XML_CLOB := TO_CLOB(PARAMS_P);

        OPEN P_RECORDSET FOR
            SELECT
                NR_ATENDIMENTO,
                CD_PACIENTE,
                DT_ENTRADA,
                DS_CLASSIFICACAO,
                CD_MEDICO,
                NM_MEDICO,
                CD_ESPECIALIDADE_MED,
                NR_CONTATO_MED,
                DS_ESPECIALIDADE_MED,
                MAX(NR_SEQ_REG_TEMPLATE) NR_SEQ_REG_TEMPLATE,
                MAX(DS_RESULTADO) DS_RESULTADO,
                MAX(DT_ATUALIZACAO) DT_ATUALIZACAO,
                TP_RESULTADO
            FROM
            (
                SELECT
                    ATD.NR_ATENDIMENTO NR_ATENDIMENTO,
                    ER.CD_PACIENTE CD_PACIENTE,
                    ATD.DT_ENTRADA DT_ENTRADA,
                    CL.DS_CLASSIFICACAO DS_CLASSIFICACAO,
                    M.CD_PESSOA_FISICA CD_MEDICO,
                    M.NM_PESSOA_FISICA NM_MEDICO,
                    OBTER_ESPECIALIDADE_MEDICO(M.CD_PESSOA_FISICA,'C') CD_ESPECIALIDADE_MED,
                    OBTER_DADOS_PF(M.CD_PESSOA_FISICA, 'TCD') NR_CONTATO_MED,
                    OBTER_ESPECIALIDADE_MEDICO(M.CD_PESSOA_FISICA,'D') DS_ESPECIALIDADE_MED,
                    ERT.NR_SEQUENCIA NR_SEQ_REG_TEMPLATE,
                    ERE1.DS_RESULTADO DS_RESULTADO,
                    ER.DT_ATUALIZACAO DT_ATUALIZACAO,
                    CASE
                        WHEN
                            ERT.NR_SEQ_TEMPLATE = TP.IMP_TMP AND ERE1.NR_SEQ_TEMP_CONTEUDO = TP.IMP_ELE THEN 'IMPRESSAO'
                        WHEN
                            ERT.NR_SEQ_TEMPLATE = TP.CON_TMP AND ERE1.NR_SEQ_TEMP_CONTEUDO = TP.CON_ELE THEN 'CONDUTA'
                        WHEN
                            ERT.NR_SEQ_TEMPLATE = TP.EVO_TMP AND ERE1.NR_SEQ_TEMP_CONTEUDO = TP.EVO_ELE THEN 'EVOLUCAO'
                        WHEN
                            ERT.NR_SEQ_TEMPLATE = TP.OBS_TMP AND ERE1.NR_SEQ_TEMP_CONTEUDO = TP.OBS_ELE THEN 'OBSERVACAO'
                        ELSE ''
                    END TP_RESULTADO
                FROM
                    EHR_REG_TEMPLATE ERT
                    INNER JOIN EHR_REGISTRO ER
                        ON (ER.NR_SEQUENCIA = ERT.NR_SEQ_REG)
                    INNER JOIN EHR_REG_ELEMENTO ERE1
                        ON (ERE1.NR_SEQ_REG_TEMPLATE = ERT.NR_SEQUENCIA)
                    INNER JOIN ATENDIMENTO_PACIENTE ATD
                        ON (ATD.NR_ATENDIMENTO = ER.NR_ATENDIMENTO)
                    INNER JOIN ATEND_CONSULTA_PEPA ACP
                        ON (ACP.NR_ATENDIMENTO = ATD.NR_ATENDIMENTO AND ACP.IE_SITUACAO = 'A')
                    INNER JOIN CLASSIFICACAO_ATENDIMENTO CL
                        ON(CL.NR_SEQUENCIA = ATD.NR_SEQ_CLASSIFICACAO)
                    INNER JOIN PESSOA_FISICA M
                        ON (M.CD_PESSOA_FISICA = ATD.CD_MEDICO_ATENDIMENTO)
                    INNER JOIN XMLTABLE('/doTasyParams/TREATMENTTEMPLATES/*[starts-with(local-name(), ''i'')]'
                                        PASSING XMLTYPE(XML_CLOB)
                                        COLUMNS
                                            REF_TMP VARCHAR2(15) PATH 'local-name()',
                                            EVO_TMP VARCHAR2(15) PATH 'evolution/template',
                                            EVO_ELE VARCHAR2(15) PATH 'evolution/element',
                                            IMP_TMP VARCHAR2(15) PATH 'impression/template',
                                            IMP_ELE VARCHAR2(15) PATH 'impression/element',
                                            CON_TMP VARCHAR2(15) PATH 'conduct/template',
                                            CON_ELE VARCHAR2(15) PATH 'conduct/element',
                                            OBS_TMP VARCHAR2(15) PATH 'observation/template',
                                            OBS_ELE VARCHAR2(15) PATH 'observation/element') TP
                        ON ( REPLACE(TP.REF_TMP,'i') = TO_CHAR(ACP.NR_SEQ_TIPO_CONSULTA) )
                WHERE
                    (
                        (TO_CHAR(ERT.NR_SEQ_TEMPLATE) = TP.IMP_TMP
                        AND TO_CHAR(ERE1.NR_SEQ_TEMP_CONTEUDO) = TP.IMP_ELE)
                        OR
                        (TO_CHAR(ERT.NR_SEQ_TEMPLATE) = TP.CON_TMP
                        AND TO_CHAR(ERE1.NR_SEQ_TEMP_CONTEUDO) = TP.CON_ELE)
                        OR
                        (TO_CHAR(ERT.NR_SEQ_TEMPLATE) = TP.EVO_TMP
                        AND TO_CHAR(ERE1.NR_SEQ_TEMP_CONTEUDO) = TP.EVO_ELE)
                        OR
                        (TO_CHAR(ERT.NR_SEQ_TEMPLATE) = TP.OBS_TMP
                        AND TO_CHAR(ERE1.NR_SEQ_TEMP_CONTEUDO) = TP.OBS_ELE)
                    )
                    AND ER.CD_PACIENTE = PATIENT_ID_P
                    AND (ATD.CD_MEDICO_ATENDIMENTO = PHYSICIAN_ID_P OR PHYSICIAN_ID_P IS NULL)
                    AND ER.DT_LIBERACAO IS NOT NULL
                    AND ER.DT_INATIVACAO IS NULL
            )
            GROUP BY
                NR_ATENDIMENTO,
                CD_PACIENTE,
                DT_ENTRADA,
                DS_CLASSIFICACAO,
                CD_MEDICO,
                NM_MEDICO,
                CD_ESPECIALIDADE_MED,
                NR_CONTATO_MED,
                DS_ESPECIALIDADE_MED,
                TP_RESULTADO
            ORDER BY
                NR_ATENDIMENTO,DT_ATUALIZACAO;

    END GET_TREATMENT_APPOINTMENTS;

    PROCEDURE GET_SCHEDULE_SPECIALTIES(NAME_P IN VARCHAR2,PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            DISTINCT
            E.CD_ESPECIALIDADE,
            E.DS_ESPECIALIDADE,
            'S' IE_PRINCIPAL
        FROM
            ESPECIALIDADE_MEDICA E
            INNER JOIN AGENDA A
               ON (E.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE AND A.IE_SITUACAO = 'A')
        WHERE
            (UPPER(E.DS_ESPECIALIDADE) LIKE REPLACE(UPPER(NAME_P),'~','%') OR NAME_P IS NULL)
        ORDER BY
            E.DS_ESPECIALIDADE ASC;

    END GET_SCHEDULE_SPECIALTIES;

    PROCEDURE GET_SCHEDULE_CARE_UNITS(PHYSICIAN_P IN VARCHAR2,PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHSSECTORS_ARR OC_PARAM_ARR;

    BEGIN

    T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);
    T_BRANCHSSECTORS_ARR := LPARAM('BRANCHSSECTORS',PARAMS_P);

    OPEN P_RECORDSET FOR

        SELECT
            CASE
            WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
            ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
            END CD_ESTABELECIMENTO,
            PJ.CD_CGC,
            ES.NM_FANTASIA_ESTAB,
            PJ.DS_ENDERECO,
            PJ.DS_BAIRRO,
            PJ.DS_COMPLEMENTO,
            PJ.NR_ENDERECO,
            PJ.DS_MUNICIPIO,
            PJ.SG_ESTADO,
            PJ.CD_CEP,
            CASE
            WHEN PHYSICIAN_P IS NULL THEN NULL
            WHEN COUNT(
                    CASE
                    WHEN (A.IE_APRES_AGENDA_EXTERNO = 'S'
                        AND (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)))
                        AND (A.CD_SETOR_AGENDA IS NOT NULL OR A.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))) THEN 1
                    ELSE NULL
                    END
                )>0 THEN 'S'
            ELSE 'N'
            END IE_AGENDA_ONLINE
        FROM
            TASY.ESTABELECIMENTO E
            INNER JOIN TASY.AGENDA A
                ON (A.CD_ESTABELECIMENTO = E.CD_ESTABELECIMENTO AND A.IE_SITUACAO = 'A')
            INNER JOIN TASY.ESTABELECIMENTO ES
                ON (ES.CD_ESTABELECIMENTO = CASE
                    WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                    ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                    END
                )
            INNER JOIN TASY.PESSOA_JURIDICA PJ
                ON (PJ.CD_CGC = ES.CD_CGC)
        WHERE
            (A.CD_PESSOA_FISICA = PHYSICIAN_P OR PHYSICIAN_P IS NULL)
            AND A.CD_TIPO_AGENDA = 3
            AND PJ.NM_FANTASIA IS NOT NULL
        GROUP BY
            A.CD_SETOR_AGENDA,
            E.CD_ESTABELECIMENTO,
            PJ.CD_CGC,
            ES.NM_FANTASIA_ESTAB,
            PJ.DS_ENDERECO,
            PJ.DS_BAIRRO,
            PJ.DS_COMPLEMENTO,
            PJ.NR_ENDERECO,
            PJ.DS_MUNICIPIO,
            PJ.SG_ESTADO,
            PJ.CD_CEP
        ORDER BY
            NM_FANTASIA_ESTAB ASC;

    END GET_SCHEDULE_CARE_UNITS;

    PROCEDURE GET_SCHEDULE_PHYSICIANS(
        NAME_P IN VARCHAR2,
        SPECIALTY_P IN VARCHAR2,
        CDESTABELECIMENTO_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHSSECTORS_ARR OC_PARAM_ARR;
    T_UNIDADE VARCHAR2(50);
    T_SETOR VARCHAR2(50);

    BEGIN

        T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);
        T_BRANCHSSECTORS_ARR := LPARAM('BRANCHSSECTORS',PARAMS_P);
        T_UNIDADE := TRIM(REGEXP_SUBSTR(CDESTABELECIMENTO_P,'[^.]+',1,1) );
        T_SETOR := TRIM(REGEXP_SUBSTR(CDESTABELECIMENTO_P,'[^.]+',1,2) );

        IF CDESTABELECIMENTO_P IS NULL THEN

            OPEN P_RECORDSET FOR
                SELECT
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    (M.UF_CRM||M.NR_CRM) NR_CRM,
                    PF.NR_IDENTIDADE,
                    TO_CHAR(PF.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                    PF.IE_SEXO,
                    'N' IE_ULTIMO,
                    COUNT(
                        CASE
                        WHEN (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)) AND A.IE_APRES_AGENDA_EXTERNO = 'S') THEN 1
                        ELSE NULL
                        END
                    ) QT_AGENDA_LIB,
                    (
                        SELECT
                            LISTAGG(SE.CD_ESPECIALIDADE,',') WITHIN GROUP (ORDER BY SE.CD_ESPECIALIDADE)
                        FROM
                        (
                            SELECT
                                DISTINCT
                                AE.CD_ESPECIALIDADE
                            FROM
                                AGENDA AE
                            WHERE
                                AE.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA
                                AND AE.IE_SITUACAO = 'A'
                        ) SE
                    ) LISTA_ESPEC,
                    (
                        SELECT
                            LISTAGG(
                                CASE
                                    WHEN AE.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(AE.CD_ESTABELECIMENTO)||'.'||'0000'
                                    ELSE TO_CHAR(AE.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(AE.CD_SETOR_AGENDA),4,'0')
                                END,','
                            )
                            WITHIN GROUP
                            (ORDER BY AE.CD_ESTABELECIMENTO)
                        FROM
                            AGENDA AE
                        WHERE
                            AE.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA
                            AND AE.IE_SITUACAO = 'A'                            
                            AND AE.CD_TIPO_AGENDA = 3
                    ) LISTA_ESTAB
                FROM
                    MEDICO M
                    INNER JOIN PESSOA_FISICA PF
                        ON (PF.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                    INNER JOIN AGENDA A
                        ON (A.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND A.IE_SITUACAO = 'A')
                    LEFT JOIN MEDICO_ESPECIALIDADE ME
                        ON (ME.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
                WHERE
                    (UPPER(PF.NM_PESSOA_FISICA) LIKE REPLACE(UPPER(NAME_P),'~','%') OR NAME_P IS NULL)
                    AND (ME.CD_ESPECIALIDADE = SPECIALTY_P OR SPECIALTY_P IS NULL)                    
                    AND A.CD_AGENDA IS NOT NULL
                    AND M.IE_SITUACAO = 'A'
                GROUP BY
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    M.UF_CRM,
                    M.NR_CRM,
                    PF.NR_IDENTIDADE,
                    PF.DT_NASCIMENTO,
                    PF.IE_SEXO
                ORDER BY
                    PF.NM_PESSOA_FISICA ASC;
        ELSE
            OPEN P_RECORDSET FOR
                SELECT
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    (M.UF_CRM||M.NR_CRM) NR_CRM,
                    PF.NR_IDENTIDADE,
                    TO_CHAR(PF.DT_NASCIMENTO,'YYYY-MM-DD') DT_NASCIMENTO,
                    PF.IE_SEXO,
                    'N' IE_ULTIMO,
                    COUNT(
                        CASE
                        WHEN (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)) AND A.IE_APRES_AGENDA_EXTERNO = 'S') THEN 1
                        ELSE NULL
                        END
                    ) QT_AGENDA_LIB,
                    (
                        SELECT
                            LISTAGG(SE.CD_ESPECIALIDADE,',') WITHIN GROUP (ORDER BY SE.CD_ESPECIALIDADE)
                        FROM
                        (
                            SELECT
                                DISTINCT
                                AE.CD_ESPECIALIDADE
                            FROM
                                AGENDA AE
                            WHERE
                                AE.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA
                                AND AE.IE_SITUACAO = 'A'
                        ) SE
                    ) LISTA_ESPEC,
                    (
                        SELECT
                            LISTAGG(
                                CASE
                                    WHEN AE.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(AE.CD_ESTABELECIMENTO)||'.'||'0000'
                                    ELSE TO_CHAR(AE.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(AE.CD_SETOR_AGENDA),4,'0')
                                END,','
                            )
                            WITHIN GROUP
                            (ORDER BY AE.CD_ESTABELECIMENTO)
                        FROM
                            AGENDA AE
                        WHERE
                            AE.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA
                            AND AE.IE_SITUACAO = 'A'                            
                            AND AE.CD_TIPO_AGENDA = 3
                    ) LISTA_ESTAB
                FROM
                    MEDICO M
                    INNER JOIN PESSOA_FISICA PF
                        ON (PF.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                    INNER JOIN AGENDA A
                        ON (A.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA AND A.IE_SITUACAO = 'A')
                    LEFT JOIN MEDICO_ESPECIALIDADE ME
                        ON (ME.CD_PESSOA_FISICA = PF.CD_PESSOA_FISICA)
                WHERE
                    (UPPER(PF.NM_PESSOA_FISICA) LIKE REPLACE(UPPER(NAME_P),'~','%') OR NAME_P IS NULL)
                    AND (ME.CD_ESPECIALIDADE = SPECIALTY_P OR SPECIALTY_P IS NULL)
                    AND A.CD_AGENDA IS NOT NULL                    
                    AND M.IE_SITUACAO = 'A'
                GROUP BY
                    PF.CD_PESSOA_FISICA,
                    PF.NM_PESSOA_FISICA,
                    PF.NR_CPF,
                    M.UF_CRM,
                    M.NR_CRM,
                    PF.NR_IDENTIDADE,
                    PF.DT_NASCIMENTO,
                    PF.IE_SEXO
                ORDER BY
                    PF.NM_PESSOA_FISICA ASC;
        END IF;

    END GET_SCHEDULE_PHYSICIANS;

    PROCEDURE GET_SCHEDULE_UNIT_SPECIALTIES(PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            E.CD_ESPECIALIDADE,
            E.DS_ESPECIALIDADE,
            A.CD_PESSOA_FISICA CD_MEDICO,
            CASE
            WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(A.CD_ESTABELECIMENTO)||'.'||'0000'
            ELSE TO_CHAR(A.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
            END CD_ESTABELECIMENTO,
            CASE
            WHEN A.IE_APRES_AGENDA_EXTERNO = 'S'
                AND (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))) THEN 'S'
            ELSE 'N'
            END IE_AGENDA_ONLINE
        FROM
            ESPECIALIDADE_MEDICA E
            INNER JOIN AGENDA A
               ON (E.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE AND A.IE_SITUACAO = 'A')
        WHERE
            A.IE_SITUACAO = 'A'
            AND A.CD_PESSOA_FISICA IS NOT NULL
        ORDER BY
            CD_MEDICO ASC,
            CD_ESTABELECIMENTO ASC;

    END GET_SCHEDULE_UNIT_SPECIALTIES;

    PROCEDURE GET_SCHEDULE_PARAMS(
        CD_MEDICO_P IN VARCHAR2,
        CD_PESSOA_FISICA_P IN VARCHAR2,
        CD_CONVENIO_P IN VARCHAR2,
        CD_CATEGORIA_P IN VARCHAR2,
        CD_PLANO_P IN VARCHAR2,
        CD_ESTABELECIMENTO_P IN VARCHAR2,
        PARAMS_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    CD_ESTAB_T VARCHAR2(250);
    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHSSECTORS_ARR OC_PARAM_ARR;
    T_UNIDADE VARCHAR2(50);
    T_SETOR VARCHAR2(50);
    T_QTD_SLOTS_P NUMBER(1);

    BEGIN

        T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);
        T_BRANCHSSECTORS_ARR := LPARAM('BRANCHSSECTORS',PARAMS_P);

        CD_ESTAB_T := CD_ESTABELECIMENTO_P;

        T_UNIDADE := TRIM(REGEXP_SUBSTR(CD_ESTAB_T,'[^.]+',1,1));
        T_SETOR := TRIM(REGEXP_SUBSTR(CD_ESTAB_T,'[^.]+',1,2));

        T_QTD_SLOTS_P := NVL(NPARAM('SLOTSQTD',PARAMS_P),1);

        OPEN P_RECORDSET FOR

            SELECT
                S.*,
                (TRUNC(SYSDATE)-TRUNC(ACU.DT_AGENDA)) QT_DIAS_ULT_CONSULTA,
                ACU.IE_CLASSIF_AGENDA IE_CLASSIF_ULT_CONSULTA,
                (TRUNC(SYSDATE)-TRUNC(ACUA.DT_AGENDA)) QT_DIAS_ULT_AGENDADA,
                CASE
                WHEN S.NR_SEQ_ULTIMA_CONSULTA IS NULL THEN 'S'
                ELSE 'N'
                END IE_PRIMEIRA,
                CASE
                WHEN CD_DOENCA_PAC IS NULL THEN 'OTHER'
                ELSE 'ONCO'
                END IE_TIPO_CLINICO
            FROM
            (
                SELECT
                    A.CD_AGENDA,
                    A.DS_AGENDA,
                    A.IE_SEXO_AGENDA,
                    M.CD_PESSOA_FISICA CD_MEDICO,
                    OBTER_NOME_MEDICO(M.CD_PESSOA_FISICA,'g') NM_MEDICO,
                    CASE
                      WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
                      ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
                    END CD_ESTABELECIMENTO,
                    PJ.NM_FANTASIA NM_FANTASIA_ESTAB,
                    EM.CD_ESPECIALIDADE,
                    EM.DS_ESPECIALIDADE,
                    (
                        SELECT
                            MAX(CD_DOENCA_CID)
                        FROM
                            CAN_LOCO_REGIONAL
                        WHERE
                            IE_SITUACAO = 'A'
                            AND DT_LIBERACAO IS NOT NULL
                            AND CD_DOENCA_CID LIKE 'C%'
                            AND CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                    ) CD_DOENCA_PAC,
                    (
                        SELECT
                            MAX(NR_SEQUENCIA)
                        FROM
                            AGENDA_CONSULTA
                        WHERE
                            CD_AGENDA = A.CD_AGENDA
                            AND IE_STATUS_AGENDA IN ('E','AD')
                            AND CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                    ) NR_SEQ_ULTIMA,
                    (
                        SELECT
                            MAX(ACM.NR_SEQUENCIA)
                        FROM
                            AGENDA_CONSULTA ACM
                        LEFT JOIN AGENDA AM
                            ON (ACM.CD_AGENDA = AM.CD_AGENDA)
                        WHERE
                            AM.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA
                            AND ACM.IE_STATUS_AGENDA IN ('E','AD')
                            AND ACM.CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                    ) NR_SEQ_ULTIMA_CONSULTA,
                    (
                        SELECT
                            MAX(NR_SEQUENCIA)
                        FROM
                            AGENDA_CONSULTA
                        WHERE
                            CD_AGENDA = A.CD_AGENDA
                            AND IE_STATUS_AGENDA NOT IN ('C')
                            AND CD_PESSOA_FISICA = CD_PESSOA_FISICA_P
                    ) NR_SEQ_ULTIMA_AGENDADA,
                    TRUNC(MONTHS_BETWEEN(SYSDATE,PF.DT_NASCIMENTO) / 12) QT_IDADE_PAC,
                    PF.IE_SEXO IE_SEXO_PAC,
                    M.NR_CRM||M.UF_CRM NM_CRM
                FROM
                    AGENDA A
                    INNER JOIN MEDICO M
                        ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                    INNER JOIN PESSOA_FISICA M
                        ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                    INNER JOIN PESSOA_FISICA PF
                        ON (PF.CD_PESSOA_FISICA = CD_PESSOA_FISICA_P)
                    INNER JOIN ESTABELECIMENTO E
                        ON (E.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO)
                    INNER JOIN ESPECIALIDADE_MEDICA EM
                        ON (EM.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE)
                    INNER JOIN TASY.ESTABELECIMENTO ES
                        ON (ES.CD_ESTABELECIMENTO = CASE
                                                      WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                                                      ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                                                    END
                                                    )
                    INNER JOIN TASY.PESSOA_JURIDICA PJ
                        ON (PJ.CD_CGC = ES.CD_CGC)
                WHERE
                    M.CD_PESSOA_FISICA = CD_MEDICO_P
                    AND A.IE_APRES_AGENDA_EXTERNO = 'S'
                    AND
                    (
                        CD_CONVENIO_P IN (SELECT CD_CONVENIO FROM REGRA_LIB_CONV_AGENDA WHERE CD_AGENDA = A.CD_AGENDA)
                        OR
                        (SELECT COUNT(CD_CONVENIO) FROM REGRA_LIB_CONV_AGENDA WHERE CD_AGENDA = A.CD_AGENDA) = 0
                    )
                    AND A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
                    AND
                    (
                          (A.CD_ESTABELECIMENTO = TO_NUMBER(T_UNIDADE) AND A.CD_SETOR_AGENDA = TO_NUMBER(T_SETOR) AND TO_NUMBER(T_UNIDADE) IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))
                           OR
                          (A.CD_ESTABELECIMENTO = TO_NUMBER(T_UNIDADE) AND T_SETOR = '0000' AND TO_NUMBER(T_UNIDADE) NOT IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))
                    )
            ) S LEFT JOIN AGENDA_CONSULTA ACU
                    ON (ACU.NR_SEQUENCIA = S.NR_SEQ_ULTIMA)
                LEFT JOIN AGENDA_CONSULTA ACUA
                    ON (ACUA.NR_SEQUENCIA = S.NR_SEQ_ULTIMA_AGENDADA)
            WHERE
                ((S.IE_SEXO_AGENDA = S.IE_SEXO_PAC) OR S.IE_SEXO_AGENDA = 'A');

    END GET_SCHEDULE_PARAMS;
    
    PROCEDURE GET_PHYSICIAN_CARE_UNITS(PHYSICIAN_P IN VARCHAR2,PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHSSECTORS_ARR OC_PARAM_ARR;

    BEGIN

    T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);
    T_BRANCHSSECTORS_ARR := LPARAM('BRANCHSSECTORS',PARAMS_P);

    OPEN P_RECORDSET FOR

        SELECT
            CASE
            WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
            ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
            END CD_ESTABELECIMENTO,
            E.CD_ESTABELECIMENTO CD_ESTAB_ORIGEM,
            PJ.CD_CGC,
            ES.NM_FANTASIA_ESTAB,
            PJ.DS_ENDERECO,
            PJ.DS_BAIRRO,
            PJ.DS_COMPLEMENTO,
            PJ.NR_ENDERECO,
            PJ.DS_MUNICIPIO,
            PJ.SG_ESTADO,
            PJ.CD_CEP,
            CASE
            WHEN PHYSICIAN_P IS NULL THEN NULL
            WHEN COUNT(
                    CASE
                    WHEN (A.IE_APRES_AGENDA_EXTERNO = 'S'
                        AND (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)))
                        AND (A.CD_SETOR_AGENDA IS NOT NULL OR A.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))) THEN 1
                    ELSE NULL
                    END
                )>0 THEN 'S'
            ELSE 'N'
            END IE_AGENDA_ONLINE,
            CASE
            WHEN COUNT(
                CASE
                WHEN A.CD_PESSOA_FISICA = PHYSICIAN_P THEN 1
                ELSE NULL
                END
            )>0 THEN 'S'
            ELSE 'N'
            END IE_TITULAR
        FROM
            TASY.ESTABELECIMENTO E
            INNER JOIN TASY.AGENDA A
                ON (A.CD_ESTABELECIMENTO = E.CD_ESTABELECIMENTO AND A.IE_SITUACAO = 'A')
            INNER JOIN TASY.ESTABELECIMENTO ES
                ON (ES.CD_ESTABELECIMENTO = CASE
                    WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                    ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                    END
                )
            INNER JOIN TASY.PESSOA_JURIDICA PJ
                ON (PJ.CD_CGC = ES.CD_CGC)
            INNER JOIN MED_PERMISSAO MP
                ON ( (MP.CD_AGENDA = A.CD_AGENDA AND MP.CD_PESSOA_FISICA = PHYSICIAN_P) OR MP.NR_SEQUENCIA = '260')
        WHERE
            (
                PHYSICIAN_P IS NULL
                OR (A.CD_PESSOA_FISICA = PHYSICIAN_P)
                OR (MP.CD_PESSOA_FISICA = PHYSICIAN_P)
            )
            AND A.CD_TIPO_AGENDA = 3
            AND PJ.NM_FANTASIA IS NOT NULL
        GROUP BY
            A.CD_SETOR_AGENDA,
            E.CD_ESTABELECIMENTO,
            PJ.CD_CGC,
            ES.NM_FANTASIA_ESTAB,
            PJ.DS_ENDERECO,
            PJ.DS_BAIRRO,
            PJ.DS_COMPLEMENTO,
            PJ.NR_ENDERECO,
            PJ.DS_MUNICIPIO,
            PJ.SG_ESTADO,
            PJ.CD_CEP
        ORDER BY
            NM_FANTASIA_ESTAB ASC;

    END GET_PHYSICIAN_CARE_UNITS;
    
    PROCEDURE GET_PATIENT_CARE_UNITS(IDENTIFIER_P IN VARCHAR2,PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    
    T_BRANCHS_ARR OC_PARAM_ARR;
    T_BRANCHSSECTORS_ARR OC_PARAM_ARR;
    
    BEGIN
    
    T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);
    T_BRANCHSSECTORS_ARR := LPARAM('BRANCHSSECTORS',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            CASE
            WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
            ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
            END CD_ESTABELECIMENTO,
            E.CD_ESTABELECIMENTO CD_ESTAB_ORIGEM,
            PJ.CD_CGC,
            ES.NM_FANTASIA_ESTAB,
            PJ.DS_ENDERECO,
            PJ.DS_BAIRRO,
            PJ.DS_COMPLEMENTO,
            PJ.NR_ENDERECO,
            PJ.DS_MUNICIPIO,
            PJ.SG_ESTADO,
            PJ.CD_CEP,
            CASE
            WHEN IDENTIFIER_P IS NULL THEN NULL
            WHEN COUNT(
                    CASE
                    WHEN (A.IE_APRES_AGENDA_EXTERNO = 'S'
                        AND (A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR)))
                        AND (A.CD_SETOR_AGENDA IS NOT NULL OR A.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHSSECTORS_ARR)))) THEN 1
                    ELSE NULL
                    END
                )>0 THEN 'S'
            ELSE 'N'
            END IE_AGENDA_ONLINE
        FROM
            TASY.ESTABELECIMENTO E
            INNER JOIN TASY.AGENDA A
                ON (A.CD_ESTABELECIMENTO = E.CD_ESTABELECIMENTO AND A.IE_SITUACAO = 'A' AND A.CD_TIPO_AGENDA = 3)
            INNER JOIN TASY.AGENDA_CONSULTA AC
                ON (AC.CD_AGENDA = A.CD_AGENDA AND AC.CD_PESSOA_FISICA = IDENTIFIER_P)
            INNER JOIN TASY.ESTABELECIMENTO ES
                ON (ES.CD_ESTABELECIMENTO = CASE
                    WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                    ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                    END
                )
            INNER JOIN TASY.PESSOA_JURIDICA PJ
                ON (PJ.CD_CGC = ES.CD_CGC AND PJ.NM_FANTASIA IS NOT NULL)
        GROUP BY
            A.CD_SETOR_AGENDA,
            E.CD_ESTABELECIMENTO,
            PJ.CD_CGC,
            ES.NM_FANTASIA_ESTAB,
            PJ.DS_ENDERECO,
            PJ.DS_BAIRRO,
            PJ.DS_COMPLEMENTO,
            PJ.NR_ENDERECO,
            PJ.DS_MUNICIPIO,
            PJ.SG_ESTADO,
            PJ.CD_CEP
        ORDER BY
            NM_FANTASIA_ESTAB ASC;

    END GET_PATIENT_CARE_UNITS;
    
    PROCEDURE GET_DISEASES(FILTER_P IN VARCHAR2,OFFSET_P IN NUMBER,LIMIT_P IN NUMBER,PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    
    T_DISEASES_ARR OC_PARAM_ARR;
    
    BEGIN
    
    T_DISEASES_ARR := LPARAM('DISEASES',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            SS.*
        FROM
        (
            SELECT
                S.*,
                ROWNUM RNUM
            FROM
            (
                SELECT
                    CD.CD_CATEGORIA CD_DOENCA_CID,
                    CD.DS_CATEGORIA DS_DOENCA_CID,
                    '' CD_CATEGORIA_CID
                FROM
                    CIDO_CATEGORIA CD
                WHERE
                    (
                        FILTER_P IS NULL
                        OR UPPER(REPLACE(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(CD.DS_CATEGORIA, 'nls_sort=binary_ai')),CHR(0))) LIKE UPPER(REPLACE(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(REPLACE(FILTER_P,'~','%'),'nls_sort=binary_ai')),CHR(0)))
                    )
                    AND ( CD.CD_CATEGORIA IN (SELECT * FROM TABLE(T_DISEASES_ARR)) OR (SELECT COUNT(*) FROM TABLE(T_DISEASES_ARR))=0 )
                    AND CD.IE_SITUACAO = 'A'
                ORDER BY
                    CD.CD_CATEGORIA ASC
            ) S
        ) SS
        WHERE
            RNUM > OFFSET_P AND RNUM <= OFFSET_P+LIMIT_P;

    END GET_DISEASES;
    
    PROCEDURE GET_TREATMENT_PROTOCOLS(DISEASE_P IN VARCHAR2,FILTER_P IN VARCHAR2,OFFSET_P IN NUMBER,LIMIT_P IN NUMBER,PARAMS_P IN VARCHAR2,P_RECORDSET OUT SYS_REFCURSOR) AS
    
    T_DISEASES_ARR OC_PARAM_ARR;
    
    BEGIN
    
    T_DISEASES_ARR := LPARAM('DISEASES',PARAMS_P);

    OPEN P_RECORDSET FOR
        SELECT
            SS.*
        FROM
        (
            SELECT
                S.*,
                ROWNUM RNUM
            FROM
            (
                SELECT
                    P.CD_PROTOCOLO||'.'||PM.NR_SEQUENCIA CD_PROTOCOLO,
                    P.NM_PROTOCOLO,
                    GETLONG( 'PROTOCOLO', 'DS_PROTOCOLO', 'CD_PROTOCOLO', P.CD_PROTOCOLO ) DS_PROTOCOLO,
                    PM.NR_SEQUENCIA NR_SEQ_MED,
                    PM.NM_MEDICACAO
                FROM
                    PROTOCOLO P
                    INNER JOIN PROTOCOLO_MEDICACAO PM
                        ON (PM.CD_PROTOCOLO = P.CD_PROTOCOLO AND PM.IE_SITUACAO = 'A')
                WHERE
                    P.CD_PROTOCOLO IN (
                        SELECT
                            DISTINCT
                            P.CD_PROTOCOLO                            
                        FROM
                            PROTOCOLO P
                            INNER JOIN PROT_MEDIC_CAT_TOPOGRAFICA PT
                                ON (PT.CD_PROTOCOLO = P.CD_PROTOCOLO)                            
                        WHERE
                            (
                                FILTER_P IS NULL
                                OR UPPER(REPLACE(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(P.NM_PROTOCOLO, 'nls_sort=binary_ai')),CHR(0))) LIKE UPPER(REPLACE(UTL_RAW.CAST_TO_VARCHAR2(NLSSORT(REPLACE(FILTER_P,'~','%'),'nls_sort=binary_ai')),CHR(0)))
                            )
                            AND (PT.CD_CATEGORIA = DISEASE_P OR DISEASE_P IS NULL)
                            AND (PT.CD_CATEGORIA IN (SELECT * FROM TABLE(T_DISEASES_ARR)) OR (SELECT COUNT(*) FROM TABLE(T_DISEASES_ARR))=0) 
                            AND P.IE_SITUACAO = 'A'
                    )
                ORDER BY
                    P.NM_PROTOCOLO ASC,
                    PM.NR_SEQUENCIA ASC
            ) S
        ) SS
        WHERE
            RNUM > OFFSET_P AND RNUM <= OFFSET_P+LIMIT_P;

    END GET_TREATMENT_PROTOCOLS;
    
   PROCEDURE GET_PATIENT_APPOINTMENTS_RES(
        PARAMS_P IN VARCHAR2,
        START_DATE_P IN VARCHAR2,
        END_DATE_P IN VARCHAR2,
        IDENTIFIER_P IN VARCHAR2,
        HISTORY_P IN VARCHAR2,
        APPOINTMENTID_P IN VARCHAR2,
        P_RECORDSET OUT SYS_REFCURSOR) AS

    T_BRANCHS_ARR OC_PARAM_ARR;

    BEGIN

      T_BRANCHS_ARR := LPARAM('BRANCHS',PARAMS_P);

      OPEN P_RECORDSET FOR
        SELECT
            *
        FROM
        (
            SELECT
                *
            FROM
            (
                SELECT
                    AC.NR_SEQUENCIA,
                    A.CD_AGENDA,
                    A.DS_AGENDA,
                    M.CD_PESSOA_FISICA CD_MEDICO,
                    M.NM_PESSOA_FISICA NM_MEDICO,
                    CASE
                        WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
                        ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
                      END CD_ESTABELECIMENTO,
                      PJ.NM_FANTASIA NM_FANTASIA_ESTAB,
                    EM.CD_ESPECIALIDADE,
                    EM.DS_ESPECIALIDADE,
                    P.CD_PESSOA_FISICA CD_PACIENTE,
                    P.NM_PESSOA_FISICA NM_PACIENTE,
                    C.CD_CONVENIO,
                    C.DS_CONVENIO,
                    CC.CD_CATEGORIA,
                    CC.DS_CATEGORIA,
                    CP.CD_PLANO,
                    CP.DS_PLANO,
                    TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                    AC.DT_AGENDA DT_AGENDA_TS,
                    TO_CHAR(AC.DT_AGENDAMENTO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                    AC.IE_STATUS_AGENDA,
                    OBTER_VALOR_DOMINIO(83,AC.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                    AP.IE_TIPO_ATENDIMENTO,
                    OBTER_VALOR_DOMINIO(12,AP.IE_TIPO_ATENDIMENTO) NM_TIPO_ATENDIMENTO,
                    A.IE_TIPO_AGENDA_CONSULTA IE_TIPO_AGENDA,
                    DECODE(A.IE_TIPO_AGENDA_CONSULTA,
                            'C','Consulta',
                            'A','Consulta',
                            'S','Servi?o',NULL) NM_TIPO_AGENDA,
                    AC.IE_CLASSIF_AGENDA,
                    ACL.DS_CLASSIFICACAO NM_CLASSIF_AGENDA,
                    A.DS_COMPLEMENTO DS_OBSERVACAO,
                    AMC.CD_MOTIVO CD_MOTIVO_CANCEL,
                    AMC.DS_MOTIVO NM_MOTIVO_CANCEL,
                    MED.NR_CRM,
                    MED.UF_CRM,
                    AC.DT_ATUALIZACAO
                FROM
                    AGENDA A
                    INNER JOIN AGENDA_CONSULTA AC
                        ON (AC.CD_AGENDA = A.CD_AGENDA AND AC.DT_AGENDA IN (
                            SELECT
                            MIN(AC1.DT_AGENDA)
                            FROM
                            AGENDA_CONSULTA AC1
                            WHERE
                            AC1.CD_AGENDA = A.CD_AGENDA
                            AND AC1.IE_STATUS_AGENDA NOT IN ('L')
                            AND AC1.CD_PESSOA_FISICA = IDENTIFIER_P
                            AND TRUNC(AC1.DT_AGENDA) = TRUNC(AC.DT_AGENDA)
                            GROUP BY
                            AC1.DT_AGENDAMENTO
                        ))
                    INNER JOIN PESSOA_FISICA P
                        ON (P.CD_PESSOA_FISICA = AC.CD_PESSOA_FISICA)
                    INNER JOIN PESSOA_FISICA M
                        ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)                    
                    INNER JOIN MEDICO MED
                        ON (M.CD_PESSOA_FISICA = MED.CD_PESSOA_FISICA)    
                    INNER JOIN ESTABELECIMENTO E
                        ON (E.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO)
                    INNER JOIN ESPECIALIDADE_MEDICA EM
                        ON (EM.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE)
                    LEFT JOIN AGENDA_CLASSIF ACL
                        ON (ACL.CD_CLASSIFICACAO = AC.IE_CLASSIF_AGENDA AND ACL.IE_SITUACAO = 'A')
                    LEFT JOIN AGENDA_MOTIVO_CANCELAMENTO AMC
                        ON (AMC.CD_MOTIVO = AC.CD_MOTIVO_CANCELAMENTO
                                AND (AMC.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO OR AMC.CD_ESTABELECIMENTO IS NULL)
                                AND AMC.IE_AGENDA = 'C' AND AMC.IE_SITUACAO = 'A')
                    LEFT JOIN ATENDIMENTO_PACIENTE AP
                        ON (AP.NR_ATENDIMENTO = AC.NR_ATENDIMENTO)
                    LEFT JOIN CONVENIO C
                        ON (C.CD_CONVENIO = AC.CD_CONVENIO)
                    LEFT JOIN CATEGORIA_CONVENIO CC
                        ON (CC.CD_CATEGORIA = AC.CD_CATEGORIA AND CC.CD_CONVENIO = C.CD_CONVENIO)
                    LEFT JOIN CONVENIO_PLANO CP
                        ON (CP.CD_PLANO = AC.CD_PLANO AND CP.CD_CONVENIO = C.CD_CONVENIO)
                    INNER JOIN TASY.ESTABELECIMENTO ES
                        ON (ES.CD_ESTABELECIMENTO = CASE
                                                      WHEN A.CD_SETOR_AGENDA IS NULL THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                                                      ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                                                    END
                                                    )
                      INNER JOIN TASY.PESSOA_JURIDICA PJ
                        ON (PJ.CD_CGC = ES.CD_CGC)
                WHERE
                    (AC.CD_PESSOA_FISICA = IDENTIFIER_P
                    AND AC.IE_STATUS_AGENDA NOT IN ('L')
                    AND (
                         (AC.DT_AGENDA BETWEEN TO_DATE(START_DATE_P,'YYYY-MM-DD HH24:MI:SS') AND TO_DATE(END_DATE_P,'YYYY-MM-DD HH24:MI:SS')) 
                         OR 
                         (START_DATE_P IS NULL AND END_DATE_P IS NULL)
                         )
                    AND A.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
                    AND APPOINTMENTID_P IS NULL)                    
                    OR
                    (AC.NR_SEQUENCIA = APPOINTMENTID_P)
                UNION ALL
                SELECT
                    AQ.NR_SEQUENCIA,
                    NULL CD_AGENDA,
                    NULL DS_AGENDA,
                    M.CD_PESSOA_FISICA CD_MEDICO,
                    M.NM_PESSOA_FISICA NM_MEDICO,
                    TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000' CD_ESTABELECIMENTO,
                    E.NM_FANTASIA_ESTAB,
                    EM.CD_ESPECIALIDADE,
                    EM.DS_ESPECIALIDADE,
                    P.CD_PESSOA_FISICA CD_PACIENTE,
                    P.NM_PESSOA_FISICA NM_PACIENTE,
                    NULL CD_CONVENIO,
                    NULL DS_CONVENIO,
                    NULL CD_CATEGORIA,
                    NULL DS_CATEGORIA,
                    NULL CD_PLANO,
                    NULL DS_PLANO,
                    TO_CHAR(AQ.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                    AQ.DT_AGENDA DT_AGENDA_TS,
                    TO_CHAR(AQ.DT_ATUALIZACAO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                    AQ.IE_STATUS_AGENDA,
                    TASY.OBTER_VALOR_DOMINIO(83,AQ.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                    NULL IE_TIPO_ATENDIMENTO,
                    NULL NM_TIPO_ATENDIMENTO,
                    AQ.IE_TIPO_PEND_AGENDA IE_TIPO_AGENDA,
                    'Tratamento' NM_TIPO_AGENDA,
                    'Q0' IE_CLASSIF_AGENDA,
                    'Quimioterapia' NM_CLASSIF_AGENDA,
                    AQM.DS_OBSERVACAO,
                    TO_CHAR(QMC.NR_SEQUENCIA) CD_MOTIVO_CANCEL,
                    QMC.DS_MOTIVO NM_MOTIVO_CANCEL,
                            MED.NR_CRM,
                    MED.UF_CRM,
                        AQ.DT_ATUALIZACAO
                FROM
                    TASY.AGENDA_QUIMIO AQ
                    LEFT JOIN TASY.AGENDA_QUIMIO_MARCACAO AQM
                        ON (AQM.NR_SEQ_ATENDIMENTO = AQ.NR_SEQ_ATENDIMENTO)
                    INNER JOIN TASY.PESSOA_FISICA P
                        ON (P.CD_PESSOA_FISICA = AQ.CD_PESSOA_FISICA)
                    INNER JOIN TASY.ESTABELECIMENTO E
                        ON (E.CD_ESTABELECIMENTO = AQ.CD_ESTABELECIMENTO)
                    INNER JOIN TASY.PESSOA_JURIDICA PJ
                        ON (PJ.CD_CGC = E.CD_CGC)
                    INNER JOIN TASY.PESSOA_FISICA M
                        ON (M.CD_PESSOA_FISICA = AQ.CD_MEDICO_RESP)
                    INNER JOIN MEDICO MED
                        ON (M.CD_PESSOA_FISICA = MED.CD_PESSOA_FISICA)    
                    LEFT JOIN TASY.MEDICO_ESPECIALIDADE ME
                        ON (ME.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                    LEFT JOIN TASY.ESPECIALIDADE_MEDICA EM
                        ON (EM.CD_ESPECIALIDADE = ME.CD_ESPECIALIDADE)
                    LEFT JOIN TASY.QT_MOTIVO_CANCELAMENTO QMC
                        ON (QMC.NR_SEQUENCIA = AQ.NR_SEQ_MOT_CANCELAMENTO AND QMC.IE_SITUACAO = 'A')                   
                WHERE
                    (AQ.CD_PESSOA_FISICA = IDENTIFIER_P    
                    AND AQ.IE_STATUS_AGENDA NOT IN ('L')
                    AND (
                         (AQ.DT_AGENDA BETWEEN TO_DATE(START_DATE_P,'YYYY-MM-DD HH24:MI:SS') AND TO_DATE(END_DATE_P,'YYYY-MM-DD HH24:MI:SS')) 
                         OR 
                         (START_DATE_P IS NULL AND END_DATE_P IS NULL)
                         )
                    AND AQ.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
                    AND APPOINTMENTID_P IS NULL)                    
                    OR
                    (AQ.NR_SEQUENCIA = APPOINTMENTID_P)
            ) S
            ORDER BY
                DT_AGENDA_TS DESC
        )
        WHERE
            (HISTORY_P = 'N' AND ROWNUM=1) OR HISTORY_P = 'S';

    END GET_PATIENT_APPOINTMENTS_RES;
    
    PROCEDURE GET_PHYSICIAN_APPOINTMENTS_RES(
          PARAMS_P IN VARCHAR2,
          START_DATE_P IN VARCHAR2,
          END_DATE_P IN VARCHAR2,
          OFFSET_P IN NUMBER,
          LIMIT_P IN NUMBER,
          TYPE_SCHEDULE_P IN VARCHAR2,
          APPOINTMENTID_P IN VARCHAR2,
          P_RECORDSET OUT SYS_REFCURSOR) AS
  
      T_PHYSICIANS_ARR OC_PARAM_ARR;
      T_CLASSIF_ARR OC_PARAM_ARR;
      T_BRANCHS_ARR OC_PARAM_ARR;
      T_BRANCHS_SECTORS_ARR OC_PARAM_ARR;
  
      BEGIN
  
      T_PHYSICIANS_ARR        := LPARAM('PHYSICIANS',PARAMS_P);    
      T_BRANCHS_ARR           := LPARAM('BRANCHS',PARAMS_P);
      T_BRANCHS_SECTORS_ARR   := LPARAM('BRANCHSSECTORS',PARAMS_P);
  
      IF UPPER(TYPE_SCHEDULE_P) = 'APPOINTMENT' THEN
  
          OPEN P_RECORDSET FOR
              SELECT
                  ROWNUM RNUM,
                  AC.NR_SEQUENCIA,
                  A.CD_AGENDA,
                  A.DS_AGENDA,
                  M.CD_PESSOA_FISICA CD_MEDICO,
                  M.NM_PESSOA_FISICA NM_MEDICO,
                  PJ.CD_CGC,
                  CASE
                  WHEN (A.CD_SETOR_AGENDA IS NULL OR E.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHS_SECTORS_ARR))) THEN TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000'
                  ELSE TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||LPAD(TO_CHAR(A.CD_SETOR_AGENDA),4,'0')
                  END CD_ESTABELECIMENTO,
                  PJ.NM_FANTASIA NM_FANTASIA_ESTAB,
                  PJ.DS_ENDERECO,
                  PJ.DS_BAIRRO,
                  PJ.DS_COMPLEMENTO,
                  PJ.NR_ENDERECO,
                  PJ.DS_MUNICIPIO,
                  PJ.SG_ESTADO,
                  PJ.CD_CEP,
                  EM.CD_ESPECIALIDADE,
                  EM.DS_ESPECIALIDADE,
                  P.CD_PESSOA_FISICA CD_PACIENTE,
                  P.NM_PESSOA_FISICA NM_PACIENTE,
                  C.CD_CONVENIO,
                  C.DS_CONVENIO,
                  CC.CD_CATEGORIA,
                  CC.DS_CATEGORIA,
                  CP.CD_PLANO,
                  CP.DS_PLANO,
                  TO_CHAR(AC.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                  TO_CHAR(AC.DT_AGENDAMENTO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                  AC.IE_STATUS_AGENDA,
                  OBTER_VALOR_DOMINIO(83,AC.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                  AP.IE_TIPO_ATENDIMENTO,
                  OBTER_VALOR_DOMINIO(12,AP.IE_TIPO_ATENDIMENTO) NM_TIPO_ATENDIMENTO,
                  A.IE_TIPO_AGENDA_CONSULTA IE_TIPO_AGENDA,
                  DECODE(A.IE_TIPO_AGENDA_CONSULTA,
                          'C','Consulta',
                          'A','Consulta',
                          'S','Servico',NULL) NM_TIPO_AGENDA,
                  AC.IE_CLASSIF_AGENDA,
                  ACL.DS_CLASSIFICACAO NM_CLASSIF_AGENDA,
                  A.DS_COMPLEMENTO DS_OBSERVACAO,
                  AMC.CD_MOTIVO CD_MOTIVO_CANCEL,
                  AMC.DS_MOTIVO NM_MOTIVO_CANCEL,
                  AC.NR_MINUTO_DURACAO,
                  MED.NR_CRM,
                  MED.UF_CRM,
                  AC.DT_ATUALIZACAO
          FROM
                  AGENDA A
                  INNER JOIN AGENDA_CONSULTA AC
                      ON (A.CD_AGENDA = AC.CD_AGENDA)
                  INNER JOIN PESSOA_FISICA P
                      ON (P.CD_PESSOA_FISICA = AC.CD_PESSOA_FISICA)
                  INNER JOIN PESSOA_FISICA M
                      ON (M.CD_PESSOA_FISICA = A.CD_PESSOA_FISICA)
                  INNER JOIN MEDICO MED
                      ON (M.CD_PESSOA_FISICA = MED.CD_PESSOA_FISICA)    
                  INNER JOIN ESTABELECIMENTO E
                      ON (E.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO)
                  INNER JOIN PESSOA_JURIDICA PJ
                      ON (PJ.CD_CGC = E.CD_CGC)
                  LEFT JOIN ESPECIALIDADE_MEDICA EM
                      ON (EM.CD_ESPECIALIDADE = A.CD_ESPECIALIDADE)
                  LEFT JOIN AGENDA_CLASSIF ACL
                      ON (ACL.CD_CLASSIFICACAO = AC.IE_CLASSIF_AGENDA AND ACL.IE_SITUACAO = 'A')
                  LEFT JOIN AGENDA_MOTIVO_CANCELAMENTO AMC
                      ON (AMC.CD_MOTIVO = AC.CD_MOTIVO_CANCELAMENTO
                              AND (AMC.CD_ESTABELECIMENTO = A.CD_ESTABELECIMENTO OR AMC.CD_ESTABELECIMENTO IS NULL)
                              AND AMC.IE_AGENDA = 'C' AND AMC.IE_SITUACAO = 'A')
                  LEFT JOIN ATENDIMENTO_PACIENTE AP
                      ON (AP.NR_ATENDIMENTO = AC.NR_ATENDIMENTO)
                  LEFT JOIN CONVENIO C
                      ON (C.CD_CONVENIO = AC.CD_CONVENIO)
                  LEFT JOIN CATEGORIA_CONVENIO CC
                      ON (CC.CD_CATEGORIA = AC.CD_CATEGORIA AND CC.CD_CONVENIO = C.CD_CONVENIO)
                  LEFT JOIN CONVENIO_PLANO CP
                      ON (CP.CD_PLANO = AC.CD_PLANO AND CP.CD_CONVENIO = C.CD_CONVENIO)
                  INNER JOIN TASY.ESTABELECIMENTO ES
                      ON (ES.CD_ESTABELECIMENTO = CASE
                                                      WHEN (A.CD_SETOR_AGENDA IS NULL OR E.CD_ESTABELECIMENTO NOT IN (SELECT * FROM TABLE(T_BRANCHS_SECTORS_ARR))) THEN TO_CHAR(E.CD_ESTABELECIMENTO)
                                                      ELSE LPARAMSPLIT(E.CD_ESTABELECIMENTO,A.CD_SETOR_AGENDA,'MAPBRANCHSSECTORS',PARAMS_P)
                                                  END)
                  INNER JOIN TASY.PESSOA_JURIDICA PJ
                      ON (PJ.CD_CGC = ES.CD_CGC)
              WHERE
                  (A.CD_PESSOA_FISICA IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR))
                  AND (
                         (AC.DT_AGENDA BETWEEN TO_DATE(START_DATE_P,'YYYY-MM-DD HH24:MI:SS') AND TO_DATE(END_DATE_P,'YYYY-MM-DD HH24:MI:SS')) 
                         OR 
                         (START_DATE_P IS NULL AND END_DATE_P IS NULL)
                         )
                  AND (
                      EM.CD_ESPECIALIDADE IS NOT NULL
                      OR
                      ( SELECT COUNT(ME.CD_ESPECIALIDADE) FROM MEDICO_ESPECIALIDADE ME WHERE ME.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA ) > 0                    
                  )
                  AND AC.IE_STATUS_AGENDA NOT IN ('L','C')
                  AND APPOINTMENTID_P IS NULL)
                  OR
                  (AC.NR_SEQUENCIA = APPOINTMENTID_P)
              ORDER BY
                  AC.DT_AGENDA DESC;
  
      ELSE
  
          OPEN P_RECORDSET FOR
              SELECT
                  ROWNUM RNUM,
                  AQ.NR_SEQUENCIA,
                  '' CD_AGENDA,
                  '' DS_AGENDA,
                  M.CD_PESSOA_FISICA CD_MEDICO,
                  M.NM_PESSOA_FISICA NM_MEDICO,
                  PJ.CD_CGC,
                  TO_CHAR(E.CD_ESTABELECIMENTO)||'.'||'0000' CD_ESTABELECIMENTO,
                  E.NM_FANTASIA_ESTAB,
                  PJ.DS_ENDERECO,
                  PJ.DS_BAIRRO,
                  PJ.DS_COMPLEMENTO,
                  PJ.NR_ENDERECO,
                  PJ.DS_MUNICIPIO,
                  PJ.SG_ESTADO,
                  PJ.CD_CEP,
                  EM.CD_ESPECIALIDADE,
                  EM.DS_ESPECIALIDADE,
                  P.CD_PESSOA_FISICA CD_PACIENTE,
                  P.NM_PESSOA_FISICA NM_PACIENTE,
                  C.CD_CONVENIO,
                  C.DS_CONVENIO,
                  CC.CD_CATEGORIA,
                  CC.DS_CATEGORIA,
                  CP.CD_PLANO,
                  CP.DS_PLANO,
                  TO_CHAR(AQ.DT_AGENDA,'YYYY-MM-DD HH24:MI:SS') DT_AGENDA,
                  TO_CHAR(AQ.DT_ATUALIZACAO,'YYYY-MM-DD HH24:MI:SS') DT_AGENDAMENTO,
                  AQ.IE_STATUS_AGENDA,
                  TASY.OBTER_VALOR_DOMINIO(83,AQ.IE_STATUS_AGENDA) DS_STATUS_AGENDA,
                  AP.IE_TIPO_ATENDIMENTO,
                  TASY.OBTER_VALOR_DOMINIO(12,AP.IE_TIPO_ATENDIMENTO) NM_TIPO_ATENDIMENTO,
                  AQ.IE_TIPO_PEND_AGENDA IE_TIPO_AGENDA,
                  'Tratamento' NM_TIPO_AGENDA,
                  'Q5' IE_CLASSIF_AGENDA,
                  'Quimioterapia' NM_CLASSIF_AGENDA,
                  AQM.DS_OBSERVACAO,
                  QMC.NR_SEQUENCIA CD_MOTIVO_CANCEL,
                  QMC.DS_MOTIVO NM_MOTIVO_CANCEL,
                  AQ.NR_MINUTO_DURACAO,
                  MED.NR_CRM,
                  MED.UF_CRM,
                  AQ.DT_ATUALIZACAO
              FROM
                  TASY.AGENDA_QUIMIO AQ
                  LEFT JOIN TASY.AGENDA_QUIMIO_MARCACAO AQM
                      ON (AQM.NR_SEQ_ATENDIMENTO = AQ.NR_SEQ_ATENDIMENTO)
                  INNER JOIN TASY.PESSOA_FISICA P
                      ON (P.CD_PESSOA_FISICA = AQ.CD_PESSOA_FISICA)
                  INNER JOIN TASY.ESTABELECIMENTO E
                      ON (E.CD_ESTABELECIMENTO = AQ.CD_ESTABELECIMENTO)
                  INNER JOIN TASY.PESSOA_JURIDICA PJ
                      ON (PJ.CD_CGC = E.CD_CGC)
                  INNER JOIN TASY.PESSOA_FISICA M
                      ON (M.CD_PESSOA_FISICA = AQ.CD_MEDICO_RESP)
          INNER JOIN MEDICO MED
                      ON (M.CD_PESSOA_FISICA = MED.CD_PESSOA_FISICA)    
                  LEFT JOIN TASY.MEDICO_ESPECIALIDADE ME
                      ON (ME.CD_PESSOA_FISICA = M.CD_PESSOA_FISICA)
                  LEFT JOIN TASY.ESPECIALIDADE_MEDICA EM
                      ON (EM.CD_ESPECIALIDADE = ME.CD_ESPECIALIDADE)
                  LEFT JOIN TASY.QT_MOTIVO_CANCELAMENTO QMC
                      ON (QMC.NR_SEQUENCIA = AQ.NR_SEQ_MOT_CANCELAMENTO AND QMC.IE_SITUACAO = 'A')
                  LEFT JOIN TASY.PACIENTE_ATENDIMENTO PA
                      ON (PA.NR_SEQ_ATENDIMENTO = AQ.NR_SEQ_ATENDIMENTO)
                  LEFT JOIN TASY.ATENDIMENTO_PACIENTE AP
                      ON (AP.NR_ATENDIMENTO = AQ.NR_ATENDIMENTO)
                  LEFT JOIN TASY.PESSOA_TITULAR_CONVENIO PTC
                      ON (PTC.CD_PESSOA_FISICA = P.CD_PESSOA_FISICA)
                  LEFT JOIN TASY.CONVENIO C
                      ON (C.CD_CONVENIO = PTC.CD_CONVENIO)
                  LEFT JOIN TASY.CATEGORIA_CONVENIO CC
                      ON (CC.CD_CATEGORIA = PTC.CD_CATEGORIA AND CC.CD_CONVENIO = PTC.CD_CONVENIO)
                  LEFT JOIN TASY.CONVENIO_PLANO CP
                      ON (CP.CD_CONVENIO = C.CD_CONVENIO AND CP.CD_PLANO = PTC.CD_PLANO_CONVENIO)
              WHERE
                  (AQ.CD_PESSOA_FISICA IN (SELECT DISTINCT CD_PESSOA_FISICA FROM TASY.ATENDIMENTO_PACIENTE WHERE CD_MEDICO_REFERIDO IN (SELECT * FROM TABLE(T_PHYSICIANS_ARR)))
                  AND (
                         (AQ.DT_AGENDA BETWEEN TO_DATE(START_DATE_P,'YYYY-MM-DD HH24:MI:SS') AND TO_DATE(END_DATE_P,'YYYY-MM-DD HH24:MI:SS')) 
                         OR 
                         (START_DATE_P IS NULL AND END_DATE_P IS NULL)
                         )
                  AND AQ.IE_STATUS_AGENDA NOT IN ('L','C')
                  AND AQ.CD_ESTABELECIMENTO IN (SELECT * FROM TABLE(T_BRANCHS_ARR))
          AND APPOINTMENTID_P IS NULL)
                  OR
                  (AQ.NR_SEQUENCIA = APPOINTMENTID_P)
              ORDER BY
                  AQ.DT_AGENDA DESC;
  
      END IF;

    END GET_PHYSICIAN_APPOINTMENTS_RES;

END OC_IRIS_PORTAL;
