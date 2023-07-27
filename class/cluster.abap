
REPORT ytest .


CLASS lcl_local DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.


    TYPES:
      "! <p class="shorttext synchronized" lang="pt">Tipo de dados (estrutura) para representar o modelo de excel</p>
      BEGIN OF ty_excel,
        id   TYPE char10,
        info TYPE char50,
      END OF ty_excel,
      "! <p class="shorttext synchronized" lang="pt">Tipo de dados (tabela) para representar o modelo de excel</p>
      tab_excel TYPE STANDARD TABLE OF ty_excel
                WITH DEFAULT KEY .

    "! <p class="shorttext synchronized" lang="pt">Cria o job com os dados do arquivo</p>
    METHODS create_job
      IMPORTING
        !file TYPE string OPTIONAL .

    "! <p class="shorttext synchronized" lang="pt">Executa o processamento do job</p>
    METHODS process_job
      IMPORTING
        !id TYPE string OPTIONAL .

  PROTECTED SECTION.

  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="pt">Retorna os dados "importados" do arquivo excel</p>
    METHODS get_data_from_file
      IMPORTING
        !im_file      TYPE string
      RETURNING
        VALUE(result) TYPE tab_excel .
    "! <p class="shorttext synchronized" lang="pt">Retorna o ID de exportação apos o processo</p>
    METHODS export_data
      IMPORTING
        !im_data      TYPE tab_excel
      RETURNING
        VALUE(result) TYPE indx-srtfd .
    "! <p class="shorttext synchronized" lang="pt">Retorna o Dados de importação</p>
    METHODS import_data
      IMPORTING
        !im_key       TYPE indx-srtfd
      RETURNING
        VALUE(result) TYPE tab_excel .
    "! <p class="shorttext synchronized" lang="pt">Retorna o número do job apos feita a inicialização</p>
    METHODS job_open
      EXPORTING
        !ex_jobname   TYPE tbtcjob-jobname
        !ex_jobnumber TYPE tbtcjob-jobcount .
    "! <p class="shorttext synchronized" lang="pt">Realiza o submit do job e informa os dados necessarios</p>
    METHODS job_submit
      IMPORTING
        !im_jobname   TYPE tbtcjob-jobname
        !im_jobnumber TYPE tbtcjob-jobcount
      RETURNING
        VALUE(result) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="pt">Finaliza o processo de criação do job</p>
    METHODS job_close
      IMPORTING
        !im_jobname   TYPE tbtcjob-jobname
        !im_jobnumber TYPE tbtcjob-jobcount .

ENDCLASS.

CLASS lcl_local IMPLEMENTATION.

  METHOD create_job .

    DATA(lt_data) = me->get_data_from_file( file ) .

    DATA(id) = me->export_data( lt_data ) .

    me->job_open( IMPORTING ex_jobname   = DATA(lv_jobname)
                            ex_jobnumber = DATA(lv_jobnumber) ) .

    IF ( lv_jobname IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( me->job_submit( im_jobname   = lv_jobname
                         im_jobnumber = lv_jobnumber ) EQ abap_false ) .
      RETURN .
    ENDIF .

    me->job_close( im_jobname   = lv_jobname
                   im_jobnumber = lv_jobnumber ) .
  ENDMETHOD .

  METHOD process_job .
  ENDMETHOD .


  METHOD get_data_from_file .

    " Esta rotina deve ser atualizada para buscar dados
    " do arquivo/ficheiro informado na tela de seleção
    TRY .
        result = VALUE #(
          ( id   = '01'
            info = |01 { cl_system_uuid=>create_uuid_c32_static( ) }| )
          ( id   = '02'
            info = |02 { cl_system_uuid=>create_uuid_c32_static( ) }| )
          ( id   = '03'
            info = |03 { cl_system_uuid=>create_uuid_c32_static( ) }| )
          ( id   = '04'
            info = |04 { cl_system_uuid=>create_uuid_c32_static( ) }| )
          ( id   = '05'
            info = |05 { cl_system_uuid=>create_uuid_c32_static( ) }| )
        ).
      CATCH cx_uuid_error  .
    ENDTRY .

  ENDMETHOD .


  METHOD export_data .

    IF ( lines( im_data ) EQ 0 ) .
      RETURN .
    ENDIF .

    " Atribuindo para nome usado para import/export
    DATA(lt_data) = im_data .

    " Enviando dados para tabela cluster
    DATA(key) = CONV indx-srtfd( 'ZZ_TEST' ) .
    EXPORT lt_data FROM lt_data TO DATABASE indx(zz) ID key.

    IF ( sy-subrc EQ 0 ) .
      result = key .
    ENDIF.

  ENDMETHOD .


  METHOD import_data .

    DATA:
      lt_data   TYPE tab_excel,
      lr_expimp TYPE REF TO cl_abap_expimp_db.

    IF ( im_key IS INITIAL ) .
      RETURN .
    ENDIF .

    " Buscando dados da tabela cluster
    IMPORT lt_data TO lt_data FROM DATABASE indx(zz) ID im_key.
    IF ( sy-subrc EQ 0 ) .
      result = lt_data .
    ENDIF .

    CREATE OBJECT lr_expimp .
    IF ( lr_expimp IS NOT BOUND ) .
      RETURN .
    ENDIF .

    TRY.
        " DELETE FROM DATABASE INDX(ZZ) ID KEY.
        CALL METHOD lr_expimp->delete
          EXPORTING
            tabname          = 'indx'
            client           = '200'
            area             = 'zz'
            id               = im_key
*           GENERIC_KEY      = ABAP_FALSE
            client_specified = abap_true.
      CATCH cx_sy_client .
      CATCH cx_sy_generic_key .
      CATCH cx_sy_incorrect_key .
    ENDTRY.

  ENDMETHOD .


  METHOD job_open .

    CLEAR:
      ex_jobname, ex_jobnumber .

    TRY .
        ex_jobname = CONV btcjob( |{ cl_system_uuid=>create_uuid_x16_static( ) }| ) .
      CATCH cx_uuid_error  .
    ENDTRY .

    IF ( ex_jobname IS INITIAL ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = ex_jobname
      IMPORTING
        jobcount         = ex_jobnumber
      EXCEPTIONS
        cant_create_job  = 01
        invalid_job_data = 02
        jobname_missing  = 03
        OTHERS           = 99.

    IF ( sy-subrc NE 0 ) .
      CLEAR: ex_jobname, ex_jobnumber .
    ENDIF .

  ENDMETHOD .


  METHOD job_submit .

    IF ( im_jobname IS INITIAL ) OR
       ( im_jobnumber IS INITIAL ) .
      RETURN .
    ENDIF .

    SUBMIT zfi_jobs_monthly_extraction
      WITH p_backg EQ abap_on
      USER syst-uname
   VIA JOB im_jobname
    NUMBER im_jobnumber AND RETURN .

    result = COND #( WHEN sy-subrc EQ 0
                     THEN abap_on
                     ELSE abap_off ) .

  ENDMETHOD .


  METHOD job_close .

    DATA:
      job_was_released TYPE btcchar1 .

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount         = im_jobnumber
        jobname          = im_jobname
        strtimmed        = 'X'
      IMPORTING
        job_was_released = job_was_released.

  ENDMETHOD .

ENDCLASS.

PARAMETERS:
  p_file  TYPE string,
  p_backg TYPE check NO-DISPLAY.

START-OF-SELECTION .

  " Background
  IF ( p_backg EQ abap_false ) .
    NEW lcl_local( )->create_job( file = p_file ) .
  ENDIF .

  " Online
  IF ( p_backg EQ abap_true ) .
    NEW lcl_local( )->process_job( ) .
  ENDIF .