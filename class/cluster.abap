
REPORT ytest .


CLASS lcl_local DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

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

    "! <p class="shorttext synchronized" lang="pt">Retorna o número do job apos feita a inicialização</p>
    METHODS get_data_from_file
      IMPORTING
        !id           TYPE string
      RETURNING
        VALUE(result) TYPE data .
    "! <p class="shorttext synchronized" lang="pt">Retorna o número do job apos feita a inicialização</p>
    METHODS job_open
      IMPORTING
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

    me->job_open( EXPORTING im_process   = ls_jobname-processo
                            im_tcode     = ls_jobname-tcode
                            im_variant   = line-variante
                  IMPORTING ex_jobname   = DATA(lv_jobname)
                            ex_jobnumber = DATA(lv_jobnumber)
    ).

    IF ( lv_jobname IS INITIAL ) .
      CONTINUE .
    ENDIF .

    IF ( me->job_submit( im_variant   = line-variante
                         im_debug     = lv_debug
                         im_jobname   = lv_jobname
                         im_jobnumber = lv_jobnumber ) EQ abap_false ) .
      CONTINUE .
    ENDIF .

    me->job_close( im_jobname   = lv_jobname
                   im_jobnumber = lv_jobnumber ) .
  ENDMETHOD .

  METHOD process_job .
  ENDMETHOD .


  "! <p class="shorttext synchronized" lang="pt">Retorna o número do job apos feita a inicialização</p>
  METHODS job_open
    IMPORTING
      !im_process   TYPE zca_tfixed_value-zprocesso
      !im_tcode     TYPE zca_tfixed_value-campo
      !im_variant   TYPE rsvar-variant
    EXPORTING
      !ex_jobname   TYPE tbtcjob-jobname
      !ex_jobnumber TYPE tbtcjob-jobcount .
  "! <p class="shorttext synchronized" lang="pt">Realiza o submit do job e informa os dados necessarios</p>
  METHODS job_submit
    IMPORTING
      !im_variant   TYPE rsvar-variant
      !im_debug     TYPE abap_bool
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

PARAMETERS:
  p_file  TYPE string,
  p_backg TYPE check NO-DISPLAY.

START-OF-SELECTION .

  " Background
  IF ( p_backg EQ abap_false ) .
    lcl_local=>create_job( file = p_file ) .
  ENDIF .

  " Online
  IF ( p_backg EQ abap_true ) .
    lcl_local=>process_job( ) .
  ENDIF .



*  IF ws_indx-begdt IS INITIAL.
*    ws_indx-begdt = sy-datum.
*  ENDIF.
*
*  ws_indx-aedat = sy-datum.
*  ws_indx-usera = sy-uname.
*  ws_indx-pgmid = sy-cprog.
*
*  EXPORT p1 = is_option TO DATABASE indx(xl) FROM ws_indx ID ws_indx-srtfd.
*
*  IF sy-subrc = 0.
*    ws_option = is_option.
*  ENDIF.
