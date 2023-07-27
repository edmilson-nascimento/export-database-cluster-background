
REPORT ytest .


CLASS lcl_local DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create_job .

    CLASS-METHODS process_job .

  PROTECTED SECTION.
  
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_local IMPLEMENTATION.

ENDCLASS.

PARAMETERS:
  p_file  TYPE p_file,
  p_backg TYPE check NO-DISPLAY.

*data(lt_outtabl) = value /yga/map_med_order_tt(
*  ( aufnr   = '006280007977'
*    vornr   = '0002'
*    steus   = 'D020'
*    ltxa1   = 'INSTALL_TROCO MT'
*    node    = '02' )
*).
*
**--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
**|INDEX|ID|AUFNR       |VORNR|STEUS|LTXA1                               |DESNUM      |POSID                 |CMNUM       |SITRD |LINE_ID|CU_TXT                          |CUITXT      |CU_ID             |POSTP|NODE|
**--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
**|   1 |  |006280007977|0002 |D020 |INSTALL_TROCO MT                    |000000009989|280019C003374        2|000000003140|003374|      1|TROÇO MT                        |Troço Aéreo |TROCO MT          |     |   2|
**|   2 |  |006280007977|0003 |D020 |INSTALAR CABO AL/ACO 160 26X2.58/7X2|000000009989|280019C003374        2|000000003140|003374|      2|CABO AL/ACO 160 26X2.58/7X2 (km)|Cabo Aéreo  |CABO ALACO 160(KM)|     |   3|
**|   3 |  |006280007977|0004 |D020 |INSTALL_APOIO_IP                    |000000009989|280019C003374        2|000000003140|003374|      3|APOIO                           |Apoio       |APOIO_AT_MT       |     |   3|
**|   4 |  |006280007977|0005 |D020 |INSTALAÇÃO ARMAÇÃO HRFSC-EDP 100    |000000009989|280019C003374        2|000000003140|003374|      4|POSTE BETAO 14M1200 C7 teste tx |POSTE       |POSTE BET 14M1200 |     |   4|
**|   5 |  |006280007977|0006 |D020 |INSTALL_APOIO_IP                    |000000009989|280019C003374        2|000000003140|003374|      5|Ponto de Ligação                |PONTO_LIGA  |PONTO_LIG         |     |   2|
**|   6 |  |006280007977|0007 |PM01 |TESTE                               |000000009989|280019C003374        2|000000003140|003374|      6|Policiamento                    |Policiamento|POLICIAMENTO      |     |   2|
**--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

START-OF-SELECTION .





  IF ws_indx-begdt IS INITIAL.
    ws_indx-begdt = sy-datum.
  ENDIF.

  ws_indx-aedat = sy-datum.
  ws_indx-usera = sy-uname.
  ws_indx-pgmid = sy-cprog.

  EXPORT p1 = is_option TO DATABASE indx(xl) FROM ws_indx ID ws_indx-srtfd.

  IF sy-subrc = 0.
    ws_option = is_option.
  ENDIF.
