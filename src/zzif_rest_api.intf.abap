INTERFACE zzif_rest_api
  PUBLIC .
  DATA ms_log   TYPE zzt_rest_log .
  DATA ms_conf  TYPE zr_vt_rest_conf.

  METHODS set_log
    IMPORTING
      is_log TYPE zzt_rest_log OPTIONAL .

  METHODS reqtrans
    CHANGING
      cv_data TYPE data OPTIONAL .

  METHODS restrans
    IMPORTING
      iv_json       TYPE string OPTIONAL
    CHANGING
      cv_msgty      TYPE bapi_mtype OPTIONAL
      cv_msgtx      TYPE bapi_msg OPTIONAL
      VALUE(cs_log) TYPE zzt_rest_log . "#EC CI_VALPAR
ENDINTERFACE.
