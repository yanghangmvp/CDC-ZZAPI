FUNCTION zfm_api_test001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_REQ) TYPE  ZZS_API_TEST001 OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_RESP) TYPE  ZZS_REST_OUT
*"----------------------------------------------------------------------
  DATA:ls_data TYPE zzs_api_test001.

  ls_data = i_req.

  DATA:lv_matnr TYPE matnr,
       lv_batch TYPE charg_d.
  DATA:lt_create TYPE TABLE FOR CREATE i_batchtp_2\\batch.

  lv_matnr  = ls_data-req-matnr.
  lv_batch  = ls_data-req-batch.

  APPEND INITIAL LINE TO lt_create ASSIGNING FIELD-SYMBOL(<fs_create>).
  "批次
  <fs_create>-batch = lv_batch.
  <fs_create>-%control-batch = cl_abap_behv=>flag_changed.
  "物料
  <fs_create>-material = lv_matnr.
  <fs_create>-%control-material = cl_abap_behv=>flag_changed.

  <fs_create>-%cid = 'C1'.

  MODIFY ENTITIES OF i_batchtp_2 PRIVILEGED
   ENTITY batch
   CREATE FROM lt_create
   MAPPED DATA(mapped)
   FAILED DATA(failed)
   REPORTED DATA(reported).

  IF failed IS NOT INITIAL.
    ROLLBACK ENTITIES.
    DATA(lv_msg) = zzcl_comm_tool=>get_bo_msg( is_reported = reported iv_component = 'BATCH' ).
  ELSE.

    COMMIT ENTITIES BEGIN
        RESPONSE OF i_batchtp_2
        FAILED DATA(failed_save)
     REPORTED DATA(reported_save).
    COMMIT ENTITIES END.
  ENDIF.





  o_resp-msgty = 'S'.
  o_resp-msgtx = 'SUCCESS'.


ENDFUNCTION.
