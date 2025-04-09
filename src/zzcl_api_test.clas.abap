CLASS zzcl_api_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_API_TEST IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main .
    DATA:lv_matnr TYPE matnr,
         lv_batch TYPE charg_d.
    DATA:lt_create TYPE TABLE FOR CREATE i_batchtp_2\\batch.

    lv_matnr  = 'CSCTDEMO003'.
    lv_batch  = '2025032101'.

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


  ENDMETHOD.
ENDCLASS.
