FUNCTION zzfm_demo_out.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
  DATA:lv_oref TYPE zzefname,
       lt_ptab TYPE abap_parmbind_tab.
  DATA:lv_numb TYPE zzenumb VALUE 'PP001'.
  DATA:lv_data TYPE string.
  DATA:lv_msgty TYPE bapi_mtype,
       lv_msgtx TYPE bapi_msg,
       lv_resp  TYPE string.
  "获取调用类
  SELECT SINGLE zzcname
    FROM zr_vt_rest_conf
   WHERE zznumb = @lv_numb
    INTO @lv_oref.
  CHECK lv_oref IS NOT INITIAL.

  lv_data =  '{' && |"manufacturingOrder": "11222211"| && '}'.
*&--调用实例化接口
  DATA:lo_oref TYPE REF TO object.

  lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( lv_numb ) ) ).
  TRY .
      CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
      CALL METHOD lo_oref->('OUTBOUND')
        EXPORTING
          iv_data  = lv_data
        CHANGING
          ev_resp  = lv_resp
          ev_msgty = lv_msgty
          ev_msgtx = lv_msgtx.
    CATCH cx_root INTO DATA(lr_root).
          if 1 = 1 .
      ENDIF.
  ENDTRY.


ENDFUNCTION.
