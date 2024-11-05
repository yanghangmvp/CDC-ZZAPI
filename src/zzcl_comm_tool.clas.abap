CLASS zzcl_comm_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS unix2timestamp
      IMPORTING
        iv_unix             TYPE i
      RETURNING
        VALUE(rv_timestamp) TYPE string.

    CLASS-METHODS iso2timestamp
      IMPORTING
        iv_iso              TYPE string
      RETURNING
        VALUE(rv_timestamp) TYPE timestampl.

    CLASS-METHODS date2iso
      IMPORTING
        iv_date       TYPE string
      RETURNING
        VALUE(rv_iso) TYPE string.

    CLASS-METHODS get_dest
      RETURNING
        VALUE(rv_dest) TYPE REF TO if_http_destination.
    "单位转外码
    CLASS-METHODS conv_uom
      IMPORTING
        iv_uom        TYPE msehi
      RETURNING
        VALUE(rv_uom) TYPE i_unitofmeasuretext-UnitOfMeasureCommercialName.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_COMM_TOOL IMPLEMENTATION.


  METHOD conv_uom.
    DATA lv_uom TYPE msehi.
    lv_uom = iv_uom.
    DATA(lo_uom) = cl_uom_maintenance=>get_instance( ).
    TRY.
        lo_uom->read( EXPORTING unit = lv_uom
                      IMPORTING unit_st = DATA(ls_unit) ).
        rv_uom = ls_unit-commercial.
      CATCH cx_uom_error.
        CHECK 1 = 1.
    ENDTRY.
  ENDMETHOD.


  METHOD date2iso.
    DATA:lv_date        TYPE string.
    IF iv_date IS INITIAL.
      RETURN.
    ENDIF.
    TRY.
        lv_date = iv_date.
        lv_date = lv_date+0(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2) && 'T00:00:00'.
        rv_iso  =  lv_date .
      CATCH cx_root INTO DATA(lr_root).
        CHECK 1 = 1.
    ENDTRY.
  ENDMETHOD.


  METHOD get_dest.

*&---定义场景使用变量
    DATA: lr_cscn TYPE if_com_scenario_factory=>ty_query-cscn_id_range.
*&---Find CA by Scenario ID
    lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = 'YY1_API' ) ).
*&---创建实例
    DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).
    lo_factory->query_ca(
            EXPORTING
              is_query           = VALUE #( cscn_id_range = lr_cscn )
            IMPORTING
              et_com_arrangement = DATA(lt_ca) ).
    IF lt_ca IS INITIAL.
      EXIT.
    ENDIF.

*&---take the first one
    READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
*&---get destination based on Communication Arrangement and the service ID
    TRY.
        rv_dest = cl_http_destination_provider=>create_by_comm_arrangement(
                    comm_scenario  = 'YY1_API'
                    service_id     = 'YY1_API_REST'
                    comm_system_id = lo_ca->get_comm_system_id( ) ).
      CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
        EXIT.
    ENDTRY.
  ENDMETHOD.


  METHOD iso2timestamp.
    DATA:lv_datum TYPE datum,
         lv_uzeit TYPE uzeit,
         lv_stamp TYPE timestampl,
         lv_iso   TYPE string.

    lv_iso = iv_iso.
    TRY.
        SPLIT lv_iso AT 'T' INTO DATA(lv_iso_d) DATA(lv_iso_t).
        lv_datum = lv_iso_d+0(4) && lv_iso_d+5(2) && lv_iso_d+8(2).
        lv_uzeit = lv_iso_t+0(2) && lv_iso_t+3(2) && lv_iso_t+6(2).

        CONVERT DATE lv_datum TIME lv_uzeit  INTO TIME STAMP lv_stamp TIME ZONE 'UTC+8'.

        rv_timestamp = lv_stamp.
      CATCH cx_root INTO DATA(lr_root).
       CHECK 1 = 1.
    ENDTRY.

  ENDMETHOD.


  METHOD unix2timestamp.
    DATA: lv_unix_timestamp TYPE i ,  " 例子：UNIX 时间戳，假设为当前时间的时间戳
          lv_utc_offset     TYPE i VALUE 0,           " UTC 偏移量，单位为秒
          lv_utc_date       TYPE d,
          lv_utc_time       TYPE t,
          lv_abap_datetime  TYPE sy-uzeit.            " ABAP 的日期时间类型

    lv_unix_timestamp = iv_unix.
    " 计算日期和时间
    lv_utc_date = sy-datum + ( lv_unix_timestamp - lv_utc_offset ) / 86400.
    lv_utc_time = ( lv_unix_timestamp - lv_utc_offset ) MOD 86400.

    " 设置 ABAP 的日期时间
    lv_abap_datetime = lv_utc_date && lv_utc_time.
  ENDMETHOD.
ENDCLASS.
