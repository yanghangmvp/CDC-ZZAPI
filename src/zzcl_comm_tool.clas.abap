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

    CLASS-METHODS get_dest_odata4
      RETURNING
        VALUE(rv_dest) TYPE REF TO if_http_destination.
    "单位转外码
    CLASS-METHODS conv_uom
      IMPORTING
        iv_uom        TYPE msehi
      RETURNING
        VALUE(rv_uom) TYPE i_unitofmeasuretext-unitofmeasurecommercialname.
    "获取最后执行时间
    CLASS-METHODS get_last_execute
      IMPORTING
        iv_numb          TYPE zzenumb
      RETURNING
        VALUE(rv_tmstmp) TYPE tzntstmpl.

    "获取最后执行时间
    CLASS-METHODS get_last_execute2
      IMPORTING
        iv_numb          TYPE zzenumb
      RETURNING
        VALUE(rv_tmstmp) TYPE tzntstmps.

    "获取BO报错消息
    CLASS-METHODS get_bo_msg
      IMPORTING is_reported  TYPE any
                iv_component TYPE string
      RETURNING VALUE(msg)   TYPE bapi_msg.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zzcl_comm_tool IMPLEMENTATION.


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

  METHOD get_dest_odata4.

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
                    service_id     = 'YY1_ODATAV4_REST'
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

  METHOD get_last_execute.
    DATA lv_tmstmp TYPE zzs_comm_log-last_changed_at.
    GET TIME STAMP FIELD lv_tmstmp.

    SELECT SINGLE *
      FROM zzt_rest_append
     WHERE zznumb   = @iv_numb
       AND zzappac  = 'DATE'
       AND zzappkey = 'LAST_EXECUTE'
      INTO @DATA(ls_append).
    IF sy-subrc = 0.
      rv_tmstmp = ls_append-zzappvalue.
    ELSE.
      ls_append-zznumb   = iv_numb.
      ls_append-zzappac  = 'DATE'.
      ls_append-zzappkey = 'LAST_EXECUTE'.
    ENDIF.

    ls_append-zzappvalue  = lv_tmstmp.

    MODIFY zzt_rest_append FROM @ls_append.

  ENDMETHOD.

  METHOD get_last_execute2.
    DATA lv_tmstmp TYPE tzntstmps.
    GET TIME STAMP FIELD lv_tmstmp.

    SELECT SINGLE *
      FROM zzt_rest_append
     WHERE zznumb   = @iv_numb
       AND zzappac  = 'DATE'
       AND zzappkey = 'LAST_EXECUTE'
      INTO @DATA(ls_append).
    IF sy-subrc = 0.
      rv_tmstmp = ls_append-zzappvalue.
    ELSE.
      ls_append-zznumb   = iv_numb.
      ls_append-zzappac  = 'DATE'.
      ls_append-zzappkey = 'LAST_EXECUTE'.
    ENDIF.

    ls_append-zzappvalue  = lv_tmstmp.

    MODIFY zzt_rest_append FROM @ls_append.

  ENDMETHOD.


  METHOD get_bo_msg.
    DATA:lv_msg     TYPE bapi_msg,
         ls_t100key TYPE scx_t100key,
         lv_msgid   TYPE symsgid,
         lv_msgno   TYPE symsgno,
         lv_msgv1   LIKE sy-msgv1,
         lv_msgv2   LIKE sy-msgv2,
         lv_msgv3   LIKE sy-msgv3,
         lv_msgv4   LIKE sy-msgv4.

    FIELD-SYMBOLS:<fs_tab>       TYPE STANDARD TABLE,
                  <fs_component> TYPE any,
                  <fs_msg>       TYPE REF TO if_abap_behv_message,
                  <fs_t100key>   TYPE any.

    ASSIGN COMPONENT iv_component OF STRUCTURE is_reported TO <fs_tab>.
    IF <fs_tab> IS ASSIGNED.

      LOOP AT <fs_tab> ASSIGNING <fs_component>.
        CLEAR:lv_msgid,lv_msgno,lv_msgv1,lv_msgv2,lv_msgv3,lv_msgv3.
        ASSIGN COMPONENT '%MSG' OF STRUCTURE <fs_component> TO <fs_msg>.
        IF <fs_msg> IS ASSIGNED AND <fs_msg> IS NOT INITIAL.
          ASSIGN <fs_msg>->('IF_T100_MESSAGE~T100KEY') TO <fs_t100key>.
          IF <fs_t100key> IS ASSIGNED AND <fs_t100key> IS NOT INITIAL.
            ASSIGN COMPONENT 'MSGID' OF STRUCTURE <fs_t100key> TO FIELD-SYMBOL(<fs_msgid>).
            ASSIGN COMPONENT 'MSGNO' OF STRUCTURE <fs_t100key> TO FIELD-SYMBOL(<fs_msgno>).
          ENDIF.
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV1') TO FIELD-SYMBOL(<fs_msgv1>).
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV2') TO FIELD-SYMBOL(<fs_msgv2>).
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV3') TO FIELD-SYMBOL(<fs_msgv3>).
          ASSIGN <fs_msg>->('IF_T100_DYN_MSG~MSGV4') TO FIELD-SYMBOL(<fs_msgv4>).
          IF <fs_msgid> IS ASSIGNED AND <fs_msgid> IS NOT INITIAL.
            lv_msgid = <fs_msgid>.
          ENDIF.
          IF <fs_msgno> IS ASSIGNED AND <fs_msgno> IS NOT INITIAL.
            lv_msgno = <fs_msgno>.
          ENDIF.
          IF <fs_msgv1> IS ASSIGNED AND <fs_msgv1> IS NOT INITIAL.
            lv_msgv1 = <fs_msgv1>.
          ENDIF.
          IF <fs_msgv2> IS ASSIGNED AND <fs_msgv2> IS NOT INITIAL.
            lv_msgv2 = <fs_msgv2>.
          ENDIF.
          IF <fs_msgv3> IS ASSIGNED AND <fs_msgv3> IS NOT INITIAL.
            lv_msgv3 = <fs_msgv3>.
          ENDIF.
          IF <fs_msgv4> IS ASSIGNED AND <fs_msgv4> IS NOT INITIAL.
            lv_msgv4 = <fs_msgv4>.
          ENDIF.
          IF lv_msgid IS NOT INITIAL
            AND lv_msgno IS NOT INITIAL.
            MESSAGE ID lv_msgid TYPE 'S' NUMBER lv_msgno
              INTO FINAL(mtext1)
              WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4.
            IF msg IS INITIAL.
              msg = mtext1.
            ELSE.
              msg = |{ msg }/{ mtext1 }|.
            ENDIF.
          ENDIF.
          UNASSIGN:<fs_msgid>,<fs_msgno>,<fs_msgv1>,<fs_msgv2>,<fs_msgv3>,<fs_msgv4>.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
