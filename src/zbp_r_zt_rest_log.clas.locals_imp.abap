CLASS lhc_zr_zt_rest_log DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zr_zt_rest_log RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR log RESULT result.

    METHODS rehandling FOR MODIFY
      IMPORTING keys FOR ACTION log~rehandling REQUEST is_requested_fields RESULT  result.
ENDCLASS.

CLASS lhc_zr_zt_rest_log IMPLEMENTATION.

  METHOD get_instance_features.
    READ ENTITIES OF zr_zt_rest_log IN LOCAL MODE
    ENTITY log
    FIELDS (  uuid criticalityline )
    WITH CORRESPONDING #( keys )
    RESULT DATA(files)
    FAILED failed.

    result = VALUE #( FOR file IN files
                   ( %tky                           = file-%tky
                     %action-rehandling = COND #( WHEN file-criticalityline = 3
                                                  THEN if_abap_behv=>fc-o-disabled
                                                  ELSE if_abap_behv=>fc-o-enabled   )
                  ) ).

  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD rehandling.
    DATA:lv_flag TYPE abap_boolean.
    DATA:lv_req  TYPE REF TO data,
         lv_resp TYPE REF TO data.
    FIELD-SYMBOLS:<fs_req>   TYPE any,
                  <fs_resp>  TYPE any,
                  <fs_value> TYPE any.
    DATA:lv_sapnum   TYPE zzesapn.
    DATA:i_json TYPE string.
    DATA:o_json TYPE string.

    READ ENTITIES OF zr_zt_rest_log IN LOCAL MODE
      ENTITY log
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_log).

    LOOP AT lt_log INTO DATA(ls_log).

      IF ls_log-criticalityline <> 3.
        "获取配置信息
        SELECT SINGLE *
          FROM zzt_rest_conf
         WHERE zznumb = @ls_log-zznumb
          INTO @DATA(ls_conf).

        "处理接收请求
        IF ls_conf-zztsysid IS INITIAL.
          CREATE DATA lv_req TYPE (ls_conf-zzipara).
          ASSIGN lv_req->* TO <fs_req>.

          CREATE DATA lv_resp TYPE (ls_conf-zzopara).
          ASSIGN lv_resp->* TO <fs_resp>.

          i_json =  /ui2/cl_json=>raw_to_string( ls_log-zzrequest ).

          /ui2/cl_json=>deserialize( EXPORTING json        = CONV string( i_json )
                                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                     CHANGING  data        = <fs_req> ).

          CALL FUNCTION ls_conf-zzfname
            EXPORTING
              i_req  = <fs_req>
            IMPORTING
              o_resp = <fs_resp>.

          "返回单据记录
          ASSIGN COMPONENT 'SAPNUM' OF STRUCTURE <fs_resp> TO <fs_value>.
          IF sy-subrc = 0.
            ls_log-zzsapn = <fs_value>.
          ENDIF.
          ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <fs_resp> TO <fs_value>.
          IF sy-subrc = 0.
            ls_log-msgty = <fs_value>.
          ENDIF.

          o_json = /ui2/cl_json=>serialize( data        = <fs_resp>
                                            "COMPRESS    = ABAP_TRUE
                                            pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

          GET TIME STAMP FIELD ls_log-ctstmpl.
          ls_log-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = o_json ).

          MODIFY ENTITIES OF zr_zt_rest_log IN LOCAL MODE
          ENTITY log
          UPDATE FIELDS (  ctstmpl zzsapn msgty zzresponse )
          WITH VALUE #( ( ctstmpl = ls_log-ctstmpl
                          zzsapn = ls_log-zzsapn
                          msgty = ls_log-msgty
                          zzresponse = ls_log-zzresponse
              %key-uuid = ls_log-uuid ) ).

        ENDIF.

        "处理推送请求
        IF ls_conf-zztsysid IS NOT INITIAL.

          DATA:lv_oref TYPE zzefname,
               lt_ptab TYPE abap_parmbind_tab.
          DATA:lv_data TYPE string.
          DATA:lv_msgty TYPE bapi_mtype,
               lv_msgtx TYPE bapi_msg.
          DATA:lo_oref TYPE REF TO object.
          lt_ptab = VALUE #( ( name  = 'IV_NUMB' kind  = cl_abap_objectdescr=>exporting value = REF #( ls_log-zznumb ) ) ).
          TRY .
              CREATE OBJECT lo_oref TYPE (lv_oref) PARAMETER-TABLE lt_ptab.
              CALL METHOD lo_oref->('OUTBOUND')
                EXPORTING
                  iv_uuid  = ls_log-uuid
                  iv_data  = lv_data
                CHANGING
                  ev_resp  = lv_resp
                  ev_msgty = lv_msgty
                  ev_msgtx = lv_msgtx.
            CATCH cx_root INTO DATA(lr_root).
              IF 1 = 1.
              ENDIF.
          ENDTRY.

          GET TIME STAMP FIELD ls_log-ctstmpl.
          "更新重处理时间
          MODIFY ENTITIES OF zr_zt_rest_log IN LOCAL MODE
          ENTITY log
          UPDATE FIELDS (  ctstmpl  )
          WITH VALUE #( ( ctstmpl = ls_log-ctstmpl
              %key-uuid = ls_log-uuid ) ).

        ENDIF.

      ENDIF.
    ENDLOOP.


    "获取最新数据
    READ ENTITIES OF zr_zt_rest_log IN LOCAL MODE
      ENTITY log
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT lt_log.

    "更新前台界面
    result = VALUE #( FOR log IN lt_log ( uuid = log-uuid
                                           %param    = log
                                           ) ).

  ENDMETHOD.
ENDCLASS.
