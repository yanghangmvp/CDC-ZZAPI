CLASS zzcl_rest_api_hly DEFINITION
  PUBLIC
  INHERITING FROM zzcl_rest_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS token
        REDEFINITION .

    METHODS hlyout  "#EC CI_VALPAR
      IMPORTING
        VALUE(iv_uuid)   TYPE zzeuuid OPTIONAL
        VALUE(iv_data)   TYPE string OPTIONAL  "#EC CI_VALPAR
        VALUE(iv_method) TYPE if_web_http_client=>method OPTIONAL
      CHANGING
        ev_resp          TYPE string  "#EC CI_VALPAR
        ev_msgty         TYPE bapi_mtype
        ev_msgtx         TYPE bapi_msg .
    METHODS zzif_rest_api~restrans
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_REST_API_HLY IMPLEMENTATION.


  METHOD hlyout.
    DATA:lr_client TYPE REF TO if_web_http_client.
    DATA:lv_json   TYPE string.
    DATA:lv_token  TYPE string.
    DATA:ls_log TYPE zzt_rest_log,
         ls_out TYPE zzs_rest_out.

    lv_json = iv_data.

    IF  iv_method IS INITIAL.
      iv_method = if_web_http_client=>post.
    ENDIF.

    IF iv_uuid IS INITIAL.
      TRY .
          DATA(lv_uuid_c32) = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
          me->zzif_rest_api~ms_log-uuid = lv_uuid_c32.
        CATCH cx_uuid_error.
              if 1 = 1 .
      ENDIF.
      ENDTRY.
    ELSE.
      me->zzif_rest_api~ms_log-uuid  = iv_uuid.
    ENDIF.

    ls_out-uuid     = me->zzif_rest_api~ms_log-uuid.
    ls_log-uuid     = me->zzif_rest_api~ms_log-uuid.
    ls_log-zznumb   = me->zzif_rest_api~ms_conf-zznumb.
    ls_log-zzname   = me->zzif_rest_api~ms_conf-zzname.
    ls_log-zzfsysid  = 'SAP'.
    ls_log-zztsysid  = me->zzif_rest_api~ms_conf-zztsysid.
    ls_log-zzrequest = /ui2/cl_json=>string_to_raw( EXPORTING iv_string   = lv_json ).
    ls_log-mimetype = 'application/json'.
    ls_log-ernam   = sy-uname.
    ls_log-bdate   = sy-datum.
    ls_log-btime   = sy-uzeit.
    GET TIME STAMP FIELD ls_log-btstmpl.
    me->zzif_rest_api~set_log( is_log = ls_log ).

    TRY.
        lr_client = cl_web_http_client_manager=>create_by_http_destination(
                    i_destination = cl_http_destination_provider=>create_by_url( i_url = CONV string( me->zzif_rest_api~ms_conf-zzurlc ) ) ).

        DATA(lo_request) = lr_client->get_http_request(   ).
        "设置请求内容格式
        lo_request->set_header_field( i_name =  'Content-type'
                                      i_value = 'application/json' ).
        "设置请求体
        lo_request->set_text( i_text = lv_json ).
        "设置验证方式
        CASE me->zzif_rest_api~ms_conf-zzauty.
          WHEN 'p'.
            ""密码认证
            lo_request->set_authorization_basic(
                            i_username = CONV string( me->zzif_rest_api~ms_conf-zzuser )
                            i_password = CONV string( me->zzif_rest_api~ms_conf-zzpwd ) ).
          WHEN 'T'.
            "Token认证
            lv_token = me->token( ).
            IF lv_token IS NOT INITIAL.
              CONCATENATE 'Bearer' lv_token INTO lv_token SEPARATED BY space.
              lo_request->set_header_field( i_name  = 'authorization'
                                            i_value = lv_token ).
            ENDIF.


          WHEN OTHERS.
        ENDCASE.
        "设置请求方式
        DATA(lo_response) = lr_client->execute( iv_method ).

        "返回HTTP JSON报文
        DATA(status) = lo_response->get_status( ).
        DATA(lv_res) = lo_response->get_text( ).

        ls_log-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string   = lv_res ).

        ev_resp = lv_res.
        "返回消息转换
        me->zzif_rest_api~restrans(
         EXPORTING
            iv_json = lv_res
          CHANGING
             cv_msgty = ev_msgty
             cv_msgtx = ev_msgtx
             cs_log = ls_log
              ).
        "关闭连接
        CALL METHOD lr_client->close.
      CATCH cx_web_http_client_error cx_http_dest_provider_error.
           if 1 = 1 .
      ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD token.
    TYPES:
      BEGIN OF ty_token,
        access_token TYPE  string,
      END OF ty_token.
    DATA:ls_token TYPE ty_token.
    DATA:lt_mapping TYPE /ui2/cl_json=>name_mappings.
    DATA:lv_json TYPE string.

    TRY.
        DATA(lr_client) = cl_web_http_client_manager=>create_by_http_destination(
                          i_destination = cl_http_destination_provider=>create_by_url( i_url = CONV string( me->zzif_rest_api~ms_conf-zztkurl ) ) ).

        DATA(lo_request) = lr_client->get_http_request(   ).

        ""密码认证
        lo_request->set_authorization_basic(
                        i_username = CONV string( me->zzif_rest_api~ms_conf-zzctid )
                        i_password = CONV string( me->zzif_rest_api~ms_conf-zzctsecret ) ).
        "设置请求内容格式
        lo_request->set_form_field( i_name = 'grant_type'  i_value = 'client_credentials' ).
        lo_request->set_form_field( i_name = 'scope'  i_value = 'write' ).


        "设置请求方式
        DATA(lo_response) = lr_client->execute( if_web_http_client=>post ).

        "返回HTTP JSON报文
        DATA(status) = lo_response->get_status( ).
        IF status-code = '200'.
          DATA(lv_res) = lo_response->get_text( ).
          /ui2/cl_json=>deserialize( EXPORTING json = lv_res CHANGING data = ls_token ).
          rv_token = ls_token-access_token.
        ENDIF.

        "关闭连接
        CALL METHOD lr_client->close.

      CATCH cx_web_http_client_error cx_http_dest_provider_error.
        IF 1 = 1 .
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD zzif_rest_api~restrans.
    TYPES:BEGIN OF ty_resp,
            message   TYPE string,
            errorcode TYPE string,
            key       TYPE string,
            oid       TYPE string,
          END OF ty_resp.
    DATA:ls_resp TYPE ty_resp.
    DATA:lv_msgty TYPE msgty.
    TRY .
        "解析UUID和接口编号
        /ui2/cl_json=>deserialize( EXPORTING json        = iv_json
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                   CHANGING  data        = ls_resp ).
      CATCH cx_root INTO DATA(lr_root).
        IF 1 = 1.
        ENDIF.
    ENDTRY.

    IF ls_resp-errorcode  = '0000'.
      lv_msgty = 'S'.
    ELSE.
      lv_msgty = 'E'.
    ENDIF.

    cv_msgty = cs_log-msgty = lv_msgty.
    cv_msgtx = ls_resp-message.
    cs_log-rdate    = sy-datum.
    cs_log-rtime    = sy-uzeit.
    GET TIME STAMP FIELD cs_log-rtstmpl.
    me->zzif_rest_api~set_log( is_log = cs_log ).

  ENDMETHOD.
ENDCLASS.
