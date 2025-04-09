CLASS zzcl_rest_api DEFINITION
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zzif_rest_api .

    METHODS constructor
      IMPORTING
        iv_numb TYPE zzenumb OPTIONAL.
    "传入方法
    METHODS inbound
      IMPORTING
        i_json TYPE string OPTIONAL
      CHANGING
        o_json TYPE string .
    "传出方法
    METHODS outbound                                     "#EC CI_VALPAR
      IMPORTING
        VALUE(iv_uuid) TYPE zzeuuid OPTIONAL
        VALUE(iv_data) TYPE string OPTIONAL
      CHANGING
        ev_resp        TYPE string                       "#EC CI_VALPAR
        ev_msgty       TYPE bapi_mtype
        ev_msgtx       TYPE bapi_msg .

    "获取token
    METHODS token
      RETURNING
        VALUE(rv_token) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_REST_API IMPLEMENTATION.


  METHOD constructor.

    DATA lv_numb TYPE zzenumb.
    lv_numb = iv_numb.
    IF lv_numb IS NOT INITIAL.
      "&--查询接口编号、系统信息
      SELECT SINGLE *
        FROM zr_vt_rest_conf
       WHERE zznumb = @lv_numb
        INTO @DATA(ls_conf).
      me->zzif_rest_api~ms_conf = ls_conf.
    ENDIF.
  ENDMETHOD.


  METHOD inbound.
*&--定义变量
    DATA:i_req  TYPE zzs_rest_in,
         o_resp TYPE zzs_rest_out.
    DATA:lv_req  TYPE REF TO data,
         lv_resp TYPE REF TO data.

    FIELD-SYMBOLS:<fs_req>   TYPE any,
                  <fs_resp>  TYPE any,
                  <fs_value> TYPE any.

    DATA:g_flag(10) TYPE c,
         g_ecode    TYPE i VALUE 0,
         g_sapnum   TYPE zzesapn.

    DATA oref TYPE REF TO cx_root.

    o_resp-msgty = 'S'.

*&--Check data
    IF i_json IS INITIAL AND g_ecode = 0.
      g_ecode = 1. "请传输接口报文
    ENDIF.

    IF g_ecode = 0.
      TRY .
          "解析UUID和接口编号
          /ui2/cl_json=>deserialize( EXPORTING json        = i_json
                                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                     CHANGING  data        = i_req ).

        CATCH cx_root INTO oref.
          g_ecode = 2. "报文解析失败，请检查报文
          o_resp-msgtx = oref->get_text( ) .
      ENDTRY.
    ENDIF.

*&--Check data
    IF i_req-uuid IS INITIAL  AND g_ecode = 0.
      g_ecode = 3."UUID不能为空
    ENDIF.

    IF i_req-znumb IS INITIAL  AND g_ecode = 0.
      g_ecode = 4."接口编号不能为空
    ENDIF.

    IF i_req-uuid IS NOT INITIAL AND g_ecode = 0.
      SELECT SINGLE uuid
        FROM zzt_rest_log
       WHERE uuid = @i_req-uuid
        INTO @DATA(lv_uuid).
      IF lv_uuid IS NOT INITIAL.
        g_ecode = 5."UUID已存在，请更换
      ENDIF.
    ENDIF.

    IF i_req-fsysid IS INITIAL  AND g_ecode = 0.
      g_ecode = 7."调用系统不能为空
    ENDIF.

*&--添加请求日志
    DATA:ls_log TYPE zzt_rest_log.
    ls_log-client = sy-mandt.
    ls_log-uuid = i_req-uuid.
    ls_log-zznumb = i_req-znumb.
    ls_log-zzfsysid = i_req-fsysid.
    ls_log-zztsysid = 'SAP'.
    ls_log-mimetype = 'application/json'.
    ls_log-zzrequest =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string   = i_json ).

    "ls_log-zzrequest = i_json.
    ls_log-ernam = sy-uname.
    ls_log-bdate = sy-datum.
    ls_log-btime = sy-uzeit.
    GET TIME STAMP FIELD ls_log-btstmpl.

    me->zzif_rest_api~set_log( is_log = ls_log ).

    IF g_ecode = 0.
      TRY .
*&--根据接口编号查询函数名
          SELECT SINGLE *
            FROM zzt_rest_conf
            WHERE zznumb = @i_req-znumb
             AND zzisst = 'X'
             INTO @DATA(gs_fconf).
          "定义请求和响应参数
          IF gs_fconf IS NOT INITIAL.
            CREATE DATA lv_req TYPE (gs_fconf-zzipara).
            ASSIGN lv_req->* TO <fs_req>.

            CREATE DATA lv_resp TYPE (gs_fconf-zzopara).
            ASSIGN lv_resp->* TO <fs_resp>.

            /ui2/cl_json=>deserialize( EXPORTING json        = i_json
                                                 pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                       CHANGING  data        = <fs_req> ).

            CALL FUNCTION gs_fconf-zzfname
              EXPORTING
                i_req  = <fs_req>
              IMPORTING
                o_resp = <fs_resp>.
          ELSE.
            g_ecode = 6."请在SAP中配置接口编号
          ENDIF.
        CATCH cx_root INTO oref.
          g_ecode = 2.
          o_resp-msgtx = oref->get_text( ) .
      ENDTRY.
    ENDIF.

    IF g_ecode <> 0.
*&--错误消息
      o_resp-msgty = 'E'.
      o_resp-uuid = i_req-uuid.
      CASE g_ecode.
        WHEN 1.
          MESSAGE s002(zgl01) INTO o_resp-msgtx.
        WHEN 2.
          "O_RESP-MSGTX = 'ERP接口调用失败，请检查报文格式！'.
        WHEN 3.
          MESSAGE s003(zgl01) INTO o_resp-msgtx.
        WHEN 4.
          MESSAGE s004(zgl01) INTO o_resp-msgtx.
        WHEN 5.
          MESSAGE s005(zgl01) INTO o_resp-msgtx.
        WHEN 6.
          MESSAGE s006(zgl01) INTO o_resp-msgtx.
        WHEN 7.
          MESSAGE s007(zgl01) INTO o_resp-msgtx.
        WHEN OTHERS.
      ENDCASE.
      o_json = /ui2/cl_json=>serialize( data        = o_resp
                                        "COMPRESS    = ABAP_TRUE
                                        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    ELSE.
      "UUID回写
      ASSIGN COMPONENT 'UUID' OF STRUCTURE <fs_resp> TO <fs_value>.
      IF sy-subrc = 0.
        <fs_value> = i_req-uuid.
      ENDIF.
      "返回单据记录
      ASSIGN COMPONENT 'SAPNUM' OF STRUCTURE <fs_resp> TO <fs_value>.
      IF sy-subrc = 0.
        g_sapnum = <fs_value>.
      ENDIF.

      o_json = /ui2/cl_json=>serialize( data        = <fs_resp>
                                        "COMPRESS    = ABAP_TRUE
                                        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
    ENDIF.

*&--添加响应日志
    ls_log-zzname = gs_fconf-zzname.
    ls_log-rdate = sy-datum.
    ls_log-rtime = sy-uzeit.
    GET TIME STAMP FIELD ls_log-rtstmpl.
    ls_log-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = o_json ).

    ls_log-zzsapn = g_sapnum.
    IF  g_ecode <> 0.
      ls_log-msgty = o_resp-msgty.
    ELSE.
      ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <fs_resp> TO <fs_value>.
      IF sy-subrc = 0.
        ls_log-msgty = <fs_value>.
      ENDIF.
    ENDIF.
    me->zzif_rest_api~set_log( is_log = ls_log ).
  ENDMETHOD.


  METHOD outbound.
    DATA:lr_client TYPE REF TO if_web_http_client.
    DATA:lv_json   TYPE string.
    DATA:lv_token  TYPE string.
    DATA:ls_log TYPE zzt_rest_log,
         ls_out TYPE zzs_rest_out.

*    "请求消息转换
*    me->zzif_rest_api~reqtrans(
*    CHANGING
*      cv_data = iv_data
*    ).
*
*    lv_json = /ui2/cl_json=>serialize( EXPORTING data        = iv_data
*                                                 pretty_name = 'X' ).

    lv_json = iv_data.

    IF iv_uuid IS INITIAL.
      TRY .
          DATA(lv_uuid_c32) = cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ).
          me->zzif_rest_api~ms_log-uuid = lv_uuid_c32.
        CATCH cx_uuid_error.
          IF 1 = 1 .
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
          WHEN 'P'.
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
        DATA(lo_response) = lr_client->execute( if_web_http_client=>post ).

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
        IF 1 = 1 .
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD token.
    TYPES:BEGIN OF ty_token,
            access_token TYPE string,
          END OF ty_token.
    DATA:lr_client TYPE REF TO if_web_http_client.
    DATA:ls_token TYPE ty_token.
    TRY.
        lr_client = cl_web_http_client_manager=>create_by_http_destination(
                    i_destination = cl_http_destination_provider=>create_by_url( i_url = CONV string( me->zzif_rest_api~ms_conf-zztkurl ) ) ).

        DATA(lo_request) = lr_client->get_http_request(   ).
        "设置请求内容格式
        lo_request->set_header_field( i_name =  'Content-type'
                                      i_value = 'application/json' ).
        "设置请求内容
        lo_request->set_form_field( i_name = 'grant_type' i_value = 'client_credentials' ).
        lo_request->set_form_field( i_name = 'scope'      i_value = CONV string( me->zzif_rest_api~ms_conf-zzscope ) ).

        "设置验证方式
        lo_request->set_authorization_basic(
                        i_username = CONV string( me->zzif_rest_api~ms_conf-zzctid )
                        i_password = CONV string( me->zzif_rest_api~ms_conf-zzctsecret ) ).

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


  METHOD zzif_rest_api~reqtrans.

  ENDMETHOD.


  METHOD zzif_rest_api~restrans.
    DATA:ls_log TYPE zzt_rest_log.
    DATA:lv_json TYPE string.

    TYPES:BEGIN OF ty_resp,
            msgty TYPE string,
            msgtx TYPE string,
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

    IF ls_resp-msgty  = 'S'.
      lv_msgty = 'S'.
    ELSE.
      lv_msgty = 'E'.
    ENDIF.


    cv_msgty = cs_log-msgty = lv_msgty.
    cv_msgtx = ls_resp-msgtx.
    cs_log-rdate    = sy-datum.
    cs_log-rtime    = sy-uzeit.
    GET TIME STAMP FIELD cs_log-rtstmpl.
    me->zzif_rest_api~set_log( is_log = cs_log ).
  ENDMETHOD.


  METHOD zzif_rest_api~set_log.
    DATA:ls_log TYPE zzt_rest_log.
    ls_log = is_log.
    TRY.
        MODIFY zzt_rest_log FROM @ls_log.
      CATCH cx_root INTO DATA(lr_root).
        DATA(lv_message) = lr_root->get_text( ).
    ENDTRY.

    me->zzif_rest_api~ms_log = ls_log.
  ENDMETHOD.
ENDCLASS.
