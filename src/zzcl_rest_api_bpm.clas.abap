CLASS zzcl_rest_api_bpm DEFINITION
  PUBLIC
  INHERITING FROM zzcl_rest_api
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "传出方法
    METHODS outbound_no_log_set
      IMPORTING
        VALUE(iv_uuid) TYPE zzeuuid OPTIONAL
        VALUE(iv_data) TYPE string OPTIONAL
        VALUE(iv_user) TYPE string OPTIONAL
        VALUE(iv_method) TYPE string OPTIONAL
      CHANGING
        ev_resp        TYPE string
        ev_msgty       TYPE bapi_mtype
        ev_msgtx       TYPE bapi_msg .
    METHODS reqtrans_no_log_set
      IMPORTING
        iv_json       TYPE string OPTIONAL
      CHANGING
        cv_msgty      TYPE bapi_mtype OPTIONAL
        cv_msgtx      TYPE bapi_msg OPTIONAL
        VALUE(cs_log) TYPE zzt_rest_log .
    METHODS gettoken
      IMPORTING
                VALUE(iv_user)   TYPE string OPTIONAL
                VALUE(iv_ts)     TYPE string OPTIONAL
      RETURNING VALUE(rv_token)  TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zzcl_rest_api_bpm IMPLEMENTATION.


  METHOD outbound_no_log_set.
    DATA:lr_client TYPE REF TO if_web_http_client.
    DATA:lv_json   TYPE string.
    DATA:lv_token  TYPE string.
    DATA:ls_log TYPE zzt_rest_log,
         ls_out TYPE zzs_rest_out,
         lv_url TYPE string,
         lv_url_hz type string.

    lv_json = iv_data.
    DATA(lv_ts) = zcl_com_util=>get_javatimestamp( ).
    lv_token = me->gettoken( EXPORTING iv_user = iv_user
                                       iv_ts   = lv_ts ).
    "http://192.168.1.88/BPM/YZSoft/ApiService/YZService.ashx?Method=PostTask&UserAccount=User01&ts=12345678&AppId=EAS&token=xxxx
    lv_url = |{ CONV string( me->zzif_rest_api~ms_conf-zzurlc ) }|.
    lv_url_hz = |Method={ iv_method }&UserAccount={ iv_user }&ts={ lv_ts }&AppId={ me->zzif_rest_api~ms_conf-zzuser }&token={ lv_token }|.
    TRY.
        lr_client = cl_web_http_client_manager=>create_by_http_destination(
                    i_destination = cl_http_destination_provider=>create_by_url( i_url = lv_url ) ).

        DATA(lo_request) = lr_client->get_http_request(   ).
        "设置请求内容格式
        lo_request->set_header_field( i_name =  'Content-type'
                                      i_value = 'application/json' ).
        lo_request->set_query( lv_url_hz ).
        "设置请求体
        lo_request->set_text( i_text = lv_json ).

        "设置请求方式
        DATA(lo_response) = lr_client->execute( if_web_http_client=>post ).

        "返回HTTP JSON报文
        DATA(status) = lo_response->get_status( ).
        DATA(lv_res) = lo_response->get_text( ).
        REPLACE ALL OCCURRENCES OF ':null' IN lv_res WITH ':""'.
        ls_log-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string   = lv_res ).
        ev_resp = lv_res.

        IF status-code NE '200'.
          ev_msgty = 'E'.
          ev_msgtx = |状态码{ status-code },异常原因:{ status-reason }| .
          RETURN.
        ENDIF.

        "返回消息转换
        me->reqtrans_no_log_set(
         EXPORTING
            iv_json = lv_res
          CHANGING
             cv_msgty = ev_msgty
             cv_msgtx = ev_msgtx
             cs_log = ls_log
              ).
        "关闭连接
        CALL METHOD lr_client->close.
      CATCH cx_web_http_client_error cx_http_dest_provider_error INTO DATA(lo_error).
        "Handle exception here.
        ev_msgty = 'E'.
        ev_msgtx = |调用接口异常:{ lo_error->get_longtext( ) }| .
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD reqtrans_no_log_set.
    TYPES:BEGIN OF ty_resp,
            message      TYPE string,
            errorcode    TYPE string,
            key          TYPE string,
            srcobject    TYPE string,
            success      TYPE abap_bool,
            errormessage TYPE string,
          END OF ty_resp.
    DATA:ls_resp TYPE ty_resp.
    DATA:lv_msgty TYPE msgty.
    TRY .
        "解析UUID和接口编号
        /ui2/cl_json=>deserialize( EXPORTING json        = iv_json
                                             pretty_name = /ui2/cl_json=>pretty_mode-none
                                   CHANGING  data        = ls_resp ).
      CATCH cx_root INTO DATA(lr_root).
    ENDTRY.

    IF ls_resp-success  = 'X'.
      lv_msgty = 'S'.
    ELSE.
      lv_msgty = 'E'.
    ENDIF.

    cv_msgty = lv_msgty.
    cv_msgtx = ls_resp-errorcode.
  ENDMETHOD.

  METHOD gettoken.
    TYPES:BEGIN OF ty_value,
            value TYPE string,
          END OF ty_value.
    DATA:lt_value       TYPE TABLE OF ty_value,
         lv_ts          TYPE string,
         lv_useraccount TYPE string,
         lv_appid       TYPE string,
         lv_appkey      TYPE string,
         lv_collect     TYPE string,
         lv_str         TYPE string,
         lv_token       TYPE string.

    lv_appid = me->zzif_rest_api~ms_conf-zzuser.
    lv_appkey = me->zzif_rest_api~ms_conf-zzpwd.

    lt_value = VALUE #( ( value = iv_ts )
                        ( value = iv_user )
                        ( value = lv_appid )
                        ( value = lv_appkey ) ).
    SORT lt_value BY value.
    LOOP AT lt_value INTO DATA(ls_value).
      IF lv_collect IS INITIAL.
        lv_collect = ls_value-value.
      ELSE.
        lv_collect = |{ lv_collect },{ ls_value-value }|.
      ENDIF.
    ENDLOOP.

    DATA(lr_algorithm) = xco_cp_hash=>algorithm.
    " Upon execution, LV_HASH_VALUE will have the value C79C561BB2CC3D0430A54B3D014C89C3089FE089.
    DATA(lv_xstring) = xco_cp=>string( lv_collect
      )->as_xstring( lr_algorithm->for( 'MD5' )
      )->value.
    lv_str = lv_xstring.
    rv_token = to_lower( lv_str ).
  ENDMETHOD.

ENDCLASS.
