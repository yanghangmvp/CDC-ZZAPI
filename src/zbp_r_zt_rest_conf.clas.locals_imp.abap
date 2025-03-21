CLASS lhc_conf DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR conf RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR conf RESULT result.

    METHODS createjson FOR DETERMINE ON SAVE
      IMPORTING keys FOR conf~createjson.

    METHODS recreate FOR MODIFY
      IMPORTING keys FOR ACTION conf~recreate RESULT result.


    METHODS set_json
      IMPORTING
        !pv_typename TYPE sxco_ad_object_name
      CHANGING
        !cv_json     TYPE string.


ENDCLASS.

CLASS lhc_conf IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD set_json.
  ENDMETHOD.

  METHOD createjson.

    DATA: lv_error   TYPE abap_boolean,
          lv_message TYPE string.
    DATA: lo_req  TYPE REF TO data,
          lo_resp TYPE REF TO data,
          lv_json TYPE string.
*&---获取UI 界面实体数据内容
    READ    ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
   ENTITY conf ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT DATA(results).

*&---出来数据文件
    LOOP AT results ASSIGNING FIELD-SYMBOL(<result>).
      <result>-mimetype = 'application/json'.

      lv_error = abap_false.

      IF <result>-zzipara IS NOT INITIAL.
        FIELD-SYMBOLS:<fs_req>    TYPE any.
        CREATE DATA lo_req TYPE (<result>-zzipara).
        ASSIGN lo_req->* TO <fs_req>.
        ASSIGN COMPONENT 'ZNUMB' OF STRUCTURE <fs_req> TO FIELD-SYMBOL(<fs_value>).
        IF sy-subrc = 0.
          <fs_value> = <result>-zznumb.
        ENDIF.

        lv_json = /ui2/cl_json=>serialize( data        = <fs_req>
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        <result>-zzrequest =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = lv_json ).

      ENDIF.

      IF <result>-zzopara IS NOT INITIAL.
        FIELD-SYMBOLS:<fs_res>    TYPE any.
        CREATE DATA lo_resp TYPE (<result>-zzopara).
        ASSIGN lo_resp->* TO <fs_res>.

        lv_json = /ui2/cl_json=>serialize( data        = <fs_res>
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        <result>-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = lv_json ).
      ENDIF.
      " 报错消息处理
      IF lv_error = abap_true.
        APPEND VALUE #(  zznumb = <result>-zznumb
            %msg = new_message(
            id       = 'ZGL01'
            number   = 000
            severity = if_abap_behv_message=>severity-error
            v1       = lv_message
            )
        )  TO reported-conf.

      ENDIF.
*      ENDIF.
    ENDLOOP.
    " 若无错则保存数据服务xsd 文件到template 附加xsdfile 文件字段中
    IF lv_error = abap_false.
      " 更新数据实体把产生xsd 内容和对应字段更新到对应实体上
      MODIFY ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
          ENTITY conf UPDATE FIELDS ( zzrequest zzresponse mimetype )
              WITH VALUE #( FOR conf IN results ( %tky        = conf-%tky
                                                  zzrequest   = conf-zzrequest
                                                  zzresponse  = conf-zzresponse
                                                  mimetype    = conf-mimetype ) ).
    ENDIF.


  ENDMETHOD.

  METHOD recreate.
    DATA: lv_error   TYPE abap_boolean,
          lv_message TYPE string.
    DATA: lo_req  TYPE REF TO data,
          lo_resp TYPE REF TO data,
          lv_json TYPE string.
*&---获取UI 界面实体数据内容
    READ    ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
   ENTITY conf ALL FIELDS WITH CORRESPONDING #( keys )
   RESULT DATA(results).

*&---出来数据文件
    LOOP AT results ASSIGNING FIELD-SYMBOL(<result>).
      <result>-mimetype = 'application/json'.

      lv_error = abap_false.

      IF <result>-zzipara IS NOT INITIAL.
        FIELD-SYMBOLS:<fs_req>    TYPE any.
        CREATE DATA lo_req TYPE (<result>-zzipara).
        ASSIGN lo_req->* TO <fs_req>.
        ASSIGN COMPONENT 'ZNUMB' OF STRUCTURE <fs_req> TO FIELD-SYMBOL(<fs_value>).
        IF sy-subrc = 0.
          <fs_value> = <result>-zznumb.
        ENDIF.

        lv_json = /ui2/cl_json=>serialize( data        = <fs_req>
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        <result>-zzrequest =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = lv_json ).

      ENDIF.

      IF <result>-zzopara IS NOT INITIAL.
        FIELD-SYMBOLS:<fs_res>    TYPE any.
        CREATE DATA lo_resp TYPE (<result>-zzopara).
        ASSIGN lo_resp->* TO <fs_res>.

        lv_json = /ui2/cl_json=>serialize( data        = <fs_res>
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        <result>-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = lv_json ).
      ENDIF.
      " 报错消息处理
      IF lv_error = abap_true.
        APPEND VALUE #(  zznumb = <result>-zznumb
            %msg = new_message(
            id       = 'ZGL01'
            number   = 000
            severity = if_abap_behv_message=>severity-error
            v1       = lv_message
            )
        )  TO reported-conf.
      ELSE.
        APPEND VALUE #(  zznumb = <result>-zznumb
            %msg = new_message(
            id       = 'ZGL01'
            number   = 000
            severity = if_abap_behv_message=>severity-success
            v1       = '更新成功'
            )
        )  TO reported-conf.
      ENDIF.
    ENDLOOP.
    " 若无错则保存数据服务xsd 文件到template 附加xsdfile 文件字段中
    IF lv_error = abap_false.
      " 更新数据实体把产生xsd 内容和对应字段更新到对应实体上
      MODIFY ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
          ENTITY conf UPDATE FIELDS ( zzrequest zzresponse mimetype )
              WITH VALUE #( FOR conf IN results ( %tky        = conf-%tky
                                                  zzrequest   = conf-zzrequest
                                                  zzresponse  = conf-zzresponse
                                                  mimetype    = conf-mimetype ) ).
    ENDIF.

    result =  VALUE #( FOR conf IN results ( %tky   = conf-%tky
                                             %param =  conf ) ).

  ENDMETHOD.

ENDCLASS.
