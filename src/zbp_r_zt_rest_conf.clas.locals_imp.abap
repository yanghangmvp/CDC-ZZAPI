
CLASS lhc_conf DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR conf RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR conf RESULT result.

    METHODS createjson FOR DETERMINE ON MODIFY
      IMPORTING keys FOR conf~createjson.

    METHODS validatestruct FOR VALIDATE ON SAVE
      IMPORTING keys FOR conf~validatestruct.
    METHODS refresh FOR MODIFY
      IMPORTING keys FOR ACTION conf~refresh RESULT result.

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
    DATA: lo_item  TYPE REF TO data,
          lv_jitem TYPE string.

    DATA(lr_tool) = NEW zzcl_print_tool( pv_typename ).
    DATA(lt_dd03) = lr_tool->mt_dd03.
    DELETE lr_tool->mt_dd03 WHERE inttype <> 'TTYP'.
    LOOP AT lr_tool->mt_dd03 INTO DATA(ls_data).

      READ TABLE lt_dd03 INTO DATA(ls_dd03) WITH KEY parent = ls_data-rollname deep = ls_data-deep + 1.
      CHECK sy-subrc = 0.
      FIELD-SYMBOLS:<fs_item>  TYPE any.
      "参数结构中的表类型
      CREATE DATA lo_item TYPE (ls_dd03-tabname).
      ASSIGN lo_item->* TO <fs_item>.

      lv_jitem = /ui2/cl_json=>serialize( data        = <fs_item>
                                          pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      "处理为驼峰命名法
      SPLIT ls_data-rollname AT '_' INTO TABLE DATA(lt_words).
      LOOP AT lt_words INTO DATA(lv_word).
        IF sy-tabix = 1.
          ls_data-rollname = lv_word.
          TRANSLATE ls_data-rollname TO LOWER CASE.
        ELSE.
          DATA(lv_c) =  lv_word+0(1).
          DATA(lv_str2) = lv_word+1(*).
          TRANSLATE lv_c TO UPPER CASE.
          TRANSLATE lv_str2 TO LOWER CASE.
          CONCATENATE  ls_data-rollname lv_c lv_str2 INTO  ls_data-rollname.
        ENDIF.
      ENDLOOP.

      CONCATENATE '"' ls_data-rollname '":[' lv_jitem  ']' INTO lv_jitem.
      DATA:lv_str TYPE string.
      CONCATENATE '"' ls_data-rollname '":' '[]' INTO lv_str.
      REPLACE ALL OCCURRENCES OF lv_str IN  cv_json WITH lv_jitem.
    ENDLOOP.
  ENDMETHOD.

  METHOD createjson.
    DATA: lv_error   TYPE abap_boolean,
          lv_message TYPE string.
    DATA: lo_req  TYPE REF TO data,
          lo_resp TYPE REF TO data,
          lv_json TYPE string.
*&---获取UI 界面实体数据内容
    READ  ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
    ENTITY conf ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(results).

*&---出来数据文件
    LOOP AT results ASSIGNING FIELD-SYMBOL(<result>).
      <result>-mimetype = 'application/json'.

      IF <result>-zzipara IS NOT INITIAL.
        DATA(lo_structure) = xco_cp_abap_dictionary=>structure( <result>-zzipara  ).
        IF lo_structure->exists( ) = abap_true.
          FIELD-SYMBOLS:<fs_req>    TYPE any.
          CREATE DATA lo_req TYPE (<result>-zzipara).
          ASSIGN lo_req->* TO <fs_req>.
          ASSIGN COMPONENT 'ZNUMB' OF STRUCTURE <fs_req> TO FIELD-SYMBOL(<fs_value>).
          IF sy-subrc = 0.
            <fs_value> = <result>-zznumb.
          ENDIF.

          lv_json = /ui2/cl_json=>serialize( data        = <fs_req>
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

          me->set_json( EXPORTING pv_typename = <result>-zzipara CHANGING cv_json = lv_json  ).

          <result>-zzrequest =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = lv_json ).
        ENDIF.
      ELSE.
        CLEAR:<result>-zzrequest .
      ENDIF.

      IF <result>-zzopara IS NOT INITIAL.
        lo_structure = xco_cp_abap_dictionary=>structure( <result>-zzopara  ).
        IF  lo_structure->exists( ) = abap_true.
          FIELD-SYMBOLS:<fs_res>    TYPE any.
          CREATE DATA lo_resp TYPE (<result>-zzopara).
          ASSIGN lo_resp->* TO <fs_res>.

          lv_json = /ui2/cl_json=>serialize( data        = <fs_res>
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
          me->set_json( EXPORTING pv_typename = <result>-zzopara CHANGING cv_json = lv_json  ).
          <result>-zzresponse =  /ui2/cl_json=>string_to_raw( EXPORTING iv_string = lv_json ).
        ENDIF.
      ELSE.
        CLEAR: <result>-zzresponse.
      ENDIF.

    ENDLOOP.

    " 更新数据实体把产生xsd 内容和对应字段更新到对应实体上
    MODIFY ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
        ENTITY conf UPDATE FIELDS ( zzrequest zzresponse mimetype )
            WITH VALUE #( FOR conf IN results ( %tky        = conf-%tky
                                                zzrequest   = conf-zzrequest
                                                zzresponse  = conf-zzresponse
                                                mimetype    = conf-mimetype ) ).
  ENDMETHOD.

  "验证数据
  METHOD validatestruct.
    READ ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
  ENTITY conf
     ALL FIELDS WITH CORRESPONDING #( keys )
  RESULT DATA(results).

    LOOP AT results INTO DATA(ls_result).
      APPEND VALUE #(  %tky               = ls_result-%tky
                       %state_area        = 'VALIDATE_STRUCT' ) TO reported-conf.

      IF ls_result-zzipara IS NOT INITIAL.
        DATA(lo_structure) = xco_cp_abap_dictionary=>structure( to_upper( ls_result-zzipara  )  ).
        IF lo_structure->exists( ) = abap_false.
          APPEND VALUE #( %tky = ls_result-%tky ) TO failed-conf.
          APPEND VALUE #( %tky               = ls_result-%tky
                          %state_area        = 'VALIDATE_STRUCT'
                           %msg              =  new_message_with_text(
                                                    text = '传入参数结构不存在'
                                                    severity = if_abap_behv_message=>severity-error
                                                    )
                          %element-zzipara = if_abap_behv=>mk-on ) TO reported-conf.

        ENDIF.
      ENDIF.
      IF ls_result-zzopara IS NOT INITIAL.
        lo_structure = xco_cp_abap_dictionary=>structure(  to_upper( ls_result-zzopara ) ).
        IF lo_structure->exists( ) = abap_false.
          APPEND VALUE #( %tky = ls_result-%tky ) TO failed-conf.
          APPEND VALUE #( %tky               = ls_result-%tky
                          %state_area        = 'VALIDATE_STRUCT'
                           %msg              = new_message_with_text(
                                                    text = '传出参数结构不存在'
                                                    severity = if_abap_behv_message=>severity-error
                                                    )
                          %element-zzopara = if_abap_behv=>mk-on ) TO reported-conf.

        ENDIF.
      ENDIF.

      IF ls_result-zzfname IS NOT INITIAL.
        IF ls_result-zzfname CP 'ZZFM*'.
*          DATA(lo_func) = xco_cp_abap_repository=>object->fugr->f( iv_name = ls_result-zzfname ).
        ENDIF.
        IF ls_result-zzfname CP 'ZZCL*'.
          DATA(lo_class) = xco_cp_abap_repository=>object->clas->for( iv_name = to_upper( ls_result-zzfname ) ).
          IF lo_class->exists( ) = abap_false.
            APPEND VALUE #( %tky = ls_result-%tky ) TO failed-conf.
            APPEND VALUE #( %tky               = ls_result-%tky
                            %state_area        = 'VALIDATE_STRUCT'
                             %msg              = new_message_with_text(
                                                      text = '处理类不存在'
                                                      severity = if_abap_behv_message=>severity-error
                                                      )
                            %element-zzfname = if_abap_behv=>mk-on ) TO reported-conf.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD refresh.
    DATA:lt_keys TYPE TABLE FOR DETERMINATION zr_zt_rest_conf\\conf~createjson.
    MOVE-CORRESPONDING keys TO lt_keys.
    me->createjson(
      EXPORTING
        keys     =  lt_keys
    ).

    READ ENTITIES OF zr_zt_rest_conf IN LOCAL MODE
  ENTITY conf
     ALL FIELDS WITH CORRESPONDING #( keys )
  RESULT DATA(lt_results).

    LOOP AT lt_results INTO DATA(ls_results).
      APPEND VALUE #( %tky               = ls_results-%tky
                      %msg              = new_message_with_text(
                                               text = 'Refresh successful!'
                                               severity = if_abap_behv_message=>severity-success
                                               )
                   ) TO reported-conf.

    ENDLOOP.

    result = VALUE #( FOR ls_travel IN lt_results ( %tky = ls_travel-%tky
                                                    %param    = ls_travel
                                              )  ).
  ENDMETHOD.

ENDCLASS.
