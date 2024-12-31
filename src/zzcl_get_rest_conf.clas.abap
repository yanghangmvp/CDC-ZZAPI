CLASS zzcl_get_rest_conf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_GET_REST_CONF IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lt_original_data TYPE STANDARD TABLE OF zc_zt_rest_conf WITH DEFAULT KEY.
    lt_original_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).
      IF <fs_original_data>-zztsysid IS NOT INITIAL.
        DATA(lv_url) = |#zzapisysid-display&/sysid('{ <fs_original_data>-zztsysid }')|.
        <fs_original_data>-zztsysidurl = lv_url.
      ENDIF.
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #(  lt_original_data ).
  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

  ENDMETHOD.
ENDCLASS.
