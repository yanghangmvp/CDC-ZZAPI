CLASS zzcl_vh_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider .
    METHODS get_class IMPORTING io_request  TYPE REF TO if_rap_query_request
                                io_response TYPE REF TO if_rap_query_response
                      RAISING   cx_rap_query_prov_not_impl
                                cx_rap_query_provider.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZZCL_VH_REPOSITORY IMPLEMENTATION.


  METHOD get_class.
    DATA: lt_class TYPE TABLE OF zr_vh_std_class.
    DATA(lo_package) = xco_cp_abap_repository=>package->for( 'ZZSTDAPI' ).


    DATA(lt_class_result) = xco_cp_abap_repository=>objects->clas->all->in( lo_package )->get(  ).


    lt_class = VALUE #( FOR result IN lt_class_result
                            ( classname = result->name
                              classdesc = result->content(  )->get_short_description( )  ) ).


    zzcl_query_utils=>filtering( EXPORTING io_filter = io_request->get_filter(  ) CHANGING ct_data = lt_class ).
    IF io_request->is_total_numb_of_rec_requested(  ) .
      io_response->set_total_number_of_records( lines( lt_class ) ).
    ENDIF.
    zzcl_query_utils=>orderby( EXPORTING it_order = io_request->get_sort_elements( )  CHANGING ct_data = lt_class ).
    zzcl_query_utils=>paging( EXPORTING io_paging = io_request->get_paging(  ) CHANGING ct_data = lt_class ).
    io_response->set_data( lt_class ).

  ENDMETHOD.


  METHOD if_rap_query_provider~select.
    TRY.
        CASE io_request->get_entity_id( ).

          WHEN 'ZR_VH_STD_CLASS'.
            get_class( io_request = io_request io_response = io_response ).
          WHEN 'ZZR_DTIMP_STRUC'.

        ENDCASE.

      CATCH cx_rap_query_provider.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
