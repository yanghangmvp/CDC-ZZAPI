CLASS zzcl_http_rest DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zzcl_http_rest IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    DATA:i_json TYPE string,
         o_json TYPE string.

*&--获取传入json报文
    i_json = request->get_text( ).

*&--SAP REST API distribute functions
    DATA:lo_rest_api TYPE REF TO zzcl_rest_api.
    CREATE OBJECT lo_rest_api.

    lo_rest_api->inbound( EXPORTING i_json = i_json CHANGING o_json = o_json ).

*&--返回HTTP JSON报文
    response->set_text( i_text = o_json ).
    response->set_content_type( content_type = 'application/json' ).
  ENDMETHOD.
ENDCLASS.
