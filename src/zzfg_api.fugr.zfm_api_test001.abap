FUNCTION zfm_api_test001.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_REQ) TYPE  ZZS_API_TEST001 OPTIONAL
*"  EXPORTING
*"     REFERENCE(O_RESP) TYPE  ZZS_REST_OUT
*"----------------------------------------------------------------------
  DATA:ls_data TYPE zzs_api_test001.

  ls_data = i_req.

  o_resp-msgty = 'S'.
  o_resp-msgtx = 'SUCCESS'.


ENDFUNCTION.
