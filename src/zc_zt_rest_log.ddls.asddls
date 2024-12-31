@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_REST_LOG'
define root view entity ZC_ZT_REST_LOG
  provider contract transactional_query
  as projection on ZR_ZT_REST_LOG
{
  key uuid,
      zzfsysid,
      zztsysid,
      @ObjectModel.text.element: [ 'zzname' ]
      zznumb,
      zzname,
      zzrequest,
      zzresponse,
      zzsapn,
      msgty,
      ernam,
      bdate,
      btime,
      btstmpl,
      rdate,
      rtime,
      rtstmpl,
      ctstmpl,
      mimeType,
      requestName,
      responseName,
      CriticalityLine

}
