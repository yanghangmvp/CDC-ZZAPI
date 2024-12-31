@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '##GENERATED ZR_ZT_REST_LOG'
define root view entity ZR_ZT_REST_LOG
  as select from zzt_rest_log
{
  key uuid,
      zzfsysid,
      zztsysid,
      zznumb,
      zzname,
      @Semantics.largeObject:
      { mimeType: 'mimeType',
      fileName: 'requestName',
      contentDispositionPreference: #INLINE }
      zzrequest,
      @Semantics.largeObject:
      { mimeType: 'mimeType',
      fileName: 'responseName',
      contentDispositionPreference: #INLINE }
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
      mimetype                       as mimeType,
      concat(uuid,'-Request.txt')  as requestName,
      concat(uuid,'-Response.txt') as responseName,
      case msgty when 'S' then 3
      else 1 end                     as CriticalityLine //1 Red 2 Yellow 3 Green

}
