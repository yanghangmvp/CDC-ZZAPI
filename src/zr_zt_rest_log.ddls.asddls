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
      contentDispositionPreference: #ATTACHMENT }
      zzrequest,
      @Semantics.largeObject:
      { mimeType: 'mimeType',
      fileName: 'responseName',
      contentDispositionPreference: #ATTACHMENT }
      zzresponse,
      zzsapn,
      msgty,
      ernam,
      bdate,
      btime,
      rdate,
      rtime,
      mimetype                       as mimeType,
      concat(zznumb,'-Request.txt')  as requestName,
      concat(zznumb,'-Response.txt') as responseName,
      case msgty when 'S' then 3
      else 2 end                     as CriticalityLine //1 Red 2 Yellow 3 Green

}
