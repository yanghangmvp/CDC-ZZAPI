@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_REST_CONF'
@ObjectModel.semanticKey: [ 'Zznumb' ]
define root view entity ZC_ZT_REST_CONF
  provider contract transactional_query
  as projection on ZR_ZT_REST_CONF
{
  key Zznumb,
  Zzname,
  Zzisst,
  Zzfname,
  Zzipara,
  Zzopara,
  ZztsysID,
  Zzurlp,
  LocalLastChangedAt,
  _sysid,
  _append:redirected to composition child ZC_ZT_REST_APPEND
  
}
