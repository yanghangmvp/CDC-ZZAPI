@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_REST_APPEND'
@ObjectModel.semanticKey: [ 'Zznumb', 'Zzappac', 'Zzappkey' ]
define view entity ZC_ZT_REST_APPEND
  as projection on ZR_ZT_REST_APPEND
{
  key Zznumb,
  key Zzappac,
  key Zzappkey,
  Zzappvalue,
  LocalLastChangedAt,
  _conf: redirected to parent ZC_ZT_REST_CONF
  
}
