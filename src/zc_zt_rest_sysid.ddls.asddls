@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View for ZR_ZT_REST_SYSID'
@ObjectModel.semanticKey: [ 'ZztsysID' ]
define root view entity ZC_ZT_REST_SYSID
  provider contract transactional_query
  as projection on ZR_ZT_REST_SYSID
{
  key ZztsysID,
  Zztsysnm,
  Zzurl,
  Zzfname,
  Zzauty,
  Zzuser,
  Zzpwd,
  Zztkurl,
  ZzctID,
  Zzctsecret,
  Zzscope,
  LocalLastChangedAt
  
}
