@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '##GENERATED ZZT_REST_SYSID'
define root view entity ZR_ZT_REST_SYSID
  as select from zzt_rest_sysid
{
  key zztsysid as ZztsysID,
  zztsysnm as Zztsysnm,
  zzurl as Zzurl,
  zzfname as Zzfname,
  zzauty as Zzauty,
  zzuser as Zzuser,
  zzpwd as Zzpwd,
  zztkurl as Zztkurl,
  zzctid as ZzctID,
  zzctsecret as Zzctsecret,
  zzscope as Zzscope,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.lastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed_at as LastChangedAt,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed_at as LocalLastChangedAt
  
}
