@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '##GENERATED ZZT_REST_APPEND'
define view entity ZR_ZT_REST_APPEND
  as select from zzt_rest_append
  association to parent ZR_ZT_REST_CONF as _conf on _conf.Zznumb = $projection.Zznumb
{
  key zznumb                as Zznumb,
  key zzappac               as Zzappac,
  key zzappkey              as Zzappkey,
      zzappvalue            as Zzappvalue,
      @Semantics.user.createdBy: true
      created_by            as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at            as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by       as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      _conf

}
