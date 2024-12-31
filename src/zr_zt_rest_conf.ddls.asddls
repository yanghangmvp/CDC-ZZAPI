@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '##GENERATED ZZT_REST_CONF'
define root view entity ZR_ZT_REST_CONF
  as select from zzt_rest_conf
  composition [0..*] of ZR_ZT_REST_APPEND as _append
  association [0..1] to ZR_ZT_REST_SYSID  as _sysid on _sysid.ZztsysID = $projection.ZztsysID
{
  key zznumb                                                        as Zznumb,
      zzname                                                        as Zzname,
      zzisst                                                        as Zzisst,
      zzfname                                                       as Zzfname,
      zzipara                                                       as Zzipara,
      zzopara                                                       as Zzopara,
      @ObjectModel.foreignKey.association: '_sysid'
      zztsysid                                                      as ZztsysID,
      zzurlp                                                        as Zzurlp,
      @Semantics.user.createdBy: true
      created_by                                                    as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at                                                    as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by                                               as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at                                               as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at                                         as LocalLastChangedAt,
      _append,
      _sysid

}
