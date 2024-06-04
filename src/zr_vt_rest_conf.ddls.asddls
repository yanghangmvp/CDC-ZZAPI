@AbapCatalog.sqlViewName: 'ZV_REST_CONF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'API配置视图'
define view ZR_VT_REST_CONF
  as select from    zzt_rest_conf  as _conf
    left outer join zzt_rest_sysid as _sysid on _conf.zztsysid = _sysid.zztsysid
{
  key _conf.zznumb,
      _conf.zzname,
      _conf.zzisst,
      _conf.zzfname,
      _conf.zzipara,
      _conf.zzopara,
      _conf.zztsysid,
      _sysid.zztsysnm,
      _sysid.zzurl,
      _conf.zzurlp,
      concat(_sysid.zzurl,_conf.zzurlp) as zzurlc,
      _sysid.zzfname                    as zzcname,
      _sysid.zzauty,
      _sysid.zzuser,
      _sysid.zzpwd,
      _sysid.zztkurl,
      _sysid.zzctid,
      _sysid.zzctsecret,
      _sysid.zzscope
}
