@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '接口编号搜索帮助'
define view entity ZR_VH_REST_NUM
  as select from zzt_rest_conf
{
  key zznumb,
      zzname
}
