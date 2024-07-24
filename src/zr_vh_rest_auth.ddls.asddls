@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '认证方式搜索帮助'
define view entity ZR_VH_REST_AUTH
  as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZZDAUTY' ) as a

{
  key a.value_low as value,
      a.text
}
