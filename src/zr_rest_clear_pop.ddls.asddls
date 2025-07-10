@EndUserText.label: '日志清理弹出窗口'
define root abstract entity ZR_REST_CLEAR_POP
{
  @EndUserText.label      : '日志保留天数'
  @UI.defaultValue:'365'
  zzdate : abap.int2;
}
