@EndUserText.label: '标准类搜索帮助'
@ObjectModel.query.implementedBy:'ABAP:ZZCL_VH_REPOSITORY'
define custom entity ZR_VH_STD_CLASS
{
      @EndUserText.label : '处理类'
  key ClassName : abap.char( 30 );
      @EndUserText.label : '类描述'
      ClassDesc : abap.sstring( 120 );

}
