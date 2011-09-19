{ $Id$ }
unit ${unit_name}DAO;

interface

uses
${uses_list}
  Generics.Collections;

type
  {**
   * Interface DAO
   *
   * @author: Aaron Bean
   * @date: ${date}
   *}
  ${type_name} = Interface(IInterface)
    function Load(const Id: Variant): ${dao_class_name};
    function QueryAll: TObjectList<${dao_class_name}>;
    function QueryAllOrderBy(const OrderColumn: string): TObjectList<${dao_class_name}>;
${query_by_definitions}
end;

implementation

end.