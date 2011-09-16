{ $Id$ }
unit ${unit_name}_dao;

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
    function load(const id: Variant): ${dao_class_name};
    function queryAll: TObjectList<${dao_class_name}>;
    function queryAllOrderBy(const orderColumn: string): TObjectList<${dao_class_name}>;
${query_by_definitions}
end;

implementation

end.