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
  ${type_name} = interface['${guid}']
    function Load(const Id: Variant): ${dao_class_name};
    function QueryAll: TObjectList<${dao_class_name}>;
    function QueryAllOrderBy(const OrderColumn: string): TObjectList<${dao_class_name}>;
    function Delete(const ${pk}: Variant): Integer;
    function Insert(var ${var_name}: ${dao_class_name}): Integer;
    function Update(var ${var_name}: ${dao_class_name}): Integer;
    function Clean: Integer;
${query_by_definitions}
${delete_by_definitions}
  end;

implementation

end.