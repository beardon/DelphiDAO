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
    function delete(const ${pk}: Variant): Integer;
    function insert(var ${var_name}: ${dao_class_name}): Integer;
    function update(var ${var_name}: ${dao_class_name}): Integer;
    function clean: Integer;
${query_by_definitions}
${delete_by_definitions}
end;

implementation

end.