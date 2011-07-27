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
    function queryAll: TList<${dao_class_name}>;
    function queryAllOrderBy(const orderColumn: string): TList<${dao_class_name}>;
    function delete(const ${pk}: Variant): Integer;
    function insert(var ${var_name}: ${dao_class_name}): Integer;
    function update(var ${var_name}: ${dao_class_name}): Integer;
    function clean: Integer;
${functions}
end;

implementation

end.