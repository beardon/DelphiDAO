unit ${unit_name};

interface

uses
${uses_list};

type
  {**
   * Interface DAO
   *
   * @author: Aaron Bean
   *}
  ${type_name} = interface
    function Load(const Id: Variant): ${dao_class_name};
    function QueryAll: ${dao_list_class_name};
    function QueryAllOrderBy(const OrderColumn: string): ${dao_list_class_name};
    function Delete(const ${pk}: Variant): Integer;
    function Insert(var ${param_name}: ${dao_class_name}): Integer;
    function Update(var ${param_name}: ${dao_class_name}): Integer;
    function Clean: Integer;
${query_by_definitions}
${delete_by_definitions}
  end;

implementation

end.