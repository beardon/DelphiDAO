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
${query_by_definitions}
  end;

implementation

end.