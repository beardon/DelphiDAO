unit ${unit_name};

interface

uses
${uses_list}
  SQLComparisonOperator,
  SQLOrderDirection;

type
  {**
   * Class that interfaces with database view '${table_name}'.
   * This class is generated by DelphiDAO, and will be overwritten.
   *
   * @author: Aaron Bean
   *}
  ${type_name} = class(TInterfacedObject, ${interface_name})
  private
    const INDEX_FIELD_MAP: ${mapping_array};
  protected
    function ReadRow(const AQuery: TTbgQuery): ${dao_class_name};
    function GetList(var AQuery: TTbgQuery): ${dao_list_class_name};
    function GetRow(var AQuery: TTbgQuery): ${dao_class_name};
  public
${index_constants}
    function Load(const Id: Variant): ${dao_class_name};
    function QueryAll: ${dao_list_class_name};
    function QueryAllOrderBy(const OrderColumn: string): ${dao_list_class_name};
${query_by_definitions}
  end;

implementation

uses
  Data.DB;

{**
 * Get Domain object by primary key
 *
 * @param String Id primary key
 * @return ${dao_class_name}
 *}
function ${type_name}.Load(const Id: Variant): ${dao_class_name};
var
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.SQL.Add('SELECT * FROM ${table_name} WHERE ${pk} = :${pk}');
  qry.ParamByName('${pk}').Value := Id;
  Result := GetRow(qry);
  qry.Free;
end;

{**
 * Get all records from view
 *}
function ${type_name}.QueryAll: ${dao_list_class_name};
var
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.SQL.Add('SELECT * FROM ${table_name}');
  Result := GetList(qry);
  qry.Free;
end;
	
{**
 * Get all records from view ordered by field
 *
 * @param orderColumn column name
 *}
function ${type_name}.QueryAllOrderBy(const OrderColumn: string): ${dao_list_class_name};
var
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.SQL.Add('SELECT * FROM ${table_name} ORDER BY ' + OrderColumn);
  Result := GetList(qry);
  qry.Free;
end;
	
${query_by_functions}

{**
 * Read row
 *
 * @return ${dao_class_name}
 *}
function ${type_name}.ReadRow(const AQuery: TTbgQuery): ${dao_class_name};
var
  ${var_name}: ${dao_class_name};
begin
  ${var_name} := ${dao_class_name}.Create;
  if not AQuery.IsEmpty then
  begin
${read_row}
  end;
  Result := ${var_name};
end;
	
function ${type_name}.GetList(var AQuery: TTbgQuery): ${dao_list_class_name};
var
  ${var_name}s: ${dao_list_class_name};
begin
  AQuery.Execute;
  ${var_name}s := ${dao_list_class_name}.Create;
  ${var_name}s.OwnsObjects := True;
  while not AQuery.Eof do
  begin
    ${var_name}s.Add(ReadRow(AQuery));
    AQuery.Next;
  end;
  Result := ${var_name}s;  
end;

{**
 * Get row
 *
 * @return ${dao_class_name}
 *}
function ${type_name}.GetRow(var AQuery: TTbgQuery): ${dao_class_name};
begin
  AQuery.Execute;
  Result := ReadRow(AQuery);
end;

end.
