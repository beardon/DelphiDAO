unit ${unit_name};

interface

uses
${uses_list}
  SQLComparisonOperator,
  SQLOrderDirection,
  TbgQuery;

type
  {**
   * Class that interfaces with database table '${table_name}'.
   * This class is generated by DelphiDAO, and will be overwritten.
   *
   * @author: Aaron Bean
   *}
  ${type_name} = class(TObject)
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
    function Delete(const ${pk}: Variant): Integer;
    function Insert(var ${var_name}: ${dao_class_name}): Integer;
    function Update(var ${var_name}: ${dao_class_name}): Integer;
    function Clean: Integer;
${query_by_definitions}
${delete_by_definitions}
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
 * Get all records from table
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
 * Get all records from table ordered by field
 *
 * @param OrderColumn column name
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
	
{**
 * Delete record from table
 * @param ${var_name} primary key
 *}
function ${type_name}.Delete(const ${pk}: Variant): Integer;
var
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.SQL.Add('DELETE FROM ${table_name} WHERE ${pk} = :${pk}');
  qry.ParamByName('${pk}').Value := ${pk};
  qry.Execute;
  Result := qry.RowsAffected;
  qry.Free;
end;
	
{**
 * Insert record to table
 *
 * @param ${dao_class_name} ${var_name}
 *}
function ${type_name}.Insert(var ${var_name}: ${dao_class_name}): Integer;
var
  id: Integer;
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.SQL.Add('INSERT INTO ${table_name}');
  qry.SQL.Add('(${insert_fields})');
  qry.SQL.Add('VALUES');
  qry.SQL.Add('(${insert_values})');
${parameter_setter}
  qry.Execute;
  id := qry.InsertId;
  ${var_name}.${pk_with_s} := id;
  Result := id;
  qry.Free;
end;
 	
{**
 * Update record in table
 *
 * @param ${dao_class_name} ${var_name}
 *}
function ${type_name}.Update(var ${var_name}: ${dao_class_name}): Integer;
var
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.SQL.Add('UPDATE ${table_name}');
  qry.SQL.Add('SET ${update_fields}');
  qry.SQL.Add('WHERE ${pk} = :${pk_with_s}');
${parameter_setter}
  qry.ParamByName('${pk_with_s}').Value := ${var_name}.${pk_with_s};
  qry.Execute;
  Result := qry.RowsAffected;
  qry.Free;
end;

{**
 * Delete all rows
 *}
function ${type_name}.Clean: Integer;
var
  qry: TTbgQuery;
begin
  qry := TTbgQuery.Create(nil);
  qry.sql.Add('DELETE FROM ${table_name}');
  qry.Execute;
  Result := qry.RowsAffected;
  qry.Free;
end;

${query_by_functions}
${delete_by_functions}

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
