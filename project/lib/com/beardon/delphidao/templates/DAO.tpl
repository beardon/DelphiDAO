{ $Id$ }
unit ${unit_name}MySQLDAO;

interface

uses
${uses_list}
  Connection,
  DB,
  DBClient,
  Generics.Collections,
  Query,
  QueryExecutor,
  SQLComparisonOperator,
  SQLOrderDirection;

type
  {**
   * Class that operates on MySQL table '${table_name}'.
   *
   * @author: Aaron Bean
   * @date: ${date}
   *}
  ${type_name} = class(TInterfacedObject, ${interface_name})
  private
    const INDEX_FIELD_MAP: ${mapping_array};
    var FConnection: TConnection;
  protected
    function ReadRow(const AClientDataset: TClientDataSet): ${dao_class_name}; 
    function GetList(var AQuery: TTBGQuery): TObjectList<${dao_class_name}>;
    function GetRow(var AQuery: TTBGQuery): ${dao_class_name};
    function Execute(var AQuery: TTBGQuery): TClientDataSet;
    function ExecuteUpdate(var AQuery: TTBGQuery): Integer;
    function QuerySingleResult(var AQuery: TTBGQuery): string;
    function ExecuteInsert(var AQuery: TTBGQuery): Integer;	
  public
${index_constants}
    constructor Create(AConnection: TConnection);
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

constructor ${type_name}.Create(AConnection: TConnection);
begin
  FConnection := AConnection;
end;

{**
 * Get Domain object by primary key
 *
 * @param String Id primary key
 * @return ${dao_class_name}
 *}
function ${type_name}.Load(const Id: Variant): ${dao_class_name};
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name} WHERE ${pk} = :${pk}');
  qry.paramByName('${pk}').Value := Id;
  Result := GetRow(qry);
  qry.Free;
end;

{**
 * Get all records from table
 *}
function ${type_name}.QueryAll: TObjectList<${dao_class_name}>;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name}');
  Result := GetList(qry);
  qry.Free;
end;
	
{**
 * Get all records from table ordered by field
 *
 * @param OrderColumn column name
 *}
function ${type_name}.QueryAllOrderBy(const OrderColumn: string): TObjectList<${dao_class_name}>;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name} ORDER BY ' + OrderColumn);
  Result := GetList(qry);
  qry.Free;
end;
	
{**
 * Delete record from table
 * @param ${var_name} primary key
 *}
function ${type_name}.Delete(const ${pk}: Variant): Integer;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('DELETE FROM ${table_name} WHERE ${pk} = :${pk}');
  qry.paramByName('${pk}').Value := ${pk};
  Result := ExecuteUpdate(qry);
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
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('INSERT INTO ${table_name}');
  qry.sql.Add('(${insert_fields})');
  qry.sql.Add('VALUES');
  qry.sql.Add('(${insert_values})');
${parameter_setter}
  id := ExecuteInsert(qry);
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
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('UPDATE ${table_name}');
  qry.sql.Add('SET ${update_fields}');
  qry.sql.Add('WHERE ${pk} = :${pk_with_s}');
${parameter_setter}
  qry.paramByName('${pk_with_s}').Value := ${var_name}.${pk_with_s};
  Result := ExecuteUpdate(qry);
  qry.Free;
end;

{**
 * Delete all rows
 *}
function ${type_name}.Clean: Integer;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('DELETE FROM ${table_name}');
  Result := ExecuteUpdate(qry);
  qry.Free;
end;

${query_by_functions}
${delete_by_functions}

{**
 * Read row
 *
 * @return ${dao_class_name}
 *}
function ${type_name}.ReadRow(const AClientDataset: TClientDataSet): ${dao_class_name};
var
  ${var_name}: ${dao_class_name};
begin
  ${var_name} := ${dao_class_name}.Create;
  if (not AClientDataset.IsEmpty) then
  begin
${read_row}
  end;
  Result := ${var_name};
end;
	
function ${type_name}.GetList(var AQuery: TTBGQuery): TObjectList<${dao_class_name}>;
var
  aClientDataSet: TClientDataSet;
  ${var_name}s: TObjectList<${dao_class_name}>;
begin
  aClientDataSet := TQueryExecutor.Execute(AQuery, FConnection);
  ${var_name}s := TObjectList<${dao_class_name}>.Create;
  ${var_name}s.OwnsObjects := True;
  while (not aClientDataSet.Eof) do
  begin
    ${var_name}s.Add(ReadRow(aClientDataSet));
    aClientDataSet.Next;
  end;
  Result := ${var_name}s;  
  aClientDataSet.Free;
end;
	
{**
 * Get row
 *
 * @return ${dao_class_name}
 *}
function ${type_name}.GetRow(var AQuery: TTBGQuery): ${dao_class_name};
var
  aClientDataSet: TClientDataSet;
begin
  aClientDataSet := TQueryExecutor.Execute(AQuery);
  Result := ReadRow(aClientDataSet);
  aClientDataSet.Free;
end; 
	
{**
 * Execute sql query
 *}
function ${type_name}.Execute(var AQuery: TTBGQuery): TClientDataSet;
begin
  Result := TQueryExecutor.Execute(AQuery, FConnection);
end; 

{**
 * Execute sql query
 *}
function ${type_name}.ExecuteUpdate(var AQuery: TTBGQuery): Integer;
begin
  Result := TQueryExecutor.ExecuteUpdate(AQuery, FConnection);
end; 

{**
 * Query for one row and one column
 *}
function ${type_name}.QuerySingleResult(var AQuery: TTBGQuery): string;
begin
  Result := TQueryExecutor.QueryForString(AQuery, FConnection);
end; 

{**
 * Insert row to table
 *}
function ${type_name}.ExecuteInsert(var AQuery: TTBGQuery): Integer;
begin
  Result := TQueryExecutor.ExecuteInsert(AQuery, FConnection);
end; 

end.