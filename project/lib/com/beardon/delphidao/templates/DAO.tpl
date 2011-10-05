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
    var fConnection: TConnection;
  protected
    function ReadRow(const Dataset: TClientDataSet): ${dao_class_name}; 
    function GetList(var Query: TTBGQuery): TObjectList<${dao_class_name}>;
    function GetRow(var Query: TTBGQuery): ${dao_class_name};
    function Execute(var Query: TTBGQuery): TClientDataSet;
    function ExecuteUpdate(var Query: TTBGQuery): Integer;
    function QuerySingleResult(var Query: TTBGQuery): string;
    function ExecuteInsert(var Query: TTBGQuery): Integer;	
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
  fConnection := AConnection;
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
function ${type_name}.ReadRow(const Dataset: TClientDataSet): ${dao_class_name};
var
  ${var_name}: ${dao_class_name};
begin
  ${var_name} := ${dao_class_name}.Create;
${read_row}
  Result := ${var_name};
end;
	
function ${type_name}.GetList(var Query: TTBGQuery): TObjectList<${dao_class_name}>;
var
  dataset: TClientDataSet;
  ${var_name}s: TObjectList<${dao_class_name}>;
begin
  dataset := TQueryExecutor.Execute(Query, fConnection);
  ${var_name}s := TObjectList<${dao_class_name}>.Create;
  ${var_name}s.OwnsObjects := True;
  while (not dataset.Eof) do
  begin
    ${var_name}s.Add(ReadRow(dataset));
    dataset.Next;
  end;
  Result := ${var_name}s;  
  dataset.Free;
end;
	
{**
 * Get row
 *
 * @return ${dao_class_name}
 *}
function ${type_name}.GetRow(var Query: TTBGQuery): ${dao_class_name};
var
  dataset: TClientDataSet;
begin
  dataset := TQueryExecutor.Execute(Query);
  Result := ReadRow(dataset);
  dataset.Free;
end; 
	
{**
 * Execute sql query
 *}
function ${type_name}.Execute(var Query: TTBGQuery): TClientDataSet;
begin
  Result := TQueryExecutor.Execute(Query, fConnection);
end; 

{**
 * Execute sql query
 *}
function ${type_name}.ExecuteUpdate(var Query: TTBGQuery): Integer;
begin
  Result := TQueryExecutor.ExecuteUpdate(Query, fConnection);
end; 

{**
 * Query for one row and one column
 *}
function ${type_name}.QuerySingleResult(var Query: TTBGQuery): string;
begin
  Result := TQueryExecutor.queryForString(Query, fConnection);
end; 

{**
 * Insert row to table
 *}
function ${type_name}.ExecuteInsert(var Query: TTBGQuery): Integer;
begin
  Result := TQueryExecutor.ExecuteInsert(Query, fConnection);
end; 

end.