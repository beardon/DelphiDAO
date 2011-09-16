{ $Id$ }
unit ${unit_name}_mysql_dao;

interface

uses
${uses_list}
  connection,
  DB,
  DBClient,
  Generics.Collections,
  query,
  query_executor;

type
  {**
   * Class that operates on MySQL table '${table_name}'.
   *
   * @author: Aaron Bean
   * @date: ${date}
   *}
  ${type_name} = class(TInterfacedObject, ${interface_name})
  protected
    function readRow(const dataset: TClientDataSet): ${dao_class_name}; 
    function getList(var qry: TTBGQuery): TObjectList<${dao_class_name}>;
    function getRow(var qry: TTBGQuery): ${dao_class_name};
    function execute(var qry: TTBGQuery): TClientDataSet;
    function executeUpdate(var qry: TTBGQuery): Integer;
    function querySingleResult(var qry: TTBGQuery): string;
    function executeInsert(var qry: TTBGQuery): Integer;	
  public
    function load(const id: Variant): ${dao_class_name};
    function queryAll: TObjectList<${dao_class_name}>;
    function queryAllOrderBy(const orderColumn: string): TObjectList<${dao_class_name}>;
    function delete(const ${pk}: Variant): Integer;
    function insert(var ${var_name}: ${dao_class_name}): Integer;
    function update(var ${var_name}: ${dao_class_name}): Integer;
    function clean: Integer;
${public_functions}
end;

implementation

{**
 * Get Domain object by primary key
 *
 * @param String id primary key
 * @return ${dao_class_name}
 *}
function ${type_name}.load(const ${pks}: Variant): ${dao_class_name};
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name} WHERE ${pk_where}');
  ${pk_set}
  Result := getRow(qry);
  qry.Free;
end;

{**
 * Get all records from table
 *}
function ${type_name}.queryAll: TObjectList<${dao_class_name}>;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name}');
  Result := getList(qry);
  qry.Free;
end;
	
{**
 * Get all records from table ordered by field
 *
 * @param orderColumn column name
 *}
function ${type_name}.queryAllOrderBy(const orderColumn: string): TObjectList<${dao_class_name}>;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name} ORDER BY ' + orderColumn);
  Result := getList(qry);
  qry.Free;
end;
	
{**
 * Delete record from table
 * @param ${var_name} primary key
 *}
function ${type_name}.delete(const ${pks}: Variant): Integer;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('DELETE FROM ${table_name} WHERE ${pk_where}');
  ${pk_set}
  Result := executeUpdate(qry);
  qry.Free;
end;
	
{**
 * Insert record to table
 *
 * @param ${dao_class_name} ${var_name}
 *}
function ${type_name}.insert(var ${var_name}: ${dao_class_name}): Integer;
var
  id: Integer;
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('INSERT INTO ${table_name}');
  qry.sql.Add('(${insert_fields2})');
  qry.sql.Add('VALUES');
  qry.sql.Add('(${insert_values2})');
${parameter_setter}
${pk_set_update}
  id := executeInsert(qry);
  Result := id;
  qry.Free;
end;
 	
{**
 * Update record in table
 *
 * @param ${dao_class_name} ${var_name}
 *}
function ${type_name}.update(var ${var_name}: ${dao_class_name}): Integer;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('UPDATE ${table_name}');
  qry.sql.Add('SET ${update_fields}');
  qry.sql.Add('WHERE ${pk_where}');
${parameter_setter}
${pk_set_update}
  Result := executeUpdate(qry);
  qry.Free;
end;

{**
 * Delete all rows
 *}
function ${type_name}.clean: Integer;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('DELETE FROM ${table_name}');
  Result := executeUpdate(qry);
  qry.Free;
end;

${query_by_functions}
${delete_by_functions}

{**
 * Read row
 *
 * @return ${dao_class_name}
 *}
function ${type_name}.readRow(const dataset: TClientDataSet): ${dao_class_name};
var
  ${var_name}: ${dao_class_name};
begin
  ${var_name} := ${dao_class_name}.Create;
${read_row}
  Result := ${var_name};
end;
	
function ${type_name}.getList(var qry: TTBGQuery): TObjectList<${dao_class_name}>;
var
  dataset: TClientDataSet;
  ${var_name}s: TObjectList<${dao_class_name}>;
begin
  dataset := TQueryExecutor.execute(qry);
  ${var_name}s := TObjectList<${dao_class_name}>.Create;
  ${var_name}s.OwnsObjects := True;
  while (not dataset.Eof) do
  begin
	${var_name}s.Add(readRow(dataset));
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
function ${type_name}.getRow(var qry: TTBGQuery): ${dao_class_name};
var
  dataset: TClientDataSet;
begin
  dataset := TQueryExecutor.execute(qry);
  Result := readRow(dataset);
  dataset.Free;
end; 
	
{**
 * Execute sql query
 *}
function ${type_name}.execute(var qry: TTBGQuery): TClientDataSet;
begin
  Result := TQueryExecutor.execute(qry);
end; 

{**
 * Execute sql query
 *}
function ${type_name}.executeUpdate(var qry: TTBGQuery): Integer;
begin
  Result := TQueryExecutor.executeUpdate(qry);
end; 

{**
 * Query for one row and one column
 *}
function ${type_name}.querySingleResult(var qry: TTBGQuery): string;
begin
  Result := TQueryExecutor.queryForString(qry);
end; 

{**
 * Insert row to table
 *}
function ${type_name}.executeInsert(var qry: TTBGQuery): Integer;
begin
  Result := TQueryExecutor.executeInsert(qry);
end; 

end.