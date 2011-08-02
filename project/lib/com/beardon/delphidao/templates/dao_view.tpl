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
   * Class that operates on MySQL view '${table_name}'.
   *
   * @author: Aaron Bean
   * @date: ${date}
   *}
  ${type_name} = class(TInterfacedObject, ${interface_name})
  protected
    function readRow(const dataset: TClientDataSet): ${dao_class_name}; 
    function getList(var qry: TTBGQuery): TList<${dao_class_name}>;
    function getRow(var qry: TTBGQuery): ${dao_class_name};
    function execute(var qry: TTBGQuery): TClientDataSet;
    function querySingleResult(var qry: TTBGQuery): string;
  public
    function load(const id: Variant): ${dao_class_name};
    function queryAll: TList<${dao_class_name}>;
    function queryAllOrderBy(const orderColumn: string): TList<${dao_class_name}>;
${query_by_definitions}
end;

implementation

{**
 * Get Domain object by primary key
 *
 * @param String id primary key
 * @return ${dao_class_name}
 *}
function ${type_name}.load(const id: Variant): ${dao_class_name};
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name} WHERE ${pk} = :${pk}');
  qry.paramByName('${pk}').Value := id;
  Result := getRow(qry);
  qry.Free;
end;

{**
 * Get all records from view
 *}
function ${type_name}.queryAll: TList<${dao_class_name}>;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name}');
  Result := getList(qry);
  qry.Free;
end;
	
{**
 * Get all records from view ordered by field
 *
 * @param orderColumn column name
 *}
function ${type_name}.queryAllOrderBy(const orderColumn: string): TList<${dao_class_name}>;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('SELECT * FROM ${table_name} ORDER BY ' + orderColumn);
  Result := getList(qry);
  qry.Free;
end;
	
${query_by_functions}

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
	
function ${type_name}.getList(var qry: TTBGQuery): TList<${dao_class_name}>;
var
  dataset: TClientDataSet;
  ${var_name}s: TList<${dao_class_name}>;
begin
  dataset := TQueryExecutor.execute(qry);
  ${var_name}s := TList<${dao_class_name}>.Create;
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
 * Query for one row and one column
 *}
function ${type_name}.querySingleResult(var qry: TTBGQuery): string;
begin
  Result := TQueryExecutor.queryForString(qry);
end; 

end.
