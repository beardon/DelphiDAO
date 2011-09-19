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
  QueryExecutor;

type
  {**
   * Class that operates on MySQL view '${table_name}'.
   *
   * @author: Aaron Bean
   * @date: ${date}
   *}
  ${type_name} = class(TInterfacedObject, ${interface_name})
  private
    fConnection: TConnection;
  protected
    function ReadRow(const Dataset: TClientDataSet): ${dao_class_name}; 
    function GetList(var Query: TTBGQuery): TObjectList<${dao_class_name}>;
    function GetRow(var Query: TTBGQuery): ${dao_class_name};
    function Execute(var Query: TTBGQuery): TClientDataSet;
    function QuerySingleResult(var Query: TTBGQuery): string;
  public
    constructor Create(aConnection: TConnection);
    function Load(const Id: Variant): ${dao_class_name};
    function QueryAll: TObjectList<${dao_class_name}>;
    function QueryAllOrderBy(const OrderColumn: string): TObjectList<${dao_class_name}>;
${query_by_definitions}
end;

implementation

constructor ${type_name}.Create(aConnection: TConnection);
begin
  fConnection := aConnection;
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
 * Get all records from view
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
 * Get all records from view ordered by field
 *
 * @param orderColumn column name
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
	
${query_by_functions}

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
  dataset := TQueryExecutor.Execute(Query, fConnection);
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
 * Query for one row and one column
 *}
function ${type_name}.QuerySingleResult(var Query: TTBGQuery): string;
begin
  Result := TQueryExecutor.queryForString(Query, fConnection);
end; 

end.
