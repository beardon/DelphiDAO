unit query_executor;

interface

uses
  connection,
  DB,
  DBClient,
  query;

type
  TQueryExecutor = class
  public
    class function execute(var qry: TTBGQuery): TClientDataSet; overload; static;
    class function execute(var qry: TTBGQuery; const connection: TConnection): TClientDataSet; overload; static;
    class function executeUpdate(var qry: TTBGQuery): Integer; overload; static;
    class function executeUpdate(var qry: TTBGQuery; const connection: TConnection): Integer; overload; static;
    class function executeInsert(var qry: TTBGQuery): Int64; overload; static;
    class function executeInsert(var qry: TTBGQuery; const connection: TConnection): Int64; overload; static;
    class function queryForString(var qry: TTBGQuery): string; overload; static;
    class function queryForString(var qry: TTBGQuery; const connection: TConnection): string; overload; static;
    class function queryForString(var qry: TTBGQuery; const fieldName: string): string; overload; static;
    class function queryForString(var qry: TTBGQuery; const fieldName: string; const connection: TConnection): string; overload; static;
  end;

implementation

uses
  Provider,
  transaction;

class function TQueryExecutor.execute(var qry: TTBGQuery): TClientDataSet;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.getCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.getConnection;
  end;
  Result := execute(qry, connection);
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.execute(var qry: TTBGQuery; const connection: TConnection): TClientDataSet;
var
  dataset: TClientDataSet;
  dsp: TDataSetProvider;
begin
  dataset := TClientDataSet.Create(nil);
  dsp := TDataSetProvider.Create(dataset);
  dsp.DataSet := connection.executeQuery(qry);
  dataset.SetProvider(dsp);
  dataset.Open;
  Result := dataset;
end;

class function TQueryExecutor.executeUpdate(var qry: TTBGQuery): Integer;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.getCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.getConnection;
  end;
  Result := executeUpdate(qry, connection);
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.executeUpdate(var qry: TTBGQuery; const connection: TConnection): Integer;
var
  dataset: TDataSet;
begin
  dataset := connection.executeQuery(qry);
  Result := connection.affectedRows;
  dataset.Free;
end;

class function TQueryExecutor.executeInsert(var qry: TTBGQuery): Int64;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.getCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.getConnection;
  end;
  Result := executeInsert(qry, connection);
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.executeInsert(var qry: TTBGQuery; const connection: TConnection): Int64;
var
  dataset: TDataSet;
begin
  dataset := connection.executeQuery(qry);
  Result := connection.insertId;
  dataset.Free;
end;

class function TQueryExecutor.queryForString(var qry: TTBGQuery): string;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.getCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.getConnection;
  end;
  Result := queryForString(qry, connection);
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.queryForString(var qry: TTBGQuery; const connection: TConnection): string;
var
  dataset: TDataSet;
begin
  dataset := connection.executeQuery(qry);
  Result := dataset.Fields[0].AsString;
  dataset.Free;
end;

class function TQueryExecutor.queryForString(var qry: TTBGQuery; const fieldName: string): string;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.getCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.getConnection;
  end;
  Result := queryForString(qry, fieldName, connection);
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.queryForString(var qry: TTBGQuery; const fieldName: string; const connection: TConnection): string;
var
  dataset: TDataSet;
begin
  dataset := connection.executeQuery(qry);
  Result := dataset.FieldByName(fieldName).AsString;
  dataset.Free;
end;

end.
