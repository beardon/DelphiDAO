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
    class function Execute(var Query: TTBGQuery): TClientDataSet; overload; static;
    class function Execute(var Query: TTBGQuery; const Connection: TConnection): TClientDataSet; overload; static;
    class function ExecuteUpdate(var Query: TTBGQuery): Integer; overload; static;
    class function ExecuteUpdate(var Query: TTBGQuery; const Connection: TConnection): Integer; overload; static;
    class function ExecuteInsert(var Query: TTBGQuery): Int64; overload; static;
    class function ExecuteInsert(var Query: TTBGQuery; const Connection: TConnection): Int64; overload; static;
    class function QueryForString(var Query: TTBGQuery): string; overload; static;
    class function QueryForString(var Query: TTBGQuery; const Connection: TConnection): string; overload; static;
    class function QueryForString(var Query: TTBGQuery; const FieldName: string): string; overload; static;
    class function QueryForString(var Query: TTBGQuery; const FieldName: string; const Connection: TConnection): string; overload; static;
  end;

implementation

uses
  Provider,
  transaction;

class function TQueryExecutor.Execute(var Query: TTBGQuery): TClientDataSet;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.GetConnection;
  end;
  Result := Execute(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.Execute(var Query: TTBGQuery; const Connection: TConnection): TClientDataSet;
var
  dataset: TClientDataSet;
  dsp: TDataSetProvider;
begin
  dataset := TClientDataSet.Create(nil);
  dsp := TDataSetProvider.Create(dataset);
  dsp.DataSet := Connection.ExecuteQuery(Query);
  dataset.SetProvider(dsp);
  dataset.Open;
  Result := dataset;
end;

class function TQueryExecutor.ExecuteUpdate(var Query: TTBGQuery): Integer;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.GetConnection;
  end;
  Result := ExecuteUpdate(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.ExecuteUpdate(var Query: TTBGQuery; const Connection: TConnection): Integer;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := Connection.AffectedRows;
  dataset.Free;
end;

class function TQueryExecutor.ExecuteInsert(var Query: TTBGQuery): Int64;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.GetConnection;
  end;
  Result := ExecuteInsert(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.ExecuteInsert(var Query: TTBGQuery; const Connection: TConnection): Int64;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := Connection.InsertId;
  dataset.Free;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery): string;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.GetConnection;
  end;
  Result := QueryForString(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery; const Connection: TConnection): string;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := dataset.Fields[0].AsString;
  dataset.Free;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery; const FieldName: string): string;
var
  connection: TConnection;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
  begin
    connection := TConnection.Create;
  end
  else
  begin
    connection := transaction.GetConnection;
  end;
  Result := QueryForString(Query, FieldName, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery; const FieldName: string; const Connection: TConnection): string;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := dataset.FieldByName(FieldName).AsString;
  dataset.Free;
end;

end.
