unit QueryExecutor;

interface

uses
  ConnectionExt,
  DB,
  DBClient,
  Query;

type
  TQueryExecutor = class
  public
    class function Execute(var Query: TTBGQuery): TClientDataSet; overload; static;
    class function Execute(var Query: TTBGQuery; const Connection: TConnectionExt): TClientDataSet; overload; static;
    class function ExecuteUpdate(var Query: TTBGQuery): Integer; overload; static;
    class function ExecuteUpdate(var Query: TTBGQuery; const Connection: TConnectionExt): Integer; overload; static;
    class function ExecuteInsert(var Query: TTBGQuery): Int64; overload; static;
    class function ExecuteInsert(var Query: TTBGQuery; const Connection: TConnectionExt): Int64; overload; static;
    class function QueryForString(var Query: TTBGQuery): string; overload; static;
    class function QueryForString(var Query: TTBGQuery; const Connection: TConnectionExt): string; overload; static;
    class function QueryForString(var Query: TTBGQuery; const FieldName: string): string; overload; static;
    class function QueryForString(var Query: TTBGQuery; const FieldName: string; const Connection: TConnectionExt): string; overload; static;
  end;

implementation

uses
  Provider,
  Transaction;

class function TQueryExecutor.Execute(var Query: TTBGQuery): TClientDataSet;
var
  connection: TConnectionExt;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
    connection := TConnectionExt.Create
  else
    connection := transaction.GetConnection;
  Result := Execute(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.Execute(var Query: TTBGQuery; const Connection: TConnectionExt): TClientDataSet;
var
  dataset: TClientDataSet;
  dsp: TDataSetProvider;
begin
  dataset := TClientDataSet.Create(nil);
  dsp := TDataSetProvider.Create(dataset);
  dsp.DataSet := Connection.ExecuteQuery(Query);
  dataset.SetProvider(dsp);
  if (dsp.DataSet.RecordCount > 0) then
    dataset.Open;
  Result := dataset;
end;

class function TQueryExecutor.ExecuteUpdate(var Query: TTBGQuery): Integer;
var
  connection: TConnectionExt;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
    connection := TConnectionExt.Create
  else
    connection := transaction.GetConnection;
  Result := ExecuteUpdate(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.ExecuteUpdate(var Query: TTBGQuery; const Connection: TConnectionExt): Integer;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := Connection.AffectedRows;
  dataset.Free;
end;

class function TQueryExecutor.ExecuteInsert(var Query: TTBGQuery): Int64;
var
  connection: TConnectionExt;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
    connection := TConnectionExt.Create
  else
    connection := transaction.GetConnection;
  Result := ExecuteInsert(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.ExecuteInsert(var Query: TTBGQuery; const Connection: TConnectionExt): Int64;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := Connection.InsertId;
  dataset.Free;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery): string;
var
  connection: TConnectionExt;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
    connection := TConnectionExt.Create
  else
    connection := transaction.GetConnection;
  Result := QueryForString(Query, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery; const Connection: TConnectionExt): string;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := dataset.Fields[0].AsString;
  dataset.Free;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery; const FieldName: string): string;
var
  connection: TConnectionExt;
  transaction: TTransaction;
begin
  transaction := TTransaction.GetCurrentTransaction;
  if (transaction = nil) then
    connection := TConnectionExt.Create
  else
    connection := transaction.GetConnection;
  Result := QueryForString(Query, FieldName, connection);
  if (transaction = nil) then
  begin
    connection.Close;
    connection.Free;
  end;
end;

class function TQueryExecutor.QueryForString(var Query: TTBGQuery; const FieldName: string; const Connection: TConnectionExt): string;
var
  dataset: TDataSet;
begin
  dataset := Connection.ExecuteQuery(Query);
  Result := dataset.FieldByName(FieldName).AsString;
  dataset.Free;
end;

end.