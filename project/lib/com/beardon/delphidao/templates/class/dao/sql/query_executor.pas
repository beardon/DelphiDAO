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
    class function executeUpdate(var qry: TTBGQuery): Integer; static;
    class function executeInsert(var qry: TTBGQuery): Int64; static;
    class function queryForString(var qry: TTBGQuery): string; static;
  end;

implementation

uses
  Provider,
  transaction;

class function TQueryExecutor.execute(var qry: TTBGQuery): TClientDataSet;
var
  connection: TConnection;
  dataset: TClientDataSet;
  dsp: TDataSetProvider;
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
  dataset := TClientDataSet.Create(nil);
  dsp := TDataSetProvider.Create(dataset);
  dsp.DataSet := connection.executeQuery(qry);
  dataset.SetProvider(dsp);
  dataset.Open;
  Result := dataset;
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
  dataset: TDataSet;
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
  dataset := connection.executeQuery(qry);
  Result := connection.affectedRows;
  dataset.Free;
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.executeInsert(var qry: TTBGQuery): Int64;
var
  connection: TConnection;
  dataset: TDataSet;
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
  dataset := connection.executeQuery(qry);
  Result := connection.insertId;
  dataset.Free;
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

class function TQueryExecutor.queryForString(var qry: TTBGQuery): string;
var
  connection: TConnection;
  dataset: TDataSet;
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
  dataset := connection.executeQuery(qry);
  Result := dataset.Fields[0].AsString;
  dataset.Free;
  if (transaction = nil) then
  begin
    connection.close;
    connection.Free;
  end;
end;

end.
