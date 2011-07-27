unit transaction;

interface

uses
  array_list,
  connection;

type
  TTransaction = class
  private
    class var connection: TConnection;
    transactions: TArrayList;
  public
    constructor Create;
    procedure commit;
    procedure rollback;
    function getConnection: TConnection;
    class function getCurrentTransaction: TTransaction; static;
  end;

implementation

uses
  query;

constructor TTransaction.Create;
var
  qry: TTBGQuery;
begin
  connection := TConnection.Create;
  if (TTransaction.transactions = nil) then
  begin
    TTransaction.transactions := TArrayList.Create;
  end;
  TTransaction.transactions.add(Self);
  qry := TTBGQuery.Create;
  qry.sql.Add('BEGIN');
  connection.executeQuery(qry);
  qry.Free;
end;

procedure TTransaction.commit;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('COMMIT');
  connection.executeQuery(qry);
  qry.Free;
  connection.close;
  TTransaction.transactions.removeLast;
end;

procedure TTransaction.rollback;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('ROLLBACK');
  connection.executeQuery(qry);
  qry.Free;
  connection.close;
  TTransaction.transactions.removeLast;
end;

function TTransaction.getConnection: TConnection;
begin
  Result := connection;
end;

class function TTransaction.getCurrentTransaction: TTransaction;
begin
  if (TTransaction.transactions <> nil) then
  begin
    Result := TTransaction(TTransaction.transactions.getLast);
  end
  else
  begin
    Result := nil;
  end;
end;

end.

