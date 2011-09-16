unit Transaction;

interface

uses
  ArrayList,
  Connection;

type
  TTransaction = class
  private
    class var fConnection: TConnection;
    fTransactions: TArrayList;
  public
    constructor Create;
    procedure Commit;
    procedure Rollback;
    function GetConnection: TConnection;
    class function GetCurrentTransaction: TTransaction; static;
  end;

implementation

uses
  Query;

constructor TTransaction.Create;
var
  qry: TTBGQuery;
begin
  fConnection := TConnection.Create;
  if (TTransaction.fTransactions = nil) then
  begin
    TTransaction.fTransactions := TArrayList.Create;
  end;
  TTransaction.fTransactions.Add(Self);
  qry := TTBGQuery.Create;
  qry.SQL.Add('BEGIN');
  fConnection.ExecuteQuery(qry);
  qry.Free;
end;

procedure TTransaction.Commit;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.SQL.Add('COMMIT');
  fConnection.ExecuteQuery(qry);
  qry.Free;
  fConnection.Close;
  TTransaction.fTransactions.RemoveLast;
end;

procedure TTransaction.Rollback;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.SQL.Add('ROLLBACK');
  fConnection.ExecuteQuery(qry);
  qry.Free;
  fConnection.Close;
  TTransaction.fTransactions.RemoveLast;
end;

function TTransaction.GetConnection: TConnection;
begin
  Result := fConnection;
end;

class function TTransaction.GetCurrentTransaction: TTransaction;
begin
  if (TTransaction.fTransactions <> nil) then
  begin
    Result := TTransaction(TTransaction.fTransactions.GetLast);
  end
  else
  begin
    Result := nil;
  end;
end;

end.

