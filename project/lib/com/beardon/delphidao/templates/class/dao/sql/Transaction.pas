unit Transaction;

interface

uses
  ArrayList,
  Connection;

type
  TTransaction = class
  private
    class var FConnection: TConnection;
    FTransactions: TArrayList;
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
  FConnection := TConnection.Create;
  if (TTransaction.FTransactions = nil) then
  begin
    TTransaction.FTransactions := TArrayList.Create;
  end;
  TTransaction.FTransactions.Add(Self);
  qry := TTBGQuery.Create;
  qry.SQL.Add('BEGIN');
  FConnection.ExecuteQuery(qry);
  qry.Free;
end;

procedure TTransaction.Commit;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.SQL.Add('COMMIT');
  FConnection.ExecuteQuery(qry);
  qry.Free;
  FConnection.Close;
  TTransaction.FTransactions.RemoveLast;
end;

procedure TTransaction.Rollback;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.SQL.Add('ROLLBACK');
  FConnection.ExecuteQuery(qry);
  qry.Free;
  FConnection.Close;
  TTransaction.FTransactions.RemoveLast;
end;

function TTransaction.GetConnection: TConnection;
begin
  Result := FConnection;
end;

class function TTransaction.GetCurrentTransaction: TTransaction;
begin
  if (TTransaction.FTransactions <> nil) then
  begin
    Result := TTransaction(TTransaction.FTransactions.GetLast);
  end
  else
  begin
    Result := nil;
  end;
end;

end.

