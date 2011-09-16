unit connection;

interface

uses
  Classes,
  DB,
  MyAccess,
  query;

type
  TConnection = class
  private
    fConnection: TMyConnection;
    fAffectedRows: Integer;
    fInsertId: Int64;
  public
    property Connection: TMyConnection read fConnection;
    property AffectedRows: Integer read fAffectedRows;
    property InsertId: Int64 read fInsertId;
    constructor Create;
    procedure Close;
    function ExecuteQuery(var Query: TTBGQuery): TDataSet;
  end;

implementation

uses
  connection_factory;

constructor TConnection.Create;
begin
  fConnection := TConnectionFactory.GetConnection;
end;

procedure TConnection.Close;
begin
  TConnectionFactory.Close(fConnection);
end;

function TConnection.ExecuteQuery(var Query: TTBGQuery): TDataSet;
begin
  Query.Connection := fConnection;
  Query.Execute;
  fAffectedRows := Query.Dataset.RowsAffected;
  fInsertId := Query.Dataset.InsertId;
  Result := Query.Dataset;
end;

end.
