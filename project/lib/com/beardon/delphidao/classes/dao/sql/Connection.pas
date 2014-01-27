unit Connection;

interface

uses
  Classes,
  DB,
  MyAccess,
  Query;

type
  TConnection = class
  private
    FAffectedRows: Integer;
    FInsertId: Int64;
  protected
    FConnection: TMyConnection;
  public
    constructor Create; virtual;
    procedure Close;
    function ExecuteQuery(var Query: TTBGQuery): TDataSet; virtual;
    property Connection: TMyConnection read FConnection;
    property AffectedRows: Integer read FAffectedRows;
    property InsertId: Int64 read FInsertId;
  end;

implementation

uses
  ConnectionFactory;

constructor TConnection.Create;
begin
  FConnection := TConnectionFactory.GetConnection;
end;

procedure TConnection.Close;
begin
  TConnectionFactory.Close(FConnection);
end;

function TConnection.ExecuteQuery(var Query: TTBGQuery): TDataSet;
begin
  Query.Connection := FConnection;
  Query.Execute;
  FAffectedRows := Query.Dataset.RowsAffected;
  FInsertId := Query.Dataset.InsertId;
  Result := Query.Dataset;
end;

end.
