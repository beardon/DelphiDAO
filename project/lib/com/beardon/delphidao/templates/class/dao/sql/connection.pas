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
    property connection: TMyConnection read fConnection;
    property affectedRows: Integer read fAffectedRows;
    property insertId: Int64 read fInsertId;
    constructor Create;
    procedure close;
    function executeQuery(var qry: TTBGQuery): TDataSet;
  end;

implementation

uses
  connection_factory;

constructor TConnection.Create;
begin
  fConnection := TConnectionFactory.getConnection;
end;

procedure TConnection.close;
begin
  TConnectionFactory.close(fConnection);
end;

function TConnection.executeQuery(var qry: TTBGQuery): TDataSet;
begin
  qry.connection := fConnection;
  qry.execute;
  fAffectedRows := qry.dataset.RowsAffected;
  fInsertId := qry.dataset.InsertId;
  Result := qry.dataset;
end;

end.
