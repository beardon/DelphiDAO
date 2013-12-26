unit ConnectionFactory;

interface

uses
  MyAccess;

type
  TConnectionFactory = class
  public
    class procedure Close(var Connection: TMyConnection); static;
    class function GetConnection: TMyConnection; static;
  end;

implementation

uses
  ConnectionProperty,
  DB,
  MyDataTypeMap;

class procedure TConnectionFactory.Close(var Connection: TMyConnection);
begin
  Connection.Close;
end;

class function TConnectionFactory.GetConnection: TMyConnection;
var
  connection: TMyConnection;
begin
  connection := TMyConnection.Create(nil);
  with (connection) do
  begin
    LoginPrompt := False;
    Username := TConnectionProperty.GetUser;
    Password := TConnectionProperty.GetPassword;
    Database := TConnectionProperty.GetDatabase;
    Port := TConnectionProperty.GetPort;
    Server := TConnectionProperty.GetHost;
    DataTypeMap.AddDBTypeRule(myIntUnsigned, ftLargeint); // MyDAC data mapping workaround
    ConnectionTimeout := 60;
    Connect;
  end;
  Result := connection;
end;

end.
