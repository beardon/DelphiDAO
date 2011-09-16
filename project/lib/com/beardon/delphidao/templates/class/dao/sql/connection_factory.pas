unit connection_factory;

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
  connection_property;

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
    ConnectionTimeout := 60;
    Connect;
  end;
  Result := connection;
end;

end.
