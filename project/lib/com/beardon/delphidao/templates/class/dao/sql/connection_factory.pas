unit connection_factory;

interface

uses
  MyAccess;

type
  TConnectionFactory = class
  public
    class procedure close(var connection: TMyConnection); static;
    class function getConnection: TMyConnection; static;
  end;

implementation

uses
  connection_property;

class procedure TConnectionFactory.close(var connection: TMyConnection);
begin
  connection.Close;
end;

class function TConnectionFactory.getConnection: TMyConnection;
var
  connection: TMyConnection;
begin
  connection := TMyConnection.Create(nil);
  with (connection) do
  begin
    LoginPrompt := False;
    Username := TConnectionProperty.getUser;
    Password := TConnectionProperty.getPassword;
    Database := TConnectionProperty.getDatabase;
    Port := TConnectionProperty.getPort;
    Server := TConnectionProperty.getHost;
    ConnectionTimeout := 60;
    Connect;
  end;
  Result := connection;
end;

end.
