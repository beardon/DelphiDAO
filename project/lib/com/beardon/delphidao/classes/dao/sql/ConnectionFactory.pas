unit ConnectionFactory;

interface

uses
  MyAccess;

type
  TConnectionFactory = class
  public
    class procedure Close(var Connection: TMyConnection); static;
    class function GetConnection: TMyConnection; overload; static;
    class function GetConnection(AUsername: string; APassword: string; ADatabase: string; APort: Integer; AServer: string): TMyConnection; overload; static;
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
  db: string;
  host: string;
  password: string;
  port: Integer;
  user: string;
begin
  user := TConnectionProperty.GetUser;
  password := TConnectionProperty.GetPassword;
  db := TConnectionProperty.GetDatabase;
  port := TConnectionProperty.GetPort;
  host := TConnectionProperty.GetHost;
  Result := GetConnection(user, password, db, port, host);
end;

class function TConnectionFactory.GetConnection(AUsername: string; APassword: string; ADatabase: string; APort: Integer; AServer: string): TMyConnection;
var
  connection: TMyConnection;
begin
  connection := TMyConnection.Create(nil);
  with (connection) do
  begin
    LoginPrompt := False;
    Username := AUsername;
    Password := APassword;
    Database := ADatabase;
    Port := APort;
    Server := AServer;
    DataTypeMap.AddDBTypeRule(myIntUnsigned, ftLargeint); // MyDAC data mapping workaround
    ConnectionTimeout := 60;
    Connect;
  end;
  Result := connection;
end;

end.
