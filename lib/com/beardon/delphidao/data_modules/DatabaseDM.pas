{$I .\Defines.inc}
unit DatabaseDM;

interface

uses
  System.SysUtils, System.Classes, Data.DB, DBAccess, MyAccess;

type
  TDatabaseDataModule = class(TDataModule)
    DBConnection: TMyConnection;
  private
    { Private declarations }
    FConnectionRetryCount: Integer;
    procedure CloseDBConnection;
    function GetDatabaseHost(ConfigPath: string): string;
    function GetDatabasePassword(ConfigPath: string): string;
    function GetDatabasePort(ConfigPath: string): Integer;
    function GetDatabaseSchema(ConfigPath: string): string;
    function GetDatabaseUsername(ConfigPath: string): string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenDBConnection(ConfigPath: string);
  end;

var
  DatabaseDataModule: TDatabaseDataModule;

implementation

uses
  ConfigLib,
  Configuration,
  MyDataTypeMap;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

constructor TDatabaseDataModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionRetryCount := 0;
end;

destructor TDatabaseDataModule.Destroy;
begin
  CloseDBConnection;
  inherited Destroy;
end;

procedure TDatabaseDataModule.CloseDBConnection;
begin
  DBConnection.Close;
  DBConnection := nil;
end;

function TDatabaseDataModule.GetDatabaseHost(ConfigPath: string): string;
begin
  Result := TConfigLib.ReadString(ConfigPath, 'database', 'host', DB_HOST);
end;

function TDatabaseDataModule.GetDatabasePassword(ConfigPath: string): string;
begin
  Result := TConfigLib.ReadString(ConfigPath, 'database', 'password', DB_PASSWORD);
end;

function TDatabaseDataModule.GetDatabasePort(ConfigPath: string): Integer;
begin
  Result := TConfigLib.ReadInteger(ConfigPath, 'database', 'port', DB_PORT);
end;

function TDatabaseDataModule.GetDatabaseSchema(ConfigPath: string): string;
begin
  Result := TConfigLib.ReadString(ConfigPath, 'database', 'schema', DB_SCHEMA);
end;

function TDatabaseDataModule.GetDatabaseUsername(ConfigPath: string): string;
begin
  Result := TConfigLib.ReadString(ConfigPath, 'database', 'username', DB_USERNAME);
end;

procedure TDatabaseDataModule.OpenDBConnection(ConfigPath: string);
begin
  with DBConnection do
  begin
    LoginPrompt := False;
    Server := GetDatabaseHost(ConfigPath);
    Port := GetDatabasePort(ConfigPath);
    Username := GetDatabaseUsername(ConfigPath);
    Password := GetDatabasePassword(ConfigPath);
    Database := GetDatabaseSchema(ConfigPath);
    Connect;
    DataTypeMap.AddDBTypeRule(myIntUnsigned, ftLargeint); // MyDAC data mapping workaround
  end;
end;

end.
