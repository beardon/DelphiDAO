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
    function GetDatabaseHost: string;
    function GetDatabasePassword: string;
    function GetDatabasePort: Integer;
    function GetDatabaseSchema: string;
    function GetDatabaseUsername: string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenDBConnection;
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
  with (DBConnection) do
  begin
    Close;
    Destroy;
  end;
  DBConnection := nil;
end;

function TDatabaseDataModule.GetDatabaseHost: string;
begin
  Result := TConfigLib.ReadString('database', 'host', DB_HOST);
end;

function TDatabaseDataModule.GetDatabasePassword: string;
begin
  Result := TConfigLib.ReadString('database', 'password', DB_PASSWORD);
end;

function TDatabaseDataModule.GetDatabasePort: Integer;
begin
  Result := TConfigLib.ReadInteger('database', 'port', DB_PORT);
end;

function TDatabaseDataModule.GetDatabaseSchema: string;
begin
  Result := TConfigLib.ReadString('database', 'schema', DB_SCHEMA);
end;

function TDatabaseDataModule.GetDatabaseUsername: string;
begin
  Result := TConfigLib.ReadString('database', 'username', DB_USERNAME);
end;

procedure TDatabaseDataModule.OpenDBConnection;
begin
  with (DBConnection) do
  begin
    LoginPrompt := False;
    Server := GetDatabaseHost;
    Port := GetDatabasePort;
    Username := GetDatabaseUsername;
    Password := GetDatabasePassword;
    Database := GetDatabaseSchema;
    Connect;
    DataTypeMap.AddDBTypeRule(myIntUnsigned, ftLargeint); // MyDAC data mapping workaround
  end;
end;

end.
