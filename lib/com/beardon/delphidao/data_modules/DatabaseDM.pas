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

procedure TDatabaseDataModule.OpenDBConnection;
begin
  with (DBConnection) do
  begin
    LoginPrompt := False;
    Server := DB_HOST;
    Port := DB_PORT;
    Username := DB_USERNAME;
    Password := DB_PASSWORD;
    Database := DB_SCHEMA;
    Connect;
    DataTypeMap.AddDBTypeRule(myIntUnsigned, ftLargeint); // MyDAC data mapping workaround
  end;
end;

end.
