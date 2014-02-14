unit TbgQuery;

interface

uses
  Classes,
  MyAccess;

type
  TTbgQuery = class(TMyQuery)
  protected
    procedure Initialize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  DatabaseDM;

constructor TTbgQuery.Create;
begin
  inherited Create(AOwner);
  Initialize;
end;

destructor TTbgQuery.Destroy;
begin
  Close;
  inherited;
end;

procedure TTbgQuery.Initialize;
begin
  Connection := DatabaseDataModule.DBConnection;
  Close;
  SQL.Clear;
end;

end.
