unit Query;

interface

uses
  Classes,
  DB,
  DBAccess,
  MyAccess;

type
  TTBGQuery = class
  private
    FQuery: TMyQuery;
    function GetConnection: TCustomMyConnection;
    function GetSQL: TStrings;
    procedure SetConnection(value: TCustomMyConnection);
    procedure SetSQL(value: TStrings);
  public
    constructor Create;
    procedure Close;
    function CopyFields(source: TDataSet): Integer;
    procedure Execute;
    function FieldByName(const FieldName: string): TField;
    function ParamByName(const Value: string): TDAParam;
    property Connection: TCustomMyConnection read GetConnection write SetConnection;
    property Dataset: TMyQuery read FQuery;
    property SQL: TStrings read GetSQL write SetSQL;
  end;

implementation

uses
  QueryFactory;

constructor TTBGQuery.Create;
begin
  FQuery := TTBGQueryFactory.GetQuery;
end;

procedure TTBGQuery.Close;
begin
  FQuery.Close;
end;

function TTBGQuery.CopyFields(source: TDataSet): Integer;
begin
  Result := FQuery.CopyFields(source);
end;

procedure TTBGQuery.Execute;
begin
  FQuery.Execute;
end;

function TTBGQuery.FieldByName(const FieldName: string): TField;
begin
  Result := FQuery.FieldByName(FieldName);
end;

function TTBGQuery.GetConnection: TCustomMyConnection;
begin
  Result := FQuery.Connection;
end;

function TTBGQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TTBGQuery.ParamByName(const Value: string): TDAParam;
begin
  Result := FQuery.ParamByName(Value);
end;

procedure TTBGQuery.SetConnection(value: TCustomMyConnection);
begin
  FQuery.Connection := value;
end;

procedure TTBGQuery.SetSQL(value: TStrings);
begin
  FQuery.SQL := value;
end;

end.
