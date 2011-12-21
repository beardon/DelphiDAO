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
    fQuery: TMyQuery;
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
    property Dataset: TMyQuery read fQuery;
    property SQL: TStrings read GetSQL write SetSQL;
  end;

implementation

uses
  QueryFactory;

constructor TTBGQuery.Create;
begin
  fQuery := TTBGQueryFactory.GetQuery;
end;

procedure TTBGQuery.Close;
begin
  fQuery.Close;
end;

function TTBGQuery.CopyFields(source: TDataSet): Integer;
begin
  Result := fQuery.CopyFields(source);
end;

procedure TTBGQuery.Execute;
begin
  fQuery.Execute;
end;

function TTBGQuery.FieldByName(const FieldName: string): TField;
begin
  Result := fQuery.FieldByName(FieldName);
end;

function TTBGQuery.GetConnection: TCustomMyConnection;
begin
  Result := fQuery.Connection;
end;

function TTBGQuery.GetSQL: TStrings;
begin
  Result := fQuery.SQL;
end;

function TTBGQuery.ParamByName(const Value: string): TDAParam;
begin
  Result := fQuery.ParamByName(Value);
end;

procedure TTBGQuery.SetConnection(value: TCustomMyConnection);
begin
  fQuery.Connection := value;
end;

procedure TTBGQuery.SetSQL(value: TStrings);
begin
  fQuery.SQL := value;
end;

end.
