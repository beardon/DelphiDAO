unit query;

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
    function getConnection: TCustomMyConnection;
    function getSQL: TStrings;
    procedure setConnection(value: TCustomMyConnection);
    procedure setSQL(value: TStrings);
  public
    property connection: TCustomMyConnection read getConnection write setConnection;
    property dataset: TMyQuery read fQuery;
    property sql: TStrings read getSQL write setSQL;
    constructor Create;
    function copyFields(source: TDataSet): Integer;
    procedure execute;
    function fieldByName(const fieldName: string): TField;
    function paramByName(const value: string): TDAParam;
  end;

implementation

uses
  query_factory;

constructor TTBGQuery.Create;
begin
  fQuery := TTBGQueryFactory.getQuery;
end;

function TTBGQuery.copyFields(source: TDataSet): Integer;
begin
  Result := fQuery.CopyFields(source);
end;

procedure TTBGQuery.execute;
begin
  fQuery.Execute;
end;

function TTBGQuery.fieldByName(const fieldName: string): TField;
begin
  Result := fQuery.FieldByName(fieldName);
end;

function TTBGQuery.getConnection: TCustomMyConnection;
begin
  Result := fQuery.Connection;
end;

function TTBGQuery.getSQL: TStrings;
begin
  Result := fQuery.SQL;
end;

function TTBGQuery.paramByName(const value: string): TDAParam;
begin
  Result := fQuery.ParamByName(value);
end;

procedure TTBGQuery.setConnection(value: TCustomMyConnection);
begin
  fQuery.Connection := value;
end;

procedure TTBGQuery.setSQL(value: TStrings);
begin
  fQuery.SQL := value;
end;

end.
