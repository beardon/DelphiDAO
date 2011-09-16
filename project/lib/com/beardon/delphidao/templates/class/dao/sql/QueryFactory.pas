unit QueryFactory;

interface

uses
  MyAccess;

type
  TTBGQueryFactory = class
  public
    class function GetQuery: TMyQuery; static;
  end;

implementation

class function TTBGQueryFactory.GetQuery: TMyQuery;
var
  query: TMyQuery;
begin
  query := TMyQuery.Create(nil);
  Result := query;
end;

end.
