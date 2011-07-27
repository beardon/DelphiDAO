unit query_factory;

interface

uses
  MyAccess;

type
  TTBGQueryFactory = class
  public
    class function getQuery: TMyQuery; static;
  end;

implementation

class function TTBGQueryFactory.getQuery: TMyQuery;
var
  query: TMyQuery;
begin
  query := TMyQuery.Create(nil);
  Result := query;
end;

end.
