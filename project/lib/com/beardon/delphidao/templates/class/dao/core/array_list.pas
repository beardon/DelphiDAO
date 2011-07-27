unit array_list;

interface

type
  TArrayList = class
  private
    tab: array of TObject;
    fSize: Integer;
  protected
  public
    property size: Integer read fSize;
    constructor Create;
    procedure add(value: TObject);
    function get(idx: Integer): TObject;
    function getLast: TObject;
    function isEmpty: Boolean;
    function removeLast: Integer;
  end;

implementation

constructor TArrayList.Create;
begin
  tab := nil;
  fSize := 0;
end;

procedure TArrayList.add(value: TObject);
begin
  SetLength(tab, Length(tab) + 1);
  tab[High(tab)] := value;
  Inc(fSize);
end;

function TArrayList.get(idx: Integer): TObject;
begin
  Result := tab[idx];
end;

function TArrayList.getLast: TObject;
begin
  if (fSize = 0) then
  begin
    Result := nil;
  end
  else
  begin
    Result := tab[fSize - 1];
  end;
end;

function TArrayList.isEmpty: Boolean;
begin
  Result := (size = 0);
end;

function TArrayList.removeLast: Integer;
begin
  Dec(fSize);
  Result := fSize;
end;

end.
