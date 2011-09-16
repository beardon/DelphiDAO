unit array_list;

interface

type
  TArrayList = class
  private
    fTab: array of TObject;
    fSize: Integer;
  protected
  public
    property Size: Integer read fSize;
    constructor Create;
    procedure Add(Value: TObject);
    function Get(Index: Integer): TObject;
    function GetLast: TObject;
    function IsEmpty: Boolean;
    function RemoveLast: Integer;
  end;

implementation

constructor TArrayList.Create;
begin
  fTab := nil;
  fSize := 0;
end;

procedure TArrayList.Add(Value: TObject);
begin
  SetLength(fTab, Length(fTab) + 1);
  fTab[High(fTab)] := Value;
  Inc(fSize);
end;

function TArrayList.Get(Index: Integer): TObject;
begin
  Result := fTab[Index];
end;

function TArrayList.GetLast: TObject;
begin
  if (fSize = 0) then
  begin
    Result := nil;
  end
  else
  begin
    Result := fTab[fSize - 1];
  end;
end;

function TArrayList.IsEmpty: Boolean;
begin
  Result := (Size = 0);
end;

function TArrayList.RemoveLast: Integer;
begin
  Dec(fSize);
  Result := fSize;
end;

end.
