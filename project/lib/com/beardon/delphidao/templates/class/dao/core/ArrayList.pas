unit ArrayList;

interface

type
  TArrayList = class
  private
    FTab: array of TObject;
    FSize: Integer;
  protected
  public
    property Size: Integer read FSize;
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
  FTab := nil;
  FSize := 0;
end;

procedure TArrayList.Add(Value: TObject);
begin
  SetLength(FTab, Length(FTab) + 1);
  FTab[High(FTab)] := Value;
  Inc(FSize);
end;

function TArrayList.Get(Index: Integer): TObject;
begin
  Result := FTab[Index];
end;

function TArrayList.GetLast: TObject;
begin
  if (FSize = 0) then
  begin
    Result := nil;
  end
  else
  begin
    Result := FTab[FSize - 1];
  end;
end;

function TArrayList.IsEmpty: Boolean;
begin
  Result := (Size = 0);
end;

function TArrayList.RemoveLast: Integer;
begin
  Dec(FSize);
  Result := FSize;
end;

end.
