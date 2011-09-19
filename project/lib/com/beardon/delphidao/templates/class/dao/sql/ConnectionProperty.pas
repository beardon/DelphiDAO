unit ConnectionProperty;

interface

type
  TConnectionProperty = class
  private
    const HOST: string = 'localhost';
    const PORT: Integer = 3306;
    const USER: string = 'username';
    const PASSWORD: string = 'password';
    const DATABASE: string = 'ceat_ce';
  public
    class function GetHost: string; static;
    class function GetPort: Integer; static;
    class function GetUser: string; static;
    class function GetPassword: string; static;
    class function GetDatabase: string; static;
  end;

implementation

class function TConnectionProperty.GetHost: string;
begin
  Result := HOST;
end;

class function TConnectionProperty.GetPort: Integer;
begin
  Result := PORT;
end;

class function TConnectionProperty.GetUser: string;
begin
  Result := USER;
end;

class function TConnectionProperty.GetPassword: string;
begin
  Result := PASSWORD;
end;

class function TConnectionProperty.GetDatabase: string;
begin
  Result := DATABASE;
end;

end.
