unit connection_property;

interface

type
  TConnectionProperty = class
  private
    const HOST = 'localhost';
    const PORT = 3306;
    const USER = 'AppUse';
    const PASSWORD = 'oldspice';
    const DATABASE = 'schema';
  public
    class function getHost: string; static;
    class function getPort: Integer; static;
    class function getUser: string; static;
    class function getPassword: string; static;
    class function getDatabase: string; static;
  end;

implementation

class function TConnectionProperty.getHost: string;
begin
  Result := HOST;
end;

class function TConnectionProperty.getPort: Integer;
begin
  Result := PORT;
end;

class function TConnectionProperty.getUser: string;
begin
  Result := USER;
end;

class function TConnectionProperty.getPassword: string;
begin
  Result := PASSWORD;
end;

class function TConnectionProperty.getDatabase: string;
begin
  Result := DATABASE;
end;

end.
