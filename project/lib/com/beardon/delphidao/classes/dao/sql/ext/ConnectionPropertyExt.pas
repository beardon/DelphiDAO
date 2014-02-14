{$I .\Defines.inc}
unit ConnectionPropertyExt;

interface

uses
  ConnectionProperty;

type
  {**
   * Class that represents DB connection properties. This class will not be overwritten.
   * This class will not be overwritten.
   *
   * @author: Aaron Bean
   *}
  TConnectionPropertyExt = class(TConnectionProperty)
  public
    class function GetHost: string; override;
  end;

implementation

uses
  Configuration;

class function TConnectionPropertyExt.GetHost: string;
var
  host: string;
begin
  host := DB_IP_PRODUCTION;
{$IFDEF TestDB}
  host := DB_IP_TEST;
{$ENDIF}
{$IFDEF DevelopmentDB}
  host := DB_IP_DEVELOPMENT;
{$ENDIF}
  Result := host;
end;

end.
