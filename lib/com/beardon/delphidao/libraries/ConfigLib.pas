unit ConfigLib;

interface

uses
  IniFiles;

type
  TConfigLib = class
  private
    class function GetIniFile: TIniFile;
  public
    class function ReadInteger(Section:string; Name: string; Default: Integer = 0): Integer;
    class function ReadString(Section:string; Name: string; Default: string = ''): string;
  end;

implementation

uses
  Configuration,
  Forms,
  SysUtils;

class function TConfigLib.GetIniFile: TIniFile;
var
  appPath: string;
begin
  appPath := ExtractFilePath(Application.ExeName);
  Result := TIniFile.Create(appPath + CONFIG_INI_FILENAME);
end;

class function TConfigLib.ReadInteger(Section: string; Name: string; Default: Integer = 0): Integer;
var
  ini: TIniFile;
begin
  ini := GetIniFile;
  Result := ini.ReadInteger(Section, Name, Default);
  ini.Free;
end;

class function TConfigLib.ReadString(Section: string; Name: string; Default: string = ''): string;
var
  ini: TIniFile;
begin
  ini := GetIniFile;
  Result := ini.ReadString(Section, Name, Default);
  ini.Free;
end;

end.
