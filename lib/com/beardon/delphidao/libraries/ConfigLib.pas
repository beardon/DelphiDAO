unit ConfigLib;

interface

uses
  IniFiles;

type
  TConfigLib = class
  private
    class function GetIniFile(Filename: string): TIniFile;
  public
    class function ReadInteger(Filename, Section, Name: string; Default: Integer = 0): Integer;
    class function ReadString(Filename, Section, Name: string; Default: string = ''): string;
  end;

implementation

uses
  Configuration,
  Forms,
  SysUtils;

class function TConfigLib.GetIniFile(Filename: string): TIniFile;
var
  appPath: string;
begin
  if (Filename = '') then
  begin
    appPath := ExtractFilePath(Application.ExeName);
    Result := TIniFile.Create(appPath + CONFIG_INI_FILENAME);
  end
  else
    Result := TIniFile.Create(Filename);
end;

class function TConfigLib.ReadInteger(Filename, Section, Name: string; Default: Integer = 0): Integer;
var
  ini: TIniFile;
begin
  ini := GetIniFile(Filename);
  Result := ini.ReadInteger(Section, Name, Default);
  ini.Free;
end;

class function TConfigLib.ReadString(Filename, Section, Name: string; Default: string = ''): string;
var
  ini: TIniFile;
begin
  ini := GetIniFile(Filename);
  Result := ini.ReadString(Section, Name, Default);
  ini.Free;
end;

end.
