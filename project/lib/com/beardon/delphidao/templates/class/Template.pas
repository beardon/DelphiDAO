unit Template;

interface

type
  TTemplate = class
  private
    FTemplate: string;
    FContent: string;
    function GetContent: string;
  public
    constructor Create(const TemplateFilename: string);
    procedure SetPair(const Key, Value: string);
    procedure Write(const Filename: string);
  end;

implementation

uses
  SysUtils;

const
  CRLF = #13#10;

constructor TTemplate.Create(const TemplateFilename: string);
begin
  FTemplate := TemplateFilename;
  FContent := GetContent;
end;

procedure TTemplate.SetPair(const Key, Value: string);
var
  tag: string;
begin
  tag := '${' + Key + '}';
  FContent := StringReplace(FContent, tag, Value, [rfReplaceAll]);
end;

function TTemplate.GetContent: string;
var
  buffer, txt: string;
  handle: TextFile;
begin
  AssignFile(handle, FTemplate);
  Reset(handle);
  while not Eof(handle) do
  begin
    ReadLn(handle, buffer);
    txt := txt + buffer + CRLF;
  end;
  CloseFile(handle);
  Result := txt;
end;

procedure TTemplate.Write(const Filename: string);
var
  handle: TextFile;
begin
  AssignFile(handle, Filename);
  ReWrite(handle);
  WriteLn(handle, FContent);
  CloseFile(handle);
end;

end.
