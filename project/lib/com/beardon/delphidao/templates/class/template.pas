unit template;

interface

type
  TTemplate = class
  private
    template: string;
    content: string;
    function getContent: string;
  public
    constructor Create(const templateFilename: string);
    procedure setPair(const key, value: string);
    procedure write(const filename: string);
  end;

implementation

uses
  SysUtils;

const
  CRLF = #13#10;

constructor TTemplate.Create(const templateFilename: string);
begin
  template := templateFilename;
  content := getContent;
end;

procedure TTemplate.setPair(const key, value: string);
var
  tag: string;
begin
  tag := '${' + key + '}';
  content := StringReplace(content, tag, value, [rfReplaceAll]);
end;

function TTemplate.getContent: string;
var
  buffer, txt: string;
  handle: TextFile;
begin
  AssignFile(handle, template);
  Reset(handle);
  while not Eof(handle) do
  begin
    ReadLn(handle, buffer);
//    if (Trim(buffer) = '') then
//    begin
//      buffer := CRLF;
//    end;
    txt := txt + buffer + CRLF;
  end;
  CloseFile(handle);
  Result := txt;
end;

procedure TTemplate.write(const filename: string);
var
  handle: TextFile;
begin
  //echo $fileName.'<br/>';
  AssignFile(handle, filename);
  ReWrite(handle);
  WriteLn(handle, content);
  CloseFile(handle);
end;

end.
