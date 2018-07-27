unit Template;

interface

const
  NO_UPDATE_FILES = 0;
  UPDATE_FILES = 1;

type
  TTemplate = class
  private
    FDoUpdateFile: Boolean;
    FTemplate: string;
    FContent: string;
    function GetContent: string;
  public
    constructor Create(const TemplateFilename: string; const DoUpdateFiles : Integer = UPDATE_FILES);
    procedure SetPair(const Key, Value: string);
    procedure Write(const Filename: string);
    function IsFileContentHashIdentical(const Filename, Hash: string) : Boolean;
  end;

implementation

uses
  HashLib,
  SysUtils,
  RegularExpressions;

const
  CRLF = #13#10;
  HASH_HEADER = 'SHA1_HASH';
  HASH_SEPARATOR = ':';
  // Match Greedy until you find the hash_header plus sep, then pull one or more non-whitespace character out
  REX_EX_STRING = HASH_HEADER + HASH_SEPARATOR + '(\S+)';

constructor TTemplate.Create(const TemplateFilename: string; const DoUpdateFiles : Integer = UPDATE_FILES);
begin
  FDoUpdateFile := True;
  if(DoUpdateFiles = NO_UPDATE_FILES) then
    FDoUpdateFile := false;
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
  hash : string;
begin
  hash := CalcHash2(FContent, haSHA1);
  if not FileExists(Filename) then
  begin
    AssignFile(handle, Filename);
    // Erase existing file and copy new content
    ReWrite(handle);
    if(FDoUpdateFile) then
      FContent := '{ ' + HASH_HEADER + HASH_SEPARATOR + hash + ' } ' + CRLF + FContent;
    WriteLn(handle, FContent);
    CloseFile(handle);
  end
  else if (FDoUpdateFile and not IsFileContentHashIdentical(Filename, hash)) then
  begin
    AssignFile(handle, Filename);
    // Erase existing file and copy new content
    ReWrite(handle);
    if(FDoUpdateFile) then
      FContent := '{ ' + HASH_HEADER + HASH_SEPARATOR + hash + ' } ' + CRLF + FContent;
    WriteLn(handle, FContent);
    CloseFile(handle);
  end;
end;

function TTemplate.IsFileContentHashIdentical(const Filename, Hash : string) : Boolean;
var
  buffer: string;
  handle: TextFile;
  match: TMatch;
  outcome: Boolean;
  regEx: TRegEx;
begin
  outcome := False;
  if FileExists(Filename) then
  begin
    AssignFile(handle, Filename);
    Reset(handle);
    // First line should be the SHA1 hash of the generated file, so parse it out and compare
    ReadLn(handle, buffer);
    Close(handle);
    regEx := TRegEx.Create(REX_EX_STRING, [roIgnoreCase]);
    match := regEx.Match(buffer);
    // Did it match, does the match have valid data (0 is the entire string I think?, and is the string identical to our hash
    if (match.Success and (match.Groups.Count > 0) and (AnsiCompareText(match.Groups[1].Value, Hash) = 0)) then
      outcome := True;
  end;
  Result := outcome;
end;

end.
