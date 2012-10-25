unit Template;

interface

const
  NO_UPDATE_FILES = 0;
  UPDATE_FILES = 1;

type
  TTemplate = class
  private
    FTemplate: string;
    FContent: string;
    function GetContent: string;
    var update_file : Boolean;
  public
    constructor Create(const TemplateFilename: string; const Update_Check : Integer = UPDATE_FILES);
    procedure SetPair(const Key, Value: string);
    procedure Write(const Filename: string);
    function IsFileContentHashIdentical(const Filename, hash: string) : Boolean;
  end;

implementation

uses
  HashLib,
  SysUtils,
  RegularExpressions;

const
  CRLF = #13#10;
  hash_header = 'SHA1_HASH';
  hash_separator = ':';
  // Match Greedy until you find the hash_header plus sep, then pull one or more non-whitespace character out
  reg_ex_string = hash_header + hash_separator + '(\S+)';



constructor TTemplate.Create(const TemplateFilename: string; const Update_check : Integer = UPDATE_FILES);
begin
  update_file := true;
  if(Update_files = NO_UPDATE_FILES) then
  begin
    update_file := false;
  end;
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
  if(not FileExists(Filename)) then
  begin
    AssignFile(handle, Filename);
    // Erase existing file and copy new content
    ReWrite(handle);
    if(update_file) then
      FContent := ' { ' + hash_header + hash_separator + hash + ' } ' + CRLF + FContent;
    WriteLn(handle, FContent);
    CloseFile(handle);
  end
  else  if (update_file AND (not IsFileContentHashIdentical(Filename, hash))) then
  begin
    AssignFile(handle, Filename);
    // Erase existing file and copy new content
    ReWrite(handle);
    if(update_file) then
      FContent := ' { ' + hash_header + hash_separator + hash + ' } ' + CRLF + FContent;
    WriteLn(handle, FContent);
    CloseFile(handle);
  end;
end;

function TTemplate.IsFileContentHashIdentical(const Filename, hash : string) : Boolean;
var
  handle: TextFile;
  buffer : string;
  reg_ex : TRegEx;
  match : TMatch;
  outcome : Boolean;
begin
  outcome := false;
  if(FileExists(filename)) then
  begin
    AssignFile(handle, filename);
    Reset(handle);
   // First line should be the SHA1 hash of the generated file, so parse it out and compare
    ReadLn(handle, buffer);
    Close(handle);

    reg_ex := TRegEx.Create(reg_ex_string, [roIgnoreCase]);
    match := reg_ex.Match(buffer);

    // Did it match, does the match have valid data (0 is the entire string I think?, and is the string identical to our hash
    if ((match.Success) AND (match.Groups.Count > 0) AND (AnsiCompareText(match.Groups[1].Value, hash) = 0)) then
    begin
       outcome := true;
    end;
  end;

  Result := outcome;
end;

end.
