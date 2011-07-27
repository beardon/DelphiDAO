unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMain = class(TForm)
    btnGenerate: TButton;
    edtPath: TEdit;
    btnDir: TButton;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnDirClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileCtrl,
  generator;

{$R *.dfm}

procedure TfrmMain.btnDirClick(Sender: TObject);
var
  path: string;
begin
  path := ExtractFilePath(Application.ExeName);
  if (SelectDirectory('Select the location where the DAO objects will be generated', '', path)) then
  begin
    edtPath.Text := path;
  end;
end;

procedure TfrmMain.btnGenerateClick(Sender: TObject);
begin
  if (SysUtils.DirectoryExists(edtPath.Text)) then
  begin
    TGenerator.generate(edtPath.Text);
  end
  else
  begin
    ShowMessage('Directory does not exist.');
  end;
end;

end.
