program DelphiDAO;

{$APPTYPE CONSOLE}

uses
  StrUtils,
  SysUtils,
  Forms,
  Inflector in 'lib\com\beardon\active_support\Inflector.pas',
  Delphinator in 'lib\com\beardon\active_support\Delphinator.pas',
  Inflectors in 'lib\com\beardon\active_support\Inflectors.pas',
  Generator in 'lib\com\beardon\delphidao\classes\Generator.pas',
  Template in 'lib\com\beardon\delphidao\classes\Template.pas',
  ArrayList in 'lib\com\beardon\delphidao\classes\dao\core\ArrayList.pas',
  SQLComparisonOperator in 'lib\com\beardon\delphidao\classes\dao\sql\SQLComparisonOperator.pas',
  SQLOrderDirection in 'lib\com\beardon\delphidao\classes\dao\sql\SQLOrderDirection.pas',
  HashLib in 'lib\com\beardon\delphidao\libraries\HashLib.pas',
  TbgQuery in 'lib\com\beardon\delphidao\classes\dao\sql\TbgQuery.pas',
  DatabaseDM in 'lib\com\beardon\delphidao\data_modules\DatabaseDM.pas' {DatabaseDataModule: TDataModule},
  Configuration in 'lib\com\beardon\delphidao\Configuration.pas';

var
  generator: TGenerator;
  outputPath: string;
  projectPath: string;

procedure ProcessParameters;
const
  DEFAULT_PROJECT_PATH = '..\..\..\project\';
  OUTPUT_PATH_PARAM = '-o';
  TEMPLATE_PATH_PARAM = '-t';
var
  i: Integer;
begin
  for i := 1 to ParamCount do
  begin
    if (LeftStr(ParamStr(i), 2) = OUTPUT_PATH_PARAM) then
      outputPath := Copy(ParamStr(i), 3, MaxInt);
    if (LeftStr(ParamStr(i), 2) = TEMPLATE_PATH_PARAM) then
      projectPath := Copy(ParamStr(i), 3, MaxInt);
  end;
  if (outputPath = '') then
    outputPath := ExtractFilePath(Application.ExeName);
  if (projectPath = '') then
    projectPath := DEFAULT_PROJECT_PATH;
end;

begin
  ProcessParameters;
  Application.CreateForm(TDatabaseDataModule, DatabaseDataModule);
  DatabaseDataModule.OpenDBConnection;
  generator := TGenerator.Create;
  try
    generator.Generate(outputPath, projectPath);
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  generator.Free;
end.
