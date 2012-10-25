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
  Connection in 'lib\com\beardon\delphidao\classes\dao\sql\Connection.pas',
  ConnectionFactory in 'lib\com\beardon\delphidao\classes\dao\sql\ConnectionFactory.pas',
  ConnectionProperty in 'lib\com\beardon\delphidao\classes\dao\sql\ConnectionProperty.pas',
  Query in 'lib\com\beardon\delphidao\classes\dao\sql\Query.pas',
  QueryExecutor in 'lib\com\beardon\delphidao\classes\dao\sql\QueryExecutor.pas',
  QueryFactory in 'lib\com\beardon\delphidao\classes\dao\sql\QueryFactory.pas',
  SQLComparisonOperator in 'lib\com\beardon\delphidao\classes\dao\sql\SQLComparisonOperator.pas',
  SQLOrderDirection in 'lib\com\beardon\delphidao\classes\dao\sql\SQLOrderDirection.pas',
  Transaction in 'lib\com\beardon\delphidao\classes\dao\sql\Transaction.pas',
  HashLib in 'lib\com\beardon\delphidao\libraries\HashLib.pas';

var
  generator: TGenerator;
  outputPath: string;
  projectPath: string;

procedure ProcessParameters;
const
  DEFAULT_PROJECT_PATH = '..\..\..\project';
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
