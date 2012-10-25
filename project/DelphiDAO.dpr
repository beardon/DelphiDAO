program DelphiDAO;

{$APPTYPE CONSOLE}

uses
  StrUtils,
  SysUtils,
  Forms,
  Generator in 'lib\com\beardon\delphidao\Generator.pas',
  Template in 'lib\com\beardon\delphidao\templates\class\Template.pas',
  ArrayList in 'lib\com\beardon\delphidao\templates\class\dao\core\ArrayList.pas',
  Connection in 'lib\com\beardon\delphidao\templates\class\dao\sql\Connection.pas',
  ConnectionFactory in 'lib\com\beardon\delphidao\templates\class\dao\sql\ConnectionFactory.pas',
  ConnectionProperty in 'lib\com\beardon\delphidao\templates\class\dao\sql\ConnectionProperty.pas',
  Query in 'lib\com\beardon\delphidao\templates\class\dao\sql\Query.pas',
  QueryExecutor in 'lib\com\beardon\delphidao\templates\class\dao\sql\QueryExecutor.pas',
  QueryFactory in 'lib\com\beardon\delphidao\templates\class\dao\sql\QueryFactory.pas',
  Transaction in 'lib\com\beardon\delphidao\templates\class\dao\sql\Transaction.pas',
  Inflector in 'lib\com\beardon\active_support\Inflector.pas',
  Delphinator in 'lib\com\beardon\active_support\Delphinator.pas',
  Inflectors in 'lib\com\beardon\active_support\Inflectors.pas',
  SQLComparisonOperator in 'lib\com\beardon\delphidao\templates\class\dao\sql\SQLComparisonOperator.pas',
  SQLOrderDirection in 'lib\com\beardon\delphidao\templates\class\dao\sql\SQLOrderDirection.pas',
  hashes in 'lib\com\beardon\utils\hashes.pas';

var
  generator: TGenerator;
  outputPath: string;
  templatePath: string;

procedure ProcessParameters;
const
  DEFAULT_TEMPLATE_PATH = '..\..\..\project\lib\com\beardon\delphidao\templates';
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
      templatePath := Copy(ParamStr(i), 3, MaxInt);
  end;
  if (outputPath = '') then
    outputPath := ExtractFilePath(Application.ExeName);
  if (templatePath = '') then
    templatePath := DEFAULT_TEMPLATE_PATH;
end;

begin
  ProcessParameters;
  generator := TGenerator.Create;
  try
    generator.Generate(outputPath, templatePath);
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  generator.Free;
end.
