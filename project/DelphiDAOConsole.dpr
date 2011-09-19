program DelphiDAOConsole;

{$APPTYPE CONSOLE}

uses
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
  Transaction in 'lib\com\beardon\delphidao\templates\class\dao\sql\Transaction.pas';

var
  path: string;

begin
  path := ParamStr(1);
  try
    if (path = '') then
    begin
      path := ExtractFilePath(Application.ExeName);
    end;
    TGenerator.Generate(path);
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
