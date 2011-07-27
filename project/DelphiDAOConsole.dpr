program DelphiDAOConsole;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Forms,
  generator in 'lib\com\beardon\delphidao\generator.pas',
  template in 'lib\com\beardon\delphidao\templates\class\template.pas',
  array_list in 'lib\com\beardon\delphidao\templates\class\dao\core\array_list.pas',
  connection in 'lib\com\beardon\delphidao\templates\class\dao\sql\connection.pas',
  connection_factory in 'lib\com\beardon\delphidao\templates\class\dao\sql\connection_factory.pas',
  connection_property in 'lib\com\beardon\delphidao\templates\class\dao\sql\connection_property.pas',
  query in 'lib\com\beardon\delphidao\templates\class\dao\sql\query.pas',
  query_executor in 'lib\com\beardon\delphidao\templates\class\dao\sql\query_executor.pas',
  query_factory in 'lib\com\beardon\delphidao\templates\class\dao\sql\query_factory.pas',
  transaction in 'lib\com\beardon\delphidao\templates\class\dao\sql\transaction.pas';

var
  path: string;

begin
  try
    path := ExtractFilePath(Application.ExeName);
    TGenerator.generate(path);
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
