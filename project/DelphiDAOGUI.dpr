program DelphiDAOGUI;

uses
  Forms,
  main in 'main.pas' {frmMain},
  template in 'lib\com\beardon\delphidao\templates\class\template.pas',
  generator in 'lib\com\beardon\delphidao\generator.pas',
  array_list in 'lib\com\beardon\delphidao\templates\class\dao\core\array_list.pas',
  connection in 'lib\com\beardon\delphidao\templates\class\dao\sql\connection.pas',
  connection_factory in 'lib\com\beardon\delphidao\templates\class\dao\sql\connection_factory.pas',
  connection_property in 'lib\com\beardon\delphidao\templates\class\dao\sql\connection_property.pas',
  query in 'lib\com\beardon\delphidao\templates\class\dao\sql\query.pas',
  query_executor in 'lib\com\beardon\delphidao\templates\class\dao\sql\query_executor.pas',
  query_factory in 'lib\com\beardon\delphidao\templates\class\dao\sql\query_factory.pas',
  transaction in 'lib\com\beardon\delphidao\templates\class\dao\sql\transaction.pas';

{$R *.res}

begin
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DelphiDAO';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
