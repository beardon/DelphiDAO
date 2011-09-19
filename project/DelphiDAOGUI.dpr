program DelphiDAOGUI;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  Template in 'lib\com\beardon\delphidao\templates\class\Template.pas',
  Generator in 'lib\com\beardon\delphidao\Generator.pas',
  ArrayList in 'lib\com\beardon\delphidao\templates\class\dao\core\ArrayList.pas',
  Connection in 'lib\com\beardon\delphidao\templates\class\dao\sql\Connection.pas',
  ConnectionFactory in 'lib\com\beardon\delphidao\templates\class\dao\sql\ConnectionFactory.pas',
  ConnectionProperty in 'lib\com\beardon\delphidao\templates\class\dao\sql\ConnectionProperty.pas',
  Query in 'lib\com\beardon\delphidao\templates\class\dao\sql\Query.pas',
  QueryExecutor in 'lib\com\beardon\delphidao\templates\class\dao\sql\QueryExecutor.pas',
  QueryFactory in 'lib\com\beardon\delphidao\templates\class\dao\sql\QueryFactory.pas',
  Transaction in 'lib\com\beardon\delphidao\templates\class\dao\sql\Transaction.pas';

{$R *.res}

begin
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DelphiDAO';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
