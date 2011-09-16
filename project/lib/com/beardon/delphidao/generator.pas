unit generator;

interface

uses
  DBClient,
  Generics.Collections;

type
  TRoutineParameter = record
    Direction: string;
    VarName: string;
    SQLType: string;
  end;
  TGenerator = class
  private
    class procedure Init(Path: string);
    class function DoesTableContainPK(const TableName: string): Boolean;
    class procedure CreateDAOFactory(const Dataset: TClientDataSet; const Path: string);
    class procedure GenerateDTOObjects(const Dataset: TClientDataSet; const Path: string);
    class procedure GenerateDAOExtObjects(const Dataset: TClientDataSet; const Path: string);
    class procedure GenerateDAOObjects(const Dataset: TClientDataSet; const Path: string);
    class procedure GenerateIDAOObjects(const Dataset: TClientDataSet; const Path: string);
    class procedure GenerateStoredRoutines(const Path: string);
    class function GetFields(const TableName: string): TClientDataSet;
    class function LowerCamelCase(const Value: string): string;
    class function UpperCamelCase(const Value: string): string;
    class function GetDelphiType(const SQLType: string): string;
    class function GetDelphiAsType(const SQLType: string): string;
    class function FixReservedWords(const Value: string): string;
    class function GetRoutineParameters(const CreateSQL: string; const IsFunction: Boolean): TList<TRoutineParameter>;
    class function GetRoutineReturnType(const CreateSQL: string): string;
    class function ConcatLongString(const InStr: string; const Multiline: Boolean): string;
  public
    class procedure Generate(Path: string); static;
  end;

implementation

uses
  Classes,
  connection_property,
  query,
  query_executor,
  StrUtils,
  SysUtils,
  template,
  Windows;

const
  CRLF = #13#10;
  CRLF2 = CRLF + CRLF;
  TAB = '  ';
  TAB2 = TAB + TAB;
  TEMPLATE_PATH = '..\..\..\project\lib\com\beardon\delphidao\templates\';

class procedure TGenerator.Generate(Path: string);
var
  qry: TTBGQuery;
  ds: TClientDataSet;
begin
  Init(Path);
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW TABLES');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  GenerateDTOObjects(ds, Path);
	GenerateDAOObjects(ds, Path);
	GenerateDAOExtObjects(ds, Path);
	GenerateIDAOObjects(ds, Path);
	CreateDAOFactory(ds, Path);
  GenerateStoredRoutines(Path);
  ds.Free;
end;

class procedure TGenerator.Init(Path: string);
begin
	CreateDir(Path);
	CreateDir(Path + '\class');
	CreateDir(Path + '\class\dto');
	CreateDir(Path + '\class\mysql');
	CreateDir(Path + '\class\mysql\ext');
	CreateDir(Path + '\class\sql');
	CreateDir(Path + '\class\dao');
	CreateDir(Path + '\class\core');
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\connection.pas'), PChar(Path + '\class\sql\connection.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\connection_factory.pas'), PChar(Path + '\class\sql\connection_factory.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\connection_property.pas'), PChar(Path + '\class\sql\connection_property.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\query.pas'), PChar(Path + '\class\sql\query.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\query_executor.pas'), PChar(Path + '\class\sql\query_executor.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\query_factory.pas'), PChar(Path + '\class\sql\query_factory.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\transaction.pas'), PChar(Path + '\class\sql\transaction.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\core\array_list.pas'), PChar(Path + '\class\core\array_list.pas'), False);
end;

class function TGenerator.DoesTableContainPK(const TableName: string): Boolean;
var
  ds: TClientDataSet;
  success: Boolean;
begin
  success := False;
	ds := GetFields(TableName);
  with (ds) do
  while (not Eof) do
  begin
    if (ds.FieldByName('Key').AsString = 'PRI') then
    begin
      success := True;
    end;
    Next;
  end;
  Result := success;
end;

class procedure TGenerator.CreateDAOFactory(const Dataset: TClientDataSet; const Path: string);
var
  tableName, tableClassName,
  usesList, functionDeclarations, implementationCode: string;
  template: TTemplate;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + Path + '\class\dao\dao_factory.pas"...');
  with (Dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableClassName := UpperCamelCase(tableName);
      usesList := usesList + TAB + tableName + '_mysql_ext_dao,' + CRLF;
      functionDeclarations := functionDeclarations + TAB2 + 'class function get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'class function TDAOFactory.get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := T' + tableClassName + 'MySQLExtDAO.Create(fConnection);' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      Next;
    end;
    usesList := LeftStr(usesList, Length(usesList) - 3) + ';';
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(TEMPLATE_PATH + 'dao_factory.tpl');
    template.SetPair('uses_list', usesList);
    template.SetPair('function_declarations', functionDeclarations);
    template.SetPair('implementation_code', implementationCode);
    template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.Write('' + Path + '\class\dao\dao_factory.pas');
    template.Free;
    WriteLn(' done.');
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateDTOObjects(const Dataset: TClientDataSet; const Path: string);
var
  tableName, tableClassName,
  typeName, privateVars, publicConstants, publicProperties,
  thisField, thisType: string;
  template: TTemplate;
  ds: TClientDataSet;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (Dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      Write('Generating ' + '"' + Path + '\class\dto\' + tableName + '.pas"...');
      tableClassName := UpperCamelCase(tableName);
      template := TTemplate.Create(TEMPLATE_PATH + 'dto.tpl');
      template.SetPair('unit_name', tableName);
      template.SetPair('table_name', tableName);
      typeName := 'T' + tableClassName;
      template.SetPair('type_name', typeName);
      publicConstants := TAB2 + 'const TABLE_NAME = ''' + tableName + ''';';
      template.SetPair('public_constants', publicConstants);
      privateVars := '';
      publicProperties := '';
      ds := GetFields(tableName);
      with (ds) do
      while (not Eof) do
      begin
        thisField := FieldByName('Field').AsString;
        thisType := GetDelphiType(FieldByName('Type').AsString);
        privateVars := privateVars + TAB2 + 'f' + UpperCamelCase(thisField) + ': ' + thisType + ';' + CRLF;
        publicProperties := publicProperties + TAB2 + 'property ' + UpperCamelCase(thisField) + ': ' + thisType + ' read f' + UpperCamelCase(thisField) + ' write f' + UpperCamelCase(thisField) + ';' + CRLF;
        Next;
      end;
      ds.Free;
      privateVars := LeftStr(privateVars, Length(privateVars) - 2);
      publicProperties := LeftStr(publicProperties, Length(publicProperties) - 2);
      template.SetPair('private_vars', privateVars);
      template.SetPair('public_properties', publicProperties);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.Write('' + Path + '\class\dto\' + tableName + '.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateDAOExtObjects(const Dataset: TClientDataSet; const Path: string);
var
  tableName, tableClassName,
  typeName, ancestorTypeName, usesList: string;
  template: TTemplate;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (Dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      if (not FileExists('' + Path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas')) then
      begin
        Write('Generating ' + '"' + Path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas"...');
        tableClassName := UpperCamelCase(tableName);
        usesList := TAB + tableName + '_mysql_dao;';
        template := TTemplate.Create(TEMPLATE_PATH + 'dao_ext.tpl');
        template.SetPair('unit_name', tableName);
        template.SetPair('uses_list', usesList);
        template.SetPair('table_name', tableName);
        typeName := 'T' + tableClassName + 'MySQLExtDAO';
        ancestorTypeName := 'T' + tableClassName + 'MySQLDAO';
        template.SetPair('type_name', typeName);
        template.SetPair('ancestor_type_name', ancestorTypeName);
        template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
        template.Write('' + Path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas');
        template.Free;
        WriteLn(' done.');
      end
      else
      begin
        WriteLn('"' + Path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas" already exists.');
      end;
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateDAOObjects(const Dataset: TClientDataSet; const Path: string);
var
  i: Integer;
  tableName, tableClassName, usesList, interfaceName,
  typeName, privateVars, publicConstants, publicProperties,
  thisField, delphiType, asType, s, s2, s3, s4: string;
  parameterSetter, insertFields, insertFields2, updateFields, insertValues,
  insertValues2, readRow, pk, queryByDef, deleteByDef,
  queryByFunc, deleteByFunc: string;
  pks: array of string;
  template: TTemplate;
  ds: TClientDataSet;
  hasPK: Boolean;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (Dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      Write('Generating ' + '"' + Path + '\class\mysql\' + tableName + '_mysql_dao.pas"...');
      hasPK := DoesTableContainPK(tableName);
      tableClassName := UpperCamelCase(tableName);
      ds := GetFields(tableName);
      parameterSetter := CRLF;
      insertFields := '';
      updateFields := '';
      insertValues := '';
      readRow := CRLF;
      pk := '';
      SetLength(pks, 0);
      queryByDef := '';
      queryByFunc := '';
      deleteByDef := '';
      deleteByFunc := '';
      with (ds) do
      while (not Eof) do
      begin
        thisField := FieldByName('Field').AsString;
        delphiType := GetDelphiType(FieldByName('Type').AsString);
        asType := GetDelphiAsType(FieldByName('Type').AsString);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := thisField;
          SetLength(pks, Length(pks) + 1);
          pks[Length(pks) - 1] := thisField;
        end
        else
        begin
          insertFields := insertFields + thisField + ', ';
          updateFields := updateFields + thisField + ' = :' + UpperCamelCase(thisField) + ', ';
          insertValues := insertValues + ':' + UpperCamelCase(thisField) + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + UpperCamelCase(thisField) + ''').Value := ' + tableName + '.' + UpperCamelCase(thisField) + ';' + CRLF;
          queryByDef := queryByDef + TAB2 + 'function queryBy' + UpperCamelCase(thisField) + '(const value: ' + delphiType + '): TObjectList<T' + tableClassName + '>;' + CRLF;
          queryByFunc := queryByFunc + 'function T' + tableClassName + 'MySQLDAO.queryBy' + UpperCamelCase(thisField) + '(const value: ' + delphiType + '): TObjectList<T' + tableClassName + '>;' + CRLF;
          queryByFunc := queryByFunc + 'var' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry: TTBGQuery;' + CRLF;
          queryByFunc := queryByFunc + 'begin' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry := TTBGQuery.Create;' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.sql.Add(''SELECT * FROM ' + tableName + ' WHERE ' + thisField + ' = :' + UpperCamelCase(thisField) + ''');' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.ParamByName(''' + UpperCamelCase(thisField) + ''').Value := value;' + CRLF;
          queryByFunc := queryByFunc + TAB + 'Result := getList(qry);' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.Free;' + CRLF;
          queryByFunc := queryByFunc + 'end;' + CRLF2;
          deleteByDef := deleteByDef + TAB2 + 'function deleteBy' + UpperCamelCase(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
          deleteByFunc := deleteByFunc + 'function T' + tableClassName + 'MySQLDAO.deleteBy' + UpperCamelCase(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
          deleteByFunc := deleteByFunc + 'var' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry: TTBGQuery;' + CRLF;
          deleteByFunc := deleteByFunc + 'begin' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry := TTBGQuery.Create;' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.sql.Add(''DELETE FROM ' + tableName + ' WHERE ' + thisField + ' = :' + UpperCamelCase(thisField) + ''');' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.ParamByName(''' + UpperCamelCase(thisField) + ''').Value := value;' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'Result := executeUpdate(qry);' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.Free;' + CRLF;
          deleteByFunc := deleteByFunc + 'end;' + CRLF2;
        end;
        readRow := readRow + TAB + tableClassName + '.' + UpperCamelCase(thisField) + ' := dataset.FieldByName(''' + thisField + ''').' + asType + ';' + CRLF;
        Next;
      end;
      ds.Free;
      if (hasPK) then
      begin
        if (Length(pks) = 1) then
        begin
          template := TTemplate.Create(TEMPLATE_PATH + 'dao.tpl');
        end
        else
        begin
          template := TTemplate.Create(TEMPLATE_PATH + 'dao_complex_pk.tpl');
        end;
      end
      else
      begin
        template := TTemplate.Create(TEMPLATE_PATH + 'dao_view.tpl');
      end;
      template.SetPair('dao_class_name', 'T' + tableClassName);
      template.SetPair('table_name', tableName);
      template.SetPair('var_name', UpperCamelCase(tableName));
      insertFields := LeftStr(insertFields, Length(insertFields) - 1);
      updateFields := LeftStr(updateFields, Length(updateFields) - 1);
      insertValues := LeftStr(insertValues, Length(insertValues) - 1);
      queryByDef := LeftStr(queryByDef, Length(queryByDef) - 2);
      queryByFunc := LeftStr(queryByFunc, Length(queryByFunc) - 2);
      deleteByDef := LeftStr(deleteByDef, Length(deleteByDef) - 2);
      deleteByFunc := LeftStr(deleteByFunc, Length(deleteByFunc) - 2);
      if (hasPK) then
      begin
        template.SetPair('pk', pk);
        s := '';
        s2 := '';
        s3 := '';
        s4 := '';
        insertFields2 := insertFields;
        insertValues2 := insertValues;
        for i := 0 to High(pks) do
        begin
          insertValues2 := insertValues2 + ', ?';
          if (i > 0) then
          begin
            s := s + ', ';
            s2 := s2 + ' AND ';
            s3 := s3 + TAB2;
          end;
          insertFields2 := insertFields2 + ', ' + pks[i];
          s := s + '$' + UpperCamelCase(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + UpperCamelCase(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + UpperCamelCase(tableName) + '->' + UpperCamelCase(pks[i]) + ');';
          s4 := s4 + CRLF;
        end;
        if (s[1] = ',') then
        begin
          s := Copy(s, 2, MaxInt);
        end;
        if (insertValues2[1] = ',') then
        begin
          insertValues2 := Copy(insertValues2, 2, MaxInt);
        end;
        if (insertFields2[1] = ',') then
        begin
          insertFields2 := Copy(insertFields2, 2, MaxInt);
        end;
        insertFields := LeftStr(insertFields, Length(insertFields) - 1);
        insertFields := ConcatLongString(insertFields, True);
        updateFields := LeftStr(updateFields, Length(updateFields) - 1);
        updateFields := ConcatLongString(updateFields, True);
        insertValues := LeftStr(insertValues, Length(insertValues) - 1);
        insertValues := ConcatLongString(insertValues, True);
        template.SetPair('insert_values2', insertValues2);
        template.SetPair('insert_fields2', insertFields2);
        template.SetPair('pk_set_update', s4);
        template.SetPair('pk_set', s3);
        template.SetPair('pk_where', s2);
        template.SetPair('pks', s);
        template.SetPair('pk_with_s', UpperCamelCase(pk));
        template.SetPair('insert_fields', insertFields);
        template.SetPair('update_fields', updateFields);
        template.SetPair('insert_values', insertValues);
        template.SetPair('parameter_setter', parameterSetter);
        template.SetPair('delete_by_definitions', deleteByDef);
        template.SetPair('delete_by_functions', deleteByFunc);
      end;
      usesList := TAB + tableName + ',' + CRLF + TAB + tableName + '_dao,';
      typeName := 'T' + tableClassName + 'MySQLDAO';
      interfaceName := 'I' + tableClassName + 'DAO';
      template.SetPair('unit_name', tableName);
      template.SetPair('uses_list', usesList);
      template.SetPair('type_name', typeName);
      template.SetPair('interface_name', interfaceName);
      template.SetPair('read_row', readRow);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.SetPair('query_by_definitions', queryByDef);
      template.SetPair('query_by_functions', queryByFunc);
      template.Write('' + Path + '\class\mysql\' + tableName + '_mysql_dao.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateIDAOObjects(const Dataset: TClientDataSet; const Path: string);
var
  i: Integer;
  tableName, tableClassName, usesList,
  typeName, thisField, delphiType, asType, s, s2, s3, s4: string;
  parameterSetter, insertFields, insertFields2, updateFields, insertValues,
  insertValues2, readRow, pk, queryByDef, deleteByDef: string;
  pks: array of string;
  template: TTemplate;
  ds: TClientDataSet;
  hasPK: Boolean;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (Dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      Write('Generating ' + '"' + Path + '\class\dao\' + tableName + '_dao.pas"...');
      hasPK := DoesTableContainPK(tableName);
      tableClassName := UpperCamelCase(tableName);
      ds := GetFields(tableName);
      parameterSetter := CRLF;
      insertFields := '';
      updateFields := '';
      insertValues := '';
      readRow := CRLF;
      pk := '';
      SetLength(pks, 0);
      queryByDef := '';
      deleteByDef := '';
      with (ds) do
      while (not Eof) do
      begin
        thisField := FieldByName('Field').AsString;
        delphiType := GetDelphiType(FieldByName('Type').AsString);
        asType := GetDelphiAsType(FieldByName('Type').AsString);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := thisField;
          SetLength(pks, Length(pks) + 1);
          pks[Length(pks) - 1] := thisField;
        end
        else
        begin
          insertFields := insertFields + thisField + ', ';
          updateFields := updateFields + thisField + ' = :' + UpperCamelCase(thisField) + ', ';
          insertValues := insertValues + ':' + UpperCamelCase(thisField) + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + UpperCamelCase(thisField) + ''').Value := ' + tableName + '.' + thisField + ';' + CRLF;
          queryByDef := queryByDef + TAB2 + 'function queryBy' + UpperCamelCase(thisField) + '(const value: ' + delphiType + '): TObjectList<T' + tableClassName + '>;' + CRLF;
          deleteByDef := deleteByDef + TAB2 + 'function deleteBy' + UpperCamelCase(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
        end;
        readRow := readRow + TAB + tableClassName + '.' + UpperCamelCase(thisField) + ' := dataset.FieldByName(''' + thisField + ''').' + asType + ';' + CRLF;
        Next;
      end;
      ds.Free;
      if (hasPK) then
      begin
        if (Length(pks) = 1) then
        begin
          template := TTemplate.Create(TEMPLATE_PATH + 'idao.tpl');
        end
        else
        begin
          template := TTemplate.Create(TEMPLATE_PATH + 'idao_complex_pk.tpl');
        end;
      end
      else
      begin
        template := TTemplate.Create(TEMPLATE_PATH + 'idao_view.tpl');
      end;
      template.SetPair('dao_class_name', 'T' + tableClassName);
      template.SetPair('table_name', tableName);
      template.SetPair('var_name', UpperCamelCase(tableName));
      if (hasPK) then
      begin
        insertFields := Copy(insertFields, 1, Length(insertFields) - 1);
        updateFields := Copy(updateFields, 1, Length(updateFields) - 1);
        insertValues := Copy(insertValues, 1, Length(insertValues) - 1);
        template.SetPair('pk', pk);
        s := '';
        s2 := '';
        s3 := '';
        s4 := '';
        insertFields2 := insertFields;
        insertValues2 := insertValues;
        for i := 0 to High(pks) do
        begin
          insertValues2 := insertValues2 + ', ?';
          if (i > 0) then
          begin
            s := s + ', ';
            s2 := s2 + ' AND ';
            s3 := s3 + TAB2;
          end;
          insertFields2 := insertFields2 + ', ' + pks[i];
          s := s + '$' + UpperCamelCase(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + UpperCamelCase(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + UpperCamelCase(tableName) + '->' + UpperCamelCase(pks[i]) + ');';
          s4 := s4 + CRLF;
        end;
        template.SetPair('insert_values2', insertValues2);
        template.SetPair('insert_fields2', insertFields2);
        template.SetPair('pk_set_update', s4);
        template.SetPair('pk_set', s3);
        template.SetPair('pk_where', s2);
        template.SetPair('pks', s);
        insertFields := Copy(insertFields, 1, Length(insertFields) - 1);
        insertFields := ConcatLongString(insertFields, True);
        updateFields := Copy(updateFields, 1, Length(updateFields) - 1);
        updateFields := ConcatLongString(updateFields, True);
        insertValues := Copy(insertValues, 1, Length(insertValues) - 1);
        insertValues := ConcatLongString(insertValues, True);
        template.SetPair('insert_fields', insertFields);
        template.SetPair('update_fields', updateFields);
        template.SetPair('insert_values', insertValues);
        template.SetPair('parameter_setter', parameterSetter);
        template.SetPair('delete_by_definitions', deleteByDef);
      end;
      usesList := TAB + tableName + ',';
      typeName := 'I' + tableClassName + 'DAO';
      template.SetPair('unit_name', tableName);
      template.SetPair('uses_list', usesList);
      template.SetPair('type_name', typeName);
      template.SetPair('read_row', readRow);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.SetPair('query_by_definitions', queryByDef);
      template.Write('' + Path + '\class\dao\' + tableName + '_dao.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateStoredRoutines(const Path: string);
var
  routineName, createSQL, funcParams, sqlParams, comment,
  sqlReturnType, delphiReturnType,
  functionDeclarations, implementationCode: string;
  paramRecs: TList<TRoutineParameter>;
  paramRec : TRoutineParameter;
  template: TTemplate;
  qry: TTBGQuery;
  ds: TClientDataSet;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + Path + '\class\stored_routines.pas"...');
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW PROCEDURE STATUS');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  with (ds) do
  begin
    First;
    while (not Eof) do
    begin
      routineName := FieldByName('Name').AsString;
      comment := FieldByName('Comment').AsString;
      qry := TTBGQuery.Create;
      qry.SQL.Add('SHOW CREATE PROCEDURE ' + routineName);
      createSQL := TQueryExecutor.QueryForString(qry, 'Create Procedure');
      qry.Free;
      paramRecs := GetRoutineParameters(createSQL, False);
      funcParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        funcParams := funcParams + 'const ' + LowerCamelCase(paramRec.VarName) + ': ' + GetDelphiType(paramRec.SQLType) + '; ';
        sqlParams := sqlParams + ':' + paramRec.VarName + ', ';
      end;
      if (funcParams <> '') then
      begin
        funcParams := Copy(funcParams, 1, Length(funcParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      functionDeclarations := functionDeclarations + TAB2 + 'class procedure ' + LowerCamelCase(routineName) + '(' + funcParams + ');' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + GetDelphiType(paramRec.SQLType) + ' ' + LowerCamelCase(paramRec.VarName) + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class procedure TStoredRoutines.' + LowerCamelCase(routineName) + '(' + funcParams + ');' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.sql.Add(''CALL ' + routineName + '(' + sqlParams + ')'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.VarName + ''').Value := ' + LowerCamelCase(paramRec.VarName) + ';' + CRLF;
      end;
      implementationCode := implementationCode + TAB + 'ds := TQueryExecutor.execute(qry);' + CRLF;
      implementationCode := implementationCode + TAB + 'ds.Free;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      paramRecs.Free;
      Next;
    end;
  end;
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW FUNCTION STATUS');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  with (ds) do
  begin
    First;
    while (not Eof) do
    begin
      routineName := FieldByName('Name').AsString;
      qry := TTBGQuery.Create;
      qry.SQL.Add('SHOW CREATE FUNCTION ' + routineName);
      createSQL := TQueryExecutor.QueryForString(qry, 'Create Function');
      qry.Free;
      paramRecs := GetRoutineParameters(createSQL, True);
      funcParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        funcParams := funcParams + 'const ' + LowerCamelCase(paramRec.VarName) + ': ' + GetDelphiType(paramRec.SQLType) + '; ';
        sqlParams := sqlParams + ':' + paramRec.VarName + ', ';
      end;
      if (funcParams <> '') then
      begin
        funcParams := Copy(funcParams, 1, Length(funcParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      sqlReturnType := GetRoutineReturnType(createSQL);
      delphiReturnType := GetDelphiType(sqlReturnType);
      functionDeclarations := functionDeclarations + TAB2 + 'class function ' + LowerCamelCase(routineName) + '(' + funcParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + GetDelphiType(paramRec.SQLType) + ' ' + LowerCamelCase(paramRec.VarName) + CRLF;
      implementationCode := implementationCode + ' * @return ' + delphiReturnType + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class function TStoredRoutines.' + LowerCamelCase(routineName) + '(' + funcParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.sql.Add(''SELECT ' + routineName + '(' + sqlParams + ') AS value'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.VarName + ''').Value := ' + LowerCamelCase(paramRec.VarName) + ';' + CRLF;
      end;
      implementationCode := implementationCode + TAB + 'ds := TQueryExecutor.execute(qry);' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := ds.FieldByName(''value'').Value;' + CRLF;
      implementationCode := implementationCode + TAB + 'ds.Free;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      paramRecs.Free;
      Next;
    end;
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(TEMPLATE_PATH + 'stored_routines.tpl');
    template.SetPair('function_declarations', functionDeclarations);
    template.SetPair('implementation_code', implementationCode);
    template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.Write('' + Path + '\class\stored_routines.pas');
    template.Free;
    WriteLn(' done.');
  end;
  ds.Free;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class function TGenerator.GetFields(const TableName: string): TClientDataSet;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.SQL.Add('DESC ' + TableName);
  Result := TQueryExecutor.Execute(qry);
  qry.Free;
end;

class function TGenerator.LowerCamelCase(const Value: string): string;
var
  cameled: string;
begin
  cameled := UpperCamelCase(Value);
  Result := LowerCase(cameled[1]) + Copy(cameled, 2, MaxInt);
end;

class function TGenerator.UpperCamelCase(const Value: string): string;
var
  i: Integer;
  cameled: string;
  humps: TStringList;
begin
  humps := TStringList.Create;
  humps.Delimiter := '_';
  humps.DelimitedText := Value;
  for i := 0 to humps.Count - 1 do
  begin
    cameled := cameled + UpperCase(humps[i][1]) + Copy(humps[i], 2, MaxInt);
  end;
  Result := FixReservedWords(cameled);
end;

class function TGenerator.GetDelphiType(const SQLType: string): string;
var
  delphiType, sqlTypeL: string;
begin
  delphiType := '';
  sqlTypeL := LowerCase(SQLType);
  if (Pos('tinyint(1)', sqlTypeL) > 0) then
  begin
    delphiType := 'Boolean';
  end
  else if ((Pos('int', sqlTypeL) > 0) or (Pos('decimal', sqlTypeL) > 0)) then
  begin
    delphiType := 'Integer';
  end;
  if ((Pos('float', sqlTypeL) > 0) or (Pos('double', sqlTypeL) > 0)) then
  begin
    delphiType := 'Extended';
  end;
  if ((Pos('char', sqlTypeL) > 0) or (Pos('text', sqlTypeL) > 0) or (Pos('enum', sqlTypeL) > 0) or (Pos('set', sqlTypeL) > 0)) then
  begin
    delphiType := 'string';
  end;
  if ((Pos('datetime', sqlTypeL) > 0) or (Pos('timestamp', sqlTypeL) > 0)) then
  begin
    delphiType := 'TDateTime';
  end
  else
  if (Pos('date', sqlTypeL) > 0) then
  begin
    delphiType := 'TDate';
  end
  else
  if (Pos('time', sqlTypeL) > 0) then
  begin
    delphiType := 'TTime';
  end;
  if ((Pos('blob', sqlTypeL) > 0) or (Pos('binary', sqlTypeL) > 0)) then
  begin
    delphiType := 'Variant';
  end;
  Result := delphiType;
end;

class function TGenerator.GetDelphiAsType(const SQLType: string): string;
var
  asType, sqlTypeL: string;
begin
  asType := '';
  sqlTypeL := LowerCase(SQLType);
  if (Pos('tinyint(1)', sqlTypeL) > 0) then
  begin
    asType := 'AsBoolean';
  end
  else if ((Pos('int', sqlTypeL) > 0) or (Pos('decimal', sqlTypeL) > 0)) then
  begin
    asType := 'AsInteger';
  end;
  if ((Pos('float', sqlTypeL) > 0) or (Pos('double', sqlTypeL) > 0)) then
  begin
    asType := 'AsExtended';
  end;
  if ((Pos('char', sqlTypeL) > 0) or (Pos('text', sqlTypeL) > 0) or (Pos('enum', sqlTypeL) > 0) or (Pos('set', sqlTypeL) > 0)) then
  begin
    asType := 'AsString';
  end;
  if ((Pos('date', sqlTypeL) > 0) or (Pos('time', sqlTypeL) > 0)) then
  begin
    asType := 'AsDateTime';
  end;
  if ((Pos('blob', sqlTypeL) > 0) or (Pos('binary', sqlTypeL) > 0)) then
  begin
    asType := 'AsVariant';
  end;
  Result := asType;
end;

class function TGenerator.FixReservedWords(const Value: string): string;
const
  RESERVED_WORDS = 'ClassTypeUnitName';
var
  corrected: string;
begin
  corrected := Value;
  if (Pos(Value, RESERVED_WORDS) > 0) then
  begin
    corrected := Value + '_';
  end;
  Result := corrected;
end;

class function TGenerator.GetRoutineParameters(const CreateSQL: string; const IsFunction: Boolean): TList<TRoutineParameter>;
var
  i: Integer;
  paramsStr: string;
  params: TStringList;
  param: TStringList;
  paramRec: TRoutineParameter;
  paramList: TList<TRoutineParameter>;
begin
  paramsStr := Copy(CreateSQL, Pos('(', CreateSQL) + 1, Pos(')', CreateSQL) - Pos('(', CreateSQL) - 1);
  paramsStr := Trim(paramsStr);
  paramsStr := StringReplace(paramsStr, #9, '', [rfReplaceAll]);
  paramsStr := StringReplace(paramsStr, #$A, '', [rfReplaceAll]);
  params := TStringList.Create;
  params.Delimiter := ',';
  params.StrictDelimiter := True;
  params.DelimitedText := paramsStr;
  paramList := TList<TRoutineParameter>.Create;
  for i := 0 to params.Count - 1 do
  begin
    param := TStringList.Create;
    param.Delimiter := ' ';
    param.DelimitedText := params[i];
    if (not IsFunction) then
    begin
      paramRec.Direction := param[0];
      paramRec.VarName := param[1];
      paramRec.SQLType := param[2];
    end
    else
    begin
      paramRec.VarName := param[0];
      paramRec.SQLType := param[1];
    end;
    paramList.Add(paramRec);
    param.Free;
  end;
  params.Free;
  Result := paramList;
end;

class function TGenerator.GetRoutineReturnType(const CreateSQL: string): string;
var
  returnTypeStr: string;
begin
  returnTypeStr := Copy(CreateSQL, Pos('RETURNS', CreateSQL));
  returnTypeStr := Copy(returnTypeStr, 1, Pos('BEGIN', returnTypeStr));
  returnTypeStr := Trim(returnTypeStr);
  returnTypeStr := StringReplace(returnTypeStr, #9, '', [rfReplaceAll]);
  returnTypeStr := StringReplace(returnTypeStr, #$A, '', [rfReplaceAll]);
  Result := returnTypeStr;
end;

class function TGenerator.ConcatLongString(const InStr: string; const Multiline: Boolean): string;
const
  BREAK_COUNT = 150;
var
  outStr, delim: string;
begin
  outStr := InStr;
  if (Length(InStr) > BREAK_COUNT) then
  begin
    if (Multiline) then
    begin
      delim := ''' + ' + CRLF + TAB2 + '''';
    end
    else
    begin
      delim := ''' + ''';
    end;
    outStr := Copy(InStr, 1, BREAK_COUNT) + delim + ConcatLongString(Copy(InStr, BREAK_COUNT + 1, MaxInt), Multiline);
  end;
  Result := outStr;
end;

end.
