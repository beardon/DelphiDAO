unit generator;

interface

uses
  DBClient;

type
  TGenerator = class
  private
    class procedure init(path: string);
    class function doesTableContainPK(const tableName: string): Boolean;
    class procedure createDAOFactory(const dataset: TClientDataSet; const path: string);
    class procedure generateDomainObjects(const dataset: TClientDataSet; const path: string);
    class procedure generateDAOExtObjects(const dataset: TClientDataSet; const path: string);
    class procedure generateDAOObjects(const dataSet: TClientDataSet; const path: string);
    class procedure generateIDAOObjects(const dataset: TClientDataSet; const path: string);
    class procedure generateStoredRoutines(const path: string);
    class function getFields(const tableName: string): TClientDataSet;
    class function getClazzName(const tableName: string): string;
    class function getDTOName(const tableName: string): string;
    class function getVarName(const tableName: string): string;
    class function getVarNameWithS(const tableName: string): string;
    class function getDelphiType(const sqlType: string): string;
    class function getDelphiAsType(const sqlType: string): string;
    class function concatLongString(const inStr: string; const multiline: Boolean): string;
  public
    class procedure generate(path: string); static;
  end;

implementation

uses
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

class procedure TGenerator.generate(path: string);
var
  qry: TTBGQuery;
  ds: TClientDataSet;
begin
  init(path);
  qry := TTBGQuery.Create;
  qry.sql.Add('SHOW TABLES');
  ds := TQueryExecutor.execute(qry);
  qry.Free;
  generateDomainObjects(ds, path);
	generateDAOObjects(ds, path);
	generateDAOExtObjects(ds, path);
	generateIDAOObjects(ds, path);
	createDAOFactory(ds, path);
  ds.Free;
end;

class procedure TGenerator.init(path: string);
begin
	CreateDir(path);
	CreateDir(path + '\class');
	CreateDir(path + '\class\dto');
	CreateDir(path + '\class\mysql');
	CreateDir(path + '\class\mysql\ext');
	CreateDir(path + '\class\sql');
	CreateDir(path + '\class\dao');
	CreateDir(path + '\class\core');
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\connection.pas'), PChar(path + '\class\sql\connection.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\connection_factory.pas'), PChar(path + '\class\sql\connection_factory.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\connection_property.pas'), PChar(path + '\class\sql\connection_property.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\query.pas'), PChar(path + '\class\sql\query.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\query_executor.pas'), PChar(path + '\class\sql\query_executor.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\query_factory.pas'), PChar(path + '\class\sql\query_factory.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\sql\transaction.pas'), PChar(path + '\class\sql\transaction.pas'), False);
  CopyFile(PChar(TEMPLATE_PATH + 'class\dao\core\array_list.pas'), PChar(path + '\class\core\array_list.pas'), False);
end;

class function TGenerator.doesTableContainPK(const tableName: string): Boolean;
var
  ds: TClientDataSet;
  success: Boolean;
begin
  success := False;
	ds := getFields(tableName);
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

class procedure TGenerator.createDAOFactory(const dataset: TClientDataSet; const path: string);
var
  tableName, tableClassName,
  usesList, functionDeclarations, implementationCode: string;
  template: TTemplate;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + path + '\class\dao\dao_factory.pas"...');
  with (dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.getDatabase).AsString;
      tableClassName := getClazzName(tableName);
      usesList := usesList + TAB + tableName + '_mysql_ext_dao,' + CRLF;
      functionDeclarations := functionDeclarations + TAB2 + 'class function get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'class function TDAOFactory.get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := T' + tableClassName + 'MySQLExtDAO.Create;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      Next;
    end;
    usesList := LeftStr(usesList, Length(usesList) - 3) + ';';
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(TEMPLATE_PATH + 'dao_factory.tpl');
    template.setPair('uses_list', usesList);
    template.setPair('function_declarations', functionDeclarations);
    template.setPair('implementation_code', implementationCode);
    template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.write('' + path + '\class\dao\dao_factory.pas');
    template.Free;
    WriteLn(' done.');
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.generateDomainObjects(const dataset: TClientDataSet; const path: string);
var
  tableName, tableClassName,
  typeName, privateVars, publicConstants, publicProperties,
  thisField, thisType: string;
  template: TTemplate;
  ds: TClientDataSet;
  hasPK: Boolean;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.getDatabase).AsString;
      Write('Generating ' + '"' + path + '\class\dto\' + tableName + '.pas"...');
      hasPK := doesTableContainPK(tableName);
      tableClassName := getClazzName(tableName);
      // is this really necessary? - AB
      if (tableClassName[Length(tableClassName)] = 's') then
      begin
        tableClassName := Copy(tableClassName, 1, Length(tableClassName) - 1);
      end;
      template := TTemplate.Create(TEMPLATE_PATH + 'domain.tpl');
      template.setPair('unit_name', tableName);
      template.setPair('table_name', tableName);
      typeName := 'T' + tableClassName;
      template.setPair('type_name', typeName);
      publicConstants := TAB2 + 'const TABLE_NAME = ''' + tableName + ''';';
      template.setPair('public_constants', publicConstants);
      privateVars := '';
      publicProperties := '';
      ds := getFields(tableName);
      with (ds) do
      while (not Eof) do
      begin
        thisField := FieldByName('Field').AsString;
        thisType := getDelphiType(FieldByName('Type').AsString);
        privateVars := privateVars + TAB2 + 'f' + thisField + ': ' + thisType + ';' + CRLF;
        if (hasPK) then
        begin
          publicProperties := publicProperties + TAB2 + 'property ' + thisField + ': ' + thisType + ' read f' + thisField + ' write f' + thisField + ';' + CRLF;
        end
        else
        begin
          publicProperties := publicProperties + TAB2 + 'property ' + thisField + ': ' + thisType + ' read f' + thisField + ';' + CRLF;
        end;
        Next;
      end;
      ds.Free;
      privateVars := LeftStr(privateVars, Length(privateVars) - 2);
      publicProperties := LeftStr(publicProperties, Length(publicProperties) - 2);
      template.setPair('private_vars', privateVars);
      template.setPair('public_properties', publicProperties);
      template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.write('' + path + '\class\dto\' + tableName + '.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.generateDAOExtObjects(const dataset: TClientDataSet; const path: string);
var
  tableName, tableClassName,
  typeName, ancestorTypeName, usesList: string;
  template: TTemplate;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.getDatabase).AsString;
      if (not FileExists('' + path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas')) then
      begin
        Write('Generating ' + '"' + path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas"...');
        tableClassName := getClazzName(tableName);
        usesList := TAB + tableName + '_mysql_dao;';
        template := TTemplate.Create(TEMPLATE_PATH + 'dao_ext.tpl');
        template.setPair('unit_name', tableName);
        template.setPair('uses_list', usesList);
        template.setPair('table_name', tableName);
        typeName := 'T' + tableClassName + 'MySQLExtDAO';
        ancestorTypeName := 'T' + tableClassName + 'MySQLDAO';
        template.setPair('type_name', typeName);
        template.setPair('ancestor_type_name', ancestorTypeName);
        template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
        template.write('' + path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas');
        template.Free;
        WriteLn(' done.');
      end
      else
      begin
        WriteLn('"' + path + '\class\mysql\ext\' + tableName + '_mysql_ext_dao.pas" already exists.');
      end;
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.generateDAOObjects(const dataSet: TClientDataSet; const path: string);
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
  with (dataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.getDatabase).AsString;
      Write('Generating ' + '"' + path + '\class\mysql\' + tableName + '_mysql_dao.pas"...');
      hasPK := doesTableContainPK(tableName);
      tableClassName := getClazzName(tableName);
      ds := getFields(tableName);
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
        delphiType := getDelphiType(FieldByName('Type').AsString);
        asType := getDelphiAsType(FieldByName('Type').AsString);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := thisField;
          SetLength(pks, Length(pks) + 1);
          pks[Length(pks) - 1] := thisField;
        end
        else
        begin
          insertFields := insertFields + thisField + ', ';
          updateFields := updateFields + thisField + ' = :' + getVarNameWithS(thisField) + ', ';
          insertValues := insertValues + ':' + getVarNameWithS(thisField) + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + getVarNameWithS(thisField) + ''').Value := ' + tableName + '.' + thisField + ';' + CRLF;
          queryByDef := queryByDef + TAB2 + 'function queryBy' + getClazzName(thisField) + '(const value: ' + delphiType + '): TList<T' + tableClassName + '>;' + CRLF;
          queryByFunc := queryByFunc + 'function T' + tableClassName + 'MySQLDAO.queryBy' + getClazzName(thisField) + '(const value: ' + delphiType + '): TList<T' + tableClassName + '>;' + CRLF;
          queryByFunc := queryByFunc + 'var' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry: TTBGQuery;' + CRLF;
          queryByFunc := queryByFunc + 'begin' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry := TTBGQuery.Create;' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.sql.Add(''SELECT * FROM ' + tableName + ' WHERE ' + thisField + ' = :' + getVarNameWithS(thisField) + ''');' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.ParamByName(''' + getVarNameWithS(thisField) + ''').Value := value;' + CRLF;
          queryByFunc := queryByFunc + TAB + 'Result := getList(qry);' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.Free;' + CRLF;
          queryByFunc := queryByFunc + 'end;' + CRLF2;
          deleteByDef := deleteByDef + TAB2 + 'function deleteBy' + getClazzName(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
          deleteByFunc := deleteByFunc + 'function T' + tableClassName + 'MySQLDAO.deleteBy' + getClazzName(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
          deleteByFunc := deleteByFunc + 'var' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry: TTBGQuery;' + CRLF;
          deleteByFunc := deleteByFunc + 'begin' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry := TTBGQuery.Create;' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.sql.Add(''DELETE FROM ' + tableName + ' WHERE ' + thisField + ' = :' + getVarNameWithS(thisField) + ''');' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.ParamByName(''' + getVarNameWithS(thisField) + ''').Value := value;' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'Result := executeUpdate(qry);' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.Free;' + CRLF;
          deleteByFunc := deleteByFunc + 'end;' + CRLF2;
        end;
        readRow := readRow + TAB + getVarName(tableName) + '.' + getVarNameWithS(thisField) + ' := dataset.FieldByName(''' + thisField + ''').' + asType + ';' + CRLF;
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
          template := TTemplate.Create(TEMPLATE_PATH + 'dao_with_complex_pk.tpl');
        end;
      end
      else
      begin
        template := TTemplate.Create(TEMPLATE_PATH + 'dao_view.tpl');
      end;
      template.setPair('dao_class_name', 'T' + tableClassName);
      template.setPair('domain_class_name', getDTOName(tableName));
      template.setPair('table_name', tableName);
      template.setPair('var_name', getVarName(tableName));
      insertFields := Copy(insertFields, 1, Length(insertFields) - 1);
      updateFields := Copy(updateFields, 1, Length(updateFields) - 1);
      insertValues := Copy(insertValues, 1, Length(insertValues) - 1);
      if (hasPK) then
      begin
        template.setPair('pk', pk);
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
          s := s + '$' + getVarNameWithS(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + getVarNameWithS(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + getVarName(tableName) + '->' + getVarNameWithS(pks[i]) + ');';
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
        insertFields := Copy(insertFields, 1, Length(insertFields) - 1);
        insertFields := concatLongString(insertFields, True);
        updateFields := Copy(updateFields, 1, Length(updateFields) - 1);
        updateFields := concatLongString(updateFields, True);
        insertValues := Copy(insertValues, 1, Length(insertValues) - 1);
        insertValues := concatLongString(insertValues, True);
        template.setPair('insert_values2', insertValues2);
        template.setPair('insert_fields2', insertFields2);
        template.setPair('pk_set_update', s4);
        template.setPair('pk_set', s3);
        template.setPair('pk_where', s2);
        template.setPair('pks', s);
        template.setPair('pk_with_s', getVarNameWithS(pk));
        template.setPair('insert_fields', insertFields);
        template.setPair('update_fields', updateFields);
        template.setPair('insert_values', insertValues);
        template.setPair('parameter_setter', parameterSetter);
        template.setPair('delete_by_definitions', deleteByDef);
        template.setPair('delete_by_functions', deleteByFunc);
      end;
      usesList := TAB + tableName + ',' + CRLF + TAB + tableName + '_dao,';
      typeName := 'T' + tableClassName + 'MySQLDAO';
      interfaceName := 'I' + tableClassName + 'DAO';
      template.setPair('unit_name', tableName);
      template.setPair('uses_list', usesList);
      template.setPair('type_name', typeName);
      template.setPair('interface_name', interfaceName);
      template.setPair('read_row', readRow);
      template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.setPair('query_by_definitions', queryByDef);
      template.setPair('query_by_functions', queryByFunc);
      template.write('' + path + '\class\mysql\' + tableName + '_mysql_dao.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.generateIDAOObjects(const dataSet: TClientDataSet; const path: string);
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
  with (dataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.getDatabase).AsString;
      Write('Generating ' + '"' + path + '\class\dao\' + tableName + '_dao.pas"...');
      hasPK := doesTableContainPK(tableName);
      tableClassName := getClazzName(tableName);
      ds := getFields(tableName);
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
        delphiType := getDelphiType(FieldByName('Type').AsString);
        asType := getDelphiAsType(FieldByName('Type').AsString);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := thisField;
          SetLength(pks, Length(pks) + 1);
          pks[Length(pks) - 1] := thisField;
        end
        else
        begin
          insertFields := insertFields + thisField + ', ';
          updateFields := updateFields + thisField + ' = :' + getVarNameWithS(thisField) + ', ';
          insertValues := insertValues + ':' + getVarNameWithS(thisField) + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + getVarNameWithS(thisField) + ''').Value := ' + tableName + '.' + thisField + ';' + CRLF;
          queryByDef := queryByDef + TAB2 + 'function queryBy' + getClazzName(thisField) + '(const value: ' + delphiType + '): TList<T' + tableClassName + '>;' + CRLF;
          deleteByDef := deleteByDef + TAB2 + 'function deleteBy' + getClazzName(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
        end;
        readRow := readRow + TAB + getVarName(tableName) + '.' + getVarNameWithS(thisField) + ' := dataset.FieldByName(''' + thisField + ''').' + asType + ';' + CRLF;
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
          template := TTemplate.Create(TEMPLATE_PATH + 'idao_with_complex_pk.tpl');
        end;
      end
      else
      begin
        template := TTemplate.Create(TEMPLATE_PATH + 'idao_view.tpl');
      end;
      template.setPair('dao_class_name', 'T' + tableClassName);
      template.setPair('table_name', tableName);
      template.setPair('var_name', getVarName(tableName));
      if (hasPK) then
      begin
        insertFields := Copy(insertFields, 1, Length(insertFields) - 1);
        updateFields := Copy(updateFields, 1, Length(updateFields) - 1);
        insertValues := Copy(insertValues, 1, Length(insertValues) - 1);
        template.setPair('pk', pk);
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
          s := s + '$' + getVarNameWithS(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + getVarNameWithS(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + getVarName(tableName) + '->' + getVarNameWithS(pks[i]) + ');';
          s4 := s4 + CRLF;
        end;
        template.setPair('insert_values2', insertValues2);
        template.setPair('insert_fields2', insertFields2);
        template.setPair('pk_set_update', s4);
        template.setPair('pk_set', s3);
        template.setPair('pk_where', s2);
        template.setPair('pks', s);
        insertFields := Copy(insertFields, 1, Length(insertFields) - 1);
        insertFields := concatLongString(insertFields, True);
        updateFields := Copy(updateFields, 1, Length(updateFields) - 1);
        updateFields := concatLongString(updateFields, True);
        insertValues := Copy(insertValues, 1, Length(insertValues) - 1);
        insertValues := concatLongString(insertValues, True);
        template.setPair('insert_fields', insertFields);
        template.setPair('update_fields', updateFields);
        template.setPair('insert_values', insertValues);
        template.setPair('parameter_setter', parameterSetter);
        template.setPair('delete_by_definitions', deleteByDef);
      end;
      usesList := TAB + tableName + ',';
      typeName := 'I' + tableClassName + 'DAO';
      template.setPair('unit_name', tableName);
      template.setPair('uses_list', usesList);
      template.setPair('type_name', typeName);
      template.setPair('read_row', readRow);
      template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.setPair('query_by_definitions', queryByDef);
      template.write('' + path + '\class\dao\' + tableName + '_dao.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.generateStoredRoutines(const path: string);
var
  routineName, createSQL,
  usesList, functionDeclarations, implementationCode: string;
  template: TTemplate;
  qry: TTBGQuery;
  ds, ds2: TClientDataSet;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + path + '\class\dao\dao_factory.pas"...');
  qry := TTBGQuery.Create;
  qry.sql.Add('SHOW PROCEDURE STATUS');
  ds := TQueryExecutor.execute(qry);
  qry.Free;
  with (ds) do
  begin
    First;
    while (not Eof) do
    begin
      routineName := FieldByName('Name').AsString;
      qry := TTBGQuery.Create;
      qry.sql.Add('SHOW CREATE PROCEDURE ' + routineName);
      ds2 := TQueryExecutor.execute(qry);
      qry.Free;
      createSQL := ds2.FieldByName('Create Procedure').AsString;
      tableClassName := getClazzName(tableName);
      usesList := usesList + TAB + tableName + '_mysql_ext_dao,' + CRLF;
      functionDeclarations := functionDeclarations + TAB2 + 'class function get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'class function TDAOFactory.get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := T' + tableClassName + 'MySQLExtDAO.Create;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      Next;
    end;
    usesList := LeftStr(usesList, Length(usesList) - 3) + ';';
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(TEMPLATE_PATH + 'dao_factory.tpl');
    template.setPair('uses_list', usesList);
    template.setPair('function_declarations', functionDeclarations);
    template.setPair('implementation_code', implementationCode);
    template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.write('' + path + '\class\dao\dao_factory.pas');
    template.Free;
    WriteLn(' done.');
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class function TGenerator.getFields(const tableName: string): TClientDataSet;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.sql.Add('DESC ' + tableName);
  Result := TQueryExecutor.execute(qry);
  qry.Free;
end;

class function TGenerator.getClazzName(const tableName: string): string;
var
  i: Integer;
  tableClassName: string;
begin
  tableClassName := UpperCase(tableName[1]) + Copy(tableName, 2, MaxInt);
  for i := 1 to Length(tableClassName) do
  begin
    if (tableClassName[i] = '_') then
    begin
      tableClassName := Copy(tableClassName, 1, i) + UpperCase(tableClassName[i + 1]) + Copy(tableClassName, i + 2, MaxInt);
    end;
  end;
  Result := tableClassName;
end;

class function TGenerator.getDTOName(const tableName: string): string;
var
  dtoName: string;
begin
  dtoName := getClazzName(tableName);
  if (dtoName[Length(dtoName)] = 's') then
  begin
    dtoName := Copy(dtoName, 1, Length(dtoName));
  end;
  Result := dtoName;
end;

class function TGenerator.getVarName(const tableName: string): string;
var
  i: Integer;
  varName: string;
begin
  varName := LowerCase(tableName[1]) + Copy(tableName, 2, MaxInt);
  for i := 1 to Length(varName) do
  begin
    if (varName[i] = '_') then
    begin
      varName := Copy(varName, 1, i) + UpperCase(varName[i + 1]) + Copy(varName, i + 2, MaxInt);
    end;
  end;
  if (varName[Length(varName)] = 's') then
  begin
    varName := Copy(varName, 1, Length(varName) - 2);
  end;
  Result := varName;
end;

class function TGenerator.getVarNameWithS(const tableName: string): string;
var
  i: Integer;
  varName: string;
begin
  varName := LowerCase(tableName[1]) + Copy(tableName, 2, MaxInt);
  for i := 1 to Length(varName) do
  begin
    if (varName[i] = '_') then
    begin
      varName := Copy(varName, 1, i) + UpperCase(varName[i + 1]) + Copy(varName, i + 2, MaxInt);
    end;
  end;
  Result := varName;
end;

class function TGenerator.getDelphiType(const sqlType: string): string;
var
  delphiType: string;
begin
  delphiType := '';
  if (Pos('tinyint(1)', sqlType) > 0) then
  begin
    delphiType := 'Boolean';
  end
  else if (Pos('int(', sqlType) > 0) then
  begin
    delphiType := 'Integer';
  end;
  if ((Pos('float', sqlType) > 0) or (Pos('double', sqlType) > 0)) then
  begin
    delphiType := 'Extended';
  end;
  if ((Pos('varchar', sqlType) > 0) or (Pos('text', sqlType) > 0) or (Pos('enum', sqlType) > 0)) then
  begin
    delphiType := 'string';
  end;
  if ((Pos('datetime', sqlType) > 0) or (Pos('timestamp', sqlType) > 0)) then
  begin
    delphiType := 'TDateTime';
  end
  else
  if (Pos('date', sqlType) > 0) then
  begin
    delphiType := 'TDate';
  end
  else
  if (Pos('date', sqlType) > 0) then
  begin
    delphiType := 'TTime';
  end;
  if (Pos('blob', sqlType) > 0) then
  begin
    delphiType := 'Variant';
  end;
  Result := delphiType;
end;

class function TGenerator.getDelphiAsType(const sqlType: string): string;
var
  asType: string;
begin
  asType := '';
  if (Pos('tinyint(1)', sqlType) > 0) then
  begin
    asType := 'AsBoolean';
  end
  else if (Pos('int(', sqlType) > 0) then
  begin
    asType := 'AsInteger';
  end;
  if ((Pos('float', sqlType) > 0) or (Pos('double', sqlType) > 0)) then
  begin
    asType := 'AsExtended';
  end;
  if ((Pos('varchar', sqlType) > 0) or (Pos('text', sqlType) > 0) or (Pos('enum', sqlType) > 0)) then
  begin
    asType := 'AsString';
  end;
  if ((Pos('date', sqlType) > 0) or (Pos('time', sqlType) > 0)) then
  begin
    asType := 'AsDateTime';
  end;
  if (Pos('blob', sqlType) > 0) then
  begin
    asType := 'AsVariant';
  end;
  Result := asType;
end;

class function TGenerator.concatLongString(const inStr: string; const multiline: Boolean): string;
const
  BREAK_COUNT = 150;
var
  outStr, delim: string;
begin
  outStr := inStr;
  if (Length(inStr) > BREAK_COUNT) then
  begin
    if (multiline) then
    begin
      delim := ''' + ' + CRLF + TAB2 + '''';
    end
    else
    begin
      delim := ''' + ''';
    end;
    outStr := Copy(inStr, 1, BREAK_COUNT) + delim + concatLongString(Copy(inStr, BREAK_COUNT + 1, MaxInt), multiline);
  end;
  Result := outStr;
end;

end.
