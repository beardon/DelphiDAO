unit generator;

interface

uses
  DBClient,
  Generics.Collections;

type
  TRoutineParameter = record
    direction: string;
    varName: string;
    sqlType: string;
  end;
  TGenerator = class
  private
    class procedure init(path: string);
    class function doesTableContainPK(const tableName: string): Boolean;
    class procedure createDAOFactory(const dataset: TClientDataSet; const path: string);
    class procedure generateDTOObjects(const dataset: TClientDataSet; const path: string);
    class procedure generateDAOExtObjects(const dataset: TClientDataSet; const path: string);
    class procedure generateDAOObjects(const dataSet: TClientDataSet; const path: string);
    class procedure generateIDAOObjects(const dataset: TClientDataSet; const path: string);
    class procedure generateStoredRoutines(const path: string);
    class function getFields(const tableName: string): TClientDataSet;
    class function lowerCamelCase(const value: string): string;
    class function upperCamelCase(const value: string): string;
//    class function getVarNameWithS(const tableName: string): string;
    class function getDelphiType(const sqlType: string): string;
    class function getDelphiAsType(const sqlType: string): string;
    class function fixReservedWords(const value: string): string;
    class function getRoutineParameters(const createSQL: string; const isFunction: Boolean): TList<TRoutineParameter>;
    class function getRoutineReturnType(const createSQL: string): string;
    class function concatLongString(const inStr: string; const multiline: Boolean): string;
  public
    class procedure generate(path: string); static;
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
  generateDTOObjects(ds, path);
	generateDAOObjects(ds, path);
	generateDAOExtObjects(ds, path);
	generateIDAOObjects(ds, path);
	createDAOFactory(ds, path);
  generateStoredRoutines(path);
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
      tableClassName := upperCamelCase(tableName);
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

class procedure TGenerator.generateDTOObjects(const dataset: TClientDataSet; const path: string);
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
  with (dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.getDatabase).AsString;
      Write('Generating ' + '"' + path + '\class\dto\' + tableName + '.pas"...');
      tableClassName := upperCamelCase(tableName);
      template := TTemplate.Create(TEMPLATE_PATH + 'dto.tpl');
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
        privateVars := privateVars + TAB2 + 'f' + upperCamelCase(thisField) + ': ' + thisType + ';' + CRLF;
        publicProperties := publicProperties + TAB2 + 'property ' + upperCamelCase(thisField) + ': ' + thisType + ' read f' + upperCamelCase(thisField) + ' write f' + upperCamelCase(thisField) + ';' + CRLF;
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
        tableClassName := upperCamelCase(tableName);
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
      tableClassName := upperCamelCase(tableName);
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
          updateFields := updateFields + thisField + ' = :' + upperCamelCase(thisField) + ', ';
          insertValues := insertValues + ':' + upperCamelCase(thisField) + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + upperCamelCase(thisField) + ''').Value := ' + tableName + '.' + upperCamelCase(thisField) + ';' + CRLF;
          queryByDef := queryByDef + TAB2 + 'function queryBy' + upperCamelCase(thisField) + '(const value: ' + delphiType + '): TList<T' + tableClassName + '>;' + CRLF;
          queryByFunc := queryByFunc + 'function T' + tableClassName + 'MySQLDAO.queryBy' + upperCamelCase(thisField) + '(const value: ' + delphiType + '): TList<T' + tableClassName + '>;' + CRLF;
          queryByFunc := queryByFunc + 'var' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry: TTBGQuery;' + CRLF;
          queryByFunc := queryByFunc + 'begin' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry := TTBGQuery.Create;' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.sql.Add(''SELECT * FROM ' + tableName + ' WHERE ' + thisField + ' = :' + upperCamelCase(thisField) + ''');' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.ParamByName(''' + upperCamelCase(thisField) + ''').Value := value;' + CRLF;
          queryByFunc := queryByFunc + TAB + 'Result := getList(qry);' + CRLF;
          queryByFunc := queryByFunc + TAB + 'qry.Free;' + CRLF;
          queryByFunc := queryByFunc + 'end;' + CRLF2;
          deleteByDef := deleteByDef + TAB2 + 'function deleteBy' + upperCamelCase(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
          deleteByFunc := deleteByFunc + 'function T' + tableClassName + 'MySQLDAO.deleteBy' + upperCamelCase(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
          deleteByFunc := deleteByFunc + 'var' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry: TTBGQuery;' + CRLF;
          deleteByFunc := deleteByFunc + 'begin' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry := TTBGQuery.Create;' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.sql.Add(''DELETE FROM ' + tableName + ' WHERE ' + thisField + ' = :' + upperCamelCase(thisField) + ''');' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.ParamByName(''' + upperCamelCase(thisField) + ''').Value := value;' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'Result := executeUpdate(qry);' + CRLF;
          deleteByFunc := deleteByFunc + TAB + 'qry.Free;' + CRLF;
          deleteByFunc := deleteByFunc + 'end;' + CRLF2;
        end;
        readRow := readRow + TAB + tableClassName + '.' + upperCamelCase(thisField) + ' := dataset.FieldByName(''' + thisField + ''').' + asType + ';' + CRLF;
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
      template.setPair('dao_class_name', 'T' + tableClassName);
      template.setPair('table_name', tableName);
      template.setPair('var_name', upperCamelCase(tableName));
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
          s := s + '$' + upperCamelCase(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + upperCamelCase(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + upperCamelCase(tableName) + '->' + upperCamelCase(pks[i]) + ');';
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
        template.setPair('pk_with_s', upperCamelCase(pk));
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
      tableClassName := upperCamelCase(tableName);
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
          updateFields := updateFields + thisField + ' = :' + upperCamelCase(thisField) + ', ';
          insertValues := insertValues + ':' + upperCamelCase(thisField) + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + upperCamelCase(thisField) + ''').Value := ' + tableName + '.' + thisField + ';' + CRLF;
          queryByDef := queryByDef + TAB2 + 'function queryBy' + upperCamelCase(thisField) + '(const value: ' + delphiType + '): TList<T' + tableClassName + '>;' + CRLF;
          deleteByDef := deleteByDef + TAB2 + 'function deleteBy' + upperCamelCase(thisField) + '(const value: ' + delphiType + '): Integer;' + CRLF;
        end;
        readRow := readRow + TAB + tableClassName + '.' + upperCamelCase(thisField) + ' := dataset.FieldByName(''' + thisField + ''').' + asType + ';' + CRLF;
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
      template.setPair('dao_class_name', 'T' + tableClassName);
      template.setPair('table_name', tableName);
      template.setPair('var_name', upperCamelCase(tableName));
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
          s := s + '$' + upperCamelCase(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + upperCamelCase(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + upperCamelCase(tableName) + '->' + upperCamelCase(pks[i]) + ');';
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
  Write('Generating ' + '"' + path + '\class\stored_routines.pas"...');
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
      comment := FieldByName('Comment').AsString;
      qry := TTBGQuery.Create;
      qry.sql.Add('SHOW CREATE PROCEDURE ' + routineName);
      createSQL := TQueryExecutor.queryForString(qry, 'Create Procedure');
      qry.Free;
      paramRecs := getRoutineParameters(createSQL, False);
      funcParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        funcParams := funcParams + 'const ' + lowerCamelCase(paramRec.varName) + ': ' + getDelphiType(paramRec.sqlType) + '; ';
        sqlParams := sqlParams + ':' + paramRec.varName + ', ';
      end;
      if (funcParams <> '') then
      begin
        funcParams := Copy(funcParams, 1, Length(funcParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      functionDeclarations := functionDeclarations + TAB2 + 'class procedure ' + lowerCamelCase(routineName) + '(' + funcParams + ');' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + getDelphiType(paramRec.sqlType) + ' ' + lowerCamelCase(paramRec.varName) + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class procedure TStoredRoutines.' + lowerCamelCase(routineName) + '(' + funcParams + ');' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.sql.Add(''CALL ' + routineName + '(' + sqlParams + ')'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.varName + ''').Value := ' + lowerCamelCase(paramRec.varName) + ';' + CRLF;
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
  qry.sql.Add('SHOW FUNCTION STATUS');
  ds := TQueryExecutor.execute(qry);
  qry.Free;
  with (ds) do
  begin
    First;
    while (not Eof) do
    begin
      routineName := FieldByName('Name').AsString;
      qry := TTBGQuery.Create;
      qry.sql.Add('SHOW CREATE FUNCTION ' + routineName);
      createSQL := TQueryExecutor.queryForString(qry, 'Create Function');
      qry.Free;
      paramRecs := getRoutineParameters(createSQL, True);
      funcParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        funcParams := funcParams + 'const ' + lowerCamelCase(paramRec.varName) + ': ' + getDelphiType(paramRec.sqlType) + '; ';
        sqlParams := sqlParams + ':' + paramRec.varName + ', ';
      end;
      if (funcParams <> '') then
      begin
        funcParams := Copy(funcParams, 1, Length(funcParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      sqlReturnType := getRoutineReturnType(createSQL);
      delphiReturnType := getDelphiType(sqlReturnType);
      functionDeclarations := functionDeclarations + TAB2 + 'class function ' + lowerCamelCase(routineName) + '(' + funcParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + getDelphiType(paramRec.sqlType) + ' ' + lowerCamelCase(paramRec.varName) + CRLF;
      implementationCode := implementationCode + ' * @return ' + delphiReturnType + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class function TStoredRoutines.' + lowerCamelCase(routineName) + '(' + funcParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.sql.Add(''SELECT ' + routineName + '(' + sqlParams + ') AS value'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.varName + ''').Value := ' + lowerCamelCase(paramRec.varName) + ';' + CRLF;
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
    template.setPair('function_declarations', functionDeclarations);
    template.setPair('implementation_code', implementationCode);
    template.setPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.write('' + path + '\class\stored_routines.pas');
    template.Free;
    WriteLn(' done.');
  end;
  ds.Free;
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

class function TGenerator.lowerCamelCase(const value: string): string;
var
  cameled: string;
begin
  cameled := upperCamelCase(value);
  Result := LowerCase(cameled[1]) + Copy(cameled, 2, MaxInt);
end;

class function TGenerator.upperCamelCase(const value: string): string;
var
  i: Integer;
  cameled: string;
  humps: TStringList;
begin
  humps := TStringList.Create;
  humps.Delimiter := '_';
  humps.DelimitedText := value;
  for i := 0 to humps.Count - 1 do
  begin
    cameled := cameled + UpperCase(humps[i][1]) + Copy(humps[i], 2, MaxInt);
  end;
  Result := fixReservedWords(cameled);
end;

class function TGenerator.getDelphiType(const sqlType: string): string;
var
  delphiType, sqlTypeL: string;
begin
  delphiType := '';
  sqlTypeL := LowerCase(sqlType);
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

class function TGenerator.getDelphiAsType(const sqlType: string): string;
var
  asType, sqlTypeL: string;
begin
  asType := '';
  sqlTypeL := LowerCase(sqlType);
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

class function TGenerator.fixReservedWords(const value: string): string;
const
  RESERVED_WORDS = 'Type';
var
  corrected: string;
begin
  corrected := value;
  if (Pos(value, RESERVED_WORDS) > 0) then
  begin
    corrected := value + '_';
  end;
  Result := corrected;
end;

class function TGenerator.getRoutineParameters(const createSQL: string; const isFunction: Boolean): TList<TRoutineParameter>;
var
  i: Integer;
  paramsStr: string;
  params: TStringList;
  param: TStringList;
  paramRec: TRoutineParameter;
  paramList: TList<TRoutineParameter>;
begin
  paramsStr := Copy(createSQL, Pos('(', createSQL) + 1, Pos(')', createSQL) - Pos('(', createSQL) - 1);
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
    if (not isFunction) then
    begin
      paramRec.direction := param[0];
      paramRec.varName := param[1];
      paramRec.sqlType := param[2];
    end
    else
    begin
      paramRec.varName := param[0];
      paramRec.sqlType := param[1];
    end;
    paramList.Add(paramRec);
    param.Free;
  end;
  params.Free;
  Result := paramList;
end;

class function TGenerator.getRoutineReturnType(const createSQL: string): string;
var
  returnTypeStr: string;
begin
  returnTypeStr := Copy(createSQL, Pos('RETURNS', createSQL));
  returnTypeStr := Copy(returnTypeStr, 1, Pos('BEGIN', returnTypeStr));
  returnTypeStr := Trim(returnTypeStr);
  returnTypeStr := StringReplace(returnTypeStr, #9, '', [rfReplaceAll]);
  returnTypeStr := StringReplace(returnTypeStr, #$A, '', [rfReplaceAll]);
  Result := returnTypeStr;
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
