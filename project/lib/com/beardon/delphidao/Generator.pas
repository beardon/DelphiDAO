unit Generator;

interface

uses
  Classes,
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
    class procedure Init(OutputPath, TemplatePath: string);
    class function CreateDeleteByDefinition(const FieldName, DelphiType: string): string;
    class function CreateDeleteByFunction(const TableName, FieldName, DelphiType: string): string;
    class function CreateQueryByDefinitions(const TableName, FieldName, DelphiType: string; const ShowDefaults: Boolean; const PrimaryKeyIndex: string = ''): string;
    class function CreateQueryByFunctions(const TableName, FieldName, DelphiType, PrimaryKeyIndex: string): string;
    class function DoesTableContainPK(const TableName: string): Boolean;
    class procedure GenerateDTOObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
    class procedure GenerateDAOExtObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
    class procedure GenerateDAOFactory(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
    class procedure GenerateDAOObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
    class procedure GenerateIDAOObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
    class procedure GenerateStoredRoutines(const OutputPath, TemplatePath: string);
    class function GetFields(const TableName: string): TClientDataSet;
    class function GetIndices(const TableName: string): TStringList;
    class function GetRoutineParameters(const CreateSQL: string; const IsFunction: Boolean): TList<TRoutineParameter>;
    class function GetRoutineReturnType(const CreateSQL: string): string;
  public
    class procedure Generate(OutputPath, TemplatePath: string); static;
  end;

implementation

uses
  ConnectionProperty,
  Delphinator,
  Inflector,
  Query,
  QueryExecutor,
  StrUtils,
  SysUtils,
  Template,
  Windows;

const
  CRLF = #13#10;
  CRLF2 = CRLF + CRLF;
  TAB = '  ';
  TAB2 = TAB + TAB;

class procedure TGenerator.Generate(OutputPath, TemplatePath: string);
var
  qry: TTBGQuery;
  ds: TClientDataSet;
begin
  Init(OutputPath, TemplatePath);
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW TABLES');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  GenerateDTOObjects(ds, OutputPath, TemplatePath);
	GenerateDAOObjects(ds, OutputPath, TemplatePath);
	GenerateDAOExtObjects(ds, OutputPath, TemplatePath);
	GenerateIDAOObjects(ds, OutputPath, TemplatePath);
	GenerateDAOFactory(ds, OutputPath, TemplatePath);
  GenerateStoredRoutines(OutputPath, TemplatePath);
  ds.Free;
end;

class procedure TGenerator.Init(OutputPath, TemplatePath: string);
begin
	CreateDir(OutputPath);
	CreateDir(OutputPath + '\class');
	CreateDir(OutputPath + '\class\dto');
	CreateDir(OutputPath + '\class\mysql');
	CreateDir(OutputPath + '\class\mysql\ext');
	CreateDir(OutputPath + '\class\sql');
	CreateDir(OutputPath + '\class\dao');
	CreateDir(OutputPath + '\class\core');
  CopyFile(PChar(TemplatePath + '\class\dao\core\ArrayList.pas'), PChar(OutputPath + '\class\core\ArrayList.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\Connection.pas'), PChar(OutputPath + '\class\sql\Connection.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\ConnectionFactory.pas'), PChar(OutputPath + '\class\sql\ConnectionFactory.pas'), False);
  // do not overwrite connection properties if they already exist
  if (not FileExists(OutputPath + '\class\sql\ConnectionProperty.pas')) then
  begin
    CopyFile(PChar(TemplatePath + '\ConnectionProperty.tpl'), PChar(OutputPath + '\class\sql\ConnectionProperty.pas'), False);
  end;
  CopyFile(PChar(TemplatePath + '\class\dao\sql\Query.pas'), PChar(OutputPath + '\class\sql\Query.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\QueryExecutor.pas'), PChar(OutputPath + '\class\sql\QueryExecutor.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\QueryFactory.pas'), PChar(OutputPath + '\class\sql\QueryFactory.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\SQLComparisonOperator.pas'), PChar(OutputPath + '\class\sql\SQLComparisonOperator.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\SQLOrderDirection.pas'), PChar(OutputPath + '\class\sql\SQLOrderDirection.pas'), False);
  CopyFile(PChar(TemplatePath + '\class\dao\sql\Transaction.pas'), PChar(OutputPath + '\class\sql\Transaction.pas'), False);
end;

class function TGenerator.CreateDeleteByDefinition(const FieldName, DelphiType: string): string;
var
  code: string;
  fieldMemberName: string;
begin
  fieldMemberName := TInflector.Memberify(FieldName);
  code := TAB2 + 'function DeleteBy' + fieldMemberName + '(const Value: ' + DelphiType + '): Integer;' + CRLF;
  Result := code;
end;

class function TGenerator.CreateDeleteByFunction(const TableName, FieldName, DelphiType: string): string;
var
  code, appendedDefault: string;
  fieldMemberName, tableClassName: string;
begin
  tableClassName := TInflector.Classify(TableName);
  fieldMemberName := TInflector.Memberify(FieldName);
  appendedDefault := '';
  code := 'function T' + tableClassName + 'MySQLDAO.DeleteBy' + fieldMemberName + '(const Value: ' + DelphiType + '): Integer;' + CRLF;
  code := code + 'var' + CRLF;
  code := code + TAB + 'qry: TTBGQuery;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'qry := TTBGQuery.Create;' + CRLF;
  code := code + TAB + 'qry.sql.Add(''DELETE FROM ' + TableName + ' WHERE ' + FieldName + ' = :' + fieldMemberName + ''');' + CRLF;
  code := code + TAB + 'qry.ParamByName(''' + fieldMemberName + ''').Value := Value;' + CRLF;
  code := code + TAB + 'Result := ExecuteUpdate(qry);' + CRLF;
  code := code + TAB + 'qry.Free;' + CRLF;
  code := code + 'end;' + CRLF2;
  Result := code;
end;

class function TGenerator.CreateQueryByDefinitions(const TableName, FieldName, DelphiType: string; const ShowDefaults: Boolean; const PrimaryKeyIndex: string = ''): string;
var
  code, pkIndexConstant: string;
  appendedComparisonOperatorDefault, appendedOrderClauseDefault, appendedOrderIndexDefault, appendedOrderDirectionDefault: string;
  fieldMemberName, tableClassName: string;
begin
  tableClassName := TInflector.Classify(TableName);
  fieldMemberName := TInflector.Memberify(FieldName);
  pkIndexConstant := 'INDEX_' + UpperCase(PrimaryKeyIndex);
  appendedComparisonOperatorDefault := '';
  appendedOrderClauseDefault := '';
  appendedOrderIndexDefault := '';
  appendedOrderDirectionDefault := '';
  if (ShowDefaults) then
  begin
    appendedComparisonOperatorDefault := ' = TSQLComparisonOperator.EQUAL';
    appendedOrderClauseDefault := ' = ''''';
    appendedOrderIndexDefault := ' = ' + pkIndexConstant;
    appendedOrderDirectionDefault := ' = TSQLOrderDirection.ASCENDING';
  end;
  code := TAB2 + 'function QueryBy' + fieldMemberName + '(const Value: ' + DelphiType + '; const ComparisonOperator: Integer' + appendedComparisonOperatorDefault + '): TObjectList<T' + tableClassName + '>;' + CRLF;
  code := code + TAB2 + 'function QueryBy' + fieldMemberName + 'OrderBy(const Value: ' + DelphiType + '; const ComparisonOperator: Integer' + appendedComparisonOperatorDefault + '; const OrderClause: string' + appendedOrderClauseDefault + '): TObjectList<T' + tableClassName + '>;' + CRLF;
  code := code + TAB2 + 'function QueryBy' + fieldMemberName + 'OrderByIndex(const Value: ' + DelphiType + '; const ComparisonOperator: Integer' + appendedComparisonOperatorDefault + '; const OrderIndex: Integer' + appendedOrderIndexDefault + '; OrderDirection: Integer' + appendedOrderDirectionDefault + '): TObjectList<T' + tableClassName + '>;' + CRLF;
  Result := code;
end;

class function TGenerator.CreateQueryByFunctions(const TableName, FieldName, DelphiType, PrimaryKeyIndex: string): string;
var
  code, params, pkIndexConstant: string;
  fieldMemberName, tableClassName: string;
begin
  tableClassName := TInflector.Classify(TableName);
  fieldMemberName := TInflector.Memberify(FieldName);
  pkIndexConstant := 'INDEX_' + UpperCase(PrimaryKeyIndex);
  code := 'function T' + tableClassName + 'MySQLDAO.QueryBy' + fieldMemberName + '(const Value: ' + DelphiType + '; const ComparisonOperator: Integer = TSQLComparisonOperator.EQUAL): TObjectList<T' + tableClassName + '>;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'Result := QueryBy' + fieldMemberName + 'OrderByIndex(Value, ComparisonOperator, ' + pkIndexConstant + ', TSQLOrderDirection.ASCENDING);' + CRLF;
  code := code + 'end;' + CRLF2;
  code := code + 'function T' + tableClassName + 'MySQLDAO.QueryBy' + fieldMemberName + 'OrderBy(const Value: ' + DelphiType + '; const ComparisonOperator: Integer = TSQLComparisonOperator.EQUAL; const OrderClause: string = ''''): TObjectList<T' + tableClassName + '>;' + CRLF;
  code := code + 'var' + CRLF;
  code := code + TAB + 'qry: TTBGQuery;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'qry := TTBGQuery.Create;' + CRLF;
  code := code + TAB + 'qry.sql.Add(''SELECT * FROM ' + TableName + ''');' + CRLF;
  code := code + TAB + 'qry.sql.Add(''WHERE ' + FieldName + ' '' + TSQLComparisonOperator.INDEX_OPERATOR_MAP[ComparisonOperator] + '' :' + fieldMemberName + ''');' + CRLF;
  code := code + TAB + 'if (OrderClause <> '''') then' + CRLF;
  code := code + TAB2 + 'qry.sql.Add(''ORDER BY '' + OrderClause);' + CRLF;
  params := TAB + 'qry.ParamByName(''' + fieldMemberName + ''').Value := Value;' + CRLF;
  if (DelphiType = 'string') then
  begin
    params := TAB + 'if (ComparisonOperator = TSQLComparisonOperator.LIKE) then' + CRLF;
    params := params + TAB2 + 'qry.ParamByName(''' + fieldMemberName + ''').Value := ''%'' + Value + ''%''' + CRLF;
    params := params + TAB + 'else' + CRLF;
    params := params + TAB2 + 'qry.ParamByName(''' + fieldMemberName + ''').Value := Value;' + CRLF;
  end;
  code := code + params;
  code := code + TAB + 'Result := getList(qry);' + CRLF;
  code := code + TAB + 'qry.Free;' + CRLF;
  code := code + 'end;' + CRLF2;
  code := code + 'function T' + tableClassName + 'MySQLDAO.QueryBy' + fieldMemberName + 'OrderByIndex(const Value: ' + DelphiType + '; const ComparisonOperator: Integer = TSQLComparisonOperator.EQUAL; const OrderIndex: Integer = ' + pkIndexConstant + '; OrderDirection: Integer = TSQLOrderDirection.ASCENDING): TObjectList<T' + tableClassName + '>;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'Result := QueryBy' + fieldMemberName + 'OrderBy(Value, ComparisonOperator, INDEX_FIELD_MAP[OrderIndex] + '' '' + TSQLOrderDirection.INDEX_DIRECTION_MAP[OrderDirection]);' + CRLF;
  code := code + 'end;' + CRLF2;
  Result := code;
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
      Break;
    end;
    Next;
  end;
  ds.Free;
  Result := success;
end;

class procedure TGenerator.GenerateDTOObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
var
  tableName, tableClassName,
  typeName, pointerTypeName, privateVars, publicConstants, publicProperties,
  fieldName, fieldMemberName, thisType: string;
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
      tableClassName := TInflector.Classify(tableName);
      Write('Generating ' + '"' + OutputPath + '\class\dto\' + tableClassName + '.pas"...');
      template := TTemplate.Create(TemplatePath + '\DTO.tpl');
      template.SetPair('unit_name', tableClassName);
      template.SetPair('table_name', tableName);
      typeName := 'T' + tableClassName;
      pointerTypeName := 'P' + tableClassName;
      template.SetPair('type_name', typeName);
      template.SetPair('pointer_type_name', pointerTypeName);
      publicConstants := TAB2 + 'const TABLE_NAME = ''' + tableName + ''';';
      template.SetPair('public_constants', publicConstants);
      privateVars := '';
      publicProperties := '';
      ds := GetFields(tableName);
      with (ds) do
      while (not Eof) do
      begin
        fieldName := FieldByName('Field').AsString;
        fieldMemberName := TInflector.Memberify(fieldName);
        thisType := TDelphinator.MySQLTypeToDelphiType(FieldByName('Type').AsString);
        privateVars := privateVars + TAB2 + 'f' + fieldMemberName + ': ' + thisType + ';' + CRLF;
        publicProperties := publicProperties + TAB2 + 'property ' + fieldMemberName + ': ' + thisType + ' read f' + fieldMemberName + ' write f' + fieldMemberName + ';' + CRLF;
        Next;
      end;
      ds.Free;
      privateVars := LeftStr(privateVars, Length(privateVars) - 2);
      publicProperties := LeftStr(publicProperties, Length(publicProperties) - 2);
      template.SetPair('private_vars', privateVars);
      template.SetPair('public_properties', publicProperties);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.Write('' + OutputPath + '\class\dto\' + tableClassName + '.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateDAOExtObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
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
      tableClassName := TInflector.Classify(tableName);
      if (not FileExists('' + OutputPath + '\class\mysql\ext\' + tableClassName + 'MySQLExtDAO.pas')) then
      begin
        Write('Generating ' + '"' + OutputPath + '\class\mysql\ext\' + tableClassName + 'MySQLExtDAO.pas"...');
        usesList := TAB + tableClassName + 'MySQLDAO;';
        template := TTemplate.Create(TemplatePath + '\DAOExt.tpl');
        template.SetPair('unit_name', tableClassName);
        template.SetPair('uses_list', usesList);
        template.SetPair('table_name', tableName);
        typeName := 'T' + tableClassName + 'MySQLExtDAO';
        ancestorTypeName := 'T' + tableClassName + 'MySQLDAO';
        template.SetPair('type_name', typeName);
        template.SetPair('ancestor_type_name', ancestorTypeName);
        template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
        template.Write('' + OutputPath + '\class\mysql\ext\' + tableClassName + 'MySQLExtDAO.pas');
        template.Free;
        WriteLn(' done.');
      end
      else
      begin
        WriteLn('"' + OutputPath + '\class\mysql\ext\' + tableName + 'MySQLExtDAO.pas" already exists.');
      end;
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateDAOFactory(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
var
  tableName, tableClassName,
  usesList, functionDeclarations, implementationCode: string;
  template: TTemplate;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + OutputPath + '\class\dao\DAOFactory.pas"...');
  with (Dataset) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableClassName := TInflector.Classify(tableName);
      usesList := usesList + TAB + tableClassName + 'MySQLExtDAO,' + CRLF;
      functionDeclarations := functionDeclarations + TAB2 + 'class function Get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'class function TDAOFactory.Get' + tableClassName + 'DAO: T' + tableClassName + 'MySQLExtDAO;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := T' + tableClassName + 'MySQLExtDAO.Create(fConnection);' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      Next;
    end;
    usesList := LeftStr(usesList, Length(usesList) - 3) + ';';
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(TemplatePath + '\DAOFactory.tpl');
    template.SetPair('uses_list', usesList);
    template.SetPair('function_declarations', functionDeclarations);
    template.SetPair('implementation_code', implementationCode);
    template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.Write('' + OutputPath + '\class\dao\DAOFactory.pas');
    template.Free;
    WriteLn(' done.');
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateDAOObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
var
  i: Integer;
  tableName, tableClassName, usesList, interfaceName,
  typeName, privateVars, publicConstants, publicProperties,
  fieldName, fieldMemberName, delphiType, asType, s, s2, s3, s4: string;
  parameterSetter, insertFields, insertFields2, updateFields, insertValues,
  insertValues2, readRow, pk, queryByDef, deleteByDef,
  mappingArray, indexConstants,
  queryByFunc, deleteByFunc: string;
  pks: array of string;
  template: TTemplate;
  ds: TClientDataSet;
  hasPK: Boolean;
  indices: TStringList;
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
      tableClassName := TInflector.Classify(tableName);
      Write('Generating ' + '"' + OutputPath + '\class\mysql\' + tableClassName + 'MySQLDAO.pas"...');
      hasPK := DoesTableContainPK(tableName);
      ds := GetFields(tableName);
      indices := GetIndices(tableName);
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
        fieldName := FieldByName('Field').AsString;
        fieldMemberName := TInflector.Memberify(fieldName);
        delphiType := TDelphinator.MySQLTypeToDelphiType(FieldByName('Type').AsString);
        asType := TDelphinator.MySQLTypeToDelphiAsType(FieldByName('Type').AsString);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := fieldName;
          SetLength(pks, Length(pks) + 1);
          pks[Length(pks) - 1] := fieldName;
        end
        else
        begin
          insertFields := insertFields + fieldName + ', ';
          updateFields := updateFields + fieldName + ' = :' + fieldMemberName + ', ';
          insertValues := insertValues + ':' + fieldMemberName + ', ';
          parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + fieldMemberName + ''').Value := ' + tableClassName + '.' + fieldMemberName + ';' + CRLF;
          queryByDef := queryByDef + CreateQueryByDefinitions(tableName, fieldName, delphiType, True, indices[0]);
          queryByFunc := queryByFunc + CreateQueryByFunctions(tableName, fieldName, delphiType, indices[0]);
          deleteByDef := deleteByDef + CreateDeleteByDefinition(fieldName, delphiType);
          deleteByFunc := deleteByFunc + CreateDeleteByFunction(tableName, fieldName, delphiType);
        end;
        readRow := readRow + TAB + tableClassName + '.' + fieldMemberName + ' := dataset.FieldByName(''' + fieldName + ''').' + asType + ';' + CRLF;
        Next;
      end;
      ds.Free;
      if (hasPK) then
      begin
        if (Length(pks) = 1) then
        begin
          template := TTemplate.Create(TemplatePath + '\DAO.tpl');
        end
        else
        begin
          template := TTemplate.Create(TemplatePath + '\DAOComplexPK.tpl');
        end;
      end
      else
      begin
        template := TTemplate.Create(TemplatePath + '\DAOView.tpl');
      end;
      mappingArray := 'array[0..' + IntToStr(indices.Count - 1) + '] of string = (''' + StringReplace(indices.DelimitedText, ',', ''',''', [rfReplaceAll]) + ''')';
      indexConstants := '';
      for i := 0 to indices.Count - 1 do
      begin
        indexConstants := indexConstants + TAB2 + 'const INDEX_' + UpperCase(indices[i]) + ' = ' + IntToStr(i) + ';' + CRLF;
      end;
      indices.Free;
      template.SetPair('dao_class_name', 'T' + tableClassName);
      template.SetPair('table_name', tableName);
      template.SetPair('var_name', tableClassName);
      insertFields := LeftStr(insertFields, Length(insertFields) - 1);
      updateFields := LeftStr(updateFields, Length(updateFields) - 1);
      insertValues := LeftStr(insertValues, Length(insertValues) - 1);
      queryByDef := LeftStr(queryByDef, Length(queryByDef) - 2);
      queryByFunc := LeftStr(queryByFunc, Length(queryByFunc) - 2);
      deleteByDef := LeftStr(deleteByDef, Length(deleteByDef) - 2);
      deleteByFunc := LeftStr(deleteByFunc, Length(deleteByFunc) - 2);
      indexConstants := LeftStr(indexConstants, Length(indexConstants) - 2);
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
          s := s + '$' + TInflector.Memberify(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + TInflector.Memberify(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + tableClassName + '->' + TInflector.Memberify(pks[i]) + ');';
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
        insertFields := TDelphinator.ConcatLongString(insertFields, True);
        updateFields := LeftStr(updateFields, Length(updateFields) - 1);
        updateFields := TDelphinator.ConcatLongString(updateFields, True);
        insertValues := LeftStr(insertValues, Length(insertValues) - 1);
        insertValues := TDelphinator.ConcatLongString(insertValues, True);
        template.SetPair('insert_values2', insertValues2);
        template.SetPair('insert_fields2', insertFields2);
        template.SetPair('pk_set_update', s4);
        template.SetPair('pk_set', s3);
        template.SetPair('pk_where', s2);
        template.SetPair('pks', s);
        template.SetPair('pk_with_s', TInflector.Memberify(pk));
        template.SetPair('insert_fields', insertFields);
        template.SetPair('update_fields', updateFields);
        template.SetPair('insert_values', insertValues);
        template.SetPair('parameter_setter', parameterSetter);
        template.SetPair('delete_by_definitions', deleteByDef);
        template.SetPair('delete_by_functions', deleteByFunc);
      end;
      usesList := TAB + tableClassName + ',' + CRLF + TAB + tableClassName + 'DAO,';
      typeName := 'T' + tableClassName + 'MySQLDAO';
      interfaceName := 'I' + tableClassName + 'DAO';
      template.SetPair('unit_name', tableClassName);
      template.SetPair('uses_list', usesList);
      template.SetPair('type_name', typeName);
      template.SetPair('interface_name', interfaceName);
      template.SetPair('read_row', readRow);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.SetPair('query_by_definitions', queryByDef);
      template.SetPair('query_by_functions', queryByFunc);
      template.SetPair('mapping_array', mappingArray);
      template.SetPair('index_constants', indexConstants);
      template.Write('' + OutputPath + '\class\mysql\' + tableClassName + 'MySQLDAO.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

class procedure TGenerator.GenerateIDAOObjects(const Dataset: TClientDataSet; const OutputPath, TemplatePath: string);
var
  i: Integer;
  tableName, tableClassName, usesList,
  typeName, fieldName, fieldMemberName, delphiType, asType: string;
  s, s2, s3, s4: string;
  pk, queryByDef, deleteByDef: string;
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
      tableClassName := TInflector.Classify(tableName);
      Write('Generating ' + '"' + OutputPath + '\class\dao\' + tableClassName + 'DAO.pas"...');
      hasPK := DoesTableContainPK(tableName);
      ds := GetFields(tableName);
      pk := '';
      SetLength(pks, 0);
      queryByDef := '';
      deleteByDef := '';
      with (ds) do
      while (not Eof) do
      begin
        fieldName := FieldByName('Field').AsString;
        fieldMemberName := TInflector.Memberify(fieldName);
        delphiType := TDelphinator.MySQLTypeToDelphiType(FieldByName('Type').AsString);
        asType := TDelphinator.MySQLTypeToDelphiAsType(FieldByName('Type').AsString);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := fieldName;
          SetLength(pks, Length(pks) + 1);
          pks[Length(pks) - 1] := fieldName;
        end
        else
        begin
          queryByDef := queryByDef + CreateQueryByDefinitions(tableName, fieldName, delphiType, False);
          deleteByDef := deleteByDef + TAB2 + 'function DeleteBy' + fieldMemberName + '(const Value: ' + delphiType + '): Integer;' + CRLF;
        end;
        Next;
      end;
      ds.Free;
      if (hasPK) then
      begin
        if (Length(pks) = 1) then
        begin
          template := TTemplate.Create(TemplatePath + '\IDAO.tpl');
        end
        else
        begin
          template := TTemplate.Create(TemplatePath + '\IDAOComplexPK.tpl');
        end;
      end
      else
      begin
        template := TTemplate.Create(TemplatePath + '\IDAOView.tpl');
      end;
      template.SetPair('dao_class_name', 'T' + tableClassName);
      template.SetPair('table_name', tableName);
      template.SetPair('var_name', tableClassName);
      if (hasPK) then
      begin
        template.SetPair('pk', pk);
        s := '';
        s2 := '';
        s3 := '';
        s4 := '';
        for i := 0 to High(pks) do
        begin
          if (i > 0) then
          begin
            s := s + ', ';
            s2 := s2 + ' AND ';
            s3 := s3 + TAB2;
          end;
          s := s + '$' + TInflector.Memberify(pks[i]);
          s2 := s2 + pks[i] + ' = ?';
          s3 := s3 + '$sqlQuery->setNumber($' + TInflector.Memberify(pks[i]) + ');';
          s3 := s3 + CRLF;
          s4 := s4 + CRLF + TAB2;
          s4 := s4 + '$sqlQuery->setNumber($' + tableClassName + '->' + TInflector.Memberify(pks[i]) + ');';
          s4 := s4 + CRLF;
        end;
        template.SetPair('pk_set_update', s4);
        template.SetPair('pk_set', s3);
        template.SetPair('pk_where', s2);
        template.SetPair('pks', s);
        deleteByDef := LeftStr(deleteByDef, Length(deleteByDef) - 2);
        template.SetPair('delete_by_definitions', deleteByDef);
      end;
      usesList := TAB + tableClassName + ',';
      typeName := 'I' + tableClassName + 'DAO';
      template.SetPair('unit_name', tableClassName);
      template.SetPair('uses_list', usesList);
      template.SetPair('type_name', typeName);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      queryByDef := LeftStr(queryByDef, Length(queryByDef) - 2);
      template.SetPair('query_by_definitions', queryByDef);
      template.Write('' + OutputPath + '\class\dao\' + tableClassName + 'DAO.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

{**
 * Create procedures and functions to access MySQL stored routines
 * (note: this requires that the user is the owner of the routine, or have SELECT access to the mysql.proc table)
 * @param Path file output path
 *}
class procedure TGenerator.GenerateStoredRoutines(const OutputPath, TemplatePath: string);
var
  routineName, delphiRoutineName, createSQL, funcParams, sqlParams, comment,
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
  Write('Generating ' + '"' + OutputPath + '\class\StoredRoutines.pas"...');
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW PROCEDURE STATUS WHERE Db = "' + TConnectionProperty.GetDatabase + '"');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  with (ds) do
  begin
    First;
    while (not Eof) do
    begin
      routineName := FieldByName('Name').AsString;
      delphiRoutineName := TInflector.Memberify(routineName);
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
        funcParams := funcParams + 'const ' + TInflector.Memberify(paramRec.VarName) + ': ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType) + '; ';
        sqlParams := sqlParams + ':' + paramRec.VarName + ', ';
      end;
      if (funcParams <> '') then
      begin
        funcParams := Copy(funcParams, 1, Length(funcParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      functionDeclarations := functionDeclarations + TAB2 + 'class procedure ' + delphiRoutineName + '(' + funcParams + ');' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType) + ' ' + TInflector.Memberify(paramRec.VarName) + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class procedure TStoredRoutines.' + delphiRoutineName + '(' + funcParams + ');' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.sql.Add(''CALL ' + routineName + '(' + sqlParams + ')'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.VarName + ''').Value := ' + TInflector.Memberify(paramRec.VarName) + ';' + CRLF;
      end;
      implementationCode := implementationCode + TAB + 'ds := TQueryExecutor.Execute(qry);' + CRLF;
      implementationCode := implementationCode + TAB + 'ds.Free;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      paramRecs.Free;
      Next;
    end;
  end;
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW FUNCTION STATUS WHERE Db = "' + TConnectionProperty.GetDatabase + '"');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  with (ds) do
  begin
    First;
    while (not Eof) do
    begin
      routineName := FieldByName('Name').AsString;
      delphiRoutineName := TInflector.Memberify(routineName);
      qry := TTBGQuery.Create;
      qry.SQL.Add('SHOW CREATE FUNCTION ' + routineName);
      createSQL := TQueryExecutor.QueryForString(qry, 'Create Function');
      qry.Free;
      paramRecs := GetRoutineParameters(createSQL, True);
      funcParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        funcParams := funcParams + 'const ' + TInflector.Memberify(paramRec.VarName) + ': ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType) + '; ';
        sqlParams := sqlParams + ':' + paramRec.VarName + ', ';
      end;
      if (funcParams <> '') then
      begin
        funcParams := Copy(funcParams, 1, Length(funcParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      sqlReturnType := GetRoutineReturnType(createSQL);
      delphiReturnType := TDelphinator.MySQLTypeToDelphiType(sqlReturnType);
      functionDeclarations := functionDeclarations + TAB2 + 'class function ' + delphiRoutineName + '(' + funcParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType) + ' ' + TInflector.Memberify(paramRec.VarName) + CRLF;
      implementationCode := implementationCode + ' * @return ' + delphiReturnType + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class function TStoredRoutines.' + delphiRoutineName + '(' + funcParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.sql.Add(''SELECT ' + routineName + '(' + sqlParams + ') AS value'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.VarName + ''').Value := ' + TInflector.Memberify(paramRec.VarName) + ';' + CRLF;
      end;
      implementationCode := implementationCode + TAB + 'ds := TQueryExecutor.Execute(qry);' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := ds.FieldByName(''value'').Value;' + CRLF;
      implementationCode := implementationCode + TAB + 'ds.Free;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      paramRecs.Free;
      Next;
    end;
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(TemplatePath + '\StoredRoutines.tpl');
    template.SetPair('function_declarations', functionDeclarations);
    template.SetPair('implementation_code', implementationCode);
    template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.Write('' + OutputPath + '\class\StoredRoutines.pas');
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

class function TGenerator.GetIndices(const TableName: string): TStringList;
var
  ds: TClientDataSet;
  indices: TStringList;
begin
  indices := TStringList.Create;
	ds := GetFields(TableName);
  with (ds) do
  while (not Eof) do
  begin
    if (ds.FieldByName('Key').AsString <> '') then
    begin
      indices.Add(ds.FieldByName('Field').AsString);
    end;
    Next;
  end;
  Result := indices;
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

end.
