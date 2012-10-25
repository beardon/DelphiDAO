{$I .\Defines.inc}
unit Generator;

interface

uses
  Classes,
  DBClient,
  Hashes,
  Generics.Collections;

type
  TRoutineParameter = record
    Direction: string;
    SQLType: string;
    VarName: string;
  end;

  TGenerator = class(TObject)
  private
    FOutputPath: string;
    FTablesDataSet: TClientDataSet;
    FTemplatePath: string;
    procedure CleanDirectory(Path: string);
    function CreateDeleteByDefinition(const FieldName, DelphiType: string): string;
    function CreateDeleteByFunction(const TableName, FieldName, DelphiType: string): string;
    function CreateQueryByDefinitions(const TableName, FieldMemberName, DelphiType: string; const ShowDefaults: Boolean; const PrimaryKeyIndex: string = ''): string;
    function CreateQueryByFunctions(const TableName, FieldName, FieldMemberName, DelphiType, PrimaryKeyIndex: string): string;
    function DoesTableContainPK(const TableName: string): Boolean;
    procedure GenerateDAOExtObjects;
    procedure GenerateDAOFactory;
    procedure GenerateDAOObjects;
    procedure GenerateDTOExtObjects;
    procedure GenerateDTOObjects;
    procedure GenerateIDAOObjects;
    procedure GenerateStoredRoutines;
    function GetFields(const TableName: string): TClientDataSet;
    function GetIndices(const TableName: string): TStringList;
    function GetRoutineParameters(const CreateSQL: string; const IsFunction: Boolean): TList<TRoutineParameter>;
    function GetRoutineReturnType(const CreateSQL: string): string;
    procedure Initialize;
  public
    procedure Generate(OutputPath, TemplatePath: string);
  end;

implementation

uses
  ConnectionProperty,
  DB,
  Delphinator,
  Inflector,
  Query,
  QueryExecutor,
  StrUtils,
  SysUtils,
  Template,
  Windows;

const
  CLASSES_PATH = '\classes';
  INTERFACES_PATH = '\interfaces';
  CORE_PATH = CLASSES_PATH + '\core';
  DAO_PATH = CLASSES_PATH + '\dao';
  DAO_EXT_PATH = DAO_PATH + '\ext';
  DTO_PATH = CLASSES_PATH + '\dto';
  DTO_EXT_PATH = DTO_PATH + '\ext';
  IDAO_PATH = INTERFACES_PATH + '\dao';
  SQL_PATH = CLASSES_PATH + '\sql';
  CRLF = #13#10;
  CRLF2 = CRLF + CRLF;
  TAB = '  ';
  TAB2 = TAB + TAB;

procedure TGenerator.CleanDirectory(Path: string);
var
  fileInfo: TSearchRec;
begin
  FindFirst(Path + '\*.pas', faAnyFile, fileInfo);
  SysUtils.DeleteFile(Path + '\' + fileInfo.Name);
  while (FindNext(fileInfo) = 0) do
    SysUtils.DeleteFile(Path + '\' + fileInfo.Name);
  SysUtils.FindClose(fileInfo);
end;

function TGenerator.CreateDeleteByDefinition(const FieldName, DelphiType: string): string;
var
  code: string;
  fieldMemberName: string;
begin
  fieldMemberName := TInflector.Memberify(FieldName);
  code := TAB2 + 'function DeleteBy' + fieldMemberName + '(const Value: ' + DelphiType + '): Integer;' + CRLF;
  Result := code;
end;

function TGenerator.CreateDeleteByFunction(const TableName, FieldName, DelphiType: string): string;
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
  code := code + TAB + 'qry.SQL.Add(''DELETE FROM ' + TableName + ' WHERE ' + FieldName + ' = :' + fieldMemberName + ''');' + CRLF;
  code := code + TAB + 'qry.ParamByName(''' + fieldMemberName + ''').Value := Value;' + CRLF;
  code := code + TAB + 'Result := ExecuteUpdate(qry);' + CRLF;
  code := code + TAB + 'qry.Free;' + CRLF;
  code := code + 'end;' + CRLF2;
  Result := code;
end;

function TGenerator.CreateQueryByDefinitions(const TableName, FieldMemberName, DelphiType: string; const ShowDefaults: Boolean; const PrimaryKeyIndex: string = ''): string;
var
  code, pkIndexConstant: string;
  appendedComparisonOperatorDefault, appendedOrderClauseDefault, appendedOrderIndexDefault, appendedOrderDirectionDefault: string;
  tableClassName, tableClassExtName: string;
begin
  tableClassName := TInflector.Classify(TableName);
  tableClassExtName := tableClassName + 'Ext';
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
  code := TAB2 + 'function QueryBy' + FieldMemberName + '(const Value: ' + DelphiType + '; const ComparisonOperator: Integer' + appendedComparisonOperatorDefault + '): TObjectList<T' + tableClassExtName + '>;' + CRLF;
  code := code + TAB2 + 'function QueryBy' + FieldMemberName + 'OrderBy(const Value: ' + DelphiType + '; const ComparisonOperator: Integer' + appendedComparisonOperatorDefault + '; const OrderClause: string' + appendedOrderClauseDefault + '): TObjectList<T' + tableClassExtName + '>;' + CRLF;
  code := code + TAB2 + 'function QueryBy' + FieldMemberName + 'OrderByIndex(const Value: ' + DelphiType + '; const ComparisonOperator: Integer' + appendedComparisonOperatorDefault + '; const OrderIndex: Integer' + appendedOrderIndexDefault + '; OrderDirection: Integer' + appendedOrderDirectionDefault + '): TObjectList<T' + tableClassExtName + '>;' + CRLF;
  Result := code;
end;

function TGenerator.CreateQueryByFunctions(const TableName, FieldName, FieldMemberName, DelphiType, PrimaryKeyIndex: string): string;
var
  code, params, pkIndexConstant: string;
  tableClassName, tableClassExtName: string;
begin
  tableClassName := TInflector.Classify(TableName);
  tableClassExtName := tableClassName + 'Ext';
  pkIndexConstant := 'INDEX_' + UpperCase(PrimaryKeyIndex);
  code := 'function T' + tableClassName + 'MySQLDAO.QueryBy' + FieldMemberName + '(const Value: ' + DelphiType + '; const ComparisonOperator: Integer = TSQLComparisonOperator.EQUAL): TObjectList<T' + tableClassExtName + '>;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'Result := QueryBy' + FieldMemberName + 'OrderByIndex(Value, ComparisonOperator, ' + pkIndexConstant + ', TSQLOrderDirection.ASCENDING);' + CRLF;
  code := code + 'end;' + CRLF2;
  code := code + 'function T' + tableClassName + 'MySQLDAO.QueryBy' + FieldMemberName + 'OrderBy(const Value: ' + DelphiType + '; const ComparisonOperator: Integer = TSQLComparisonOperator.EQUAL; const OrderClause: string = ''''): TObjectList<T' + tableClassExtName + '>;' + CRLF;
  code := code + 'var' + CRLF;
  code := code + TAB + 'qry: TTBGQuery;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'qry := TTBGQuery.Create;' + CRLF;
  code := code + TAB + 'qry.SQL.Add(''SELECT * FROM ' + TableName + ''');' + CRLF;
  code := code + TAB + 'qry.SQL.Add(''WHERE ' + FieldName + ' '' + TSQLComparisonOperator.INDEX_OPERATOR_MAP[ComparisonOperator] + '' :' + FieldMemberName + ''');' + CRLF;
  code := code + TAB + 'if (OrderClause <> '''') then' + CRLF;
  code := code + TAB2 + 'qry.SQL.Add(''ORDER BY '' + OrderClause);' + CRLF;
  params := TAB + 'qry.ParamByName(''' + FieldMemberName + ''').Value := Value;' + CRLF;
  if (DelphiType = 'string') then
  begin
    params := TAB + 'if (ComparisonOperator = TSQLComparisonOperator.LIKE) then' + CRLF;
    params := params + TAB2 + 'qry.ParamByName(''' + FieldMemberName + ''').Value := ''%'' + Value + ''%''' + CRLF;
    params := params + TAB + 'else' + CRLF;
    params := params + TAB2 + 'qry.ParamByName(''' + FieldMemberName + ''').Value := Value;' + CRLF;
  end;
  code := code + params;
  code := code + TAB + 'Result := getList(qry);' + CRLF;
  code := code + TAB + 'qry.Free;' + CRLF;
  code := code + 'end;' + CRLF2;
  code := code + 'function T' + tableClassName + 'MySQLDAO.QueryBy' + FieldMemberName + 'OrderByIndex(const Value: ' + DelphiType + '; const ComparisonOperator: Integer = TSQLComparisonOperator.EQUAL; const OrderIndex: Integer = ' + pkIndexConstant + '; OrderDirection: Integer = TSQLOrderDirection.ASCENDING): TObjectList<T' + tableClassExtName + '>;' + CRLF;
  code := code + 'begin' + CRLF;
  code := code + TAB + 'Result := QueryBy' + FieldMemberName + 'OrderBy(Value, ComparisonOperator, INDEX_FIELD_MAP[OrderIndex] + '' '' + TSQLOrderDirection.INDEX_DIRECTION_MAP[OrderDirection]);' + CRLF;
  code := code + 'end;' + CRLF2;
  Result := code;
end;

function TGenerator.DoesTableContainPK(const TableName: string): Boolean;
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

procedure TGenerator.Generate(OutputPath, TemplatePath: string);
var
  qry: TTBGQuery;
begin
  FOutputPath := OutputPath;
  FTemplatePath := TemplatePath;
  Initialize;
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW TABLES');
  FTablesDataSet := TQueryExecutor.Execute(qry);
  qry.Free;
  GenerateDTOObjects;
  GenerateDTOExtObjects;
	GenerateDAOObjects;
	GenerateDAOExtObjects;
{$IFDEF GenerateInterfaces}
	GenerateIDAOObjects;
{$ENDIF}
	GenerateDAOFactory;
  GenerateStoredRoutines;
  FTablesDataSet.Free;
end;

procedure TGenerator.GenerateDAOExtObjects;
var
  ancestorTypeName: string;
  tableBaseClass: string;
  tableDAOExtName: string;
  tableDAOName: string;
  tableName: string;
  template: TTemplate;
  typeName: string;
  usesList: string;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (FTablesDataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableBaseClass := TInflector.Classify(tableName);
      tableDAOName := tableBaseClass + 'DAO';
      tableDAOExtName := tableDAOName + 'Ext';
      if (not FileExists('' + FOutputPath + DAO_EXT_PATH + '\' + tableDAOExtName + '.pas')) then
      begin
        Write('Generating ' + '"' + FOutputPath + DAO_EXT_PATH + '\' + tableDAOExtName + '.pas"...');
        usesList := TAB + tableDAOName + ';';
        template := TTemplate.Create(FTemplatePath + '\DAOExt.tpl', NO_UPDATE_FILES);
        template.SetPair('table_name', tableName);
        template.SetPair('unit_name', tableBaseClass);
        template.SetPair('uses_list', usesList);
        typeName := 'T' + tableDAOExtName;
        ancestorTypeName := 'T' + tableDAOName;
        template.SetPair('ancestor_type_name', ancestorTypeName);
        template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
        template.SetPair('type_name', typeName);
        template.Write('' + FOutputPath + DAO_EXT_PATH + '\' + tableDAOExtName + '.pas');
        template.Free;
        WriteLn(' done.');
      end
      else
      begin
        WriteLn('"' + FOutputPath + DAO_EXT_PATH + '\' + tableDAOExtName + '.pas" already exists (extended classes are not overwritten).');
      end;
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

procedure TGenerator.GenerateDAOFactory;
var
  functionDeclarations: string;
  implementationCode: string;
  tableBaseClass: string;
  tableDAOName: string;
  tableName: string;
  template: TTemplate;
  typeName: string;
  usesList: string;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + FOutputPath + DAO_PATH + '\DAOFactory.pas"...');
  with (FTablesDataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableBaseClass := TInflector.Classify(tableName);
      tableDAOName := tableBaseClass + 'DAO';
      typeName := 'T' + tableDAOName;
      usesList := usesList + TAB + tableDAOName + ',' + CRLF;
      functionDeclarations := functionDeclarations + TAB2 + 'class function Get' + tableDAOName + ': ' + typeName + ';' + CRLF;
      implementationCode := implementationCode + 'class function TDAOFactory.Get' + tableDAOName + ': ' + typeName + ';' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := ' + typeName + '.Create(FConnection);' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      Next;
    end;
    usesList := LeftStr(usesList, Length(usesList) - 3) + ';';
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(FTemplatePath + '\DAOFactory.tpl');
    template.SetPair('uses_list', usesList);
    template.SetPair('function_declarations', functionDeclarations);
    template.SetPair('implementation_code', implementationCode);
    template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.Write('' + FOutputPath + DAO_PATH + '\DAOFactory.pas');
    template.Free;
    WriteLn(' done.');
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

procedure TGenerator.GenerateDAOObjects;
var
  asType: string;
  deleteByDef: string;
  deleteByFunc: string;
  delphiType: string;
  ds: TClientDataSet;
  fieldMemberName: string;
  fieldMemberNames: TStringList;
  fieldName: string;
  firstIndex: string;
  hasPK: Boolean;
  i: Integer;
  indexConstants: string;
  indices: TStringList;
  insertFields: string;
  insertValues: string;
  interfaceName: string;
  isNullable: Boolean;
  mappingArray: string;
  parameterSetter: string;
  pk: string;
  pkCount: Integer;
  queryByDef: string;
  queryByFunc: string;
  readRow: string;
  sqlType: string;
  tableClassBase: string;
  tableDAOInterfaceName: string;
  tableDAOName: string;
  tableDTOName: string;
  tableDTOVariableName: string;
  tableName: string;
  template: TTemplate;
  typeName: string;
  updateFields: string;
  usesList: string;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (FTablesDataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableClassBase := TInflector.Classify(tableName);
      tableDAOName := tableClassBase + 'DAO';
      tableDAOInterfaceName := 'I' + tableDAOName;
      tableDTOName := tableClassBase + 'DTO';
      tableDTOVariableName := 'A' + tableDTOName;
      Write('Generating ' + '"' + FOutputPath + DAO_PATH + '\' + tableDAOName + '.pas"...');
      hasPK := DoesTableContainPK(tableName);
      ds := GetFields(tableName);
      indices := GetIndices(tableName);
      if (indices.Count > 0) then
        firstIndex := indices[0]
      else
        firstIndex := ds.FieldByName('Field').AsString;
      deleteByDef := '';
      deleteByFunc := '';
      insertFields := '';
      insertValues := '';
      pk := '';
      queryByDef := '';
      queryByFunc := '';
      updateFields := '';
      parameterSetter := CRLF;
      readRow := CRLF;
      pkCount := 0;
      fieldMemberNames := TStringList.Create;
      with (ds) do
      while (not Eof) do
      begin
        fieldName := FieldByName('Field').AsString;
        fieldMemberName := TInflector.Memberify(fieldName);
        i := 1;
        while (fieldMemberNames.IndexOf(fieldMemberName) > -1) do
        begin
          Inc(i);
          fieldMemberName := TInflector.Memberify(fieldName) + IntToStr(i);
        end;
        fieldMemberNames.Add(fieldMemberName);
        isNullable := (FieldByName('Null').AsString = 'YES');
        sqlType := FieldByName('Type').AsString;
        delphiType := TDelphinator.MySQLTypeToDelphiType(sqlType, isNullable);
        asType := TDelphinator.MySQLTypeToDelphiAsType(sqlType, isNullable);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := fieldName;
          Inc(pkCount);
        end
        else
        begin
          if (sqlType <> 'timestamp') then
          begin
            insertFields := insertFields + fieldName + ', ';
            updateFields := updateFields + fieldName + ' = :' + fieldMemberName + ', ';
            insertValues := insertValues + ':' + fieldMemberName + ', ';
            parameterSetter := parameterSetter + TAB + 'qry.ParamByName(''' + fieldMemberName + ''').Value := ' + tableDTOVariableName + '.' + fieldMemberName + ';' + CRLF;
            deleteByDef := deleteByDef + CreateDeleteByDefinition(fieldName, delphiType);
            deleteByFunc := deleteByFunc + CreateDeleteByFunction(tableName, fieldName, delphiType);
          end;
          queryByDef := queryByDef + CreateQueryByDefinitions(tableName, fieldMemberName, delphiType, True, firstIndex);
          queryByFunc := queryByFunc + CreateQueryByFunctions(tableName, fieldName, fieldMemberName, delphiType, firstIndex);
        end;
        readRow := readRow + TAB2 + tableDTOVariableName + '.' + fieldMemberName + ' := AClientDataset.FieldByName(''' + fieldName + ''').' + asType + ';' + CRLF;
        Next;
      end;
      fieldMemberNames.Free;
      ds.Free;
      if (hasPK) then
      begin
        if (pkCount = 1) then
{$IFDEF GenerateInterfaces}
          template := TTemplate.Create(FTemplatePath + '\DAOInterfaced.tpl')
{$ELSE}
          template := TTemplate.Create(FTemplatePath + '\DAO.tpl')
{$ENDIF}
        else
          WriteLn(' skipped (no support for complex primary keys).');
      end
      else
{$IFDEF GenerateInterfaces}
        template := TTemplate.Create(FTemplatePath + '\DAOViewInterfaced.tpl');
{$ELSE}
        template := TTemplate.Create(FTemplatePath + '\DAOView.tpl');
{$ENDIF}
      indexConstants := '';
      if (indices.Count > 0) then
      begin
        mappingArray := 'array[0..' + IntToStr(indices.Count - 1) + '] of string = (''' + StringReplace(indices.DelimitedText, ',', ''',''', [rfReplaceAll]) + ''')';
        for i := 0 to indices.Count - 1 do
          indexConstants := indexConstants + TAB2 + 'const INDEX_' + UpperCase(indices[i]) + ' = ' + IntToStr(i) + ';' + CRLF;
      end
      else
      begin
        mappingArray := 'array[0..0] of string = (''' + firstIndex + ''')';
        indexConstants := indexConstants + TAB2 + 'const INDEX_' + UpperCase(firstIndex) + ' = 0;' + CRLF;
      end;
      indices.Free;
      template.SetPair('dao_class_name', 'T' + tableDTOName);
      template.SetPair('table_name', tableName);
      template.SetPair('var_name', tableDTOVariableName);
      indexConstants := LeftStr(indexConstants, Length(indexConstants) - 2);
      insertFields := LeftStr(insertFields, Length(insertFields) - 1);
      insertValues := LeftStr(insertValues, Length(insertValues) - 1);
      queryByDef := LeftStr(queryByDef, Length(queryByDef) - 2);
      queryByFunc := LeftStr(queryByFunc, Length(queryByFunc) - 2);
      deleteByDef := LeftStr(deleteByDef, Length(deleteByDef) - 2);
      deleteByFunc := LeftStr(deleteByFunc, Length(deleteByFunc) - 2);
      updateFields := LeftStr(updateFields, Length(updateFields) - 1);
      if (hasPK) then
      begin
        template.SetPair('pk', pk);
        insertFields := LeftStr(insertFields, Length(insertFields) - 1);
        insertFields := TDelphinator.ConcatLongString(insertFields, True);
        insertValues := LeftStr(insertValues, Length(insertValues) - 1);
        insertValues := TDelphinator.ConcatLongString(insertValues, True);
        updateFields := LeftStr(updateFields, Length(updateFields) - 1);
        updateFields := TDelphinator.ConcatLongString(updateFields, True);
        template.SetPair('delete_by_definitions', deleteByDef);
        template.SetPair('delete_by_functions', deleteByFunc);
        template.SetPair('insert_fields', insertFields);
        template.SetPair('insert_values', insertValues);
        template.SetPair('parameter_setter', parameterSetter);
        template.SetPair('pk_with_s', TInflector.Memberify(pk));
        template.SetPair('update_fields', updateFields);
      end;
      usesList := TAB + tableDTOName + ',';
{$IFDEF GenerateInterfaces}
      usesList := CRLF + TAB + tableDAOName + 'Interface,';
{$ENDIF}
      typeName := 'T' + tableDAOName;
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.SetPair('index_constants', indexConstants);
{$IFDEF GenerateInterfaces}
      interfaceName := 'I' + tableDAOName;
      template.SetPair('interface_name', interfaceName);
{$ENDIF}
      template.SetPair('mapping_array', mappingArray);
      template.SetPair('query_by_definitions', queryByDef);
      template.SetPair('query_by_functions', queryByFunc);
      template.SetPair('read_row', readRow);
      template.SetPair('type_name', typeName);
      template.SetPair('unit_name', tableClassBase);
      template.SetPair('uses_list', usesList);
      template.Write('' + FOutputPath + DAO_PATH + '\' + tableDAOName + '.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

procedure TGenerator.GenerateDTOExtObjects;
var
  ancestorTypeName: string;
  pointerTypeName: string;
  tableClassBase: string;
  tableDTOExtName: string;
  tableDTOName: string;
  tableName: string;
  template: TTemplate;
  typeName: string;
  usesList: string;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (FTablesDataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableClassBase := TInflector.Classify(tableName);
      tableDTOName := tableClassBase + 'DTO';
      tableDTOExtName := tableDTOName + 'Ext';
      typeName := 'T' + tableDTOExtName;
      ancestorTypeName := 'T' + tableDTOName;
      pointerTypeName := 'P' + tableDTOExtName;
      usesList := TAB + tableDTOName + ';';
      if (not FileExists('' + FOutputPath + DTO_EXT_PATH + '\' + tableDTOExtName + '.pas')) then
      begin
        Write('Generating ' + '"' + FOutputPath + DTO_EXT_PATH + '\' + tableDTOExtName + '.pas"...');
        template := TTemplate.Create(FTemplatePath + '\DTOExt.tpl', NO_UPDATE_FILES);
        template.SetPair('ancestor_type_name', ancestorTypeName);
        template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
        template.SetPair('pointer_type_name', pointerTypeName);
        template.SetPair('table_name', tableName);
        template.SetPair('type_name', typeName);
        template.SetPair('unit_name', tableDTOName);
        template.SetPair('uses_list', usesList);
        template.Write('' + FOutputPath + DTO_EXT_PATH + '\' + tableDTOExtName + '.pas');
        template.Free;
        WriteLn(' done.');
      end
      else
      begin
        WriteLn('"' + FOutputPath + DTO_EXT_PATH + '\' + tableDTOExtName + '.pas" already exists (extended classes are not overwritten).');
      end;
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

procedure TGenerator.GenerateDTOObjects;
var
  assignAssignments: string;
  delphiType: string;
  ds: TClientDataSet;
  fieldMemberName: string;
  fieldMemberNames: TStringList;
  fieldName: string;
  i: Integer;
  isNullable: Boolean;
  pointerTypeName: string;
  publicConstants: string;
  publicProperties: string;
  protectedVars: string;
  sqlType: string;
  tableBaseClass: string;
  tableDTOName: string;
  tableDTOVariableName: string;
  tableName: string;
  template: TTemplate;
  typeName: string;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (FTablesDataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableBaseClass := TInflector.Classify(tableName);
      tableDTOName := tableBaseClass + 'DTO';
      Write('Generating ' + '"' + FOutputPath + DTO_PATH + '\' + tableDTOName + '.pas"...');
      template := TTemplate.Create(FTemplatePath + '\DTO.tpl');
      template.SetPair('table_name', tableName);
      template.SetPair('unit_name', tableDTOName);
      typeName := 'T' + tableDTOName;
      tableDTOVariableName := 'A' + tableDTOName;
      pointerTypeName := 'P' + tableDTOName;
      template.SetPair('pointer_type_name', pointerTypeName);
      template.SetPair('type_name', typeName);
      template.SetPair('var_name', tableDTOVariableName);
      publicConstants := TAB2 + 'const TABLE_NAME = ''' + tableName + ''';';
      template.SetPair('public_constants', publicConstants);
      assignAssignments := '';
      protectedVars := '';
      publicProperties := '';
      ds := GetFields(tableName);
      fieldMemberNames := TStringList.Create;
      while (not ds.Eof) do
      begin
        fieldName := ds.FieldByName('Field').AsString;
        i := 1;
        fieldMemberName := TInflector.Memberify(fieldName);
        while (fieldMemberNames.IndexOf(fieldMemberName) > -1) do
        begin
          Inc(i);
          fieldMemberName := TInflector.Memberify(fieldName) + IntToStr(i);
        end;
        fieldMemberNames.Add(fieldMemberName);
        isNullable := (ds.FieldByName('Null').AsString = 'YES');
        sqlType := ds.FieldByName('Type').AsString;
        delphiType := TDelphinator.MySQLTypeToDelphiType(sqlType, isNullable);
        protectedVars := protectedVars + TAB2 + 'F' + fieldMemberName + ': ' + delphiType + '; //' + sqlType + CRLF;
        publicProperties := publicProperties + TAB2 + 'property ' + fieldMemberName + ': ' + delphiType + ' read F' + fieldMemberName + ' write F' + fieldMemberName + ';' + CRLF;
        assignAssignments := assignAssignments + TAB2 + fieldMemberName + ' := ' + typeName + '(' + tableDTOVariableName + ').' + fieldMemberName + ';' + CRLF;
        ds.Next;
      end;
      fieldMemberNames.Free;
      ds.Free;
      protectedVars := LeftStr(protectedVars, Length(protectedVars) - 2);
      publicProperties := LeftStr(publicProperties, Length(publicProperties) - 2);
      assignAssignments := LeftStr(assignAssignments, Length(assignAssignments) - 2);
      template.SetPair('assign_assignments', assignAssignments);
      template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
      template.SetPair('protected_vars', protectedVars);
      template.SetPair('public_properties', publicProperties);
      template.Write('' + FOutputPath + DTO_PATH + '\' + tableDTOName + '.pas');
      template.Free;
      WriteLn(' done.');
      Next;
    end;
  end;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

procedure TGenerator.GenerateIDAOObjects;
var
  asType: string;
  deleteByDef: string;
  delphiType: string;
  ds: TClientDataSet;
  fieldMemberName: string;
  fieldMemberNames: TStringList;
  fieldName: string;
  hasPK: Boolean;
  i: Integer;
  isNullable: Boolean;
  pk: string;
  pkCount: Integer;
  queryByDef: string;
  sqlType: string;
  tableClassBase: string;
  tableDAOName: string;
  tableDTOExtName: string;
  tableDTOName: string;
  tableDTOVariableName: string;
  tableName: string;
  template: TTemplate;
  typeName: string;
  usesList: string;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  with (FTablesDataSet) do
  begin
    First;
    while (not Eof) do
    begin
      tableName := FieldByName('Tables_in_' + TConnectionProperty.GetDatabase).AsString;
      tableClassBase := TInflector.Classify(tableName);
      tableDAOName := tableClassBase + 'DAO';
      tableDTOName := tableClassBase + 'DTO';
      tableDTOExtName := tableDTOName + 'Ext';
      tableDTOVariableName := 'A' + tableDTOExtName;
      Write('Generating ' + '"' + FOutputPath + DAO_PATH + '\' + tableDAOName + '.pas"...');
      hasPK := DoesTableContainPK(tableName);
      ds := GetFields(tableName);
      pk := '';
      queryByDef := '';
      deleteByDef := '';
      fieldMemberNames := TStringList.Create;
      pkCount := 0;
      with (ds) do
      while (not Eof) do
      begin
        fieldName := FieldByName('Field').AsString;
        i := 1;
        fieldMemberName := TInflector.Memberify(fieldName);
        while (fieldMemberNames.IndexOf(fieldMemberName) > -1) do
        begin
          Inc(i);
          fieldMemberName := TInflector.Memberify(fieldName) + IntToStr(i);
        end;
        fieldMemberNames.Add(fieldMemberName);
        isNullable := (FieldByName('Null').AsString = 'YES');
        sqlType := FieldByName('Type').AsString;
        delphiType := TDelphinator.MySQLTypeToDelphiType(sqlType, isNullable);
        asType := TDelphinator.MySQLTypeToDelphiAsType(sqlType, isNullable);
        if (FieldByName('Key').AsString = 'PRI') then
        begin
          pk := fieldName;
          Inc(pkCount);
        end
        else
        begin
          if (sqlType <> 'timestamp') then
            deleteByDef := deleteByDef + TAB2 + 'function DeleteBy' + fieldMemberName + '(const Value: ' + delphiType + '): Integer;' + CRLF;
          queryByDef := queryByDef + CreateQueryByDefinitions(tableName, fieldMemberName, delphiType, False);
        end;
        Next;
      end;
      fieldMemberNames.Free;
      ds.Free;
      if (hasPK) then
      begin
        if (pkCount = 1) then
          template := TTemplate.Create(FTemplatePath + '\IDAO.tpl')
        else
          WriteLn(' skipped (no support for complex primary keys).');
      end
      else
        template := TTemplate.Create(FTemplatePath + '\IDAOView.tpl');
      if (Assigned(template)) then
      begin
        template.SetPair('dao_class_name', 'T' + tableDTOExtName);
        template.SetPair('table_name', tableName);
        template.SetPair('param_name', tableDTOVariableName);
        if (hasPK) then
        begin
          template.SetPair('pk', pk);
          deleteByDef := LeftStr(deleteByDef, Length(deleteByDef) - 2);
          template.SetPair('delete_by_definitions', deleteByDef);
        end;
        usesList := TAB + tableDTOExtName + ',';
        typeName := 'I' + tableDAOName;
        template.SetPair('unit_name', tableDAOName);
        template.SetPair('uses_list', usesList);
        template.SetPair('type_name', typeName);
        template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
        queryByDef := LeftStr(queryByDef, Length(queryByDef) - 2);
        template.SetPair('query_by_definitions', queryByDef);
        template.Write('' + FOutputPath + DAO_PATH + '\' + tableDAOName + '.pas');
        template.Free;
        WriteLn(' done.');
      end;
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
 *}
procedure TGenerator.GenerateStoredRoutines;
var
  comment: string;
  createSQL: string;
  delphiReturnType: string;
  delphiRoutineName: string;
  ds: TClientDataSet;
  functionParams: string;
  functionDeclarations: string;
  implementationCode: string;
  paramRec : TRoutineParameter;
  paramRecs: TList<TRoutineParameter>;
  qry: TTBGQuery;
  routineName: string;
  sqlParams: string;
  sqlReturnType: string;
  template: TTemplate;
begin
{$IFNDEF CONSOLE}
  AllocConsole;
{$ENDIF}
  Write('Generating ' + '"' + FOutputPath + CLASSES_PATH + '\StoredRoutines.pas"...');
  qry := TTBGQuery.Create;
  qry.SQL.Add('SHOW PROCEDURE STATUS WHERE Db = "' + TConnectionProperty.GetDatabase + '"');
  ds := TQueryExecutor.Execute(qry);
  qry.Free;
  with (ds) do
  if (not IsEmpty) then
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
      functionParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        functionParams := functionParams + 'const ' + TInflector.Memberify(paramRec.VarName) + ': ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType, False) + '; ';
        sqlParams := sqlParams + ':' + paramRec.VarName + ', ';
      end;
      if (functionParams <> '') then
      begin
        functionParams := Copy(functionParams, 1, Length(functionParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      functionDeclarations := functionDeclarations + TAB2 + 'class procedure ' + delphiRoutineName + '(' + functionParams + ');' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType, False) + ' ' + TInflector.Memberify(paramRec.VarName) + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class procedure TStoredRoutines.' + delphiRoutineName + '(' + functionParams + ');' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.SQL.Add(''CALL ' + routineName + '(' + sqlParams + ')'');' + CRLF;
      for paramRec in paramRecs do
      begin
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.VarName + ''').Value := ' + TInflector.Memberify(paramRec.VarName) + ';' + CRLF;
      end;
      implementationCode := implementationCode + TAB + 'ds := TQueryExecutor.Execute(qry);' + CRLF;
      implementationCode := implementationCode + TAB + 'ds.Free;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.Free;' + CRLF;
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
  if (not IsEmpty) then
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
      functionParams := '';
      sqlParams := '';
      for paramRec in paramRecs do
      begin
        functionParams := functionParams + 'const ' + TInflector.Memberify(paramRec.VarName) + ': ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType, False) + '; ';
        sqlParams := sqlParams + ':' + paramRec.VarName + ', ';
      end;
      if (functionParams <> '') then
      begin
        functionParams := Copy(functionParams, 1, Length(functionParams) - 2);
        sqlParams := Copy(sqlParams, 1, Length(sqlParams) - 2);
      end;
      sqlReturnType := GetRoutineReturnType(createSQL);
      delphiReturnType := TDelphinator.MySQLTypeToDelphiType(sqlReturnType, False);
      functionDeclarations := functionDeclarations + TAB2 + 'class function ' + delphiRoutineName + '(' + functionParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + '{**' + CRLF;
      if (comment <> '') then
      begin
        implementationCode := implementationCode + ' * ' + comment + CRLF;
        implementationCode := implementationCode + ' *' + CRLF;
      end;
      implementationCode := implementationCode + ' * @param ' + TDelphinator.MySQLTypeToDelphiType(paramRec.SQLType, False) + ' ' + TInflector.Memberify(paramRec.VarName) + CRLF;
      implementationCode := implementationCode + ' * @return ' + delphiReturnType + CRLF;
      implementationCode := implementationCode + '*}' + CRLF;
      implementationCode := implementationCode + 'class function TStoredRoutines.' + delphiRoutineName + '(' + functionParams + '): ' + delphiReturnType + ';' + CRLF;
      implementationCode := implementationCode + 'var' + CRLF;
      implementationCode := implementationCode + TAB + 'ds: TClientDataSet;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry: TTBGQuery;' + CRLF;
      implementationCode := implementationCode + 'begin' + CRLF;
      implementationCode := implementationCode + TAB + 'qry := TTBGQuery.Create;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.SQL.Add(''SELECT ' + routineName + '(' + sqlParams + ') AS value'');' + CRLF;
      for paramRec in paramRecs do
        implementationCode := implementationCode + TAB + 'qry.ParamByName(''' + paramRec.VarName + ''').Value := ' + TInflector.Memberify(paramRec.VarName) + ';' + CRLF;
      implementationCode := implementationCode + TAB + 'ds := TQueryExecutor.Execute(qry);' + CRLF;
      implementationCode := implementationCode + TAB + 'Result := ds.FieldByName(''value'').Value;' + CRLF;
      implementationCode := implementationCode + TAB + 'ds.Free;' + CRLF;
      implementationCode := implementationCode + TAB + 'qry.Free;' + CRLF;
      implementationCode := implementationCode + 'end;' + CRLF;
      implementationCode := implementationCode + CRLF;
      paramRecs.Free;
      Next;
    end;
    functionDeclarations := LeftStr(functionDeclarations, Length(functionDeclarations) - 2);
    implementationCode := LeftStr(implementationCode, Length(implementationCode) - 2);
    template := TTemplate.Create(FTemplatePath + '\StoredRoutines.tpl');
    template.SetPair('date', FormatDateTime('yyyy-mm-dd hh:nn', Now));
    template.SetPair('function_declarations', functionDeclarations);
    template.SetPair('implementation_code', implementationCode);
    template.Write('' + FOutputPath + CLASSES_PATH + '\StoredRoutines.pas');
    template.Free;
    WriteLn(' done.');
  end;
  ds.Free;
{$IFNDEF CONSOLE}
  FreeConsole;
{$ENDIF}
end;

function TGenerator.GetFields(const TableName: string): TClientDataSet;
var
  qry: TTBGQuery;
begin
  qry := TTBGQuery.Create;
  qry.SQL.Add('DESC ' + TableName);
  Result := TQueryExecutor.Execute(qry);
  qry.Free;
end;

function TGenerator.GetIndices(const TableName: string): TStringList;
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
      indices.Add(ds.FieldByName('Field').AsString);
    Next;
  end;
  Result := indices;
end;

function TGenerator.GetRoutineParameters(const CreateSQL: string; const IsFunction: Boolean): TList<TRoutineParameter>;
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

function TGenerator.GetRoutineReturnType(const CreateSQL: string): string;
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

procedure TGenerator.Initialize;
begin
	CreateDir(FOutputPath);
	CreateDir(FOutputPath + CLASSES_PATH);
	CreateDir(FOutputPath + CORE_PATH);
	CreateDir(FOutputPath + DAO_PATH);
  CreateDir(FOutputPath + DAO_EXT_PATH);
	CreateDir(FOutputPath + DTO_PATH);
	CreateDir(FOutputPath + DTO_EXT_PATH);
	CreateDir(FOutputPath + SQL_PATH);
{$IFDEF GenerateInterfaces}
  CreateDir(FOutputPath + INTERFACES_PATH);
  CreateDir(FOutputPath + IDAO_PATH);
{$ENDIF}
// need to compare files, so cannot blanket delete
//	CleanDirectory(FOutputPath + DAO_PATH);
//  CleanDirectory(FOutputPath + DTO_PATH);
//	CleanDirectory(FOutputPath + IDAO_PATH);
  CopyFile(PChar(FTemplatePath + '\classes\dao\core\ArrayList.pas'), PChar(FOutputPath + CORE_PATH + '\ArrayList.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\Connection.pas'), PChar(FOutputPath + SQL_PATH + '\Connection.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\ConnectionFactory.pas'), PChar(FOutputPath + SQL_PATH + '\ConnectionFactory.pas'), False);
  // do not overwrite connection properties if they already exist
  if (not FileExists(FOutputPath + '\classes\sql\ConnectionProperty.pas')) then
    CopyFile(PChar(FTemplatePath + '\ConnectionProperty.tpl'), PChar(FOutputPath + SQL_PATH + '\ConnectionProperty.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\Query.pas'), PChar(FOutputPath + SQL_PATH + '\Query.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\QueryExecutor.pas'), PChar(FOutputPath + SQL_PATH + '\QueryExecutor.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\QueryFactory.pas'), PChar(FOutputPath + SQL_PATH + '\QueryFactory.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\SQLComparisonOperator.pas'), PChar(FOutputPath + SQL_PATH + '\SQLComparisonOperator.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\SQLOrderDirection.pas'), PChar(FOutputPath + SQL_PATH + '\SQLOrderDirection.pas'), False);
  CopyFile(PChar(FTemplatePath + '\classes\dao\sql\Transaction.pas'), PChar(FOutputPath + SQL_PATH + '\Transaction.pas'), False);
end;

end.