// Copyright (c) 2017, ProjectBOM authors
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//
// Author: Sergey Sharybin (sergey.vfx@gmail.com)

unit database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, array_list, db;

type TTableFieldType = (TFT_INTEGER, TFT_STRING, TFT_BLOB, TFT_REAL);

const TFF_NONE          = (0);
      TFF_PRIMARY_KEY   = (1 shl 0);
      TFF_AUTOINCREMENT = (1 shl 1);

type TTableColumn = class
 public
  name: string;
  field_type: TTableFieldType;
  fielf_flags: integer;

  constructor Create(aname: string; afield_type: TTableFieldType);
  constructor Create(aname: string;
                     afield_type: TTableFieldType;
                     afield_flags: integer);
end;

type TTable = class
 public
  name: string;
  columns: TArrayList;  // List of TTableColumn.

  constructor Create(aname: string);
  constructor Create(aname: string; acolumns: TArrayList);
  destructor Destroy(); override;
end;

// TODO(sergey): Some of the methods and properties here are stupid bypass to
// the underlyign TQuery. This is annoying, but on another hand helps avoiding
// accidents when TQuery is used directly to modify data without taking
// autocommit into account. Needs some nicer solution or naming here!
type TBaseDatabase = class
 public
  //////////////////////////////////////////////////////////////////////////////
  // Construction / destruction.
  constructor Create();
  destructor Destroy(); override;

  //////////////////////////////////////////////////////////////////////////////
  // Database connect / disconnect.
  function connect(database_name: string) : boolean; virtual; abstract;
  procedure disconnect(); virtual; abstract;

  //////////////////////////////////////////////////////////////////////////////
  // Query.

  procedure executeQuery(); virtual;
  procedure executeQuery(sql: string); virtual;

  function querySingleFieldValue(table_name: string;
                                 field_name: string): TField;

  function lastInsertID(): Integer; virtual; abstract;

  // Iterate over results.
  procedure openQuery(); virtual;
  function queryRecordCount(): integer; virtual;
  function queryEof(): boolean; virtual;
  procedure queryNextRow();
  procedure closeQuery(); virtual;
  function fieldByName(const field_name: string): TField; virtual;

  //////////////////////////////////////////////////////////////////////////////
  // Transaction.

  procedure commitTransaction(); virtual; abstract;
  procedure rollbackTransaction(); virtual; abstract;

  //////////////////////////////////////////////////////////////////////////////
  // Table manipulation.

  function checkTableExists(table_name: string): boolean; virtual;
  procedure createTable(const table: TTable); virtual;
  procedure dropTable(table_name: string);

  //////////////////////////////////////////////////////////////////////////////
  // Security.

  // Check whether given name is a valid identifier.
  function checkIdentifierValid(name: string): boolean; virtual;

  // Raise an exception if given name is not a correct identifier.
  procedure validateIdentifier(name: string); virtual;
 protected
  query_: TSQLQuery;

  // If true, every modification query will commit the changes.
  autocommit_: boolean;

  procedure performAutocommit(); virtual;

  // Shortcuts to query_.SQL.
  function GetSQL(): TStringList;
  procedure SetSQL(const value: TStringList);

  // Shortcuts to query_.params
  function GetParams(): TParams;
  procedure SetParams(value: TParams);
 public
  // Properties.
  property autocommit: boolean read autocommit_ write autocommit_;
  property SQL: TStringlist read GetSQL write SetSQL;
  property Params: TParams read GetParams Write SetParams;
end;

implementation

function fieldTypeAsString(field_type: TTableFieldType): string;
begin
  // TODO(sergey): The names might not be compatible with some databases.
  case field_type of
    TFT_INTEGER: result := 'INTEGER';
    TFT_STRING: result := 'TEXT';
    TFT_BLOB: result := 'BLOB';
    TFT_REAL: result := 'REAL';
    else raise Exception.Create('Unknown field type');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TTableColumn.

constructor TTableColumn.Create(aname: string; afield_type: TTableFieldType);
begin
  name := aname;
  field_type := afield_type;
  fielf_flags := TFF_NONE;
end;

constructor TTableColumn.Create(aname: string;
                                afield_type: TTableFieldType;
                                afield_flags: integer);
begin
  name := aname;
  field_type := afield_type;
  fielf_flags := afield_flags;
end;

////////////////////////////////////////////////////////////////////////////////
// TTable.

constructor TTable.Create(aname: string);
begin
  name := aname;
  columns := TArrayList.Create();
end;

constructor TTable.Create(aname: string; acolumns: TArrayList);
begin
  name := aname;
  columns.Free();
  columns := acolumns;
end;

destructor TTable.Destroy();
var column_pointer: Pointer;
    column: TTableColumn;
begin
  for column_pointer in columns do begin
    column := TTableCOlumn(column_pointer);
    column.Free();
  end;
  columns.Free();
end;

////////////////////////////////////////////////////////////////////////////////
// Construction / destruction.

constructor TBaseDatabase.Create();
begin
  query_ := TSQLQuery.Create(nil);
  autocommit_ := true;
end;

destructor TBaseDatabase.Destroy();
begin
  FreeAndNil(query_);
end;

////////////////////////////////////////////////////////////////////////////////
// Internal heleprs.

procedure TBaseDatabase.performAutocommit();
begin
  if autocommit_ then begin
    commitTransaction();
  end;
end;

function TBaseDatabase.GetSQL(): TStringList;
begin
  result := query_.SQL;
end;

procedure TBaseDatabase.SetSQL(const value: TStringList);
begin
  query_.SQL := value;
end;

function TBaseDatabase.GetParams(): TParams;
begin
  result := query_.Params;
end;

procedure TBaseDatabase.SetParams(value: TParams);
begin
  query_.Params := value;
end;

////////////////////////////////////////////////////////////////////////////////
// Query.

procedure TBaseDatabase.executeQuery();
begin
  query_.ExecSQL();
  performAutocommit();
  query_.Close();
end;

procedure TBaseDatabase.executeQuery(sql: string);
begin
  query_.Close();
  query_.SQL.Text := sql;
  query_.ExecSQL();
  performAutocommit();
  query_.Close();
end;

function TBaseDatabase.querySingleFieldValue(table_name: string;
                                             field_name: string): TField;
begin
  validateIdentifier(table_name);
  validateIdentifier(field_name);
  query_.Close();
  query_.SQL.Text := 'SELECT ' + field_name + ' FROM ' + table_name;
  query_.Open();
  if query_.EOF then begin
    result := Nil;
  end else begin
    result := query_.FieldByName(field_name);
    // TODO(sergey): Check there are no other values.
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Iterate over results.

procedure TBaseDatabase.openQuery();
begin
  query_.Open();
end;

function TBaseDatabase.queryRecordCount(): integer;
begin
  result := query_.RecordCount;
end;

function TBaseDatabase.queryEof(): boolean;
begin
  result := query_.Eof;
end;

procedure TBaseDatabase.queryNextRow();
begin
  query_.Next();
end;

procedure TBaseDatabase.closeQuery();
begin
  query_.Close();
end;

function TBaseDatabase.fieldByName(const field_name: string): TField;
begin
  result := query_.fieldByName(field_name);
end;

////////////////////////////////////////////////////////////////////////////////
// Table manipulation.

function TBaseDatabase.checkTableExists(table_name: string): boolean;
begin
  validateIdentifier(table_name);
  query_.Close();
  query_.SQL.Text := 'SELECT count(*) AS ''count'' FROM sqlite_master ' +
                                     'WHERE type="table" AND name=:table_name';
  query_.Params.ParamByName('table_name').AsString := table_name;
  query_.Open();
  result := query_.FieldByName('count').AsInteger <> 0;
  query_.Close();
end;

procedure TBaseDatabase.createTable(const table: TTable);
var sql_text: string;
    column_pointer: Pointer;
    column: TTableColumn;
    first_column: boolean;
begin
  validateIdentifier(table.name);
  sql_text := 'CREATE TABLE ' + table.name + ' (';
  first_column := true;
  for column_pointer in table.columns do begin
    column := TTableColumn(column_pointer);
    validateIdentifier(column.name);
    if not first_column then begin
      sql_text += ', ';
    end;
    sql_text += column.name + ' ' + fieldTypeAsString(column.field_type);
    // TODO(sergey): This is not really poertable across all databses.
    // Primary key.
    if column.fielf_flags and TFF_PRIMARY_KEY <> 0 then begin
      sql_text += ' PRIMARY KEY';
    end;
    // Auto increment.
    if column.fielf_flags and TFF_AUTOINCREMENT <> 0 then begin
      sql_text += ' AUTOINCREMENT';
    end;
    first_column := false;
  end;
  sql_text += ')';
  // We can't create empty tables.
  if first_column then begin
    raise Exception.Create('Attempt to create table without columns');
  end;
  // Run actual query.
  executeQuery(sql_text);
end;

procedure TBaseDatabase.dropTable(table_name: string);
begin
  validateIdentifier(table_name);
  executeQuery('DROP TABLE ' + table_name);
end;

////////////////////////////////////////////////////////////////////////////////
// Security.

function TBaseDatabase.checkIdentifierValid(name: string): boolean;
var i, len: integer;
begin
  len := length(name);
  // Empty name is bad.
  if len = 0 then begin
    result := false;
    exit;
  end;
  // We should start with a character.
  if not (((name[1] >= 'A') and (name[1] <= 'Z')) or
          ((name[1] >= 'a') and (name[1] <= 'z')) or
           (name[1] = '_')) then begin
    result := false;
    exit;
  end;
  // Check rest of the name.
  for i := 2 to len do begin
    if not (((name[1] >= 'A') and (name[1] <= 'Z')) or
            ((name[1] >= 'a') and (name[1] <= 'z')) or
            ((name[1] >= '0') and (name[1] <= '9')) or
             (name[1] = '_')) then begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;

procedure TBaseDatabase.validateIdentifier(name: string);
begin
  if not checkIdentifierValid(name) then begin
    Raise Exception.Create('Attempt to use unsafe identifier');
  end;
end;

end.

