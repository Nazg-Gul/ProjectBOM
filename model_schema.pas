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

unit model_schema;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database, array_list, db, fgl;

type TTableList = specialize TFPGList<TTable>;

type TModelSchema = class
 public
  constructor Create();
  destructor Destroy(); override;

  // Bring given database to an exact state of the schema.
  procedure prepareDatabase(database: TBaseDatabase);

  // Get schema version stored in database.
  function getDatabaseSchemaVersion(database: TBaseDatabase): integer;

 protected
  version_: integer;
  tables_: TTableList;  // List of TTable.

  // Ensure all tables exists.
  procedure ensureTables(database: TBaseDatabase);
  procedure ensureTable(database: TBaseDatabase; const table: TTable);

  // Re-create the whole database from scratch.
  procedure recreateDatabase(database: TBaseDatabase);
  procedure recreateTable(database: TBaseDatabase; const table: TTable);
end;

implementation

constructor TModelSchema.Create();
begin
  //////////////////////////////////////////////////////////////////////////////
  // Schema version.
  // NOTE: Increase it every time something is changing below.
  version_ := 1;

  //////////////////////////////////////////////////////////////////////////////
  // Initialize storage for tables.
  tables_ := TTableList.Create();

  //////////////////////////////////////////////////////////////////////////////
  // Initialize all the tables.

  // Schema.
  tables_.Add(TTable.Create(
      'schema',
      TArrayList.Create(
          [
            TTableColumn.Create('version', TFT_INTEGER)
          ]
      )));

  // Project.
  tables_.Add(TTable.Create(
      'project',
      TArrayList.Create(
          [
            TTableColumn.Create('id',
                                TFT_INTEGER,
                                TFF_PRIMARY_KEY + TFF_AUTOINCREMENT),
            TTableColumn.Create('parent_id', TFT_INTEGER),
            TTableColumn.Create('name', TFT_STRING),
            TTableColumn.Create('icon', TFT_BLOB)
          ]
      )));

  // Item.
  tables_.Add(TTable.Create(
      'item',
      TArrayList.Create(
          [
            TTableColumn.Create('id',
                                TFT_INTEGER,
                                TFF_PRIMARY_KEY + TFF_AUTOINCREMENT),
            TTableColumn.Create('project_id', TFT_INTEGER),
            TTableColumn.Create('name', TFT_STRING),
            TTableColumn.Create('description', TFT_STRING),
            TTableColumn.Create('image', TFT_BLOB),
            TTableColumn.Create('price', TFT_REAL),
            TTableColumn.Create('currency_id', TFT_INTEGER),
            TTableColumn.Create('url', TFT_STRING),
            TTableColumn.Create('status', TFT_INTEGER)
          ]
      )));

  // Currency.
  tables_.Add(TTable.Create(
      'currency',
      TArrayList.Create(
          [
            TTableColumn.Create('id',
                                TFT_INTEGER,
                                TFF_PRIMARY_KEY + TFF_AUTOINCREMENT),
            TTableColumn.Create('name', TFT_STRING),
            TTableColumn.Create('code', TFT_STRING),
            TTableColumn.Create('sign', TFT_STRING),
            TTableColumn.Create('value', TFT_REAL),
            TTableColumn.Create('reference_id', TFT_INTEGER)
          ]
      )));
end;

destructor TModelSchema.Destroy();
var table: TTable;
begin
  for table in tables_ do begin
    table.Free();
  end;
  tables_.Free();
end;

function TModelSchema.getDatabaseSchemaVersion(
        database: TBaseDatabase): integer;
var field: TField;
begin
  // If table does not exist, early ouytput of missig schema.
  if not database.checkTableExists('schema') then begin
    result := 0;
    exit;
  end;
  // Table exists but has no info, buggy situation.
  // Consider no schema at all.
  field := database.querySingleFieldValue('schema', 'version');
  if field = Nil then begin
    result := 0;
    exit;
  end;
  // Get proper value.
  result := field.AsInteger;
end;

procedure TModelSchema.prepareDatabase(database: TBaseDatabase);
var database_version: integer;
begin
  database_version := getDatabaseSchemaVersion(database);
  // If no schema version available, re-create all tables.
  if database_version = 0 then begin
    recreateDatabase(database);
    exit;
  end;
  // TODO(sergey): This is handy for development, but after initial release it's
  // better to only do what is required.
  // First of all, ensure all missing tables exists.
  ensureTables(database);
  // Run versioning code to tweak old existing tables.
  // ...
  // Store new schema version.
  // TODO(sergey): Avoid direct query.
  database.SQL.Text := 'UPDATE schema SET version=:version';
  database.Params.ParamByName('version').AsInteger := version_;
  database.executeQuery();
end;

procedure TModelSchema.ensureTables(database: TBaseDatabase);
var table: TTable;
begin
  for table in tables_ do begin
    ensureTable(database, table);
  end;
end;

procedure TModelSchema.ensureTable(database: TBaseDatabase;
                                   const table: TTable);
begin
  if not database.checkTableExists(table.name) then begin
    database.createTable(table);
  end;
end;

procedure TModelSchema.recreateDatabase(database: TBaseDatabase);
var table: TTable;
begin
  // Re-create all tables.
  for table in tables_ do begin
    recreateTable(database, table);
  end;
  // Store schema version.
  // TODO(sergey): Avoid direct query.
  database.SQL.Text := 'INSERT INTO schema(version) VALUES(:version)';
  database.Params.ParamByName('version').AsInteger := version_;
  database.executeQuery();
end;

procedure TModelSchema.recreateTable(database: TBaseDatabase;
                                     const table: TTable);
begin
  if database.checkTableExists(table.name) then begin
    database.dropTable(table.name);
  end;
  database.createTable(table);
end;

end.

