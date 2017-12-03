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

unit database_sqlite3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, database, sqldb, sqlite3conn;

type TSqlite3Database = class(TBaseDatabase)
 public
  // Construction / destruction.
  constructor Create();
  destructor Destroy(); override;

  // Query.
  function lastInsertID(): Integer; override;

  // Database connect / disconnect.
  function connect(database_name: string) : boolean; override;
  procedure disconnect(); override;

  // Transaction.
  procedure commitTransaction(); override;
  procedure rollbackTransaction(); override;

 protected
  connection_: TSQLite3Connection;
  transaction_: TSQLTransaction;
end;

implementation

////////////////////////////////////////////////////////////////////////////////
// Construction / destruction.

constructor TSqlite3Database.Create();
begin
  inherited;
  connection_ := TSQLite3Connection.Create(nil);
  transaction_ := TSQLTransaction.Create(connection_);
  connection_.transaction := transaction_;
  // Set up SQL query
  query_.DataBase := connection_;
  query_.Transaction := transaction_;
end;

destructor TSqlite3Database.Destroy();
begin
  inherited;
  FreeAndNil(transaction_);
  FreeAndNil(connection_);
end;

////////////////////////////////////////////////////////////////////////////////
// Query.

function TSqlite3Database.lastInsertID(): Integer;
begin
  result := connection_.GetInsertID;
end;

////////////////////////////////////////////////////////////////////////////////
// Database connect / disconnect.

function TSqlite3Database.connect(database_name: string) : boolean;
begin
  connection_.DatabaseName := database_name;
  connection_.Connected := true;
  result := true;
end;

procedure TSqlite3Database.disconnect();
begin
  connection_.Connected := false;
end;

////////////////////////////////////////////////////////////////////////////////
// Transaction.
procedure TSqlite3Database.commitTransaction();
begin
  transaction_.Commit();
end;

procedure TSqlite3Database.rollbackTransaction();
begin
  transaction_.Rollback();
end;

end.

