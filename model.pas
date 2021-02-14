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

unit model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, database, model_schema, fgl;

////////////////////////////////////////////////////////////////////////////////
// NOTE: Direct modifications inside of projects / items will not be visible  //
//       or effective for until changes are committed via model API.          //
////////////////////////////////////////////////////////////////////////////////

type TModel = class;

////////////////////////////////////////////////////////////////////////////////
// Currency.

type TModelCurrency = class
 public
  id: integer;
  name: string;  // i.e. Russian Ruble, Euro, US Dollar...
  code: string;  // i.e. RUR, USD, EUR...
  sign: string;  // i.e. $, €
  value: double;  // Value against reference currency.
                  // How much reference currency fits into 1 of the current one.
  reference: TModelCurrency;

  constructor Create(); overload;

  function getReferenceID(): integer;

  // Get given value in a way that is readable within the current currency.
  // For example, if one calls getReadableValue(123) for EUR currency the result
  // will be €123.
  function getReadableValue(val: double): string;

  // Convert self.value to a given currency, return new value.
  function convertTo(model: TModel;
                     from_value: double;
                     to_currency: TModelCurrency): double;
end;

type TModelCurrencyConversionMatrix = array of array of double;

type TModelCurrencyList = specialize TFPGList<TModelCurrency>;

////////////////////////////////////////////////////////////////////////////////
// Generic serializable image / icon.

type TModelImage = class
 public
  picture: TPicture;
  // TODO(sergey): File path, original name or anything like that?
  // TODO(sergey): Or maybe raw bytes?

  constructor Create(); overload;
  destructor Destroy(); override;

  procedure loadFromFile(const file_name: string);
  procedure loadFromPicture(const new_picture: TPicture);
  procedure loadFromBytes(const bytes: TBytes);
end;

////////////////////////////////////////////////////////////////////////////////
// Item definition.

type TModelItemStatus = (
  // Status is unknown, newly added to the database.
  TMIS_NONE      = 0,
  // The item has been ordered.
  TMIS_ORDERED   = 1,
  // The item was sent by the seller.
  TMIS_SENT      = 2,
  // The item has been delivered.
  TMIS_DELIVERED = 3,
  // Fully done with an item, can scratch it from the BOM.
  TMIS_DONE      = 5,
  // There is no need in this item anymore, but keep it in list just for
  // reference.
  TMIS_CANCELLED = 6
);

// Get number of item statuses.
function getNumItemStatus(): integer;
// Get name of the given item status.
function getItemnStatusName(status: TModelItemStatus): string;

type TModelItem = class
 public
  id: integer;
  name: String;
  description: String;
  image: TModelImage;
  price: double;
  currency: TModelCurrency;
  url: String;
  status: TModelItemStatus;

  constructor Create(); overload;
  destructor Destroy(); override;

  procedure loadImageFromFile(const file_name: string);
  procedure loadImageFromPicture(const picture: TPicture);
  procedure loadImageFromDatabase(database: TBaseDatabase);

  // Unload image from memory, only use for items coming from the databse!
  procedure unloadImage();

  // Get image TPictore object. Simple wraper, will not do anything smart like
  // ensuring image is loaded from the database.
  function getImagePicture(): TPicture;

  // Get binary dump of the image pixture.
  function getImagePictureData(): TBytes;

  function getCurrencyID(): integer;

  // When item is done its cost is deducted from the remaining bill.
  function isDone(): boolean;

  // When item is cancelled its cost is deducted from the any bills.
  function isCancelled(): boolean;

  // Check based on the item status to see whether the item was paid for.
  // Is used when calcualting remaining project cost.
  function isPaid(): boolean;
end;

type TModelItemList = specialize TFPGList<TModelItem>;

////////////////////////////////////////////////////////////////////////////////
// Project definition.

type TModelProject = class;
type TModelProjectList = specialize TFPGList<TModelProject>;

type TModelProject = class
 public
  parent: TModelProject;
  id: integer;
  name: String;
  icon: TModelImage;
  items: TModelItemList;
  projects: TModelProjectList;

  constructor Create(); overload;
  destructor Destroy(); override;

  function getParentID(): integer;

  // TODO(sergey): Which currency?
  function getTotalProjectCost(model: TModel;
                               currency: TModelCurrency) : Double;
  function getRemainingProjectCost(model: TModel;
                                   currency: TModelCurrency) : Double;
end;

////////////////////////////////////////////////////////////////////////////////
// Event types.

TModelCurrencyNotifyEvent = procedure(Sender: TObject;
                                      const currency: TModelCurrency) of object;

TModelItemNotifyEvent = procedure(Sender: TObject;
                                  const project: TModelProject;
                                  const item: TModelItem) of object;

TModelProjectNotifyEvent = procedure(Sender: TObject;
                                     const project: TModelProject) of object;

////////////////////////////////////////////////////////////////////////////////
// Model itself (includes everything).

type TModel = class
 protected
  ///////////////////////////////////////
  // Model itself.
  currencies_: TModelCurrencyList;
  projects_: TModelProjectList;

  ///////////////////////////////////////
  // Database.
  schema_: TModelSchema;
  database_: TBaseDatabase;

  ///////////////////////////////////////
  // Event handlers.

  // Currency.
  on_currency_add_: TModelCurrencyNotifyEvent;
  on_currency_edit_: TModelCurrencyNotifyEvent;
  on_currency_delete_: TModelCurrencyNotifyEvent;

  // Project.
  on_project_add_: TModelProjectNotifyEvent;
  on_project_edit_: TModelProjectNotifyEvent;
  on_project_delete_: TModelProjectNotifyEvent;

  // Item.
  on_item_add_: TModelItemNotifyEvent;
  on_item_edit_: TModelItemNotifyEvent;
  on_item_delete_: TModelItemNotifyEvent;

  ///////////////////////////////////////
  // Load from database.

  procedure loadDatabase();
  procedure loadCurrencies();
  procedure loadProjects();
  procedure loadProjects(var projects: TModelProjectList;
                         parent: TModelProject);
  procedure loadProjectsExpand(var projects: TModelProjectList);
  procedure loadProjectExpand(var project: TModelProject);

  ///////////////////////////////////////
  // Validation and verification.
  procedure verifyCurrencyIsInModel(const currency: TModelCurrency);
  procedure verifyProjectIsInModel(const project: TModelProject);
  procedure verifyItemIsInModel(const project: TModelProject;
                                const item: TModelItem);
 public
  constructor Create(database: TBaseDatabase); overload;
  destructor Destroy(); override;

  ///////////////////////////////////////
  // Entity manipulation.

  // Currency.
  function getAllCurrencies(): TModelCurrencyList;
  function getCurrencyByID(id: integer): TModelCurrency;
  function getCurrencyBySign(sign: string): TModelCurrency;
  function getCurrencyByCode(code: string): TModelCurrency;
  function getCurrencyByGuess(value: string): TModelCurrency;

  function getCurrencyIndex(currency: TModelCurrency): integer;

  function getCurrencyConversionMatrix(): TModelCurrencyConversionMatrix;

  procedure addCurrency(const currency: TModelCurrency);
  procedure editCurrency(const currency: TModelCurrency);
  procedure deleteCurrency(const currency: TModelCurrency);

  // Project.
  function getAllProjects(): TModelProjectList;
  procedure addProject(const project: TModelProject);
  procedure editProject(const project: TModelProject);
  procedure deleteProject(const project: TModelProject);

  // Item.
  procedure addItem(const project: TModelProject; const item: TModelItem);
  procedure editItem(const project: TModelProject; const item: TModelItem);
  procedure deleteItem(const project: TModelProject; const item: TModelItem);

  procedure expandItem(item: TModelItem);
  procedure unexpandItem(item: TModelItem);

 protected
  // Currency setters.
  procedure setOnCurrencyAdd(value: TModelCurrencyNotifyEvent); virtual;
  procedure setOnCurrencyEdit(value: TModelCurrencyNotifyEvent); virtual;
  procedure setOnCurrencyDelete(value: TModelCurrencyNotifyEvent); virtual;

  // Project setters.
  procedure setOnProjectAdd(value: TModelProjectNotifyEvent); virtual;
  procedure setOnProjectEdit(value: TModelProjectNotifyEvent); virtual;
  procedure setOnProjectDelete(value: TModelProjectNotifyEvent); virtual;

  // Item setters.
  procedure setOnItemAdd(value: TModelItemNotifyEvent); virtual;
  procedure setOnItemEdit(value: TModelItemNotifyEvent); virtual;
  procedure setOnItemDelete(value: TModelItemNotifyEvent); virtual;

 public
  // Currency event properties.
  property onCurrencyAdd: TModelCurrencyNotifyEvent read on_currency_add_
                                                    write setOnCurrencyAdd;
  property onCurrencyEdit: TModelCurrencyNotifyEvent read on_currency_edit_
                                                     write setOnCurrencyEdit;
  property onCurrencyDelete: TModelCurrencyNotifyEvent
      read on_currency_delete_
      write setOnCurrencyDelete;

  // Project event properties.
  property onProjectAdd: TModelProjectNotifyEvent read on_project_add_
                                                  write setOnProjectAdd;
  property onProjectEdit: TModelProjectNotifyEvent read on_project_edit_
                                                   write setOnProjectEdit;
  property onProjectDelete: TModelProjectNotifyEvent read on_project_delete_
                                                     write setOnProjectDelete;

  // Item event properties.
  property onItemAdd: TModelItemNotifyEvent read on_item_add_
                                            write setOnItemAdd;
  property onItemEdit: TModelItemNotifyEvent read on_item_edit_
                                             write setOnItemEdit;
  property onItemDelete: TModelItemNotifyEvent read on_item_delete_
                                               write setOnItemDelete;
end;

implementation

////////////////////////////////////////////////////////////////////////////////
// Currency implementation

constructor TModelCurrency.Create();
begin
  id := -1;
end;

function TModelCurrency.getReferenceID(): integer;
begin
  if reference <> nil then begin
    result := reference.id;
  end else begin
    result := -1;
  end;
end;

function TModelCurrency.getReadableValue(val: double): string;
begin
  // TODO(sergey): Some currencies needs to be post-fixed.
  result := sign + FloatToStr(val);
end;

function TModelCurrency.convertTo(model: TModel;
                                  from_value: double;
                                  to_currency: TModelCurrency): double;
var
    conversion_matrix: TModelCurrencyConversionMatrix;
    i, j: integer;
begin
  conversion_matrix := model.getCurrencyConversionMatrix();
  i := model.getCurrencyIndex(self);
  j := model.getCurrencyIndex(to_currency);
  if conversion_matrix[j, i] = -1 then begin
    // No conversion between currencies.
    // TODO(sergey): Somehow inform interface?
    result := 0;
    exit;
  end;
  result := conversion_matrix[j, i] * from_value;
  // Round to 2 digits after the comma.
  result := round(result * 100) / 100;
end;

////////////////////////////////////////////////////////////////////////////////
// Image implementation

constructor TModelImage.Create();
begin
  picture := nil;
end;

destructor TModelImage.Destroy();
begin
  if picture <> nil then begin
    picture.Free();
  end;
end;

procedure TModelImage.loadFromFile(const file_name: string);
begin
  if picture = nil then begin
    picture := TPicture.Create();
  end;
  picture.LoadFromFile(file_name);
end;

procedure TModelImage.loadFromPicture(const new_picture: TPicture);
begin
  if picture = nil then begin
    picture := TPicture.Create();
  end;
  picture.Assign(new_picture);
end;

procedure TModelImage.loadFromBytes(const bytes: TBytes);
var stream: TBytesStream;
begin
  if picture = nil then begin
    picture := TPicture.Create();
  end;
  if bytes <> nil then begin
    stream := TBytesStream.Create(bytes);
    picture.LoadFromStream(stream);
    stream.Free();
  end else begin
    picture.Clear();
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Item implementation

function getNumItemStatus(): integer;
begin
  // TODO(sergey): What would be more reliable way without too much trickery?
  // Especially, trickery which will deal with possibly added/removed status.
  result := Integer(TMIS_CANCELLED);
end;

function getItemnStatusName(status: TModelItemStatus): string;
begin
  case status of
    TMIS_NONE:      result := 'None';
    TMIS_ORDERED:   result := 'Ordered';
    TMIS_SENT:      result := 'Sent';
    TMIS_DELIVERED: result := 'Delivered';
    TMIS_DONE:      result := 'Done';
    TMIS_CANCELLED: result := 'Cancelled';
    else result := 'Unknown';
  end;
end;

constructor TModelItem.Create();
begin
  id := -1;  // Indicates that item is not yet in the database.
  currency := nil;
  image := nil;
  status := TMIS_NONE;
end;

destructor TModelItem.Destroy();
begin
  if image <> nil then begin
    image.Free();
  end;
end;

procedure TModelItem.loadImageFromFile(const file_name: string);
begin
  if image = nil then begin
    image := TModelImage.Create();
  end;
  image.loadFromFile(file_name);
end;

procedure TModelItem.loadImageFromPicture(const picture: TPicture);
begin
  if picture.Height = 0 then begin
    if image <> nil then begin
      FreeAndNil(image);
    end;
    exit;
  end;
  if image = nil then begin
    image := TModelImage.Create();
  end;
  image.loadFromPicture(picture);
end;

procedure TModelItem.loadImageFromDatabase(database: TBaseDatabase);
begin
  if id = -1 then begin
    raise Exception.Create('Attempt to load image for image ' +
                           'outside of database');
    exit;
  end;
  // Select image from the database.
  database.SQL.Text := 'SELECT image FROM item WHERE id=:id';
  database.Params.ParamByName('id').AsInteger := id;
  database.openQuery();
  if database.queryEof() then begin
    raise Exception.Create('Item is not committed to database');
  end;
  // Read image from memory.
  if image = nil then begin
    image := TModelImage.Create();
  end;
  image.loadFromBytes(database.fieldByName('image').AsBytes);
  // Close everything.
  database.closeQuery();
end;

procedure TModelItem.unloadImage();
begin
  if id = -1 then begin
    raise Exception.Create('Do not ever unload image of unsaved image!');
  end;
  // TODO(sergey): Maybe instead keep image but only free it's picture, so
  // we can save some special flags?
  if image <> nil then begin
    FreeAndNil(image);
  end;
end;

function TModelItem.getImagePicture(): TPicture;
begin
  if image <> nil then begin
    result := image.picture;
  end else begin
    result := nil;
  end;
end;

function TModelItem.getImagePictureData(): TBytes;
var stream: TBytesStream;
begin
  // Nothing if image is empty.
  if image = nil then begin
    result := nil;
    exit;
  end;
 // Save data to a bytes stream.
 stream := TBytesStream.Create();
 image.picture.SaveToStream(stream);
 // Copy result.
 // TODO(sergey): This duplicates image in memory.
 SetLength(result, stream.Size);
 Move(stream.Bytes[0], result[0], stream.Size);
 // Free all the memory.
 stream.Free();
end;

function TModelItem.getCurrencyID(): integer;
begin
 if currency <> nil then begin
   result := currency.id;
 end else begin
   result := -1;
 end;
end;

function TModelItem.isDone(): boolean;
begin
 result := status = TMIS_DONE;
end;

function TModelItem.isCancelled(): boolean;
begin
 result := status = TMIS_CANCELLED;
end;

function TModelItem.isPaid(): boolean;
begin
 result := (status = TMIS_ORDERED) or
           (status = TMIS_SENT) or
           (status = TMIS_DELIVERED) or
           (status = TMIS_DONE);
end;

////////////////////////////////////////////////////////////////////////////////
// Project implementation

constructor TModelProject.Create();
begin
  id := -1;  // Indicates that project is not yet in the database.
  icon := nil;
  items := TModelItemList.Create();
  projects := TModelProjectList.Create();
end;

destructor TModelProject.Destroy();
var
    item: TModelItem;
    project: TModelProject;
begin
  // Free all items.
  for item in items do begin
    item.Free();
  end;
  items.Free();
  // Free all nested projects.
  for project in projects do begin
    project.Free();
  end;
  projects.Free();
end;

function TModelProject.getParentID(): integer;
begin
  if parent <> nil then begin
    result := parent.id;
  end else begin
    result := -1;
  end;
end;

function TModelProject.getTotalProjectCost(model: TModel;
                                           currency: TModelCurrency) : Double;
var
  i: integer;
  item: TModelItem;
  project: TModelProject;
begin
  result := 0.0;
  for i := 0 to items.Count - 1 do begin
    item := TModelItem(items[i]);
    if item.isCancelled() then begin
      continue;
    end;
    if item.currency <> nil then begin
      result += item.currency.convertTo(model, item.price, currency);
    end;
  end;
  for project in projects do begin
    result += project.getTotalProjectCost(model, currency);
  end;
end;

function TModelProject.getRemainingProjectCost(model: TModel;
                                               currency: TModelCurrency) : Double;
var
  i: integer;
  item: TModelItem;
  project: TModelProject;
begin
  result := 0.0;
  for i := 0 to items.Count - 1 do begin
    item := TModelItem(items[i]);
    if item.isPaid() then begin
      continue;
    end;
    if item.currency <> nil then begin
      result += item.currency.convertTo(model, item.price, currency);
    end;
  end;
  for project in projects do begin
    result += project.getRemainingProjectCost(model, currency);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Model implementation

constructor TModel.Create(database: TBaseDatabase);
begin
  inherited Create;
  currencies_ := TModelCurrencyList.Create();
  projects_ := TModelProjectList.Create();
  schema_ := TModelSchema.Create();
  database_ := database;
  // Make sure database is ready to be used.
  schema_.prepareDatabase(database_);
  // Load all existing entities from the database.
  loadDatabase();
end;

destructor TModel.Destroy();
var currency: TModelCurrency;
    project: TModelProject;
begin
  for currency in currencies_ do begin
    currency.Free();
  end;
  for project in projects_ do begin
    project.Free();
  end;
  projects_.Free();
  currencies_.Free();
  schema_.Free();
end;

///////////////////////////////////////
// Currency manipulation.

function TModel.getAllCurrencies(): TModelCurrencyList;
begin
  result := currencies_;
end;

function TModel.getCurrencyByID(id: integer): TModelCurrency;
var currency: TModelCurrency;
begin
  result := nil;
  for currency in currencies_ do begin
    if currency.id = id then begin
      result := currency;
      break;
    end;
  end;
end;

function TModel.getCurrencyBySign(sign: string): TModelCurrency;
var currency: TModelCurrency;
begin
  result := nil;
  for currency in currencies_ do begin
    if currency.sign = sign then begin
      result := currency;
      break;
    end;
  end;
end;

function TModel.getCurrencyByCode(code: string): TModelCurrency;
var currency: TModelCurrency;
    split_codes: TStringList;
    i: integer;
    current_code: string;
begin
  result := nil;
  split_codes := TStringList.Create();
  for currency in currencies_ do begin
    split_codes.Clear();
    split_codes.Delimiter := ';';
    split_codes.StrictDelimiter := true;
    split_codes.DelimitedText := currency.code;
    for i := 0 to split_codes.Count - 1 do begin
      current_code := split_codes.Strings[i];
      if current_code = code then begin
        split_codes.Free();
        result := currency;
        exit;
      end;
    end;
  end;
  split_codes.Free();
end;

function TModel.getCurrencyByGuess(value: string): TModelCurrency;
var currency: TModelCurrency;
begin
  // Try to guess as sign.
  currency := getCurrencyBySign(value);
  if currency <> nil then begin
    result := currency;
    exit;
  end;
  // Try to guess as code.
  currency := getCurrencyByCode(value);
  if currency <> nil then begin
    result := currency;
    exit;
  end;
  // We've failed...
  result := nil;
end;

function TModel.getCurrencyIndex(currency: TModelCurrency): integer;
begin
  result := currencies_.IndexOf(currency);
end;

function TModel.getCurrencyConversionMatrix(): TModelCurrencyConversionMatrix;
var
    matrix: TModelCurrencyConversionMatrix;
    currency: TModelCurrency;
    i, j, k, n: integer;
    is_better: boolean;
    new_ratio: double;

  function distanceToOne(value: double): double;
  begin
    result := abs(1.0 - value);
  end;

begin
  n := currencies_.Count;
  // Allocate memory for the matrix.
  SetLength(matrix, n, n);
  // Forbidden/unkown conversion is denoted by -1.
  for i := 0 to n - 1 do begin
    for j := 0 to n - 1 do begin
      matrix[i, j] := -1;
    end;
  end;
  // Fill in diagonal of the matrix, conversion to self is always free.
  for i := 0 to n - 1 do begin
    matrix[i, i] := 1;
  end;
  // Fill conversion based on a reference currency.
  for currency in currencies_ do begin
    if currency.reference = nil then begin
      continue;
    end;
    if currency.value <> 0 then begin
      i := getCurrencyIndex(currency);
      j := getCurrencyIndex(currency.reference);
      matrix[j, i] := currency.value;
      matrix[i, j] := 1 / currency.value;
    end
  end;
  // Create conversion matrix.
  for i := 0 to n - 1 do begin
    for j := 0 to n - 1 do begin
      for k := 0 to n - 1 do begin
        if (matrix[i, k] = -1) or (matrix[k ,j] = -1) then begin
          // No conversion in at least one of transitive step, can not use k
          // to optimize conversion out.
          continue;
        end;
        new_ratio := matrix[i, k] * matrix[k, j];
        // There is no conversion from i to j, but there is ANY conversion
        // from i to k and from k to j. Can simply use that, still better
        // than nothing!
        is_better := (matrix[i, j] = -1);
        // Or alternatively, use better conversion from i to j via k.
        // We try to keep conversion coefficient as close to 1 as possible.
        is_better := is_better or
            (distanceToOne(matrix[i, j]) > distanceToOne(new_ratio));
        // Store updated value, if needed.
        if is_better then begin
          matrix[i, j] := new_ratio;
        end;
      end;
    end;
  end;
  // Set result.
  result := matrix;
end;

procedure TModel.addCurrency(const currency: TModelCurrency);
begin
  // Add currency to model itself.
  currencies_.Add(currency);
  // TODO(sergey): Sort currencies?
  // Add currency to the database.
  database_.SQL.Text :=
          'INSERT INTO currency ' +
          '(name, code, sign, value, reference_id) ' +
          'VALUES(:name, :code, :sign, :value, :reference_id)';
  database_.Params.ParamByName('name').AsString := currency.name;
  database_.Params.ParamByName('code').AsString := currency.code;
  database_.Params.ParamByName('sign').AsString := currency.sign;
  database_.Params.ParamByName('value').AsFloat := currency.value;
  database_.Params.ParamByName('reference_id').AsInteger :=
      currency.getReferenceID();
  database_.executeQuery();
  currency.id := database_.lastInsertID();
  // Inform interface about changes.
  if assigned(on_currency_add_) then begin
    on_currency_add_(self, currency);
  end;
end;

procedure TModel.editCurrency(const currency: TModelCurrency);
begin
  // Modify/verify project itself.
  verifyCurrencyIsInModel(currency);
  // Update database.
  database_.SQL.Text := 'UPDATE currency SET ' +
                        'name = :name, ' +
                        'code = :code, ' +
                        'sign = :sign, ' +
                        'value = :value, ' +
                        'reference_id = :reference_id ' +
                        'WHERE id=:id';
  database_.Params.ParamByName('id').AsInteger := currency.id;
  database_.Params.ParamByName('name').AsString := currency.name;
  database_.Params.ParamByName('code').AsString := currency.code;
  database_.Params.ParamByName('sign').AsString := currency.sign;
  database_.Params.ParamByName('value').AsFloat := currency.value;
  database_.Params.ParamByName('reference_id').AsInteger :=
      currency.getReferenceID();
  database_.executeQuery();
  // Inform interface about changes.
  if assigned(on_currency_edit_) then begin
    on_currency_edit_(self, currency);
  end;
end;

procedure TModel.deleteCurrency(const currency: TModelCurrency);
begin
  // Modify/verify project itself.
  verifyCurrencyIsInModel(currency);
  // Update database.
  database_.SQL.Text := 'DELETE FROM currency WHERE id=:id';
  database_.Params.ParamByName('id').AsInteger := currency.id;
  database_.executeQuery();
  // Inform interface about changes.
  if assigned(on_currency_delete_) then begin
    on_currency_delete_(self, currency);
  end;
  // Free memory used by project.
  currency.Free();
end;

///////////////////////////////////////
// Project manipulation.

function TModel.getAllProjects(): TModelProjectList;
begin
  result := projects_;
end;

procedure TModel.addProject(const project: TModelProject);
begin
  // Add project to model itself.
  if project.parent <> nil then begin
    project.parent.projects.Add(project);
  end else begin
    projects_.Add(project);
  end;
  // TODO(sergey): Sort projects?
  // Add project to the database.
  database_.SQL.Text := 'INSERT INTO project (parent_id, name) ' +
                        'VALUES(:parent_id, :name)';
  database_.Params.ParamByName('parent_id').AsInteger := project.getParentID();
  database_.Params.ParamByName('name').AsString := project.name;
  database_.executeQuery();
  project.id := database_.lastInsertID();
  // Inform interface about changes.
  if assigned(on_project_add_) then begin
    on_project_add_(self, project);
  end;
end;

procedure TModel.editProject(const project: TModelProject);
  function findOldParent(const project: TModelProject): TModelProject;
    function findParentRecurse(const projects: TModelProjectList;
                               const project: TModelProject): TModelProject;
    var
        current_project: TModelProject;
        current_parent: TModelProject;
    begin
      result := nil;
      for current_project in projects do begin
        // Check if project could be found in the current project's subprojects.
        if current_project.projects.IndexOf(project) <> -1 then begin
          result := current_project;
          exit;
        end;
        // Check deeper.
        current_parent := findParentRecurse(current_project.projects, project);
        if current_parent <> nil then begin
          result := current_parent;
          exit;
        end;
      end;
    end;
  begin
    result := findParentRecurse(projects_, project);
  end;

var
    old_parent: TModelProject;

begin
  // Modify/verify project itself.
  verifyProjectIsInModel(project);
  // Make sure hierarchy is properly updated.
  old_parent := findOldParent(project);
  if old_parent <> project.parent then begin
    // Remove project from old parent's subprojects.
    if old_parent <> nil then begin
      old_parent.projects.Remove(project);
    end else begin
      projects_.Remove(project);
    end;
    // Add project to a new parent.
    if project.parent <> nil then begin
      project.parent.projects.Add(project);
    end else begin
      projects_.Add(project);
    end;
    // TODO(sergey): Sort?
  end;
  // Update database.
  database_.SQL.Text := 'UPDATE project SET parent_id=:parent_id, ' +
                        'name=:name WHERE id=:id';
  database_.Params.ParamByName('id').AsInteger := project.id;
  database_.Params.ParamByName('parent_id').AsInteger := project.getParentID();
  database_.Params.ParamByName('name').AsString := project.name;
  database_.executeQuery();
  // Inform interface about changes.
  if assigned(on_project_edit_) then begin
    on_project_edit_(self, project);
  end;
end;

procedure TModel.deleteProject(const project: TModelProject);

  procedure deleteProjectRecurse(project: TModelProject);
  var
      current_project: TModelProject;
  begin
    // Delete all children.
    for current_project in project.projects do begin
      deleteProjectRecurse(current_project);
    end;
    // Delete from database.
    database_.SQL.Text := 'DELETE FROM project WHERE id=:id';
    database_.Params.ParamByName('id').AsInteger := project.id;
    database_.executeQuery();
  end;

begin
  // Modify/verify project itself.
  verifyProjectIsInModel(project);
  // Remove from the parent.
  if project.parent <> nil then begin
    project.parent.projects.Remove(project);
  end else begin
    projects_.Remove(project);
  end;
  // Recursively delete all nested projects.
  deleteProjectRecurse(project);
  // Inform interface about changes.
  if assigned(on_project_delete_) then begin
    on_project_delete_(self, project);
  end;
  // Free memory used by project.
  project.Free();
end;

///////////////////////////////////////
// Item manipulation.

procedure TModel.addItem(const project: TModelProject;
                         const item: TModelItem);
var image_data: TBytes;
begin
  // Add item to model itself.
  project.items.Add(item);
  // TODO(sergey): Sort items?
  // Dump image to a string.
  image_data := item.getImagePictureData();
  // Add project to the database.
  database_.SQL.Text := 'INSERT INTO item ' +
      '(project_id, name, description, image, price, currency_id, url, ' +
       'status) ' +
      'VALUES(:project_id, :name, :description, :image, :price, ' +
             ':currency_id, :url, :status)';
  database_.Params.ParamByName('project_id').AsInteger := project.id;
  database_.Params.ParamByName('name').AsString := item.name;
  database_.Params.ParamByName('description').AsString := item.description;
  database_.Params.ParamByName('image').AsBytes := image_data;
  database_.Params.ParamByName('price').AsFloat := item.price;
  database_.Params.ParamByName('currency_id').AsInteger :=
      item.getCurrencyID();
  database_.Params.ParamByName('url').AsString := item.url;
  database_.Params.ParamByName('status').AsInteger := Integer(item.status);
  database_.executeQuery();
  item.id := database_.lastInsertID();
  // Inform interface about changes.
  if assigned(on_item_add_) then begin
    on_item_add_(self, project, item);
  end;
end;

procedure TModel.editItem(const project: TModelProject;
                          const item: TModelItem);
var image_data: TBytes;
begin
  // Modify/verify item itself.
  verifyItemIsInModel(project, item);
  // Dump image to a string.
  image_data := item.getImagePictureData();
  // Update database.
  database_.SQL.Text := 'UPDATE item SET ' +
                        'name=:name, ' +
                        'description=:description, ' +
                        'image=:image, ' +
                        'price=:price, ' +
                        'currency_id=:currency_id, ' +
                        'url=:url, ' +
                        'status=:status ' +
                        'WHERE id=:id';
  database_.Params.ParamByName('id').AsInteger := item.id;
  database_.Params.ParamByName('name').AsString := item.name;
  database_.Params.ParamByName('description').AsString := item.description;
  database_.Params.ParamByName('image').AsBytes := image_data;
  database_.Params.ParamByName('price').AsFloat := item.price;
  database_.Params.ParamByName('currency_id').AsInteger :=
      item.getCurrencyID();
  database_.Params.ParamByName('url').AsString := item.url;
  database_.Params.ParamByName('status').AsInteger := Integer(item.status);
  database_.executeQuery();
  // Inform interface about changes.
  if assigned(on_item_edit_) then begin
    on_item_edit_(self, project, item);
  end;
end;

procedure TModel.deleteItem(const project: TModelProject;
                            const item: TModelItem);
begin
  // Modify/verify item itself.
  verifyItemIsInModel(project, item);
  project.items.Remove(item);
  // Update database.
  database_.SQL.Text := 'DELETE FROM item WHERE id=:id';
  database_.Params.ParamByName('id').AsInteger := item.id;
  database_.executeQuery();
  // Inform interface about changes.
  if assigned(on_item_delete_) then begin
    on_item_delete_(self, project, item);
  end;
  // Free memory used by item.
  item.Free();
end;

procedure TModel.expandItem(item: TModelItem);
begin
  item.loadImageFromDatabase(database_);
end;

procedure TModel.unexpandItem(item: TModelItem);
begin
  item.unloadImage();
end;

///////////////////////////////////////
// Load from database.

procedure TModel.loadDatabase();
begin
  loadCurrencies();
  loadProjects();
end;

procedure TModel.loadCurrencies();
var currency: TModelCurrency;
    ref_ids: array of integer;
    index: integer;
begin
  // First we read all information about currencies except of setting pointer
  // to their reference currency.
  database_.SQL.Text := 'SELECT * FROM currency ORDER BY NAME';
  database_.openQuery();
  SetLength(ref_ids, database_.queryRecordCount());
  index := 0;
  while not database_.queryEof() do begin
    currency := TModelCurrency.Create();
    currency.id := database_.fieldByName('id').AsInteger;
    currency.name := database_.fieldByName('name').AsString;
    currency.sign := database_.fieldByName('sign').AsString;
    currency.code := database_.fieldByName('code').AsString;
    currency.value := database_.fieldByName('value').AsFloat;
    ref_ids[index] := database_.fieldByName('reference_id').AsInteger;
    currencies_.Add(currency);
    database_.queryNextRow();
    inc(index);
  end;
  database_.closeQuery();
  // Now when we know all pointers we can solve integer ID to a proper
  // reference.
  index := 0;
  for currency in currencies_ do begin
    if ref_ids[index] = -1 then begin
      currency.reference := nil;
      inc(index);
      continue;
    end;
    currency.reference := getCurrencyByID(ref_ids[index]);
    assert(currency.reference <> nil);
    inc(index);
  end;
end;

procedure TModel.loadProjects();
begin
  loadProjects(projects_, nil);
end;

procedure TModel.loadProjects(var projects: TModelProjectList;
                              parent: TModelProject);
var
    project: TModelProject;
    parent_id: integer;
begin
  if parent <> nil then begin
    parent_id := parent.id;
  end else begin
    parent_id := -1;
  end;
  // Load all current level projects first.
  database_.SQL.Text := 'SELECT * FROM project WHERE parent_id=:parent_id ' +
                        'ORDER BY NAME';
  database_.Params.ParamByName('parent_id').AsInteger := parent_id;
  database_.openQuery();
  while not database_.queryEof() do begin
    project := TModelProject.Create();
    project.id := database_.fieldByName('id').AsInteger;
    project.parent := parent;
    project.name := database_.fieldByName('name').AsString;
    projects.Add(project);
    database_.queryNextRow();
  end;
  database_.closeQuery();
  // Recursively expand projects, includes both items and subprojects.
  loadProjectsExpand(projects);
end;

procedure TModel.loadProjectsExpand(var projects: TModelProjectList);
var i: integer;
    project: TModelProject;
begin
  for i := 0 to projects.Count - 1 do begin
    project := projects.Items[i];
    loadProjectExpand(project);
  end;
end;

procedure TModel.loadProjectExpand(var project: TModelProject);
var item: TModelItem;
begin
  // Load all items first.
  database_.SQL.Text := 'SELECT id, name, description, price, currency_id, ' +
                               'url, status ' +
                        'FROM item WHERE project_id=:project_id ' +
                        'ORDER BY NAME';
  database_.Params.ParamByName('project_id').AsInteger := project.id;
  database_.openQuery();
  while not database_.queryEof() do begin
    item := TModelItem.Create();
    item.id := database_.fieldByName('id').AsInteger;
    item.name := database_.fieldByName('name').AsString;
    item.description := database_.fieldByName('description').AsString;
    item.price := database_.fieldByName('price').AsFloat;
    item.currency := getCurrencyByID(database_.fieldByName(
        'currency_id').AsInteger);
    item.url := database_.fieldByName('url').AsString;
    item.status := TModelItemStatus(database_.fieldByName('status').AsInteger);
    project.items.Add(item);
    database_.queryNextRow();
  end;
  database_.closeQuery();
  // Load all nested projects.
  loadProjects(project.projects, project);
end;

///////////////////////////////////////
// Validation and verification.

procedure TModel.verifyCurrencyIsInModel(const currency: TModelCurrency);
begin
  if currencies_.IndexOf(currency) = -1 then begin
    raise Exception.Create('Attempt to save currency which is not in the model');
  end;
end;

procedure TModel.verifyProjectIsInModel(const project: TModelProject);
  function checkProjectInModel(const project: TModelProject): boolean;
    function checkProjectInModelRecurse(const projects: TModelProjectList;
                                        const project: TModelProject): boolean;
    var
        current_project: TModelProject;
    begin
      result := false;
      for current_project in projects do begin
        if current_project = project then begin
          result := true;
          exit;
        end;
        if checkProjectInModelRecurse(current_project.projects,
                                      project) then begin
          result := true;
          exit;
        end;
      end;
    end;
  begin
    result := checkProjectInModelRecurse(projects_, project);
  end;
begin
  if not checkProjectInModel(project) then begin
    raise Exception.Create('Attempt to save project which is not in the model');
  end;
end;

procedure TModel.verifyItemIsInModel(const project: TModelProject;
                                     const item: TModelItem);
begin
  verifyProjectIsInModel(project);
  if project.items.IndexOf(item) = -1 then begin
    raise Exception.Create('Attempt to save non-existing item');
  end;
end;

///////////////////////////////////////
// Properties writers.

// Currency.
procedure TModel.setOnCurrencyAdd(value: TModelCurrencyNotifyEvent);
begin
  on_currency_add_ := value;
end;

procedure TModel.setOnCurrencyEdit(value: TModelCurrencyNotifyEvent);
begin
  on_currency_edit_ := value;
end;

procedure TModel.setOnCurrencyDelete(value: TModelCurrencyNotifyEvent);
begin
  on_currency_delete_ := value;
end;

// Project.
procedure TModel.setOnProjectAdd(value: TModelProjectNotifyEvent);
begin
  on_project_add_ := value;
end;

procedure TModel.setOnProjectEdit(value: TModelProjectNotifyEvent);
begin
  on_project_edit_ := value;
end;

procedure TModel.setOnProjectDelete(value: TModelProjectNotifyEvent);
begin
  on_project_delete_ := value;
end;

// Item.
procedure TModel.setOnItemAdd(value: TModelItemNotifyEvent);
begin
  on_item_add_ := value;
end;

procedure TModel.setOnItemEdit(value: TModelItemNotifyEvent);
begin
  on_item_edit_ := value;
end;

procedure TModel.setOnItemDelete(value: TModelItemNotifyEvent);
begin
  on_item_delete_ := value;
end;

end.

