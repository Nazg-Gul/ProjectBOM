// Copyright (c) 2017, ProjectBOM authors

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.

// Author: Sergey Sharybin (sergey.vfx@gmail.com)

unit item_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Spin, model, item_fetcher, item_fetcher_ebay, LCLType,
  util;

type

  { TItemForm }

  TItemForm = class(TForm)
    itemStatusComboBox: TComboBox;
    imageFetchButton: TButton;
    imageClearButton: TButton;
    itemCurrencyComboBox: TComboBox;
    itemPriceEdit: TFloatSpinEdit;
    itemImage: TImage;
    itemImageLocalRadioButton: TRadioButton;
    itemImageRemoteRadioButton: TRadioButton;
    itemImagePathEdit: TEdit;
    imageShowFileSelectorButton: TBitBtn;
    itemFetchButton: TBitBtn;
    itemNameEdit: TEdit;
    itemPriceLabel: TLabel;
    itemCurrencyLabel: TLabel;
    itemURLEdit: TEdit;
    itemNameLabel: TLabel;
    itemImageLabel: TLabel;
    itemDescriptionLabel: TLabel;
    itemDescriptionMemo: TMemo;
    itemURLLabel: TLabel;
    itemCancelButton: TBitBtn;
    itemConfirmButton: TBitBtn;
    itemStatusLabel: TLabel;
    openDialog: TOpenDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure imageClearButtonClick(Sender: TObject);
    procedure imageFetchButtonClick(Sender: TObject);
    procedure itemImageLocalRadioButtonChange(Sender: TObject);
    procedure itemImageRemoteRadioButtonChange(Sender: TObject);
    procedure imageShowFileSelectorButtonClick(Sender: TObject);
    procedure itemFetchButtonClick(Sender: TObject);
  private
    // This is a model within which item manipulation is happening.
    // Supposed to be set by the parent form.
    model_: TModel;

    // This is a project item belongs to.
    project_: TModelProject;

    // This is a item which is being edited, If it's Nil, then form will
    // create new item in model on confirm, otherwise it will update this
    // item in model.
    item_: TModelItem;

    // Deinitialization of create/edit process.
    procedure endCreateOrEditItem();

    // Interface update callbacks.
    procedure updateImageSourceType();

    // Check whether input is a valid image properties.
    function checkInputValid() : Boolean;

    procedure fillItemFromForm(var item: TModelItem);

    procedure fillPossibleItemStatus();
    function getSelectedItemStatus(): TModelItemStatus;
    procedure selectStatus(status: TModelItemStatus);

    procedure fillPossibleCurrencies(model: TModel);
    function getSelectedCurrency(): TModelCurrency;
    procedure selectCurrency(currency: TModelCurrency);

  public
    // Constructors.
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;
                       model: TModel;
                       project: TModelProject); reintroduce; overload;
    constructor Create(TheOwner: TComponent;
                       model: TModel;
                       project: TModelProject;
                       item: TModelItem); reintroduce; overload;

    // Initialization of create/edit process.
    procedure beginCreateItem(model: TModel; project: TModelProject);
    procedure beginEditItem(model: TModel;
                            project: TModelProject;
                            item: TModelItem);
  end;

var
  ItemForm: TItemForm;

implementation

{$R *.lfm}

////////////////////////////////////////////////////////////////////////////////
// Constructor.

constructor TItemForm.Create(TheOwner: TComponent);
begin
  // TODO(sergey): This might be handy to not have model, for some debug
  // perhaps? Or shall we forbid creating form without model at all?
  inherited Create(TheOwner);
  fillPossibleItemStatus();
  fillPossibleCurrencies(nil);
  model_ := Nil;
  item_ := Nil;
end;

constructor TItemForm.Create(TheOwner: TComponent;
                             model: TModel;
                             project: TModelProject);
begin
  inherited Create(TheOwner);
  fillPossibleItemStatus();
  fillPossibleCurrencies(nil);
  beginCreateItem(model, project);
end;

constructor TItemForm.Create(TheOwner: TComponent;
                             model: TModel;
                             project: TModelProject;
                             item: TModelItem);
begin
  inherited Create(TheOwner);
  fillPossibleItemStatus();
  fillPossibleCurrencies(model);
  beginEditItem(model, project, item);
end;

////////////////////////////////////////////////////////////////////////////////
// Interface callbacks.

procedure TItemForm.FormClose(Sender: TObject;
                              var CloseAction: TCloseAction);
var new_item: TModelItem;
begin
  // Check whether we need/can send new/edited item to model.
  if (ModalResult <> mrOk) or (model_ = Nil) then
  begin
    endCreateOrEditItem();
    exit;
  end;
  if not checkInputValid() then begin
    CloseAction := caNone;;
    exit;
  end;
  // Submit item to model.
  if item_ = Nil then
  begin
    // Create new item and add it to the model.
    // TODO(sergey): Which project it belongs to?
    new_item := TModelItem.Create();
    fillItemFromForm(new_item);
    model_.addItem(project_, new_item);
  end else begin
    fillItemFromForm(item_);
    model_.editItem(project_, item_);
  end;
  // Clear the mess after us.
  endCreateOrEditItem();
end;

procedure TItemForm.imageClearButtonClick(Sender: TObject);
begin
  itemImage.Picture.Clear();
end;

procedure TItemForm.imageFetchButtonClick(Sender: TObject);
var image_filepath: string;
begin
  image_filepath := fetchFileToTemp(itemImagePathEdit.Text);
  if image_filepath <> '' then begin
    itemImage.Picture.LoadFromFile(image_filepath);
    DeleteFile(image_filepath);
  end else begin
    Application.MessageBox('Error fetching image from internet.',
                           'Error',
                           MB_ICONERROR)
  end;
end;

procedure TItemForm.itemImageLocalRadioButtonChange(Sender: TObject);
begin
  updateImageSourceType();
end;

procedure TItemForm.itemImageRemoteRadioButtonChange(Sender: TObject);
begin
  updateImageSourceType();
end;

procedure TItemForm.imageShowFileSelectorButtonClick(Sender: TObject);
begin
  if openDialog.Execute then begin
    try
      itemImage.Picture.LoadFromFile(openDialog.Filename);
    except
      on EInvalidGraphic do begin
        Application.MessageBox('Error loading image file.',
                               'Error',
                               MB_ICONERROR);
      end;
    end;
  end;
end;

procedure TItemForm.itemFetchButtonClick(Sender: TObject);
var fetcher: TItemFetcher;
    item: TModelItem;
    image_picture: TPicture;
begin
  fetcher := TItemFetcher.createForSource(model_, itemURLEdit.Text);
  if fetcher = nil then begin
    Application.MessageBox('No idea how to fetch given reosurce.',
                           'Error',
                           MB_ICONERROR);
    exit;
  end;
  screen.Cursor := crHourGlass;
  item := TModelItem.Create();
  if fetcher.fetch(itemURLEdit.Text, item) then begin
    // Name.
    if (itemNameEdit.Text = '') and (item.name <> '') then begin
      itemNameEdit.Text := item.name;
    end;
    // Price.
    if (itemPriceEdit.Value = 0) and (item.price <> 0) then begin
      itemPriceEdit.Value := item.price;
      selectCurrency(item.currency);
    end;
    // Image.
    if itemImage.Picture.Height = 0 then begin
      image_picture := item.getImagePicture();
      if image_picture <> nil then begin
        itemImage.Picture.Assign(image_picture);
      end;
    end;
  end;
  FreeAndNil(item);
  fetcher.Free();
  screen.Cursor := crDefault;
end;

////////////////////////////////////////////////////////////////////////////////
// Interface update callbacks.

procedure TItemForm.updateImageSourceType();
begin
  // TODO(sergey): Avoid multiple updates (happens once per radio button).
  if itemImageLocalRadioButton.Checked then begin
    imageShowFileSelectorButton.Enabled := True;
    imageFetchButton.Enabled := False;
  end else if itemImageRemoteRadioButton.Checked then begin
    imageShowFileSelectorButton.Enabled := False;
    imageFetchButton.Enabled := True;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Public API to contro lwhether we edit or create project. Handy to avoid
// allocating new for mfor each action.

procedure TItemForm.beginCreateItem(model: TModel; project: TModelProject);
begin
  // Window title.
  caption := 'Create item';
  // Set internal variables.
  model_ := model;
  project_ := project;
  item_ := Nil;
  // Reset interface to the new create state.
  itemNameEdit.text := '';
  itemDescriptionMemo.Lines.Text := '';
  itemPriceEdit.Value := 0.0;
  itemCurrencyComboBox.Text := '';
  itemURLEdit.text := '';
  itemImageLocalRadioButton.Checked := true;
  itemImagePathEdit.text := '';
  itemImage.Picture.Clear();
  selectStatus(TMIS_NONE);
  selectCurrency(nil);
end;

procedure TItemForm.beginEditItem(model: TModel;
                                  project: TModelProject;
                                  item: TModelItem);
begin
  if item = Nil then begin
    beginCreateItem(model, project);
    exit;
  end;
  // Window title.
  caption := 'Edit itrm';
  // Set internal variables.
  model_ := model;
  project_ := project;
  item_ := item;
  // Reset interface to the new create state.
  itemNameEdit.text := item.name;
  itemDescriptionMemo.Lines.Text := item.description;
  itemPriceEdit.Value := item.price;
  itemCurrencyComboBox.Text := '';
  itemURLEdit.text := item.url;
  // TODO(sergey): What to initialize image info to?
  itemImageLocalRadioButton.Checked := true;
  itemImagePathEdit.text := '';
  itemImage.Picture.Assign(item.getImagePicture());
  selectStatus(item.status);
  selectCurrency(item.currency);
end;

procedure TItemForm.endCreateOrEditItem();
begin
  model_ := Nil;
  project_ := Nil;
  item_ := Nil;
end;

function TItemForm.checkInputValid() : Boolean;
begin
  result := True;
  if itemNameEdit.Text = '' then begin
    result := False;
    // TODO(sergey): Use Application.MessageBox.
    MessageDlg('Item name can not be empty.', mtError, [mbOk], 0);
  end;
  // TODO(sergey): Check uniqness of the peoject name?
end;

procedure TItemForm.fillItemFromForm(var item: TModelItem);
begin
  item.name := itemNameEdit.text;
  item.description := itemDescriptionMemo.Lines.Text;
  item.price := itemPriceEdit.Value;
  item.url := itemURLEdit.Text;
  item.loadImageFromPicture(itemImage.Picture);
  item.currency := getSelectedCurrency();
  item.status := getSelectedItemStatus();
end;

////////////////////////////////////////////////////////////////////////////////
// Item status.

procedure TItemForm.fillPossibleItemStatus();
var i, num_status: integer;
    status: TModelItemStatus;
    status_name: string;
begin
  itemStatusComboBox.Items.BeginUpdate();
  itemStatusComboBox.Items.Clear;
  num_status := getNumItemStatus();
  for i := 0 to num_status do begin
    status := TModelItemStatus(i);
    status_name := getItemnStatusName(status);
    itemStatusComboBox.AddItem(status_name, TObject(PtrUint(i)));
  end;
  itemStatusComboBox.Items.EndUpdate();
end;

function TItemForm.getSelectedItemStatus(): TModelItemStatus;
var index: integer;
    current_status_code: PtrUint;
begin
  index := itemStatusComboBox.ItemIndex;
  current_status_code := PtrUint(itemStatusComboBox.Items.Objects[index]);
  result := TModelItemStatus(current_status_code);
end;

procedure TItemForm.selectStatus(status: TModelItemStatus);
var i: integer;
    current_status_code: PtrUint;
    current_status: TModelItemStatus;
begin
  for i := 0 to itemStatusComboBox.Items.Count - 1 do begin
    current_status_code := PtrUint(itemStatusComboBox.Items.Objects[i]);
    current_status := TModelItemStatus(current_status_code);
    if current_status = status then begin
      itemStatusComboBox.ItemIndex := i;
      break;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Currency.

procedure TItemForm.fillPossibleCurrencies(model: TModel);
var currency: TModelCurrency;
    currency_list: TModelCurrencyList;
    currency_name: string;
begin
  itemCurrencyComboBox.Items.BeginUpdate();
  itemCurrencyComboBox.Items.Clear;
  if model <> nil then begin
    currency_list := model.getAllCurrencies();
    itemCurrencyComboBox.AddItem('<UNKNOWN>', nil);
    for currency in currency_list do begin
      currency_name := currency.code + ' - ' + currency.sign;
      itemCurrencyComboBox.AddItem(currency_name, currency);
    end;
  end;
  itemCurrencyComboBox.Items.EndUpdate();
end;

function TItemForm.getSelectedCurrency(): TModelCurrency;
var index: integer;
begin
  index := itemCurrencyComboBox.ItemIndex;
  result := TModelCurrency(itemCurrencyComboBox.Items.Objects[index]);
end;

procedure TItemForm.selectCurrency(currency: TModelCurrency);
var i: integer;
    current_currency: TModelCurrency;
begin
  for i := 0 to itemCurrencyComboBox.Items.Count - 1 do begin
    current_currency := TModelCurrency(itemCurrencyComboBox.Items.Objects[i]);
    if current_currency = currency then begin
      itemCurrencyComboBox.ItemIndex := i;
      break;
    end;
  end;
end;

end.

