unit currency_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListViewFilterEdit, ListFilterEdit, LvlGraphCtrl,
  Forms, Controls, Graphics, Dialogs, Buttons, DbCtrls, DBGrids, Grids,
  StdCtrls, ExtCtrls, Spin, model;

type

  { TCurrencyForm }

  TCurrencyForm = class(TForm)
    currencyCide: TEdit;
    currencySign: TEdit;
    currencyValueLabel1: TLabel;
    deleteButton: TButton;
    cancelButton: TButton;
    currencyName: TEdit;
    editPanel: TPanel;
    currencyValue: TFloatSpinEdit;
    currencyValueLabel: TLabel;
    currencyReference: TComboBox;
    formConfirmButton: TBitBtn;
    nameLabel: TLabel;
    signLabel: TLabel;
    codeLabel: TLabel;
    saveButton: TButton;
    insertButton: TButton;
    editButton: TButton;
    currencyGrid: TStringGrid;
    procedure cancelButtonClick(Sender: TObject);
    procedure currencyGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure deleteButtonClick(Sender: TObject);
    procedure editButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure insertButtonClick(Sender: TObject);
    procedure saveButtonClick(Sender: TObject);
  private
    model_: TModel;
    currency_: TModelCurrency;

    procedure fillExistingCurrencies();
    procedure fillGridRowFromCurrency(row_index: integer;
                                      currency: TModelCurrency);
    procedure checkButtonsEnabled();
    procedure enterInsertEditState();
    procedure exitInsertEditState();
    procedure fillEditFormFromCurrentCurrency();
    procedure fillEditFormFromCurrency(const currency: TModelCurrency);
    procedure fillCurrencyFromEditForm(const currency: TModelCurrency);
    function getCurrentCurrency(): TModelCurrency;
    procedure fillCurrenciesEditComboBox();
    procedure selectCurrencyInEditComboBox(const currency: TModelCurrency);
  public
    constructor Create(the_owner: TComponent; model: TModel); overload;
  end;

var
  CurrencyForm: TCurrencyForm;

implementation

{$R *.lfm}

procedure TCurrencyForm.FormCreate(Sender: TObject);
begin
  currency_ := nil;
  checkButtonsEnabled();
  exitInsertEditState();
end;

procedure TCurrencyForm.FormDestroy(Sender: TObject);
begin
  if (currency_ <> nil) and (currency_.id = -1) then begin
    FreeAndNil(currency_);
  end;
end;

procedure TCurrencyForm.FormShow(Sender: TObject);
begin
  // NOTE: Can not do this in Create() because there is some weird SIGSEGV
  // happening.
  fillExistingCurrencies();
  checkButtonsEnabled();
  fillEditFormFromCurrentCurrency();
end;

procedure TCurrencyForm.insertButtonClick(Sender: TObject);
begin
  currency_ := TModelCurrency.Create();
  fillEditFormFromCurrency(currency_);
  enterInsertEditState();
end;

procedure TCurrencyForm.saveButtonClick(Sender: TObject);
var new_grid_index: integer;
begin
  // TODO(sergey): There are various of things to be checked here:
  // - Check there is no duplicate use of currency CODE.
  // - Check there are no cyclic references.
  fillCurrencyFromEditForm(currency_);
  if currency_.id = -1 then begin
    // Inform model.
    model_.addCurrency(currency_);
    // Insert into grid.
    new_grid_index := currencyGrid.RowCount;
    currencyGrid.RowCount := currencyGrid.RowCount + 1;
    fillGridRowFromCurrency(new_grid_index, currency_);
    // Select the row.
    currencyGrid.Row := new_grid_index;
    currencyGrid.Col := 0;
  end else begin
    new_grid_index := currencyGrid.Row;
    fillGridRowFromCurrency(new_grid_index, currency_);
    model_.editCurrency(currency_);
  end;
  exitInsertEditState();
  currencyGrid.SetFocus();
end;

procedure TCurrencyForm.currencyGridSelection(Sender: TObject; aCol,
  aRow: Integer);
var currency: TModelCurrency;
begin
  currency := getCurrentCurrency();
  fillEditFormFromCurrency(currency);
  checkButtonsEnabled();
end;

procedure TCurrencyForm.deleteButtonClick(Sender: TObject);
var currency: TModelCurrency;
    selected_button: integer;
begin
  currency := getCurrentCurrency();
  assert(currency <> nil);
  selected_button := MessageDlg('Delete currency "' + currency.name + '"?',
                                mtConfirmation, mbYesNo, 0);
  if selected_button = mrYes then begin
    currencyGrid.DeleteRow(currencyGrid.Row);
    model_.deleteCurrency(currency);
  end;
  fillEditFormFromCurrentCurrency();
  checkButtonsEnabled();
end;

procedure TCurrencyForm.cancelButtonClick(Sender: TObject);
begin
  if currency_.id = -1 then begin
    FreeAndNil(currency_);
  end;
  fillEditFormFromCurrentCurrency();
  exitInsertEditState();
end;

procedure TCurrencyForm.editButtonClick(Sender: TObject);
begin
  currency_ := getCurrentCurrency();
  assert(currency_ <> nil);
  enterInsertEditState();
end;

constructor TCurrencyForm.Create(the_owner: TComponent; model: TModel);
begin
  model_ := model;
  // TODO(sergey): Set handlers of currency updates?
  //
  // On the one hand it's correct thing to do, on the other it is weird to set
  // event handlers to a global state from a short-living form.
  inherited Create(the_owner);
end;

procedure TCurrencyForm.fillExistingCurrencies();
var currency_list: TModelCurrencyList;
    currency: TModelCurrency;
    row_index: integer;
begin
  currency_list := model_.getAllCurrencies();
  currencyGrid.RowCount := currency_list.Count + 1;
  row_index := 1;
  for currency in currency_list do begin
    fillGridRowFromCurrency(row_index, currency);
    inc(row_index);
  end;
end;

procedure TCurrencyForm.fillGridRowFromCurrency(row_index: integer;
                                                currency: TModelCurrency);
begin
  currencyGrid.Rows[row_index][0] := currency.name;
  currencyGrid.Rows[row_index][1] := currency.sign;
  currencyGrid.Rows[row_index][2] := currency.code;
  // Store pointer to object.
  currencyGrid.Objects[0, row_index] := currency;
  if currency.reference <> nil then begin
    // TODO(sergey): Include sign of the reference currency.
    currencyGrid.Rows[row_index][3] :=
        currency.reference.getReadableValue(currency.value);
  end else begin
    currencyGrid.Rows[row_index][3] := '';
  end;
end;

procedure TCurrencyForm.checkButtonsEnabled();
var has_selection: boolean;
begin
  has_selection := currencyGrid.RowCount > 1;
  deleteButton.Enabled := has_selection;
  editButton.Enabled := has_selection;
end;

procedure TCurrencyForm.enterInsertEditState();
begin
  currencyGrid.Enabled := false;
  insertButton.Visible := false;
  editButton.Visible := false;
  deleteButton.Visible := false;
  cancelButton.Visible := true;
  saveButton.Visible := true;
  editPanel.Enabled := true;
  currencyName.SetFocus();
  formConfirmButton.Enabled := false;
end;

procedure TCurrencyForm.exitInsertEditState();
begin
  currencyGrid.Enabled := true;;
  insertButton.Visible := true;
  editButton.Visible := true;
  deleteButton.Visible := true;
  cancelButton.Visible := false;
  saveButton.Visible := false;
  editPanel.Enabled := false;
  formConfirmButton.Enabled := true;
end;

procedure TCurrencyForm.fillEditFormFromCurrentCurrency();
var currency: TModelCurrency;
begin
  currency := getCurrentCurrency();
  fillEditFormFromCurrency(currency);
end;

procedure TCurrencyForm.fillEditFormFromCurrency(
    const currency: TModelCurrency);
begin
  // TODO(sergey): Avoid re-filling the combo box every time we start editing.
  fillCurrenciesEditComboBox();
  if currency <> nil then begin
    currencyName.Text := currency.name;
    currencySign.Text := currency.sign;
    currencyCide.Text := currency.code;
    currencyValue.Value := currency.value;
    selectCurrencyInEditComboBox(currency.reference);
  end else begin
    currencyName.Text := '';
    currencySign.Text := '';
    currencyCide.Text := '';
    currencyValue.Value := 0.0;
    selectCurrencyInEditComboBox(nil);
  end;
end;

procedure TCurrencyForm.fillCurrencyFromEditForm(const currency: TModelCurrency);
begin
  currency.name := currencyName.Text;
  currency.sign := currencySign.Text;
  currency.code := currencyCide.Text;
  currency.value := currencyValue.Value;
  currency.reference :=
      TModelCurrency(
          currencyReference.Items.Objects[currencyReference.ItemIndex]);
end;

function TCurrencyForm.getCurrentCurrency(): TModelCurrency;
begin
  if currencyGrid.Row < 1 then begin
    result := nil;
    exit;
  end;
  result := TModelCurrency(currencyGrid.Objects[0, currencyGrid.Row]);
end;

procedure TCurrencyForm.fillCurrenciesEditComboBox();
var currency_list: TModelCurrencyList;
    currency: TModelCurrency;
begin
  currencyReference.Items.BeginUpdate();
  currencyReference.Items.Clear();
  currency_list := model_.getAllCurrencies();
  currencyReference.AddItem('<none>', nil);
  for currency in currency_list do begin
    currencyReference.AddItem(currency.code, currency);
  end;
  currencyReference.Items.EndUpdate();
end;

procedure TCurrencyForm.selectCurrencyInEditComboBox(
    const currency: TModelCurrency);
var i: integer;
begin
  for i := 0 to currencyReference.Items.Count do begin
    if currencyReference.Items.Objects[i] = currency then begin
      currencyReference.ItemIndex := i;
      break;
    end;
  end;
end;

end.

