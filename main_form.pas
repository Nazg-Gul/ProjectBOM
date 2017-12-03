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

unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, Menus, EditBtn, project_form, item_form, model,
  database_sqlite3, lcltype, ActnList, util, lclintf, Clipbrd, currency_form;

type

  { TMainForm }

  TMainForm = class(TForm)
    itemStatusLabel: TLabel;
    itemStatusCaptionLabel: TLabel;
    itemURLLabel: TLabel;
    itemURLCaptionLabel: TLabel;
    itemPriceLabel: TLabel;
    itemImage: TImage;
    itemImageList: TImageList;
    itemDescriptionCaptionLabel: TLabel;
    itemDescriptionLabel: TLabel;
    itemPriceCaptionLabel: TLabel;
    copyURLToClipboardMenuItem: TMenuItem;
    currencyEditorMenuItem: TMenuItem;
    separatorMenuItem0: TMenuItem;
    urlPopupMenu: TPopupMenu;
    projectImageList: TImageList;
    itemsGroup: TGroupBox;
    itemInfoPanel: TPanel;
    itemListPanel: TPanel;
    mainMenu: TMainMenu;
    fileMenuItem: TMenuItem;
    exitMenuItem: TMenuItem;
    helpMenuItem: TMenuItem;
    aboutMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    addProjectMenuItem: TMenuItem;
    deleteProjectMenuItem: TMenuItem;
    editProjectMenuItem: TMenuItem;
    addItemMenuItem: TMenuItem;
    deleteItemMenuItem: TMenuItem;
    editItemMenuItem: TMenuItem;
    addProjectPopupMenuItem: TMenuItem;
    editProjectPopupMenuItem: TMenuItem;
    deleteProjectPopupMenuItem: TMenuItem;
    itemTreePopupMenu: TPopupMenu;
    addItemPopupMenuItem: TMenuItem;
    editItemPopupMenuItem: TMenuItem;
    deleteItemPopupMenuItem: TMenuItem;
    projectTreePopupMenu: TPopupMenu;
    separatorMenuItem: TMenuItem;
    projectGroupBox: TGroupBox;
    projectTreeView: TTreeView;
    itemAndInfoSplitter: TSplitter;
    projectAndItemSplitter: TSplitter;
    itemImageAndFieldsSplitter: TSplitter;
    statusBar: TStatusBar;
    mainToolBar: TToolBar;
    addProjectToolButton: TToolButton;
    editProjectToolButton: TToolButton;
    deleteProjectToolButton: TToolButton;
    addItemToolButton: TToolButton;
    editItemToolButton: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    deleteItemToolButton: TToolButton;
    itemTreeView: TTreeView;
    procedure addItemMenuItemClick(Sender: TObject);
    procedure addProjectMenuItemClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure currencyEditorMenuItemClick(Sender: TObject);
    procedure deleteItemMenuItemClick(Sender: TObject);
    procedure deleteProjectPopupMenuItemClick(Sender: TObject);
    procedure editItemMenuItemClick(Sender: TObject);
    procedure editProjectMenuItemClick(Sender: TObject);
    procedure exitMenuItemClick(Sender: TObject);
    procedure formClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure formCreate(Sender: TObject);
    procedure itemTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure itemTreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure itemTreeViewDblClick(Sender: TObject);
    procedure itemTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure itemTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure itemTreeViewSelectionChanged(Sender: TObject);
    procedure itemURLLabelClick(Sender: TObject);
    procedure copyURLToClipboardMenuItemClick(Sender: TObject);
    procedure projectTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure projectTreeViewDblClick(Sender: TObject);
    procedure projectTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: boolean);
    procedure projectTreeViewKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure projectTreeViewSelectionChanged(Sender: TObject);
  private
    database_: TSqlite3Database;
    model_: TModel;
    prev_visible_item_: TModelItem;

    // This is a project for which items list is populated.
    current_item_list_project_: TModelProject;

    ///////////////////////////////////////
    // Model

    // Construction/destruction.
    procedure initializeModel();
    procedure destroyModel();

    ////////////////////////
    // Model event handlers.

    // Project.
    procedure onProjectAdd(Sender: TObject; const project: TModelProject);
    procedure onProjectEdit(Sender: TObject; const project: TModelProject);
    procedure onProjectDelete(Sender: TObject; const project: TModelProject);

    // Item.
    procedure onItemAdd(Sender: TObject;
                        const project: TModelProject;
                        const item: TModelItem);
    procedure onItemEdit(Sender: TObject;
                         const project: TModelProject;
                         const item: TModelItem);
    procedure onItemDelete(Sender: TObject;
                           const project: TModelProject;
                           const item: TModelItem);

    ///////////////////////////////////////
    // Invoke various operations.

    // Currency.
    procedure openCurrencyEditor();

    function getInterfaceCurrency(): TModelCurrency;

    // Project.
    procedure addOrEditProjectInvoke(project: TModelProject);
    procedure deleteProjectInvoke(project: TModelProject);

    // Item.
    procedure addOrEditItemInvoke(item: TModelItem);
    procedure deleteItemInvoke(item: TModelItem);

    ///////////////////////////////////////
    // Interaction with interface.

    function getActiveProject(): TModelProject;
    function getActiveItem(): TModelItem;

    procedure fillProjectTreeNode(const project: TModelProject;
                                   node: TTreeNode);
    procedure fillItemTreeNode(const item: TModelItem; node: TTreeNode);

    ///////////////////////////////////////
    // Interface update callbacks.

    ///////////////////////
    // Visible information.

    procedure fillSummaryForActiveProject();
    procedure fillItemsForActiveProject();
    procedure fillItemDetails(item: TModelItem);

    ///////////////////
    // Enabled buttons.

    // Whole interface.
    procedure checkInterfaceEnabled();
    // Menus.
    procedure checkMenuItemsEnabled();
    procedure checkMenuProjectItemsEnabled();
    procedure checkMenuItemItemsEnabled();
    // Popup menus.
    procedure checkPopupMenuItemsEnabled();
    procedure checkPopupMenuProjectItemsEnabled();
    procedure checkPopupMenuItemItemsEnabled();
    // Toolbars.
    procedure checkToolbarButtonsEnabled();
    procedure checkToolbarProjectButtonsEnabled();
    procedure checkToolbarItemButtonsEnabled();
    // Everything related on active project.
    procedure checkProjectButtonsEnabled();
    // Everything related on active item.
    procedure checkItemButtonsEnabled();
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

////////////////////////////////////////////////////////////////////////////////
// Interface handlers and callbacks.

procedure TMainForm.formCreate(Sender: TObject);
begin
  initializeModel();
  checkInterfaceEnabled();
  prev_visible_item_ := nil;
  fillItemDetails(nil);
  if projectTreeView.Items.Count > 0 then begin
    projectTreeView.Items[0].Selected := true;
  end;
  if itemTreeView.Items.Count > 0 then begin
    fillItemDetails(TModelItem(itemTreeView.Items[0].Data));
  end;
end;

procedure TMainForm.formClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  destroyModel();
end;

///////////////////////////////////////
// Tree callbacks.

///////////////////
// Projects.

procedure TMainForm.projectTreeViewEditing(Sender: TObject;
  Node: TTreeNode;
  var AllowEdit: boolean);
begin
  // TODO(sergey): Would be cool to support in-place editing.
  AllowEdit := False;
end;

procedure TMainForm.projectTreeViewKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  active_project: TModelProject;
begin
  if key = VK_DELETE then
  begin
    active_project := getActiveProject();
    if active_project <> nil then
    begin
      deleteProjectInvoke(active_project);
    end;
  end;
end;

procedure TMainForm.projectTreeViewSelectionChanged(Sender: TObject);
begin
  // Fill in items for a changed selected project.
  fillItemsForActiveProject();
  fillSummaryForActiveProject();
end;

procedure TMainForm.projectTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  // Check which buttons becomes available.
  checkProjectButtonsEnabled();
end;

procedure TMainForm.projectTreeViewDblClick(Sender: TObject);
var
  active_project: TModelProject;
begin
  active_project := getActiveProject();
  if active_project <> nil then
  begin
    addOrEditProjectInvoke(active_project);
  end;
end;

///////////////////
// Items.

procedure TMainForm.itemTreeViewEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  // TODO(sergey): Would be cool to support in-place editing.
  AllowEdit := False;
end;

procedure TMainForm.itemTreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  active_item: TModelItem;
begin
  if key = VK_DELETE then
  begin
    active_item := getActiveItem();
    if active_item <> nil then
    begin
      deleteItemInvoke(active_item);
    end;
  end;
end;

procedure TMainForm.itemTreeViewSelectionChanged(Sender: TObject);
var item: TModelItem;
begin
  checkItemButtonsEnabled();
  item := getActiveItem();
  fillItemDetails(item);
end;

procedure TMainForm.itemURLLabelClick(Sender: TObject);
var item: TModelItem;
begin
  item := getActiveItem();
  if item <> nil then begin
    OpenURL(item.url);
  end;
end;

procedure TMainForm.copyURLToClipboardMenuItemClick(Sender: TObject);
var item: TModelItem;
begin
  item := getActiveItem();
  if item <> nil then begin
    if item.url <> '' then begin
      Clipboard.AsText := item.url;
    end;
  end;
end;

procedure TMainForm.itemTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  // Check which buttons becomes available.
  checkItemButtonsEnabled();
end;

procedure TMainForm.itemTreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var item: TModelItem;
begin
  with Sender.Canvas do begin
    item := TModelItem(node.Data);
    // TODO(sergey): Use some sort of theme.
    if item.isDone() then begin
      Font.Color := clGray;
    end else begin
      Font.Color := clBlack;
    end;
  end;
end;

procedure TMainForm.itemTreeViewDblClick(Sender: TObject);
var
  active_item: TModelItem;
begin
  active_item := getActiveItem();
  if active_item <> nil then
  begin
    addOrEditItemInvoke(active_item);
  end;
end;

///////////////////////////////////////
// Menus and toolbasrs callbacks.

///////////////////
// Simply bypass to a lower level implementation.

// Project manipulation.

procedure TMainForm.addProjectMenuItemClick(Sender: TObject);
begin
  addOrEditProjectInvoke(nil);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage(GetTempFile('.png'));
end;

procedure TMainForm.currencyEditorMenuItemClick(Sender: TObject);
begin
  openCurrencyEditor();
end;

procedure TMainForm.editProjectMenuItemClick(Sender: TObject);
begin
  addOrEditProjectInvoke(getActiveProject());
end;

procedure TMainForm.deleteProjectPopupMenuItemClick(Sender: TObject);
begin
  deleteProjectInvoke(getActiveProject());
end;

procedure TMainForm.exitMenuItemClick(Sender: TObject);
begin
  close();
end;

// Item manipulation.

procedure TMainForm.addItemMenuItemClick(Sender: TObject);
begin
  addOrEditItemInvoke(Nil);
end;

procedure TMainForm.editItemMenuItemClick(Sender: TObject);
begin
  addOrEditItemInvoke(getActiveItem());
end;

procedure TMainForm.deleteItemMenuItemClick(Sender: TObject);
begin
  deleteItemInvoke(getActiveItem());
end;

////////////////////////////////////////////////////////////////////////////////
// Internal logic implementation.

///////////////////////////////////////
// Model

// Construction/destruction.

procedure TMainForm.initializeModel();
  procedure fillProjects(projects_list: TModelProjectList;
                         parent_node: TTreeNode);
  var
      project: TModelProject;
      tree_node: TTreeNode;
  begin
    for project in projects_list do begin
      tree_node := projectTreeView.Items.AddChild(parent_node, '');
      fillProjectTreeNode(project, tree_node);
      fillProjects(project.projects, tree_node);
    end;
  end;

begin
  // First initialize and connect to the database.
  database_ := TSqlite3Database.Create();
  database_.autocommit := true;
  // TODO(sergey): Store database in some more accessible place.
  database_.connect('projectbom.sqlite');
  // Construct model.
  model_ := TModel.Create(database_);
  // Project events.
  model_.OnProjectAdd := @onProjectAdd;
  model_.onProjectEdit := @onProjectEdit;
  model_.onProjectDelete := @onProjectDelete;
  // Item events.
  model_.OnItemAdd := @onItemAdd;
  model_.onItemEdit := @onItemEdit;
  model_.onItemDelete := @onItemDelete;
  // Create interface for all existing projects.
  fillProjects(model_.getAllProjects(), nil);
  // Sort all projects and items.
  projectTreeView.AlphaSort();
  if projectTreeView.Items.Count > 0 then begin
    projectTreeView.Items[0].Selected := true;
  end;
  // Sort items.
  itemTreeView.AlphaSort();
end;

procedure TMainForm.destroyModel();
begin
  // First fulyl finish with the model.
  FreeAndNil(model_);
  // Then disconnect from database and fully destroy connection.
  database_.disconnect();
  FreeAndNil(database_);
end;

////////////////////////////////////////////////////////////////////////////////
// Model event handlers.

// Project.

procedure TMainForm.onProjectAdd(Sender: TObject; const project: TModelProject);
var
  tree_node, parent_node: TTreeNode;
begin
  if project.parent <> nil then begin
    parent_node := projectTreeView.Items.FindNodeWithData(project.parent);
  end else begin
    parent_node := nil;
  end;
  tree_node := projectTreeView.Items.AddChild(parent_node, '');
  fillProjectTreeNode(project, tree_node);
  tree_node.Selected := True;
  // TODO(sergey): Set item image indes.
  // TODO(sergey): Avoid re-sorting the whole tree.
  projectTreeView.AlphaSort();
  // TOOO(sergey): Seems onSelectionChange is not invoked from here.
  // TODO(sergey): De-duplicate with event handler.
  fillItemsForActiveProject();
  fillSummaryForActiveProject();
end;

procedure TMainForm.onProjectEdit(Sender: TObject; const project: TModelProject);
var
  tree_node: TTreeNode;
  old_parent_tree_node, new_parent_tree_node: TTreeNode;
begin
  tree_node := projectTreeView.Items.FindNodeWithData(project);
  assert(tree_node <> nil);
  // Update position of node in the hierarchy.
  old_parent_tree_node := tree_node.Parent;
  new_parent_tree_node := projectTreeView.Items.FindNodeWithData(project.parent);
  if old_parent_tree_node <> new_parent_tree_node then begin
    // tree_node.Parent := new_parent_tree_node;
    tree_node.Delete();
    tree_node := projectTreeView.Items.AddChild(new_parent_tree_node, '');
    tree_node.Selected := true;
  end;
  // Update tree node.
  fillProjectTreeNode(project, tree_node);
  // TODO(sergey): Set item image indes.
  // TODO(sergey): Avoid re-sorting the whole tree.
  projectTreeView.AlphaSort();
end;

procedure TMainForm.onProjectDelete(Sender: TObject; const project: TModelProject);
var
  tree_node: TTreeNode;
begin
  tree_node := projectTreeView.Items.FindNodeWithData(project);
  assert(tree_node <> nil);
  projectTreeView.Items.Delete(tree_node);
end;

// Item.

procedure TMainForm.onItemAdd(Sender: TObject;
                              const project: TModelProject;
                              const item: TModelItem);
var
  tree_node: TTreeNode;
begin
  if project <> getActiveProject() then begin
    // No need to add items to tree for inactive project.
    exit;
  end;
  tree_node := itemTreeView.Items.Add(Nil, '');
  fillItemTreeNode(item, tree_node);
  tree_node.Selected := True;
  // TODO(sergey): Set item image indes.
  // TODO(sergey): Avoid re-sorting the whole tree.
  itemTreeView.AlphaSort();
  // Update interface.
  fillSummaryForActiveProject
end;

procedure TMainForm.onItemEdit(Sender: TObject;
                               const project: TModelProject;
                               const item: TModelItem);
var
  tree_node: TTreeNode;
begin
  if project <> getActiveProject() then begin
    // No need to edit items in tree for inactive project.
    exit;
  end;
  tree_node := itemTreeView.Items.FindNodeWithData(item);
  assert(tree_node <> nil);
  // Update tree node.
  tree_node.Text := item.name;
  // TODO(sergey): Set item image indes.
  // TODO(sergey): Avoid re-sorting the whole tree.
  itemTreeView.AlphaSort();
  // Update interface.
  fillSummaryForActiveProject();
  if item = prev_visible_item_ then begin
    fillItemDetails(item);
  end;
end;

procedure TMainForm.onItemDelete(Sender: TObject;
                                 const project: TModelProject;
                                 const item: TModelItem);
var
  tree_node: TTreeNode;
begin
  if project <> getActiveProject() then begin
    // No need to delete items from tree for inactive project.
    exit;
  end;
  tree_node := itemTreeView.Items.FindNodeWithData(item);
  assert(tree_node <> nil);
  tree_node.Delete();
  // Update interface.
  fillSummaryForActiveProject
end;

///////////////////////////////////////
// Invoke various operations.

///////////////////
// Currency

procedure TMainForm.openCurrencyEditor();
var
  currency_form: TCurrencyForm;
  modal_result: integer;
begin
  // Show form, wait for it to fully tackle situation.
  currency_form := TCurrencyForm.Create(nil, model_);
  modal_result := currency_form.ShowModal();
  currency_form.Free();
  // Check whether adding/editing of project succeeded.
  if modal_result = mrOk then
  begin
    // TODO(sergey): Handle result.
  end;
end;

function TMainForm.getInterfaceCurrency(): TModelCurrency;
begin
  // TODO(sergey): Make it configurable.
  result := model_.getCurrencyByCode('EUR');
end;

///////////////////
// Project manipulation.

// Add or edit project.

// If given project is Nil then new one will be added, otherwise given
// project will be edited.
procedure TMainForm.addOrEditProjectInvoke(project: TModelProject);
var
  project_form: TProjectForm;
  modal_result: integer;
begin
  // Show form, wait for it to fully tackle situation.
  project_form := TProjectForm.Create(nil, model_, project);
  modal_result := project_form.ShowModal();
  FreeAndNil(project_form);
  // Check whether adding/editing of project succeeded.
  if modal_result = mrOk then
  begin
    // TODO(sergey): Handle result.
  end;
end;

// Delete requested project.
procedure TMainForm.deleteProjectInvoke(project: TModelProject);
var
  selected_button: integer;
begin
  assert(project <> nil);
  selected_button := messagedlg('Delete project "' + project.name +
    '"?', mtConfirmation,
    mbYesNo, 0);
  if selected_button = mrYes then
  begin
    model_.deleteProject(project);
    // TODO(sergey): Select next/prev project.
  end;
end;

///////////////////
// Item manipulation.

procedure TMainForm.addOrEditItemInvoke(item: TModelItem);
var
  item_form: TItemForm;
  modal_result: integer;
begin
  if item <> nil then begin
    model_.expandItem(item);
  end;
  // Show form, wait for it to fully tackle situation.
  item_form := TItemForm.Create(nil, model_, getActiveProject(), item);
  modal_result := item_form.ShowModal();
  FreeAndNil(item_form);
  // Check whether adding/editing of project succeeded.
  if modal_result = mrOk then
  begin
    if item <> nil then begin
      if item <> prev_visible_item_ then begin
        model_.unexpandItem(item);
      end;
    end;
  end;
end;

procedure TMainForm.deleteItemInvoke(item: TModelItem);
var
  selected_button: integer;
begin
  assert(item <> nil);
  selected_button := MessageDlg('Delete item "' + item.name +
    '"?', mtConfirmation,
    mbYesNo, 0);
  if selected_button = mrYes then
  begin
    model_.deleteItem(getActiveProject(), item);
    // TODO(sergey): Select next/prev item.
  end;
end;

///////////////////////////////////////
// Interaction with interface.

function TMainForm.getActiveProject(): TModelProject;
var
  selected_tree_node: TTreeNode;
begin
  selected_tree_node := projectTreeView.Selected;
  if selected_tree_node = nil then
  begin
    Result := nil;
    exit;
  end;
  Result := TModelProject(selected_tree_node.Data);
end;

function TMainForm.getActiveItem(): TModelItem;
var
  selected_tree_node: TTreeNode;
begin
  selected_tree_node := itemTreeView.Selected;
  if selected_tree_node = nil then
  begin
    Result := nil;
    exit;
  end;
  Result := TModelItem(selected_tree_node.Data);
end;

procedure TMainForm.fillProjectTreeNode(const project: TModelProject;
                                        node: TTreeNode);
begin
  node.Text := project.name;
  node.Data := project;
  node.ImageIndex := 0;
  node.SelectedIndex := node.ImageIndex;
end;

procedure TMainForm.fillItemTreeNode(const item: TModelItem; node: TTreeNode);
begin
  node.Text := item.name;
  node.Data := item;
  node.ImageIndex := 0;
  node.SelectedIndex := node.ImageIndex;
end;

///////////////////////////////////////
// Interface update callbacks.

///////////////////////
// Visible informaiton.

procedure TMainForm.fillSummaryForActiveProject();
var
    active_project: TModelProject;
    currency: TModelCurrency;
    total_cost, remaining_cost: double;
begin
  active_project := getActiveProject();
  if active_project = Nil then begin
    statusBar.Panels[0].Text := '';
    statusBar.Panels[1].Text := '';
    exit;
  end;
  currency := getInterfaceCurrency();
  if currency <> nil then begin
    total_cost := active_project.getTotalProjectCost(model_, currency);
    remaining_cost := active_project.getRemainingProjectCost(model_, currency);
    statusBar.Panels[0].Text := 'Project cost: ' +
        currency.getReadableValue(total_cost);
    statusBar.Panels[1].Text := 'Remaining cost: ' +
        currency.getReadableValue(remaining_cost);
  end;
end;

procedure TMainForm.fillItemsForActiveProject();
var
  active_project: TModelProject;
  i: Integer;
  tree_node: TTreeNode;
  item: TModelItem;
begin
  active_project := getActiveProject();
  // Don't bother re-fillign same project, everything is supposed to be up to
  // date already.
  if active_project = current_item_list_project_ then begin
    exit();
  end;
  current_item_list_project_ := active_project;
  // Start with clearing items from old project.
  itemTreeView.items.Clear();
  if active_project = Nil then begin
    exit;
  end;
  // Fill in items for active project.
  for i := 0 to active_project.items.Count - 1 do begin
    item := TModelItem(active_project.items[i]);
    tree_node := itemTreeView.Items.Add(Nil, '');
    fillItemTreeNode(item, tree_node);
  end;
  itemTreeView.AlphaSort();
  if active_project.items.Count <> 0 then begin
    itemTreeView.Items[0].Selected := true;
  end;
end;

procedure TMainForm.fillItemDetails(item: TModelItem);
var interface_currency: TModelCurrency;
    interface_price: double;
begin
  interface_currency := getInterfaceCurrency();
  // Save memory by unloading unused data from currently visible item.
  if prev_visible_item_ <> nil then begin
    model_.unexpandItem(prev_visible_item_);
  end;
  // Show data for the item.
  if item <> nil then begin
    model_.expandItem(item);
    itemImage.Picture.Assign(item.getImagePicture());
    itemStatusLabel.Caption := getItemnStatusName(item.status);
    if item.currency <> nil then begin
      itemPriceLabel.Caption := item.currency.getReadableValue(item.price);
      if (interface_currency <> nil) and
         (item.currency <> interface_currency) then begin
        interface_price := item.currency.convertTo(model_,
                                                   item.price,
                                                   interface_currency);
        itemPriceLabel.Caption := itemPriceLabel.Caption + ' (' +
            interface_currency.getReadableValue(interface_price) + ')';
      end;
    end else begin
      itemPriceLabel.Caption := FloatToStr(item.price);
    end;
    itemURLLabel.Caption := item.url;
    itemDescriptionLabel.Caption := item.description;
    // Update visibility.
    itemInfoPanel.Visible := true;
    itemAndInfoSplitter.Visible := true;
    // Image visibility.
    itemImage.Visible := itemImage.Picture.Height <> 0;
    itemImageAndFieldsSplitter.Visible := itemImage.Visible;
    // URL visibility.
    itemURLLabel.Visible := item.url <> '';
    itemURLCaptionLabel.Visible := itemURLLabel.Visible;
    // Description visibility.
    itemDescriptionLabel.Visible := item.description <> '';
    itemDescriptionCaptionLabel.Visible := itemDescriptionLabel.Visible;
  end else begin
    itemInfoPanel.Visible := false;
    itemAndInfoSplitter.Visible := false;
  end;
  // Save currently visible item.
  prev_visible_item_ := item;
end;

///////////////////
// Enabled buttons.

// Whole interface.

procedure TMainForm.checkInterfaceEnabled();
begin
  checkMenuItemsEnabled();
  checkPopupMenuItemsEnabled();
  checkToolbarButtonsEnabled();
end;

// Menus.

procedure TMainForm.checkMenuItemsEnabled();
begin
  checkMenuProjectItemsEnabled();
  checkMenuItemItemsEnabled();
end;

procedure TMainForm.checkMenuProjectItemsEnabled();
begin
  if getActiveProject() = nil then
  begin
    editProjectMenuItem.Enabled := False;
    deleteProjectMenuItem.Enabled := False;
  end
  else
  begin
    editProjectMenuItem.Enabled := True;
    deleteProjectMenuItem.Enabled := True;
  end;
end;

procedure TMainForm.checkMenuItemItemsEnabled();
begin
  // Add item button is hidden if no active project.
  if getActiveProject() = nil then
  begin
    addItemMenuItem.Enabled := False;
  end
  else
  begin
    addItemMenuItem.Enabled := True;
  end;
  // If there is no active project, there supposed to be no active item as well.
  if getActiveItem() = nil then
  begin
    editItemMenuItem.Enabled := False;
    deleteItemMenuItem.Enabled := False;
  end
  else
  begin
    editItemMenuItem.Enabled := True;
    deleteItemMenuItem.Enabled := True;
  end;
end;

// Popup menus.

procedure TMainForm.checkPopupMenuItemsEnabled();
begin
  checkPopupMenuProjectItemsEnabled();
  checkPopupMenuItemItemsEnabled();
end;

procedure TMainForm.checkPopupMenuProjectItemsEnabled();
begin
  if getActiveProject() = nil then
  begin
    editProjectPopupMenuItem.Enabled := False;
    deleteProjectPopupMenuItem.Enabled := False;
  end
  else
  begin
    editProjectPopupMenuItem.Enabled := True;
    deleteProjectPopupMenuItem.Enabled := True;
  end;
end;

procedure TMainForm.checkPopupMenuItemItemsEnabled();
begin
  // Add item button is hidden if no active project.
  if getActiveProject() = nil then
  begin
    addItemPopupMenuItem.Enabled := False;
  end
  else
  begin
    addItemPopupMenuItem.Enabled := True;
  end;
  // If there is no active project, there supposed to be no active item as well.
  if getActiveItem() = nil then
  begin
    editItemPopupMenuItem.Enabled := False;
    deleteItemPopupMenuItem.Enabled := False;
  end
  else
  begin
    editItemPopupMenuItem.Enabled := True;
    deleteItemPopupMenuItem.Enabled := True;
  end;
end;

// Toolbar.

procedure TMainForm.checkToolbarButtonsEnabled();
begin
  checkToolbarProjectButtonsEnabled();
  checkToolbarItemButtonsEnabled();
end;

procedure TMainForm.checkToolbarProjectButtonsEnabled();
begin
  if getActiveProject() = nil then
  begin
    editProjectToolButton.Enabled := False;
    deleteProjectToolButton.Enabled := False;
  end
  else
  begin
    editProjectToolButton.Enabled := True;
    deleteProjectToolButton.Enabled := True;
  end;
end;

procedure TMainForm.checkToolbarItemButtonsEnabled();
begin
  // Add item button is hidden if no active project.
  if getActiveProject() = nil then
  begin
    addItemToolButton.Enabled := False;
  end
  else
  begin
    addItemToolButton.Enabled := True;
  end;
  // If there is no active project, there supposed to be no active item as well.
  if getActiveItem() = nil then
  begin
    editItemToolButton.Enabled := False;
    deleteItemToolButton.Enabled := False;
  end
  else
  begin
    editItemToolButton.Enabled := True;
    deleteItemToolButton.Enabled := True;
  end;
end;

// Project.
procedure TMainForm.checkProjectButtonsEnabled();
begin
  checkMenuProjectItemsEnabled();
  checkPopupMenuProjectItemsEnabled();
  checkToolbarProjectButtonsEnabled();
  // Indirect dependencies to buttons via items.
  checkItemButtonsEnabled();
end;

// Item.
procedure TMainForm.checkItemButtonsEnabled();
begin
  checkMenuItemItemsEnabled();
  checkPopupMenuItemItemsEnabled();
  checkToolbarItemButtonsEnabled();
end;

end.
