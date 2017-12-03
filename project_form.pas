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

unit project_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, EditBtn, Buttons, model, contnrs;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    projectParentComboBox: TComboBox;
    projectConfirmButton: TBitBtn;
    projectCancelButton: TBitBtn;
    iconShowFileSelectorButton: TBitBtn;
    iconPathEdit: TEdit;
    iconRemoteRadioButton: TRadioButton;
    projectIconLabel: TLabel;
    projectNameEdit: TEdit;
    projectNameLabel: TLabel;
    iconLocalRadioButton: TRadioButton;
    projectParentLabel: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure iconLocalRadioButtonChange(Sender: TObject);
    procedure iconRemoteRadioButtonChange(Sender: TObject);

  private
    // This is a model within which project manipulation is happening.
    // Supposed to be set by the parent form.
    model_: TModel;

    // This is a project which is being edited, If it's Nil, then form will
    // create new project in model on confirm, otherwise it will update this
    // project in model.
    project_: TModelProject;

    // Deinitialization of create/edit process.
    procedure endCreateOrEditProject();

    // Interface update callbacks.
    procedure updateIconSourceType();

    // Check whether input is a valid project properties.
    function checkInputValid() : Boolean;

    procedure fillProjectFromForm(var project: TModelProject);
  public
    // Constructors.
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;
                       model: TModel); reintroduce; overload;
    constructor Create(TheOwner: TComponent;
                       model: TModel;
                       project: TModelProject); reintroduce; overload;

    // Initialization of create/edit process.
    procedure beginCreateProject(model: TModel);
    procedure beginEditProject(model: TModel; project: TModelProject);

    procedure fillPossibleProjectParents(model: TModel; project: TModelProject);
    function getSelectedProject(): TModelProject;
    procedure selectParent(project_parent: TModelProject);
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.lfm}

////////////////////////////////////////////////////////////////////////////////
// Constructor.

constructor TProjectForm.Create(TheOwner: TComponent);
begin
  // TODO(sergey): This might be handy to not have model, for some debug
  // perhaps? Or shall we forbid creating form without model at all?
  inherited Create(TheOwner);
  fillPossibleProjectParents(nil, nil);
  model_ := Nil;
  project_ := Nil;
end;

constructor TProjectForm.Create(TheOwner: TComponent; model: TModel);
begin
  inherited Create(TheOwner);
  fillPossibleProjectParents(model, nil);
  beginCreateProject(model);
end;

constructor TProjectForm.Create(TheOwner: TComponent;
                                model: TModel;
                                project: TModelProject);
begin
  inherited Create(TheOwner);
  fillPossibleProjectParents(model, project);
  beginEditProject(model, project);
end;

////////////////////////////////////////////////////////////////////////////////
// Interface callbacks.

procedure TProjectForm.FormClose(Sender: TObject;
                                 var CloseAction: TCloseAction);
var new_project: TModelProject;
begin
  // Check whether we need/can send new/edited project to model.
  if (ModalResult <> mrOk) or (model_ = Nil) then
  begin
    endCreateOrEditProject();
    exit;
  end;
  if not checkInputValid() then begin
    CloseAction := caNone;;
    exit;
  end;
  // Submit project to model.
  if project_ = Nil then
  begin
    // Create new project and add it to the model.
    new_project := TModelProject.Create();
    fillProjectFromForm(new_project);
    model_.addProject(new_project);
  end else begin
    fillProjectFromForm(project_);
    model_.editProject(project_);
  end;
  // Clear the mess after us.
  endCreateOrEditProject();
end;

procedure TProjectForm.iconLocalRadioButtonChange(Sender: TObject);
begin
  updateIconSourceType();
end;

procedure TProjectForm.iconRemoteRadioButtonChange(Sender: TObject);
begin
  updateIconSourceType();
end;

////////////////////////////////////////////////////////////////////////////////
// Interface update callbacks.

procedure TProjectForm.updateIconSourceType();
begin
  // TODO(sergey): Avoid multiple updates (happens once per radio button).
  if iconLocalRadioButton.Checked then begin
    iconShowFileSelectorButton.Enabled := True;
  end else if iconRemoteRadioButton.Checked then begin
    iconShowFileSelectorButton.Enabled := False;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Public API to contro lwhether we edit or create project. Handy to avoid
// allocating new for mfor each action.

procedure TProjectForm.beginCreateProject(model: TModel);
begin
  // Window title.
  caption := 'Create project';
  // Set internal variables.
  model_ := model;
  project_ := Nil;
  // Reset interface to the new create state.
  selectParent(nil);
  projectNameEdit.text := '';
  iconLocalRadioButton.Checked := true;
  iconPathEdit.text := '';
end;

procedure TProjectForm.beginEditProject(model: TModel; project: TModelProject);
begin
  if project = Nil then begin
    beginCreateProject(model);
    exit;
  end;
  // Window title.
  caption := 'Edit project';
  // Set internal variables.
  model_ := model;
  project_ := project;
  // Reset interface to the new edit state.
  selectParent(project.parent);
  projectNameEdit.text := project.name;
  // TODO(sergey): What to initialize icon to?
  iconLocalRadioButton.Checked := true;
  iconPathEdit.text := '';
end;

procedure TProjectForm.endCreateOrEditProject();
begin
  model_ := Nil;
  project_ := Nil;
end;

function TProjectForm.checkInputValid() : Boolean;
begin
  result := True;
  if projectNameEdit.Text = '' then begin
    result := False;
    MessageDlg('Project name can not be empty.', mtError, [mbOk], 0);
  end;
  // TODO(sergey): Check uniqness of the peoject name?
end;

procedure TProjectForm.fillProjectFromForm(var project: TModelProject);
begin
  project.parent := getSelectedProject();
  project.name := projectNameEdit.text;
  // TODO(sergey): Fill in icon.
end;

////////////////////////////////////////////////////////////////////////////////
// Project parent.

procedure TProjectForm.fillPossibleProjectParents(model: TModel;
                                                  project: TModelProject);
var
    project_queue: TQueue;
    current_project: TModelProject;
begin
  projectParentComboBox.items.BeginUpdate();
  projectParentComboBox.items.Clear();
  projectParentComboBox.AddItem('None', nil);
  if model = nil then begin
    projectParentComboBox.items.EndUpdate();
    exit;
  end;
  // Fill in initial queue.
  project_queue := TQueue.Create();
  for current_project in model.getAllProjects() do begin
    project_queue.Push(current_project);
  end;
  // Handle thew queue.
  while project_queue.Count <> 0 do begin
    current_project := TModelProject(project_queue.Pop());
    if project = current_project then begin
      continue;
    end;
    projectParentComboBox.AddItem(current_project.name, current_project);
  end;
  // Finish.
  project_queue.Free();
  projectParentComboBox.items.EndUpdate();
end;

function TProjectForm.getSelectedProject(): TModelProject;
var index: integer;
begin
  index := projectParentComboBox.ItemIndex;
  result := TModelProject(projectParentComboBox.Items.Objects[index]);
end;

procedure TProjectForm.selectParent(project_parent: TModelProject);
var i: integer;
    current_project: TModelProject;
begin
  for i := 0 to projectParentComboBox.Items.Count - 1 do begin
    current_project := TModelProject(projectParentComboBox.Items.Objects[i]);
    if current_project = project_parent then begin
      projectParentComboBox.ItemIndex := i;
      break;
    end;
  end;
end;

end.

