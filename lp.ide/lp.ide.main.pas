unit lp.ide.main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, ExtCtrls, StdCtrls, lp.edits, ImgList

  , DBXJSON

  , lp.ide.module.source
  ;

type
  TLPIdeMain = class(TForm)
    MMMain: TMainMenu;
    Panel1: TPanel;
    Panel2: TPanel;
    LPIStatusBar: TStatusBar;
    Panel3: TPanel;
    MnuFile: TMenuItem;
    MnuProjectNew: TMenuItem;
    MnuProjectOpen: TMenuItem;
    N1: TMenuItem;
    MnuExit: TMenuItem;
    Panel4: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    ConsoleLog: TListBox;
    Label2: TLabel;
    ProjectTree: TTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TbMain: TToolBar;
    tbOpenProject: TToolButton;
    tbNewModule: TToolButton;
    PCMain: TPageControl;
    ILMain: TImageList;
    MnuRun: TMenuItem;
    MnuRunDebug: TMenuItem;
    MnuRunOptions: TMenuItem;
    N2: TMenuItem;
    MnuProjectSave: TMenuItem;
    MnuProjectSaveWithName: TMenuItem;
    ToolButton3: TToolButton;
    tbNewPorject: TToolButton;
    MnuProject: TMenuItem;
    MnuModuloNew: TMenuItem;
    MnuModuloElimina: TMenuItem;
    tbRemoveModule: TToolButton;
    ILTab: TImageList;
    tbSaveProject: TToolButton;
    tbRun: TToolButton;
    ToolButton2: TToolButton;
    procedure MnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MnuModuloNewClick(Sender: TObject);
    procedure MnuModuloEliminaClick(Sender: TObject);
    procedure ProjectTreeDblClick(Sender: TObject);
    procedure PCMainChange(Sender: TObject);
    procedure MnuProjectNewClick(Sender: TObject);
    procedure MnuProjectOpenClick(Sender: TObject);
    procedure MnuRunDebugClick(Sender: TObject);
    procedure MnuProjectSaveClick(Sender: TObject);
  private
    { Private declarations }
    CurrentProject:string;
  public
    { Public declarations }
    { -- project -- }
    procedure NewProject;
    procedure CloseProject;
    procedure OpenProject;
    procedure SaveProject;
    { -- module -- }
    procedure NewModule(Name,Data: string; Parent:TTreeNode);
    procedure RemoveModule(Node:TTreeNode);
    { -- source tab and tree -- }
    function  ModuleNameByNode(Node:TTreeNode):string;
    procedure OpenTab(Node:TTreeNode);
    procedure CloseTab(Node:TTreeNode);
    procedure SelectNote(Sheet:TTabSheet);
    procedure UpdateNote(Sheet:TTabSheet; Data:string);
    procedure UpdateStatusBar;
  end;

var
  LPIdeMain: TLPIdeMain;

const
  NewProjectSource ='import "stdlib"; '#13#13
    +'let main = fn() { '#13
    +'  println("Hello, World!"); '#13
    +'};'#13
    +' '#13
    +'main();';

implementation

uses lp.utils;

{$R *.dfm}

procedure TLPIdeMain.CloseProject;
var
  current:TTreeNode;
begin
  for current in ProjectTree.Items do
  begin
    CloseTab(current);
    TStringList(current.Data).Free;
  end;

  ProjectTree.Items.Clear;
end;

procedure TLPIdeMain.CloseTab(Node: TTreeNode);
var
  S:string;
  i:Integer;
  current:TTabSheet;
begin
  current := nil;
  S:=ModuleNameByNode(Node);

  for i := 0 to PCMain.PageCount-1 do
    if (LowerCase(PCMain.Pages[i].Caption)=LowerCase(S)) then
    begin
      current := PCMain.Pages[i];
      Break;
    end;

  if Assigned(current) then
  begin
    current.Free;
  end;
end;

procedure TLPIdeMain.FormCreate(Sender: TObject);
begin
  NewProject;
end;

procedure TLPIdeMain.MnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TLPIdeMain.MnuModuloEliminaClick(Sender: TObject);
begin
  if (ProjectTree.Selected<>nil) then
  if (ProjectTree.Selected.Text<>'main') then
  if (MessageDlg('Vuoi rimuovere il modulo selezionato, Sei Sicuto/a?'#13#10'Se il modulo ha submoduli saranno eliminati anche quelli.'
    ,mtConfirmation
    ,mbYesNo,0)=mrYes) then
  begin
    RemoveModule(ProjectTree.Selected);
  end;
end;

procedure TLPIdeMain.MnuModuloNewClick(Sender: TObject);

  function ModuleNameIsValid(Name:string): Boolean;
  begin
    Result := Name<>'';
  end;

var
  S:string;
begin
  if (ProjectTree.Selected<>nil) then
  begin
    repeat
      S:=Trim(InputBox('Aggiungi Modulo', 'Module Name',''));
      if ModuleNameIsValid(S) then
        NewModule(S,'',ProjectTree.Selected);
    until ModuleNameIsValid(S) or (S='');
  end;
end;

procedure TLPIdeMain.MnuProjectNewClick(Sender: TObject);
begin
  CloseProject;
  NewProject;
end;

procedure TLPIdeMain.MnuProjectOpenClick(Sender: TObject);
begin
  CloseProject;
  OpenProject;
end;

procedure TLPIdeMain.MnuProjectSaveClick(Sender: TObject);
begin
  SaveProject;
end;

procedure TLPIdeMain.MnuRunDebugClick(Sender: TObject);
begin
  //
end;

function TLPIdeMain.ModuleNameByNode(Node: TTreeNode):string;
begin
  Result := '';
  repeat
    if (Result<>'') then Result:='.'+Result;
    Result:= Node.Text+Result;
    Node:= Node.Parent;
  until (Node=nil);
end;

procedure TLPIdeMain.NewModule(Name,Data: string; Parent:TTreeNode);
var
  NM{,X}:TTreeNode;
begin
  Name := LowerCase(Name);

  NM:=ProjectTree.Items.AddChild(Parent,Name);
  NM.Data := TStringList.Create;
  TStringList(NM.Data).Text := Data;
{
  X := NM.Parent;
  while (X<>nil) do
  begin
    X.Expand(false);
    X:= X.Parent;
  end;
}
  ProjectTree.Selected := NM;
  NM.MakeVisible;

  OpenTab(NM);
end;

procedure TLPIdeMain.NewProject;
begin
  CloseProject;
  CurrentProject:='';
  UpdateStatusBar;
  NewModule('main', NewProjectSource, nil);
end;

procedure TLPIdeMain.OpenProject;
begin
//
end;

procedure TLPIdeMain.OpenTab(Node:TTreeNode);
var
  i:Integer;
  current:TTabSheet;
  S,D:string;
  F:TLPModuleSourceFrame;
begin
  current := nil;
  D:= TStringList(Node.Data).Text;
  S:= ModuleNameByNode(Node);

  for i := 0 to PCMain.PageCount-1 do
    if (LowerCase(PCMain.Pages[i].Caption)=LowerCase(S)) then
    begin
      current := PCMain.Pages[i];
      Break;
    end;

  if NOT Assigned(current) then
  begin
    current := TTabSheet.Create(PCMain);
    current.PageControl:= PCMain;
    current.Caption:= S;
    F:=TLPModuleSourceFrame.Create(current);
    F.LabelTop.Caption := 'Source of <'+S+'> module';
    F.MemoSource.Text := D;
    F.Align := alClient;
    F.Parent:= current;
  end;

  PCMain.ActivePage := current;
end;

procedure TLPIdeMain.PCMainChange(Sender: TObject);
begin
  SelectNote(PCMain.ActivePage);
end;

procedure TLPIdeMain.ProjectTreeDblClick(Sender: TObject);
begin
  if ProjectTree.Selected<>nil then
    OpenTab(ProjectTree.Selected);
end;

procedure TLPIdeMain.RemoveModule(Node: TTreeNode);
begin
  CloseTab(Node);
  TStringList(Node.Data).Free;
  ProjectTree.Items.Delete(Node);
end;

procedure TLPIdeMain.SaveProject;
var
  i:Integer;
  F:TLPModuleSourceFrame;
  S:string;
  J,Z:TJSONObject;
  M:TJSONArray;
  Current:TTreeNode;
begin
  for i := 0 to PCMain.PageCount-1 do
  begin
    F:= TLPModuleSourceFrame( PCMain.Pages[i].Controls[0] );
    UpdateNote( PCMain.Pages[i], F.MemoSource.Text );
  end;

  if (CurrentProject='') then
    with TSaveDialog.Create(Self) do
    try
      if Execute then
      begin
        S:=ChangeFileExt(FileName,'.lpi');

        J:= TJSONObject.Create;
        M:= TJSONArray.Create;
        try
          for current in ProjectTree.Items do
            if Current.Text='main' then
              J.AddPair('main', TStringList(Current.Data).Text)
            else
            begin
              Z:=TJSONObject.Create;
              Z.AddPair('name', ModuleNameByNode(Current));
              Z.AddPair('source', TStringList(Current.Data).Text);

              M.AddElement(Z);
            end;

          J.AddPair('modules', M);

          JSONSaveToFile(J, S);
          CurrentProject := S;
          UpdateStatusBar;
        finally
          J.Free;
        end;
      end;
    finally
      Free;
    end;

end;

procedure TLPIdeMain.SelectNote(Sheet: TTabSheet);
var
  S,S1:string;
  node:TTreeNode;
begin
  S:=Sheet.Caption;
  node := ProjectTree.Items.GetFirstNode;
  while (S<>'') do
  begin
    S1:= StrSplit(S,'.');
    if (S1<>'') then
    begin
      repeat
        if node.Text=S1 then
        begin
          if node.HasChildren and (S<>'') then
            node := node.getFirstChild;

          Break;
        end
        else node := node.getNextSibling;
      until (node=nil);
    end;
  end;
  node.MakeVisible;
  node.Selected:= True;
end;

procedure TLPIdeMain.UpdateNote(Sheet: TTabSheet; Data: string);
var
  S,S1:string;
  node:TTreeNode;
begin
  S:=Sheet.Caption;
  node := ProjectTree.Items.GetFirstNode;
  while (S<>'') do
  begin
    S1:= StrSplit(S,'.');
    if (S1<>'') then
    begin
      repeat
        if node.Text=S1 then
        begin
          if node.HasChildren and (S<>'') then
            node := node.getFirstChild;

          Break;
        end
        else node := node.getNextSibling;
      until (node=nil);
    end;
  end;
  TStringList( node.Data).Text := Data;
end;

procedure TLPIdeMain.UpdateStatusBar;
begin
  LPIStatusBar.Panels[0].Text := 'Current project = '  + CurrentProject;
end;

end.
