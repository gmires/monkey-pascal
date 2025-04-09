unit lp.ide.main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, ExtCtrls, StdCtrls, lp.edits, ImgList

  , DBXJSON, Generics.Collections

  , lp.ide.module.source
  , lp.base64
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
    procedure MnuProjectSaveWithNameClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    { -- -- }
    function  SourceDrawBreakPoint(Module:string; ACurrentLine: Integer): Boolean;
    procedure SourceBreakPointClick(Module:string; ALine: Integer);
  private
    { Private declarations }
    BreakPoints:TStringList;
    CurrentProject:string;
    function  ProjectToStreamCompress: TMemoryStream;
    procedure StreamToProject(stream:TMemoryStream);
  public
    { Public declarations }
    { -- project -- }
    procedure NewProject;
    procedure CloseProject;
    procedure OpenProject;
    procedure SaveProject(CurrentProjectPath:string='');
    { -- module -- }
    procedure NewModule(Name,Data: string; Parent:TTreeNode);
    procedure AddModule(Name,Data: string);
    procedure RemoveModule(Node:TTreeNode);
    { -- source tab and tree -- }
    function  ModuleNameByNode(Node:TTreeNode):string;
    procedure OpenTab(Node:TTreeNode);
    procedure CloseTab(Node:TTreeNode);
    procedure SelectNote(Sheet:TTabSheet);
    function  GetNodeByModuleName(ModuleName:string):TTreeNode;
    function  GetParentNodeByModuleName(ModuleName:string):TTreeNode;
    procedure UpdateNode(Sheet:TTabSheet; Data:string);
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

procedure TLPIdeMain.AddModule(Name, Data: string);
var
  Node,Parent:TTreeNode;
begin
  Name := LowerCase(Name);
  Parent := GetParentNodeByModuleName(Name);
  if (Parent<>nil) then
  begin
    while (Pos('.', Name)>0) do
      StrSplit(Name,'.');

    Node := ProjectTree.Items.AddChild(Parent,Name);
    Node.Data := TStringList.Create;
    TStringList(Node.Data).Text := Data;
  end;
end;

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
  BreakPoints.Clear;
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

procedure TLPIdeMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BreakPoints.Free;
end;

procedure TLPIdeMain.FormCreate(Sender: TObject);
begin
  BreakPoints:=TStringList.Create;
  NewProject;
end;

function TLPIdeMain.GetNodeByModuleName(ModuleName: string): TTreeNode;
var
  S1:string;
begin
  Result := ProjectTree.Items.GetFirstNode;
  while (ModuleName<>'') do
  begin
    S1:= StrSplit(ModuleName,'.');
    if (S1<>'') then
    begin
      repeat
        if Result.Text=S1 then
        begin
          if Result.HasChildren and (ModuleName<>'') then
            Result := Result.getFirstChild;

          Break;
        end
        else Result := Result.getNextSibling;
      until (Result=nil);
    end;
  end;
end;

function TLPIdeMain.GetParentNodeByModuleName(ModuleName: string): TTreeNode;
var
  S:string;
begin
  S:='';
  while (Pos('.', ModuleName)>0) do
  begin
    if S<>'' then S:=S+'.';
    S:= S+StrSplit(ModuleName,'.');
  end;
  Result := GetNodeByModuleName(S);
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
  SaveProject(CurrentProject);
end;

procedure TLPIdeMain.MnuProjectSaveWithNameClick(Sender: TObject);
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
var
  M:TMemoryStream;
begin
  With TOpenDialog.Create(Self) do
  try
    Title := 'Open LPI Project';
    Filter:= 'LPI Project files (*.lpi)';
    if Execute then
    begin
      CloseProject;
      M:= TMemoryStream.Create;
      try
        M.LoadFromFile(FileName);
        StreamToProject(M);
      finally
        M.Free;
      end;
      CurrentProject:=FileName;
      UpdateStatusBar;
    end;
  finally
    Free;
  end;
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
    F.Module := S;
    F.LabelTop.Caption := 'Source of <'+S+'> module';
    F.MemoSource.Text := D;
    F.Align := alClient;
    F.Parent:= current;
    F.OnModuleDrawBreakPoint := SourceDrawBreakPoint;
    F.OnModuleBreakPointClick:= SourceBreakPointClick;
  end;

  PCMain.ActivePage := current;
end;

procedure TLPIdeMain.PCMainChange(Sender: TObject);
begin
  SelectNote(PCMain.ActivePage);
end;

function TLPIdeMain.ProjectToStreamCompress: TMemoryStream;
var
  S:TStringStream;
  J,Z:TJSONObject;
  M:TJSONArray;
  Current:TTreeNode;
begin
  Result := TMemoryStream.Create;

  J:= TJSONObject.Create;
  M:= TJSONArray.Create;
  try
    for current in ProjectTree.Items do
      if Current.Text='main' then
        J.AddPair('main', StringToBase64(TStringList(Current.Data).Text))
      else
      begin
        Z:=TJSONObject.Create;
        Z.AddPair('name', ModuleNameByNode(Current));
        Z.AddPair('source',StringToBase64( TStringList(Current.Data).Text));

        M.AddElement(Z);
      end;

    J.AddPair('modules', M);

    S:= TStringStream.Create;
    try
      S.WriteString(JSONToString(J));;
      S.Position:=0;
      CompressStream(S,Result);
      Result.Position := 0;
    finally
      S.Free;
    end;

  finally
    J.Free;
  end;
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

procedure TLPIdeMain.SaveProject(CurrentProjectPath:string);
var
  i:Integer;
  F:TLPModuleSourceFrame;
  S:string;
  MS:TMemoryStream;
begin
  for i := 0 to PCMain.PageCount-1 do
  begin
    F:= TLPModuleSourceFrame( PCMain.Pages[i].Controls[0] );
    UpdateNode( PCMain.Pages[i], F.MemoSource.Text );
  end;

  MS:=ProjectToStreamCompress;
  if MS.Size>0 then
  begin
    S:=CurrentProjectPath;
    if (S='') then
    begin
      with TSaveDialog.Create(Self) do
      try
        if Execute then
          S:=ChangeFileExt(FileName,'.lpi');
      finally
        Free;
      end;
    end;
    MS.SaveToFile(S);
  end;

  CurrentProject := S;
  UpdateStatusBar;
end;

procedure TLPIdeMain.SelectNote(Sheet: TTabSheet);
var
  node:TTreeNode;
begin
  node := GetNodeByModuleName(Sheet.Caption);
  if (node<>nil) then
  begin
    node.MakeVisible;
    node.Selected:= True;
  end;
end;

procedure TLPIdeMain.SourceBreakPointClick(Module:string; ALine: Integer);
var
  I:Integer;
  S:string;
begin
  S:= Module + '.' + IntToStr(ALine);
  I:= BreakPoints.IndexOf(S);
  if (I<0) then
    BreakPoints.Add(S)
  else
    BreakPoints.Delete(I);
end;

function TLPIdeMain.SourceDrawBreakPoint(Module:string; ACurrentLine: Integer): Boolean;
begin
 Result := (BreakPoints.IndexOf(Module + '.' + IntToStr(ACurrentLine))>=0);
end;

procedure TLPIdeMain.StreamToProject(stream:TMemoryStream);
var
  J:TJSONObject;
  M:TJSONArray;
  i:Integer;
begin
  J:=JSONLoadToCompress(stream);
  if (J<>nil) then
  try
    NewModule('main', Base64ToString(J.Get('main').JsonValue.Value),nil);
    M:= J.Get('modules').JsonValue as TJSONArray;
    if (M.Size>0) then
      for i := 0 to M.Size-1 do
        AddModule((M.Get(i) as TJSONObject).Get('name').JsonValue.Value
          , Base64ToString((M.Get(i) as TJSONObject).Get('source').JsonValue.Value));
  finally
    J.Free;
  end;
end;

procedure TLPIdeMain.UpdateNode(Sheet: TTabSheet; Data: string);
var
  node:TTreeNode;
begin
  node := GetNodeByModuleName(Sheet.Caption);
  if (node<>nil) then
    TStringList( node.Data).Text := Data;
end;

procedure TLPIdeMain.UpdateStatusBar;
begin
  if CurrentProject='' then
    LPIStatusBar.Panels[0].Text := 'Current project = NewProject'
  else
    LPIStatusBar.Panels[0].Text := 'Current project = '  + CurrentProject;
end;

end.
