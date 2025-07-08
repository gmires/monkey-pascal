unit lp.ide.main;

interface

{$I ..\lp.lib\lp.inc}

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, ExtCtrls, StdCtrls, lp.edits,  StrUtils

  , {$IFDEF LPI_D28} JSON {$ELSE} DBXJSON {$ENDIF}
  , Generics.Collections

  , lp.ide.module.source
  , lp.ide.debugger

  , lp.base64
  , lp.builtins
  , lp.environment
  , lp.evaluator
  , lp.parser
  , lp.lexer
  , lp.advobject
  , {$IFDEF LPI_D28} System.ImageList {$ELSE} ImgList, System.ImageList {$ENDIF}
  ;

type
  TLPIdeMode = (mStandard, mEmbedded);

  TLPIdeMain = class(TForm)
    MMMain: TMainMenu;
    Panel1: TPanel;
    Panel2: TPanel;
    MnuFile: TMenuItem;
    MnuProjectNew: TMenuItem;
    MnuProjectOpen: TMenuItem;
    N1: TMenuItem;
    MnuExit: TMenuItem;
    TbMain: TToolBar;
    tbOpenProject: TToolButton;
    tbNewModule: TToolButton;
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
    PMTabs: TPopupMenu;
    MnuTabClose: TMenuItem;
    MnuTabCloseAll: TMenuItem;
    MnuTabCloseAllOther: TMenuItem;
    N3: TMenuItem;
    PMRun: TPopupMenu;
    MnuRunStepByStep: TMenuItem;
    N4: TMenuItem;
    MnuRunOneStep: TMenuItem;
    PMConsole: TPopupMenu;
    MnuConsoleClear: TMenuItem;
    Panel5: TPanel;
    Label1: TLabel;
    ConsoleLog: TListBox;
    Splitter1: TSplitter;
    Panel6: TPanel;
    Splitter2: TSplitter;
    PCMain: TPageControl;
    Panel4: TPanel;
    Label2: TLabel;
    ProjectTree: TTreeView;
    LPIStatusBar: TStatusBar;
    MnuCerca: TMenuItem;
    MnuFind: TMenuItem;
    MnuReplace: TMenuItem;
    FindDialogModule: TFindDialog;
    ReplaceDialogModule: TReplaceDialog;
    { -- -- }
    function  SourceDrawBreakPoint(Module:string; ACurrentLine: Integer): Boolean;
    procedure SourceBreakPointClick(Module:string; ALine: Integer);
    function  EvalNotifer(AModule:string; ALine, APos: Integer; AEnv:TEnvironment; AEval:TEvaluator; var AContinue:Boolean):Boolean;
    { -- -- }
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
    procedure PMTabsPopup(Sender: TObject);
    procedure MnuTabCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MnuTabCloseAllClick(Sender: TObject);
    procedure MnuTabCloseAllOtherClick(Sender: TObject);
    procedure MnuRunOptionsClick(Sender: TObject);
    procedure MnuRunOneStepClick(Sender: TObject);
    procedure MnuConsoleClearClick(Sender: TObject);
    procedure MnuFindClick(Sender: TObject);
    procedure MnuReplaceClick(Sender: TObject);
    procedure FindDialogModuleFind(Sender: TObject);
    procedure ReplaceDialogModuleReplace(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    LPIdeMode:TLPIdeMode;
    FSelPos: integer;
    BreakPoints:TStringList;
    CurrentProjectModifed:Boolean;
    CurrentProject:string;
    CurrentProjectParams:string;
    DebuggerNextStep:Boolean;
    function  ProjectToJSON: TJSONObject;
    function  ProjectToStreamCompress: TMemoryStream;
    procedure StreamToProject(stream:TMemoryStream);
  public
    { Public declarations }
    { -- project -- }
    procedure NewProject;
    function  RunProject(DebugStetByStep:Boolean=false):Boolean;
    function  CloseProject:Boolean;
    procedure OpenProject; overload;
    procedure OpenProject(AProjectName:string; AProjectSource:TMemoryStream); overload;
    function  SaveProject(CurrentProjectPath:string=''):Boolean;
    { -- module -- }
    procedure NewModule(Name,Data: string; Parent:TTreeNode);
    procedure AddModule(Name,Data: string);
    procedure RemoveModule(Node:TTreeNode);
    { -- source tab and tree -- }
    function  ModuleNameByNode(Node:TTreeNode):string;
    procedure OpenTab(Node:TTreeNode);
    procedure CloseTab(Node:TTreeNode); overload;
    procedure CloseTab(ModuleName:string); overload;
    procedure CheckAndCloseTab(Sheet:TTabSheet);
    procedure SelectNote(Sheet:TTabSheet);
    function  GetNodeByModuleName(ModuleName:string):TTreeNode;
    function  GetParentNodeByModuleName(ModuleName:string):TTreeNode;
    function  GetSourceNodeByModuleName(ModuleName:string):string;
    procedure UpdateNodes;
    procedure UpdateNode(ModuleName: string; Data:string);
    procedure UpdateStatusBar;
    { -- utility -- }
    procedure AddToConsoleLog(value:string);
    procedure ErrorToConsoleLog(value:string);
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

function LpiOpenEmbeddedIde(AProjectName: string; var AProjectSource:TMemoryStream): Boolean;

implementation

uses lp.utils;

{$R *.dfm}

function LpiOpenEmbeddedIde(AProjectName: string; var AProjectSource:TMemoryStream): Boolean;
begin
  With TLPIdeMain.Create(Application) do
  try
    CurrentProject := AProjectName;
    LPIdeMode := mEmbedded;
    OpenProject(AProjectName, AProjectSource);

    Result := (ShowModal=mrOk);
    if Result then
    begin
      AProjectSource.Free;
      AProjectSource:= ProjectToStreamCompress;
    end;
  finally
    Free;
  end;
end;

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

procedure TLPIdeMain.AddToConsoleLog(value: string);
begin
  ConsoleLog.Items.Add('>>> ' + value);
end;

procedure TLPIdeMain.CheckAndCloseTab(Sheet: TTabSheet);
var
  F: TLPModuleSourceFrame;
begin
  F:= TLPModuleSourceFrame( Sheet.Controls[0] );
  if F.modified then
  if (MessageDlg('Il sorgente del modulo è stato modificato, vuoi salvare prima di chiudere?'
    ,mtConfirmation
    ,mbYesNo,0)=mrYes) then
  begin
    UpdateNode(Sheet.Caption, F.MemoSource.Text);
  end;
  CloseTab(Sheet.Caption);
end;

function TLPIdeMain.CloseProject:Boolean;
var
  current:TTreeNode;
begin
  Result := True;

  UpdateNodes;
  if CurrentProjectModifed then
  if (MessageDlg('Il progetto è stato modifica, vuoi salvare le modifiche?'
    ,mtConfirmation
    ,mbYesNo,0)=mrYes) then
  begin
    Result:=SaveProject(CurrentProject);
  end;

  if Result then
  begin
    for current in ProjectTree.Items do
    begin
      CloseTab(current);
      TStringList(current.Data).Free;
    end;

    ProjectTree.Items.Clear;
    BreakPoints.Clear;
  end;
end;

procedure TLPIdeMain.CloseTab(ModuleName: string);
var
  i:Integer;
  current:TTabSheet;
begin
  current := nil;

  for i := 0 to PCMain.PageCount-1 do
    if (LowerCase(PCMain.Pages[i].Caption)=LowerCase(ModuleName)) then
    begin
      current := PCMain.Pages[i];
      Break;
    end;

  if Assigned(current) then
  begin
    current.Free;
  end;
end;

procedure TLPIdeMain.CloseTab(Node: TTreeNode);
begin
  CloseTab(ModuleNameByNode(Node));
end;

procedure TLPIdeMain.ErrorToConsoleLog(value: string);
begin
  ConsoleLog.Items.Add('ERROR >>> ' + value);
end;

function TLPIdeMain.EvalNotifer(AModule: string; ALine, APos: Integer; AEnv: TEnvironment; AEval: TEvaluator; var AContinue: Boolean): Boolean;
begin
  Result := True;
  if (Pos('main', AModule)>0) then { -- questo perchè tutti i moduli dell'applicazione fanno riferimento a main, il resto sono builtin, e quindi non debuggabili -- }
    if (BreakPoints.IndexOf(AModule+'.'+IntToStr(ALine))>=0) or DebuggerNextStep then
      LPIDebugger(GetSourceNodeByModuleName(AModule),ALine, AEnv, AEval, DebuggerNextStep, AContinue);
end;

procedure TLPIdeMain.FindDialogModuleFind(Sender: TObject);
var
  S : string;
  startpos : integer;
  F:TLPModuleSourceFrame;
begin
  F:= TLPModuleSourceFrame( PCMain.ActivePage.Controls[0] );
  with TFindDialog(Sender) do
  begin
    {If the stored position is 0 this cannot be a find next. }
    if FSelPos = 0 then
      Options := Options - [frFindNext];

     { Figure out where to start the search and get the corresponding
       text from the memo. }
    if frfindNext in Options then
    begin
      { This is a find next, start after the end of the last found word. }
      StartPos := FSelPos + Length(Findtext);
      S := Copy(F.MemoSource.Lines.Text, StartPos, MaxInt);
    end
    else
    begin
      { This is a find first, start at the, well, start. }
      S := F.MemoSource.Lines.Text;
      StartPos := 1;
    end;
    { Perform a global case-sensitive search for FindText in S }
    FSelPos := Pos(FindText, S);
    if FSelPos > 0 then
    begin
       { Found something, correct position for the location of the start
         of search. }
      FSelPos := FSelPos + StartPos - 1;
      F.MemoSource.SelStart := FSelPos - 1;
      F.MemoSource.SelLength := Length(FindText);
      F.MemoSource.SetFocus;
    end
    else
    if (TFindDialog(Sender).Tag=0) then
    begin
      { No joy, show a message. }
      if frfindNext in Options then
        S := Concat('There are no further occurences of "', FindText,'" in Memo1.')
      else
        S := Concat('Could not find "', FindText, '" in Current Source.');
      MessageDlg(S, mtError, [mbOK], 0);
    end;
  end;

end;

procedure TLPIdeMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  BreakPoints.Free;
end;

procedure TLPIdeMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  UpdateNodes;
  if CurrentProjectModifed then
  begin
    CanClose := (MessageDlg('Il progetto è stato modifica, vuoi chiudere senza salvare le modifiche, Sei sicuro/a?'
      ,mtConfirmation
      ,mbYesNo,0)=mrYes);
  end;
end;

procedure TLPIdeMain.FormCreate(Sender: TObject);
begin
  LPIdeMode := mStandard;

  Screen.MenuFont.Name := 'consolas';
  Screen.MenuFont.Size := 8;

  LPIdeMain:=Self;
  BreakPoints:=TStringList.Create;
  CloseProject;
  NewProject;
end;

procedure TLPIdeMain.FormShow(Sender: TObject);
begin
  // ***
  // ** configure embedded lpi ide for integrations
  // ***
  tbNewPorject.Visible := (LPIdeMode=mStandard);
  tbOpenProject.Visible := tbNewPorject.Visible;
  MnuProjectNew.Visible := tbNewPorject.Visible;
  MnuProjectOpen.Visible:= tbNewPorject.Visible;
  N1.Visible:= tbNewPorject.Visible;
  MnuProjectSaveWithName.Visible:= tbNewPorject.Visible;
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

function TLPIdeMain.GetSourceNodeByModuleName(ModuleName: string): string;
begin
  Result := TStringList( GetNodeByModuleName(ModuleName).Data ).Text;
end;

procedure TLPIdeMain.MnuReplaceClick(Sender: TObject);
begin
  FSelPos := 0;
  ReplaceDialogModule.Execute;
end;

procedure TLPIdeMain.MnuFindClick(Sender: TObject);
begin
  FSelPos := 0;
  FindDialogModule.Execute;
end;

procedure TLPIdeMain.MnuConsoleClearClick(Sender: TObject);
begin
  ConsoleLog.Clear;
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
      S:=LowerCase(Trim(InputBox('Aggiungi Modulo', 'Module Name','')));
      if ModuleNameIsValid(S) then
        NewModule(S,'',ProjectTree.Selected);
    until ModuleNameIsValid(S) or (S='');
  end;
end;

procedure TLPIdeMain.MnuProjectNewClick(Sender: TObject);
begin
  if CloseProject then
    NewProject;
end;

procedure TLPIdeMain.MnuProjectOpenClick(Sender: TObject);
begin
//  if CloseProject then
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
  RunProject(Sender=MnuRunStepByStep);
end;

procedure TLPIdeMain.MnuRunOneStepClick(Sender: TObject);
begin
  MnuRunDebugClick(MnuRunStepByStep);
end;

procedure TLPIdeMain.MnuRunOptionsClick(Sender: TObject);
begin
  CurrentProjectParams := InputBox('LPI Run options','Launch with parameters..', CurrentProjectParams);
end;

procedure TLPIdeMain.MnuTabCloseAllClick(Sender: TObject);
var
  i:Integer;
begin
  for i := PCMain.PageCount-1 downto 0 do
    if (PCMain.Pages[i].Caption<>'main') then
      CheckAndCloseTab(PCMain.Pages[i]);

  SelectNote(PCMain.ActivePage);
end;

procedure TLPIdeMain.MnuTabCloseAllOtherClick(Sender: TObject);
var
  i:Integer;
  P:TTabSheet;
begin
  P := PCMain.ActivePage;
  for i := PCMain.PageCount-1 downto 0 do
    if (PCMain.Pages[i].Caption<>'main') and (PCMain.Pages[i]<>P) then
      CheckAndCloseTab(PCMain.Pages[i]);

  PCMain.ActivePage := P;
  SelectNote(P);
end;

procedure TLPIdeMain.MnuTabCloseClick(Sender: TObject);
begin
  CheckAndCloseTab(PCMain.ActivePage);
  SelectNote(PCMain.ActivePage);
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
  NM:TTreeNode;
begin
  Name := LowerCase(Name);

  NM:=ProjectTree.Items.AddChild(Parent,Name);
  NM.Data := TStringList.Create;
  TStringList(NM.Data).Text := Data;
  ProjectTree.Selected := NM;
  NM.MakeVisible;

  OpenTab(NM);
end;

procedure TLPIdeMain.NewProject;
begin
  CurrentProjectParams:='';
  CurrentProjectModifed:=false;
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

procedure TLPIdeMain.OpenProject(AProjectName:string; AProjectSource: TMemoryStream);
begin
  if (AProjectSource.Size>0) then
  begin
    CloseProject;
    StreamToProject(AProjectSource);
    CurrentProject:=AProjectName;
    UpdateStatusBar;
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
    F.ModuleSrc:= D;
    F.Align := alClient;
    F.Parent:= current;
    F.OnModuleDrawBreakPoint := SourceDrawBreakPoint;
    F.OnModuleBreakPointClick:= SourceBreakPointClick;
    F.CheckMModified;
  end;

  PCMain.ActivePage := current;
end;

procedure TLPIdeMain.PCMainChange(Sender: TObject);
begin
  SelectNote(PCMain.ActivePage);
end;

procedure TLPIdeMain.PMTabsPopup(Sender: TObject);
begin
  MnuTabClose.Enabled := ((PCMain.ActivePage<>nil) and (PCMain.ActivePage.Caption<>'main'));
end;

function TLPIdeMain.ProjectToJSON: TJSONObject;
var
  Z:TJSONObject;
  M:TJSONArray;
  current: TTreeNode;
begin
  Result:= TJSONObject.Create;
  M:= TJSONArray.Create;
  for current in ProjectTree.Items do
    if Current.Text='main' then
      Result.AddPair('main', StringToBase64(TStringList(Current.Data).Text))
    else
    begin
      Z:=TJSONObject.Create;
      Z.AddPair('name', ModuleNameByNode(Current));
      Z.AddPair('source',StringToBase64( TStringList(Current.Data).Text));

      M.AddElement(Z);
    end;

  Result.AddPair('modules', M);
end;

function TLPIdeMain.ProjectToStreamCompress: TMemoryStream;
var
  S:TStringStream;
  J:TJSONObject;
begin
  Result := TMemoryStream.Create;

  J:= ProjectToJSON;
  try
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

procedure TLPIdeMain.ReplaceDialogModuleReplace(Sender: TObject);
var
  F:TLPModuleSourceFrame;
begin
  F:= TLPModuleSourceFrame( PCMain.ActivePage.Controls[0] );
  if (F.MemoSource.SelLength=0) then
    FindDialogModuleFind(Sender);
  if (F.MemoSource.SelLength>0) then
    F.MemoSource.SelText := ReplaceDialogModule.ReplaceText;

  if frReplaceAll in ReplaceDialogModule.Options then
  begin
    ReplaceDialogModule.Tag:=1;
    FindDialogModuleFind(Sender);
    while (F.MemoSource.SelLength>0) do
    begin
      F.MemoSource.SelText := ReplaceDialogModule.ReplaceText;
      FindDialogModuleFind(Sender);
    end;
    ReplaceDialogModule.Tag:=0;
  end;
end;

function TLPIdeMain.RunProject(DebugStetByStep:Boolean):Boolean;
var
  i,y:Integer;
  J:TJSONObject;
  M:TJSONArray;
  L:TLexer;
  P:TParser;
  Env:TEnvironment;
  Evl:TEvaluator;
  Par:TArrayObject;
  EvR:TEvalObject;
  MainProgram:TASTProgram;
  S,S1:string;
begin
  DebuggerNextStep := DebugStetByStep;
  Result:=True;
  ClearProjectModule;
  UpdateNodes;
  J:=ProjectToJSON;
  try
    M:= J.Get('modules').JsonValue as TJSONArray;
    if (M.Size>0) then
      for i := 0 to M.Size-1 do
      begin
        L:=TLexer.Create(Base64ToString((M.Get(i) as TJSONObject).Get('source').JsonValue.Value)
          , (M.Get(i) as TJSONObject).Get('name').JsonValue.Value);
        try
          P:=TParser.Create(L);
          try
            builtedmodules.Add(L.Module, P.ParseProgram);
            Result := P.Errors.Count=0;
            if NOT Result then
            begin
              for y := 0 to P.Errors.Count-1 do
                ErrorToConsoleLog(P.Errors[y]);
            end;
          finally
            P.Free;
          end;
        finally
          L.Free;
        end;
      end;

    if Result then
    begin
      L:=TLexer.Create(Base64ToString(J.Get('main').JsonValue.Value), 'main');
      try
        P:=TParser.Create(L);
        try
          MainProgram := P.ParseProgram;
          Result := P.Errors.Count=0;
          if NOT Result then
            for y := 0 to P.Errors.Count-1 do
              ErrorToConsoleLog(P.Errors[y]);
        finally
          P.Free;
        end;
      finally
        L.Free;
      end;

      if Result then
      begin
        Par:=TArrayObject.CreateWithElements;
        try
          Par.GcManualFree:= True;
          for i := 0 to ParamCount do
            Par.Elements.Add(TStringObject.Create(ParamStr(i)));

          if (CurrentProject<>'') then
            Par.Elements.Add(TStringObject.Create(CurrentProject));

          S:=CurrentProjectParams;
          while (S<>'') do
          begin
            S1:= StrSplit(S,' ');
            if (S1<>'') then
              Par.Elements.Add(TStringObject.Create(S1));
          end;

          for i := 0 to Par.Elements.Count-1 do
            Par.Elements[i].GcManualFree := True;

          Env:=TEnvironment.Create;
          Evl:=TEvaluator.Create;
          try
            Evl.EvalNotifierEvent := EvalNotifer;
            Env.SetOrCreateValue('params', Par, True);
            EvR := Evl.Run(MainProgram, Env);
            if (EvR<>nil) then
              AddToConsoleLog(EvR.Inspect)
          finally
            Env.Free;
            Evl.Free;
          end;

        finally
          for i := 0 to Par.Elements.Count-1 do
            Par.Elements[i].Free;
          Par.Free;
        end;
      end;
    end;
  finally
    J.Free;
  end;
end;

function TLPIdeMain.SaveProject(CurrentProjectPath:string):Boolean;
var
  S:string;
  MS:TMemoryStream;
begin
  Result := True;
  UpdateNodes;

  if (LPIdeMode=mEmbedded) then
  begin
    CurrentProjectModifed := False;
    ModalResult := mrOk;
  end
  else
  begin
    MS:=ProjectToStreamCompress;
    if MS.Size>0 then
    begin
      S:=CurrentProjectPath;
      if (S='') then
      begin
        with TSaveDialog.Create(Self) do
        try
          if Execute then
            if FileName<>'' then
              S:=ChangeFileExt(FileName,'.lpi');
        finally
          Free;
        end;
      end;

      Result := (S<>'');
      if Result then
      begin
        MS.SaveToFile(S);
        CurrentProjectModifed := False;
      end;
    end;

    CurrentProject := S;
    UpdateStatusBar;
  end;
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

procedure TLPIdeMain.UpdateNode(ModuleName: string; Data: string);
var
  node:TTreeNode;
begin
  node := GetNodeByModuleName(ModuleName);
  if (node<>nil) then
  begin
    CurrentProjectModifed:= CurrentProjectModifed or (TrimRight(TStringList( node.Data).Text) <> TrimRight(Data));
    TStringList( node.Data).Text := Data;
  end;
end;

procedure TLPIdeMain.UpdateNodes;
var
  i:Integer;
  F:TLPModuleSourceFrame;
begin
  for i := 0 to PCMain.PageCount-1 do
  begin
    F:= TLPModuleSourceFrame( PCMain.Pages[i].Controls[0] );
    UpdateNode( PCMain.Pages[i].Caption, F.MemoSource.Text );
    F.ModuleSrc := F.MemoSource.Text;
    F.CheckMModified;
  end;
end;

procedure TLPIdeMain.UpdateStatusBar;
begin
  if CurrentProject='' then
    LPIStatusBar.Panels[0].Text := 'Current project = NewProject'
  else
    LPIStatusBar.Panels[0].Text := 'Current project = '  + CurrentProject;
end;

{ -- override builtin function -- }

function _PrintLn(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to args.Count-1 do
    LPIdeMain.AddToConsoleLog(args[i].Inspect);
end;

function _Print(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
  S: string;
begin
  Result := nil;
  for i := 0 to args.Count-1 do
    S:=S+' '+args[i].Inspect;

  S:=TrimLeft(S);
  if S<>'' then
    LPIdeMain.AddToConsoleLog(S);
end;

function _ReadLn(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
var
  S:string;
begin
  S:= '<< ';
  if (args.Count>1) then
    Result:= TErrorObject.newError('wrong number of arguments. got=%d, want max=1', [args.Count])
  else
  if ((args.Count=1) and (args[0].ObjectType<>STRING_OBJ)) then
    Result:= TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
  begin
    if ((args.Count=1) and (args[0].ObjectType=STRING_OBJ)) then
      S:=TStringObject(args[0]).Value;

    Result := TStringObject.Create(InputBox('LPI IDE Input',S,''));
  end;
end;

function _Wait(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  Result := TNullObject.Create;
end;

procedure init;
begin
  builtins['println'] := TBuiltinObject.Create(_PrintLn);
  builtins['print'] := TBuiltinObject.Create(_Print);
  builtins['readln'] := TBuiltinObject.Create(_ReadLn);
  builtins['wait'] := TBuiltinObject.Create(_Wait);
end;

initialization
  init;

end.
