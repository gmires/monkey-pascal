unit lp.ide.debugger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ValEdit, ExtCtrls, lp.edits, ComCtrls, ToolWin,
  ClipBrd, ImgList, Menus

  , lp.environment
  , lp.evaluator
  , lp.lexer
  , lp.parser

  ;

var
  FormTop: Integer;
  FormLeft: Integer;

type
  TLPIdeDebugger = class(TForm)
    Panel1: TPanel;
    TbMain: TToolBar;
    tbStepInto: TToolButton;
    tbContinue: TToolButton;
    tbExit: TToolButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    LSource: TLPIListBox;
    Splitter1: TSplitter;
    Panel5: TPanel;
    Label1: TLabel;
    VLEnv: TValueListEditor;
    Panel4: TPanel;
    Label3: TLabel;
    edtEval: TEdit;
    MEvalResult: TMemo;
    Splitter2: TSplitter;
    ILDebugger: TImageList;
    pmEnv: TPopupMenu;
    MnuCopyValue: TMenuItem;
    ComboEnv: TComboBox;
    procedure tbStepIntoClick(Sender: TObject);
    procedure tbContinueClick(Sender: TObject);
    procedure tbExitClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtEvalKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MnuCopyValueClick(Sender: TObject);
    function  LSourceDrawArrow(ACurrentLine: Integer): Boolean;
    procedure pmEnvPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure ShowEnvironment(env:TEnvironment);
  public
    { Public declarations }
    Env:TEnvironment;
    Evl:TEvaluator;
  end;

var
  LPIdeDebugger: TLPIdeDebugger;

procedure LPIDebugger(ASource:string; ALine:Integer; AEnv:TEnvironment; AEval:TEvaluator; var ANext:Boolean; var AContinue:Boolean);
function  Eval(value:string; Evl:TEvaluator; env:TEnvironment):string;

implementation

{$R *.dfm}

procedure LPIDebugger(ASource:string; ALine:Integer; AEnv:TEnvironment; AEval:TEvaluator; var ANext:Boolean; var AContinue:Boolean);
var
  F:TLPIdeDebugger;
  S:string;
  SMResult:Integer;
begin
  F:= TLPIdeDebugger.Create(nil);
  try
    if (FormTop<>0) then
    begin
      F.Position := poDesigned;
      F.Top := FormTop;
      F.Left := FormLeft;
    end;
    F.LSource.Items.Text := ASource;
    F.LSource.Tag := ALine;
    F.LSource.Selected[ALine-1] := True;
    F.Env := AEnv;
    F.Evl := AEval;

    F.ShowEnvironment(AEnv);

    SMResult := F.ShowModal;
    ANext    := (SMResult = mrRetry);
    AContinue:= (SMResult <> mrCancel);
  finally
    F.Free;
  end;
end;

function  Eval(value:string; Evl:TEvaluator; env:TEnvironment):string;
var
  L:TLexer;
  P:TParser;
  R:TEvalObject;
begin
  L:=TLexer.Create(value,'');
  try
    P:= TParser.Create(L);
    try
      R := Evl.Eval(P.ParseProgram, env);
      if Assigned(R) then
        Result := R.Inspect;
    finally
      P.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TLPIdeDebugger.edtEvalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
    MEvalResult.Text := Eval(edtEval.Text, Evl, Env);
end;

procedure TLPIdeDebugger.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormTop:=Self.Top;
  FormLeft:=Self.Left;
end;

procedure TLPIdeDebugger.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key=VK_F10) or (Key=VK_F11)) then tbStepInto.Click
  else
  if (Key=VK_F5) then tbContinue.Click
  else
  if (Key=VK_ESCAPE) then tbExit.Click;
end;

function TLPIdeDebugger.LSourceDrawArrow(ACurrentLine: Integer): Boolean;
begin
  Result := (LSource.Tag=ACurrentLine);
end;

procedure TLPIdeDebugger.MnuCopyValueClick(Sender: TObject);
begin
  Clipboard.AsText := VLEnv.Values[ VLEnv.Keys[VLEnv.Row] ];
end;

procedure TLPIdeDebugger.pmEnvPopup(Sender: TObject);
begin
  MnuCopyValue.Enabled := (VLEnv.Strings.Count>0);
end;

procedure TLPIdeDebugger.ShowEnvironment(env: TEnvironment);
var
  i:Integer;
  S:string;
begin
  for i := VLEnv.Strings.Count-1 downto 0 do
    VLEnv.DeleteRow(i);

  for S in env.Store.Keys do
    VLEnv.InsertRow(S, env.Store[S].Inspect, True);
end;

procedure TLPIdeDebugger.tbContinueClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TLPIdeDebugger.tbExitClick(Sender: TObject);
begin
  if MessageDlg('Vuoi interrompere il debug, Sei sicuro/a ?', mtConfirmation, mbYesNo, 0)=mrYes then
    ModalResult := mrCancel;
end;

procedure TLPIdeDebugger.tbStepIntoClick(Sender: TObject);
begin
  ModalResult := mrRetry;
end;

initialization
  FormLeft := 0;
  FormTop := 0;

end.
