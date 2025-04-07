unit lpi.debugger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Grids, ValEdit, ImgList, Menus, ClipBrd

  , lp.environment, lp.edits, lp.evaluator, lp.parser, lp.lexer

  ;

type
  TLPDebugger = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    BtnStepInto: TButton;
    Panel2: TPanel;
    Label2: TLabel;
    ILDbg: TImageList;
    BtnContinue: TButton;
    BtnStop: TButton;
    pmEnv: TPopupMenu;
    MnuCopyValue: TMenuItem;
    LSrcView: TLPIListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    edtEval: TEdit;
    MEvalResult: TMemo;
    Label3: TLabel;
    Panel5: TPanel;
    VLEnv: TValueListEditor;
    Label1: TLabel;
    procedure BtnStepIntoClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure LSrcViewDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure MnuCopyValueClick(Sender: TObject);
    procedure pmEnvPopup(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function LSrcViewDrawArrow(ACurrentLine: Integer): Boolean;
    procedure edtEvalKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Env:TEnvironment;
    Evl:TEvaluator;
  public
    { Public declarations }
  end;

procedure DebuggerShow(ASource:string; ALine:Integer; AEnv:TEnvironment; AEval:TEvaluator; var ANext:Boolean; var AContinue:Boolean);
function  Eval(value:string; Evl:TEvaluator; env:TEnvironment):string;

var
  LPDebugger: TLPDebugger;

implementation

function Eval(value:string; Evl:TEvaluator; env:TEnvironment):string;
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

procedure DebuggerShow(ASource:string; ALine:Integer; AEnv:TEnvironment; AEval:TEvaluator; var ANext:Boolean; var AContinue:Boolean);
var
  F:TLPDebugger;
  S:string;
  SMResult:Integer;
begin
  F:= TLPDebugger.Create(nil);
  try
    F.LSrcView.Items.Text := ASource;
    F.LSrcView.Tag := ALine;
    F.LSrcView.Selected[ALine-1] := True;
    F.Env := AEnv;
    F.Evl := AEval;

    for S in AEnv.Store.Keys do
      F.VLEnv.InsertRow(S,AEnv.Store[S].Inspect, True);

    SMResult := F.ShowModal;
    ANext    := (SMResult = mrRetry);
    AContinue:= (SMResult <> mrCancel);
  finally
    F.Free;
  end;
end;


{$R *.dfm}

procedure TLPDebugger.BtnContinueClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TLPDebugger.BtnStepIntoClick(Sender: TObject);
begin
  ModalResult := mrRetry;
end;

procedure TLPDebugger.BtnStopClick(Sender: TObject);
begin
  if MessageDlg('Vuoi interrompere il debug, Sei sicuro/a ?', mtConfirmation, mbYesNo, 0)=mrYes then
    ModalResult := mrCancel;
end;

procedure TLPDebugger.edtEvalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
    MEvalResult.Text := Eval(edtEval.Text, Evl, Env);
end;

procedure TLPDebugger.FormCreate(Sender: TObject);
begin
  Env := nil;
  Evl := nil;
end;

procedure TLPDebugger.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key=VK_F10) or (Key=VK_F11)) then BtnStepInto.Click
  else
  if (Key=VK_F5) then BtnContinue.Click
  else
  if (Key=VK_ESCAPE) then BtnStop.Click;
end;

function TLPDebugger.LSrcViewDrawArrow(ACurrentLine: Integer): Boolean;
begin
  Result := (LSrcView.Tag=ACurrentLine);
end;

procedure TLPDebugger.LSrcViewDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LB: TLPIListBox;
begin
  LB := TLPIListBox(Control);

  if odSelected in State then
  begin
    LB.Canvas.Brush.Color:= clRed;
    LB.Canvas.Font.Color := clWhite;
    LB.Canvas.Font.Style := [fsBold, fsUnderline];
  end;

  LB.Canvas.FillRect(Rect);
  LB.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, LB.Items[Index]);

  if (odFocused in State) and not (odNoFocusRect in State) then
    LB.Canvas.DrawFocusRect(Rect);
end;

procedure TLPDebugger.MnuCopyValueClick(Sender: TObject);
begin
  Clipboard.AsText := VLEnv.Values[ VLEnv.Keys[VLEnv.Row] ];
end;

procedure TLPDebugger.pmEnvPopup(Sender: TObject);
begin
  MnuCopyValue.Enabled := (VLEnv.Strings.Count>0);
end;

end.
