unit lpi.debugger;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Grids, ValEdit, ImgList, Menus, ClipBrd

  , lp.environment

  ;

type
  TLPDebugger = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    BtnStepInto: TButton;
    VLEnv: TValueListEditor;
    Label1: TLabel;
    Panel2: TPanel;
    LSrcView: TListBox;
    Label2: TLabel;
    ILDbg: TImageList;
    BtnContinue: TButton;
    BtnStop: TButton;
    pmEnv: TPopupMenu;
    MnuCopyValue: TMenuItem;
    procedure BtnStepIntoClick(Sender: TObject);
    procedure BtnContinueClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure LSrcViewDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MnuCopyValueClick(Sender: TObject);
    procedure pmEnvPopup(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure DebuggerShow(ASource:string; ALine:Integer; AEnv:TEnvironment; var ANext:Boolean; var AContinue:Boolean);

var
  LPDebugger: TLPDebugger;

implementation

procedure DebuggerShow(ASource:string; ALine:Integer; AEnv:TEnvironment; var ANext:Boolean; var AContinue:Boolean);
var
  F:TLPDebugger;
  S:string;
  SMResult:Integer;
begin
  F:= TLPDebugger.Create(nil);
  try
    F.LSrcView.Items.Text := ASource;
    F.LSrcView.Selected[ALine-1] := True;

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

procedure TLPDebugger.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key=VK_F10) or (Key=VK_F11)) then BtnStepInto.Click
  else
  if (Key=VK_F5) then BtnContinue.Click
  else
  if (Key=VK_ESCAPE) then BtnStop.Click;
end;

procedure TLPDebugger.LSrcViewDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LB: TListBox;
begin
  LB := TListBox(Control);

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
