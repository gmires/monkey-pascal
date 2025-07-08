unit lp.ide.module.source;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, lp.edits, ExtCtrls;

type
  TLPModuleDrawBreakPoint = function(Module:string; ALine: Integer): Boolean of object;
  TLPModuleBreakPointClick = procedure(Module:string; ALine: Integer) of object;

  TLPModuleSourceFrame = class(TFrame)
    PanelBackgroud: TPanel;
    FLabelTop: TLabel;
    FMemoSource: TLPIMemo;
    procedure FMemoSourceBreakPointClick(ALine: Integer);
    function  FMemoSourceDrawBreakPoint(ACurrentLine: Integer): Boolean;
    procedure FMemoSourceChange(Sender: TObject);
  private
    { Private declarations }
    FModuleDrawBreakPoint: TLPModuleDrawBreakPoint;
    FModuleBreakPointClick: TLPModuleBreakPointClick;
    function  GetModified: Boolean;
  public
    procedure CheckMModified;
  public
    { Public declarations }
    Module:string;
    ModuleSrc:string;
    property modified: Boolean read GetModified;
    property MemoSource : TLPIMemo read FMemoSource;
    property LabelTop: TLabel read FLabelTop;
    property OnModuleDrawBreakPoint: TLPModuleDrawBreakPoint read FModuleDrawBreakPoint
      write FModuleDrawBreakPoint;
    property OnModuleBreakPointClick: TLPModuleBreakPointClick read FModuleBreakPointClick
      write FModuleBreakPointClick;
  end;

implementation

{$R *.dfm}

procedure TLPModuleSourceFrame.CheckMModified;
begin
  if modified then
    PanelBackgroud.Color := clRed
  else
    PanelBackgroud.Color := clBlack;
end;

procedure TLPModuleSourceFrame.FMemoSourceBreakPointClick(ALine: Integer);
begin
  if Assigned(FModuleBreakPointClick) then
    FModuleBreakPointClick(Module, ALine);
end;

procedure TLPModuleSourceFrame.FMemoSourceChange(Sender: TObject);
begin
  CheckMModified;
end;

function TLPModuleSourceFrame.FMemoSourceDrawBreakPoint(ACurrentLine: Integer): Boolean;
begin
  Result := Assigned(FModuleDrawBreakPoint);
  if Result then
    Result := FModuleDrawBreakPoint(Module, ACurrentLine);
end;

function TLPModuleSourceFrame.GetModified: Boolean;
begin
  Result := (MemoSource.Text<>ModuleSrc);
end;

end.
