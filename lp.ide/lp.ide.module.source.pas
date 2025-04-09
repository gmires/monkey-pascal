unit lp.ide.module.source;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
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
  private
    { Private declarations }
    FModuleDrawBreakPoint: TLPModuleDrawBreakPoint;
    FModuleBreakPointClick: TLPModuleBreakPointClick;
  public
    { Public declarations }
    Module:string;
    property MemoSource : TLPIMemo read FMemoSource;
    property LabelTop: TLabel read FLabelTop;
    property OnModuleDrawBreakPoint: TLPModuleDrawBreakPoint read FModuleDrawBreakPoint
      write FModuleDrawBreakPoint;
    property OnModuleBreakPointClick: TLPModuleBreakPointClick read FModuleBreakPointClick
      write FModuleBreakPointClick;
  end;

implementation

{$R *.dfm}

procedure TLPModuleSourceFrame.FMemoSourceBreakPointClick(ALine: Integer);
begin
  if Assigned(FModuleBreakPointClick) then
    FModuleBreakPointClick(Module, ALine);
end;

function TLPModuleSourceFrame.FMemoSourceDrawBreakPoint(ACurrentLine: Integer): Boolean;
begin
  Result := Assigned(FModuleDrawBreakPoint);
  if Result then
    Result := FModuleDrawBreakPoint(Module, ACurrentLine);
end;

end.
