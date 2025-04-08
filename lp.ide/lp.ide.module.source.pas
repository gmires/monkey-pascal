unit lp.ide.module.source;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, lp.edits, ExtCtrls;

type
  TLPModuleSourceFrame = class(TFrame)
    PanelBackgroud: TPanel;
    FLabelTop: TLabel;
    FMemoSource: TLPIMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    property MemoSource : TLPIMemo read FMemoSource;
    property LabelTop: TLabel read FLabelTop;
  end;

implementation

{$R *.dfm}

end.
