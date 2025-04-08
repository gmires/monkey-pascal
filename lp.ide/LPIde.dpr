program LPIde;

uses
  Forms,
  lp.ide.main in 'lp.ide.main.pas' {LPIdeMain},
  lp.utils in '..\lp.lib\lp.utils.pas',
  lp.ide.module.source in 'lp.ide.module.source.pas' {LPModuleSourceFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLPIdeMain, LPIdeMain);
  Application.Run;
end.
