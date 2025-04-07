program LPIde;

uses
  Forms,
  lp.ide.main in 'lp.ide.main.pas' {LPIdeMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLPIdeMain, LPIdeMain);
  Application.Run;
end.
