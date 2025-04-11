program LPIde;

uses
  Forms,
  lp.ide.main in 'lp.ide.main.pas' {LPIdeMain},
  lp.ide.module.source in 'lp.ide.module.source.pas' {LPModuleSourceFrame: TFrame},
  lp.advobject in '..\lp.lib\lp.advobject.pas',
  lp.base64 in '..\lp.lib\lp.base64.pas',
  lp.builtins in '..\lp.lib\lp.builtins.pas',
  lp.environment in '..\lp.lib\lp.environment.pas',
  lp.evaluator in '..\lp.lib\lp.evaluator.pas',
  lp.lexer in '..\lp.lib\lp.lexer.pas',
  lp.parser in '..\lp.lib\lp.parser.pas',
  lp.token in '..\lp.lib\lp.token.pas',
  lp.utils in '..\lp.lib\lp.utils.pas',
  lp.ide.debugger in 'lp.ide.debugger.pas' {LPIdeDebugger};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLPIdeMain, LPIdeMain);
  Application.Run;
end.
