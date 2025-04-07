program LPInterpreter;

uses
  Forms,
  lpi.main in 'lpi.main.pas' {LPMain},
  lp.builtins in '..\lp.lib\lp.builtins.pas',
  lp.environment in '..\lp.lib\lp.environment.pas',
  lp.evaluator in '..\lp.lib\lp.evaluator.pas',
  lp.lexer in '..\lp.lib\lp.lexer.pas',
  lp.parser in '..\lp.lib\lp.parser.pas',
  lp.token in '..\lp.lib\lp.token.pas',
  lp.utils in '..\lp.lib\lp.utils.pas',
  lp.advobject in '..\lp.lib\lp.advobject.pas',
  lpi.debugger in 'lpi.debugger.pas' {LPDebugger},
  lp.edits in '..\lp.components\lp.edits.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLPMain, LPMain);
  Application.Run;
end.
