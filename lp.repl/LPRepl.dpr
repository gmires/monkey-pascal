program LPRepl;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  lp.builtins in '..\lp.lib\lp.builtins.pas',
  lp.environment in '..\lp.lib\lp.environment.pas',
  lp.evaluator in '..\lp.lib\lp.evaluator.pas',
  lp.lexer in '..\lp.lib\lp.lexer.pas',
  lp.parser in '..\lp.lib\lp.parser.pas',
  lp.token in '..\lp.lib\lp.token.pas';

const
  HEADER = 'LP Interpreter REPL v.1.0b (JM) --- ';
  PROMPT = '>> ';

function isExitCommand(value:string):Boolean;
var
  S:string;
begin
  S:=LowerCase(value);
  Result := ((S='quit') or (S='q'));
end;

var
  Env: TEnvironment;
  line: string;
  Lexer:TLexer;
  Parser:TParser;
  ASTProgram: TASTProgram;
  EvalObj:TEvalObject;
begin
  Env:= TEnvironment.Create;
  try

    Writeln(HEADER);
    while True do
    begin

      Write(PROMPT);
      Readln(line);

      if isExitCommand(line) then
        Break;

      try
        Lexer:=TLexer.Create(line);
        try
          Parser:= TParser.Create(Lexer);
          try
            ASTProgram := Parser.ParseProgram;
            try
              EvalObj := Eval(ASTProgram, Env);
              if (EvalObj<>nil) then
              begin
                Writeln(EvalObj.Inspect);
                if NOT Env.Store.ContainsValue(EvalObj) then
                  FreeAndNil(EvalObj);
              end;
            finally
              ASTProgram.Free;
            end;

          finally
            Parser.Free;
          end;
        finally
          Lexer.Free;
        end;
      except
        On E:Exception do
          Writeln('ERROR: ' + E.Message);
      end;
    end;

  finally
    Env.Free;
  end;
end.
