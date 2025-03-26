program LPRepl;

{$APPTYPE CONSOLE}

uses
  SysUtils, IOUtils, Classes,
  lp.builtins in '..\lp.lib\lp.builtins.pas',
  lp.environment in '..\lp.lib\lp.environment.pas',
  lp.evaluator in '..\lp.lib\lp.evaluator.pas',
  lp.lexer in '..\lp.lib\lp.lexer.pas',
  lp.parser in '..\lp.lib\lp.parser.pas',
  lp.token in '..\lp.lib\lp.token.pas',
  lp.utils in '..\lp.lib\lp.utils.pas',
  lp.advobject in '..\lp.lib\lp.advobject.pas';


procedure StartREPL;

  function isExitCommand(value:string):Boolean;
  var
    S:string;
  begin
    S:=LowerCase(value);
    Result := ((S='quit') or (S='q'));
  end;

const
  HEADER = 'LP Interpreter REPL v.1.0b (JM) --- ';
  PROMPT = '>> ';
var
  Env: TEnvironment;
  Eval: TEvaluator;
  line: string;
  Lexer:TLexer;
  Parser:TParser;
  ASTProgram: TASTProgram;
  EvalObj:TEvalObject;
  i:Integer;
begin
  Env :=TEnvironment.Create;
  Eval:=TEvaluator.Create;
  try

    Writeln(HEADER);
    while True do
    begin

      Write(PROMPT);
      Readln(line);

      if isExitCommand(line) then
        Break;

      try
        Lexer:=TLexer.Create(line, 'main');
        try
          Parser:= TParser.Create(Lexer);
          try
            ASTProgram := Parser.ParseProgram;
            try
              if (Parser.Errors.Count>0) then
              begin
                for i := 0 to Parser.Errors.Count-1 do
                  Writeln(Parser.Errors[i]);
              end
              else
              begin
                EvalObj := Eval.Run(ASTProgram, Env);
                if (EvalObj<>nil) then
                  Writeln(EvalObj.Inspect);
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
    Eval.Sweep(Env);
    Eval.Sweep(Env);
    Eval.Free;
    Env.Free;
  end;
end;

procedure StartProgram;
var
  MainModule,
  MainFolder:string;
  MainProgram:TASTProgram;

  function RecursiveModuleImport(APath:string; ARelPath:string):Boolean;
  var
    R:TSearchRec;
    L:TLexer;
    P:TParser;
    F:TStringList;
    CModule:string;
    i:Integer;
  begin
    Result := True;
    if FindFirst(APath+'*.*', faAnyFile, R) = 0 then
    begin
      repeat
        if R.Attr = faDirectory then
        begin
          if ((R.Name<>'.') and (R.Name<>'..')) then
            RecursiveModuleImport(APath+R.Name+'\', ARelPath+R.Name+'.');
        end
        else
        if (UpperCase(ExtractFileExt(R.Name))='.LPI') then
        begin
          CModule := ARelPath + TPath.GetFileNameWithoutExtension(R.Name);
          F:=TStringList.Create;
          try
            F.LoadFromFile(APath+R.Name);
            L:=TLexer.Create(F.Text,CModule);
            try
              P := TParser.Create(L);
              try
                if (CModule=MainModule) then
                  MainProgram:= P.ParseProgram
                else
                  builtedmodules.Add(CModule, P.ParseProgram);

                if (P.Errors.Count>0) then
                begin
                  for i := 0 to P.Errors.Count-1 do
                    Writeln(P.Errors[i]);
                  Readln;
                  Exit(False);
                end;

              finally
                P.Free;
              end;
            finally
              L.Free;
            end;
          finally
            F.Free;
          end;
        end;

      until FindNext(R) <> 0;
      FindClose(R);
    end;

  end;

var
  Env:TEnvironment;
  Evl:TEvaluator;
  EvR:TEvalObject;
  Par:TArrayObject;
  i:Integer;
begin
  MainProgram:= nil;
  MainModule:= TPath.GetFileNameWithoutExtension(ExtractFileName(ParamStr(1)));
  MainFolder:= ExtractFilePath(TPath.GetFullPath(ParamStr(1)));

  if RecursiveModuleImport(MainFolder, '') then
  begin
    Par:= TArrayObject.CreateWithElements;
    try
      Par.GcManualFree:= True;
      for i := 0 to ParamCount do
        Par.Elements.Add(TStringObject.Create(ParamStr(i)));
      for i := 0 to Par.Elements.Count-1 do
        Par.Elements[i].GcManualFree := True;

      Env:=TEnvironment.Create;
      Evl:=TEvaluator.Create;
      try
        if Assigned(MainProgram) then
        begin

          Env.SetOrCreateValue('params', Par, True);
          EvR := Evl.Run(MainProgram, Env);
          if ((EvR<>nil) and (EvR.ObjectType=ERROR_OBJ)) then
          begin
            Writeln(EvR.Inspect);
            Readln;
          end;

        end
        else
        begin
          Writeln('ERROR, Main Module <'+MainModule+'> not found.');
          Readln;
        end;
      finally
        Evl.Sweep(Env);
        Evl.Sweep(Env);
        Evl.Free;
        Env.Free;
      end;
    finally
      for i := 0 to Par.Elements.Count-1 do
        Par.Elements[i].Free;
      Par.Free;
    end;
  end;
end;

begin
  if ParamCount>0 then
    StartProgram
  else
    StartREPL;
end.
