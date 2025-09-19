unit lp.builtins;

interface

{$I lp.inc}

uses  classes, SysUtils, Generics.Collections, Variants, StrUtils, DateUtils
  , {$IFDEF LPI_D28} Winapi.Windows {$ELSE} Windows {$ENDIF}
  , lp.utils
  , lp.lexer
  , lp.parser
  , lp.environment
  , lp.evaluator;

procedure WaitForAnyKeyPressed(const TextMessage: string='');
procedure BuiltedModuleAdd(ModuleName, ModuleSource:string);

implementation

uses lp.advobject;

/////////////////////////////
// base object constructor
//////////
function _DateTimeConstructor(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  Result := ArgumentValidate(args, 0, 1, [STRING_OBJ]);
  if (Result=nil) then
  begin
    Result := TDateTimeObject.Create;
    if ((args.Count=1) and (args[0].ObjectType=STRING_OBJ)) then
      TDateTimeObject(Result).Value := ISO8601ToDate(TStringObject(args[0]).Value);
  end;
end;
////
////////////////////////////

procedure BuiltedModuleAdd(ModuleName, ModuleSource:string);
var
  L:TLexer;
  P:TParser;
begin
  if NOT builtedmodules.ContainsKey(ModuleName) then
  begin
    L:=TLexer.Create(ModuleSource, ModuleName);
    try
      P := TParser.Create(L);
      try
        builtedmodules.Add(ModuleName, P.ParseProgram);
      finally
        P.Free;
      end;
    finally
      L.Free;
    end;
  end;
end;

procedure WaitForKeyPressed(KeyCode: Word; const TextMessage: string);
var
  Handle: THandle;
  Buffer: TInputRecord;
  Counter: Cardinal;
begin
  Handle := GetStdHandle(STD_INPUT_HANDLE);
  if Handle = 0 then
    RaiseLastOSError;
  if not (TextMessage = '') then
    Write(TextMessage);
  while True do
  begin
    Sleep(0);
    if not GetNumberOfConsoleInputEvents(Handle, Counter) then
      RaiseLastOSError;
    if not (Counter = 0) then
    begin
      if not ReadConsoleInput(Handle, Buffer, 1, Counter) then
        RaiseLastOSError;
      if (Buffer.EventType = KEY_EVENT) and Buffer.Event.KeyEvent.bKeyDown then
        if (KeyCode = 0) or (KeyCode = Buffer.Event.KeyEvent.wVirtualKeyCode) then
          Break
    end;
  end
end;

procedure WaitForAnyKeyPressed(const TextMessage: string='');
begin
  WaitForKeyPressed(0, TextMessage);
end;

function _PrintLn(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to args.Count-1 do
  begin
    Write(args[i].Inspect);
    Writeln;
  end;
end;

function _Print(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to args.Count-1 do
    Write(args[i].Inspect);
end;


function _ReadLn(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
const
  PROMPT = '<< ';
var
  S:string;
begin
  S:='';
  Write(PROMPT); Readln(S);
  Result := TStringObject.Create(S);
end;

function _Wait(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  Result := nil;
  if ((args.Count>0) and (args[0].ObjectType=STRING_OBJ)) then
    WaitForAnyKeyPressed(TStringObject(args[0]).Value)
  else
    WaitForAnyKeyPressed;
end;


const
  M_COUNT  = 1;
  M_STDLIB = ' /* MODULE (STDLIB) */ '
    +' const PI = 3.141592653589793; '
    +' const E = 2.718281828459045; '
    +' const System = System(); '
    +' const JSON = JSON(); '
    +' '
    +' function ARRAY.empty = fn(){ '
    +'   return self.size()==0; '
    +' }; '
    +' '
    +' function ARRAY.reverse = fn(){ '
    +'   let r = []; '
    +'   foreach v in self { '
    +'      r.insert(v, 0); '
    +'   }; '
    +'   return r; '
    +' }; '
    +' '
    +' function ARRAY.sort = fn(f) { '
    +'   if (self.size() <= 1){ '
    +'     return self; '
    +'   } else { '
    +'     let left = []; '
    +'     let right = []; '
    +'     let i = 1; '
    +'     while (i < self.size()){ '
    +'       if (f(self[i], self[0]) ) { '
    +'         left.push(self[i]); '
    +'       } else { '
    +'         right.push(self[i]); '
    +'       }; '
    +'       i := i+1; '
    +'     }; '
    +'    '
    +'     return left.sort(f).concat([self[0], right.sort(f)]); '
    +'   }; '
    +' }; '
    +' '
    +' function ARRAY.map = fn(f) { '
    +'   let r = []; '
    +'   foreach v in self { '
    +'     r.push(f(v)); '
    +'   }; '
    +'   return r; '
    +' };'
    +' '
    +' function ARRAY.filter = fn(f) { '
    +'   let r = []; '
    +'   foreach v in self { '
    +'     if(f(v)){ '
    +'       r.push(v); '
    +'     }; '
    +'   }; '
    +'   return r; '
    +' };'
    +' '
    +' function ARRAY.concat = fn(x){ '
    +'   let r = self.clone(); '
    +'   if (x.type()=="ARRAY"){ '
    +'     foreach v in x { '
    +'       if(v.type()=="ARRAY"){ '
    +'         foreach k in v { '
    +'           r.push(k); '
    +'         } '
    +'       } else { '
    +'         r.push(v); '
    +'       }; '
    +'     }; '
    +'   } else { '
    +'     r.push(x); '
    +'   } '
    +'   return r; '
    +' };'
    +'   ';
var
  MODULES: Array[0..M_COUNT-1, 0..1] of string = (
    ('stdlib', M_STDLIB)
  );

procedure init;
var
  L:TLexer;
  P:TParser;
  i:Integer;
begin
  builtins.Add('print', TBuiltinObject.Create(_Print));
  builtins.Add('println', TBuiltinObject.Create(_PrintLn));
  builtins.Add('readln', TBuiltinObject.Create(_ReadLn));
  builtins.Add('wait', TBuiltinObject.Create(_Wait));
  builtins.Add('Datetime', TBuiltinObject.Create(_DateTimeConstructor));

  for i := Low(MODULES) to High(MODULES) do
    BuiltedModuleAdd(MODULES[i][0], MODULES[i][1]);
end;

initialization
  init;

end.
