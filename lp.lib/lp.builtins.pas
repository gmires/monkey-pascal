unit lp.builtins;

interface

uses  classes, SysUtils, Generics.Collections, Variants, StrUtils
  , lp.utils
  , lp.lexer
  , lp.parser
  , lp.environment
  , lp.evaluator;

implementation

function _PrintLn(args: TList<TEvalObject>): TEvalObject;
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

function _ReadLn(args: TList<TEvalObject>): TEvalObject;
const
  PROMPT = '<< ';
var
  S:string;
begin
  S:='';
  Write(PROMPT); Readln(S);
  Result := TStringObject.Create(S);
end;

const
  M_COUNT  = 1;
  M_STDLIB = ' /* MODULE (STDLIB) */ '
    +' const PI = 3.141592653589793; '
    +' const E = 2.718281828459045; '
    +' '
    +' function ARRAY.empty = fn(){ '
    +'   return self.size()==0; '
    +' }; '
    +' '
    +' function ARRAY.reverse = fn(){ '
    +'   let r = []; '
    +'   foreach v in self { '
    +'      r := r.insert(v, 0); '
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
    +'         left := left.push(self[i]); '
    +'       } else { '
    +'         right:= right.push(self[i]); '
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
    +'     r := r.push(f(v)); '
    +'   }; '
    +'   return r; '
    +' };'
    +' '
    +' function ARRAY.filter = fn(f) { '
    +'   let r = []; '
    +'   foreach v in self { '
    +'     if(f(v)){ '
    +'       r := r.push(v); '
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
    +'           r := r.push(k); '
    +'         } '
    +'       } else { '
    +'         r := r.push(v); '
    +'       }; '
    +'     }; '
    +'   } else { '
    +'     r := r.push(x); '
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
  builtins.Add('println', TBuiltinObject.Create(_PrintLn));
  builtins.Add('readln', TBuiltinObject.Create(_ReadLn));

  for i := Low(MODULES) to High(MODULES) do
  begin
    L:=TLexer.Create(MODULES[i][1],MODULES[i][0]);
    try
      P := TParser.Create(L);
      try
        builtedmodules.Add(MODULES[i][0], P.ParseProgram);
      finally
        P.Free;
      end;
    finally
      L.Free;
    end;
  end;
end;

initialization
  init;

end.
