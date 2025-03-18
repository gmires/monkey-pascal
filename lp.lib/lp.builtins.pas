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
  M_COUNT = 1;
  M_ARRAY =' /* MODULE ARRAY BUILTIN */'
    +'let arrayFilter = fn(x, f) { '
    +'   let r = []; '
    +'   let i = 0; '
    +'   for(i < len(x); i := i+1){ '
    +'     if(f(x[i])){ '
    +'       r := push(r, x[i]); '
    +'     }; '
    +'   }; '
    +' '
    +'  return r; '
    +'};'
    +' '
    +'let arrayMap = fn(x, f) { '
    +'   let r = []; '
    +'   let i = 0;  '
    +'   for(i < len(x); i := i+1){ '
    +'     r := push(r, f(x[i])); '
    +'   }; '
    +' '
    +'  return r; '
    +'};'
    +' '
    +'let arrayConcat = fn(x) { '
    +'  let r = []; '
    +'  let i = 0; '
    +'  while (i<len(x)) { '
    +'    if (typeof(x[i])=="ARRAY"){ '
    +'      let y = 0; '
    +'      while (y<len(x[i])) { '
    +'        r := push(r, x[i][y]); '
    +'        y := y+1; '
    +'      }; '
    +'    } else { '
    +'      r := push(r, x[i]); '
    +'    }; '
    +'    i := i+1; '
    +'  }; '
    +' '
    +'  return r; '
    +'}; '
    +' '
    +'let arraySort = fn(a, f) { '
    +'  if (len(a) <= 1){ '
    +'    return a; '
    +'  } else { '
    +'    let left = []; '
    +'    let right = []; '
    +' '
    +'    let i = 1; '
    +'    while (i < len(a)){ '
    +'      if (f(a[i], a[0]) ) { '
    +'        left := push(left,a[i]); '
    +'      } else { '
    +'        right:= push(right,a[i]); '
    +'      }; '
    +'      i := i+1; '
    +'    }; '
    +' '
    +'    return arrayConcat([arraySort(left,f),a[0],arraySort(right,f)]); '
    +'  }; '
    +'}; '
    ;
var
  MODULES: Array[0..M_COUNT-1, 0..1] of string = (
    ('array',M_ARRAY)
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
    L:=TLexer.Create(MODULES[i][1]);
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
