unit lp.builtins;

interface

uses  classes, SysUtils, Generics.Collections, Variants, StrUtils
  , lp.utils
  , lp.lexer
  , lp.parser
  , lp.environment
  , lp.evaluator;

implementation

function _TypeOf(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
    Result := TStringObject.Create(args[0].ObjectType);
end;

function _Len(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  begin
    if args[0].ObjectType=ARRAY_OBJ then
      Result := TNumberObject.Create(TArrayObject(args[0]).Elements.Count)
    else
    if args[0].ObjectType=STRING_OBJ then
      Result := TNumberObject.Create(Length(TStringObject(args[0]).Value))
    else
      Result := TErrorObject.newError('argument to `len` not supported, got %s', [args[0].ObjectType]);
  end;
end;

function _ArrayFirst(args: TList<TEvalObject>): TEvalObject;
begin
  Result := nil;
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if args[0].ObjectType<>ARRAY_OBJ then
    Result := TErrorObject.newError('argument to `first` must be ARRAY, got %s', [args[0].ObjectType])
  else
  begin
    if (args[0] is TArrayObject) then
      if (TArrayObject(args[0]).Elements.Count>0) then
        Result := TArrayObject(args[0]).Elements[0];

    if Result=nil then
      Result := TNullObject.Create;
  end;
end;

function _ArrayLast(args: TList<TEvalObject>): TEvalObject;
begin
  Result := nil;
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if args[0].ObjectType<>ARRAY_OBJ then
    Result := TErrorObject.newError('argument to `first` must be ARRAY, got %s', [args[0].ObjectType])
  else
  begin
    if (args[0] is TArrayObject) then
      if (TArrayObject(args[0]).Elements.Count>0) then
        Result := TArrayObject(args[0]).Elements[TArrayObject(args[0]).Elements.Count-1];

    if Result=nil then
      Result := TNullObject.Create;
  end;
end;

function _ArrayRest(args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if args[0].ObjectType<>ARRAY_OBJ then
    Result := TErrorObject.newError('argument to `first` must be ARRAY, got %s', [args[0].ObjectType])
  else
  begin
    if (TArrayObject(args[0]).Elements.Count>0) then
    begin
      Result := TArrayObject.Create;
      TArrayObject(Result).Elements := TList<TEvalObject>.Create;
      for i := 1 to TArrayObject(args[0]).Elements.Count-1 do
        TArrayObject(Result).Elements.Add(TArrayObject(args[0]).Elements[i].Clone);
    end
    else Result := TNullObject.Create;
  end;
end;

function _ArrayPush(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if args[0].ObjectType<>ARRAY_OBJ then
    Result := TErrorObject.newError('argument to `first` must be ARRAY, got %s', [args[0].ObjectType])
  else
  begin
    Result := TArrayObject(args[0]).Clone;
    TArrayObject(Result).Elements.Add(args[1]);
  end;
end;

function _ArrayInsert(args: TList<TEvalObject>): TEvalObject;
var
  i:Integer;
begin
  if (args.Count<>3) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=3', [args.Count])
  else
  if args[0].ObjectType<>ARRAY_OBJ then
    Result := TErrorObject.newError('argument to `first` must be ARRAY, got %s', [args[0].ObjectType])
  else
  if args[2].ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('argument to `thirty` must be NUMBER (index of array), got %s', [args[2].ObjectType])
  else
  begin
    i := Trunc(TNumberObject(args[2]).Value);
    if ((i<0) or (i>TArrayObject(args[0]).Elements.Count)) then
      Result:= TErrorObject.newError('index out of range), 0 to %d', [TArrayObject(args[0]).Elements.Count])
    else
    begin
      Result := TArrayObject(args[0]).Clone;
      TArrayObject(Result).Elements.Insert(i, args[1]);
    end;
  end;
end;

function _Delete(args: TList<TEvalObject>): TEvalObject;
var
  i,idx: Integer;
  keyr: THashkey;
begin
  Result := nil;
  if (args.Count<>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
  else
  if ((args[0].ObjectType<>ARRAY_OBJ) and (args[0].ObjectType<>HASH_OBJ)) then
    Result := TErrorObject.newError('argument to `first` must be ARRAY or HASHMAP, got %s', [args[0].ObjectType])
  else
  begin
    if args[0].ObjectType=ARRAY_OBJ then
    begin
      if args[1].ObjectType=NUMBER_OBJ then
      begin
        idx := Trunc(TNumberObject(args[1]).Value);

        Result := TArrayObject.Create;
        TArrayObject(Result).Elements := TList<TEvalObject>.Create;
        for i := 0 to TArrayObject(args[0]).Elements.Count-1 do
          if (i<>idx) then
            TArrayObject(Result).Elements.Add(TArrayObject(args[0]).Elements[i].Clone);
      end
      else Result := TErrorObject.newError('invalid index type for ARRAY , got %s', [args[1].ObjectType]);
    end
    else
    if args[0].ObjectType=HASH_OBJ then
    begin
      keyr := THashkey.fromObject(args[1]);
      try
        if keyr.ObjectType=ERROR_OBJ then
          Result := TErrorObject.newError('invalid index type for HashMap , got %s', [args[1].ObjectType])
        else
        begin
          Result := THashObject(args[0]).Clone as THashObject;
          if THashObject(Result).Pairs.ContainsKey(keyr) then
          begin
            THashObject(Result).Pairs[keyr].Free;
            THashObject(Result).Pairs.Remove(keyr);
          end;
        end;
      finally
        keyr.Free;
      end;
    end;
  end;
end;

function _HashKeysOf(args: TList<TEvalObject>): TEvalObject;
var
  hkey: THashkey;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
  else
  if (args[0].ObjectType<>HASH_OBJ) then
    Result := TErrorObject.newError('argument to `first` must be HASHMAP, got %s', [args[0].ObjectType])
  else
  begin
    Result := TArrayObject.Create;
    TArrayObject(Result).Elements := TList<TEvalObject>.Create;
    for hkey in THashObject(args[0]).Pairs.Keys do
      if (hkey.ObjectType=STRING_OBJ) then
        TArrayObject(Result).Elements.Add(TStringObject.Create(hkey.FValueStr))
      else
      if (hkey.ObjectType=NUMBER_OBJ) then
        TArrayObject(Result).Elements.Add(TNumberObject.Create(hkey.FValueNum))
      else
      if (hkey.ObjectType=BOOLEAN_OBJ) then
        TArrayObject(Result).Elements.Add(TBooleanObject.Create(hkey.FValueBool));
  end;
end;


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

function _Join(args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
  j,S: string;
  A: TArrayObject;
begin
  j:=',';
  if (args.Count>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, max=2', [args.Count])
  else
  if (args[0].ObjectType<>ARRAY_OBJ) then
    Result := TErrorObject.newError('argument to `first` must be ARRAY, got %s', [args[0].ObjectType])
  else
  if (args.Count=2) and (args[1].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument to `second` must be STRING, got %s', [args[1].ObjectType])
  else
  begin
    A:= TArrayObject(args[0]);
    if (args.Count=2) then
      j:= TStringObject(args[1]).Value;
    if A.Elements.Count>0 then
    begin
      S:='';
      for i := 0 to A.Elements.Count-1 do
        S:=S+A.Elements[i].Inspect+j;
      S:= Copy(S,1,Length(S)-1);

      Result := TStringObject.Create(S);
    end
    else Result := TNullObject.Create;
  end;
end;

function _Split(args: TList<TEvalObject>): TEvalObject;
var
  j: char;
  S,S1: string;
begin
  j:=',';
  if (args.Count>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, max=2', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument to `first` must be STRING, got %s', [args[0].ObjectType])
  else
  if (args.Count=2) and (args[1].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument to `second` must be STRING, got %s', [args[1].ObjectType])
  else
  begin
    if ((args.Count=2) and (TStringObject(args[1]).Value<>'')) then
      j:= TStringObject(args[1]).Value[1];

    Result := TArrayObject.Create;
    TArrayObject(Result).Elements := TList<TEvalObject>.Create;

    S:= TStringObject(args[0]).Value;
    While (S<>'') do
    begin
      S1:= StrSplit(S,j);
      TArrayObject(Result).Elements.Add(TStringObject.Create(S1));
    end;
  end;
end;

function _Trim(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
    Result := TStringObject.Create(Trim(TStringObject(args[0]).Value));
end;

function _LTrim(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
    Result := TStringObject.Create(TrimLeft(TStringObject(args[0]).Value));
end;

function _RTrim(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
    Result := TStringObject.Create(TrimRight(TStringObject(args[0]).Value));
end;

function _StrCopy(args: TList<TEvalObject>): TEvalObject;
var
  O,C:Integer;
begin
  if (args.Count<>3) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=3', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
  if (args[1].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[1].ObjectType])
  else
  if (args[2].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[2].ObjectType])
  else
  begin
    O:= Trunc(TNumberObject(args[1]).Value);
    C:= Trunc(TNumberObject(args[2]).Value);
    Result := TStringObject.Create(Copy(TStringObject(args[0]).Value,O,C));
  end;
end;

function _StrLeft(args: TList<TEvalObject>): TEvalObject;
var
  L:Integer;
begin
  if (args.Count<>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
  if (args[1].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[1].ObjectType])
  else
  begin
    L:= Trunc(TNumberObject(args[1]).Value);
    Result := TStringObject.Create(LeftStr(TStringObject(args[0]).Value,L));
  end;
end;

function _StrRight(args: TList<TEvalObject>): TEvalObject;
var
  L:Integer;
begin
  if (args.Count<>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
  if (args[1].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[1].ObjectType])
  else
  begin
    L:= Trunc(TNumberObject(args[1]).Value);
    Result := TStringObject.Create(RightStr(TStringObject(args[0]).Value,L));
  end;
end;

function _StrToNumDef(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
  if (args[1].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[1].ObjectType])
  else
    Result := TNumberObject.Create(StrToFloatDef(TStringObject(args[0]).Value, TNumberObject(args[1]).Value));
end;

function _NumToStr(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>1) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
  else
  if (args[0].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[0].ObjectType])
  else
    Result := TStringObject.Create(FloatToStr(TNumberObject(args[0]).Value));
end;

function _FormatFloat(args: TList<TEvalObject>): TEvalObject;
begin
  if (args.Count<>2) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
  else
  if (args[0].ObjectType<>STRING_OBJ) then
    Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
  else
  if (args[1].ObjectType<>NUMBER_OBJ) then
    Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[1].ObjectType])
  else
    Result := TStringObject.Create(FormatFloat(TStringObject(args[0]).Value, TNumberObject(args[1]).Value));
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
  builtins.Add('typeof', TBuiltinObject.Create(_TypeOf));
  builtins.Add('len', TBuiltinObject.Create(_Len));
  builtins.Add('first', TBuiltinObject.Create(_ArrayFirst));
  builtins.Add('last', TBuiltinObject.Create(_ArrayLast));
  builtins.Add('rest', TBuiltinObject.Create(_ArrayRest));
  builtins.Add('delete', TBuiltinObject.Create(_Delete));
  builtins.Add('push', TBuiltinObject.Create(_ArrayPush));
  builtins.Add('insert', TBuiltinObject.Create(_ArrayInsert));
  builtins.Add('println', TBuiltinObject.Create(_PrintLn));
  builtins.Add('readln', TBuiltinObject.Create(_ReadLn));
  builtins.Add('keyof', TBuiltinObject.Create(_HashKeysOf));
  builtins.Add('join', TBuiltinObject.Create(_Join));
  builtins.Add('split', TBuiltinObject.Create(_Split));
  builtins.Add('trim', TBuiltinObject.Create(_Trim));
  builtins.Add('rtrim', TBuiltinObject.Create(_RTrim));
  builtins.Add('ltrim', TBuiltinObject.Create(_LTrim));
  builtins.Add('copy', TBuiltinObject.Create(_StrCopy));
  builtins.Add('left', TBuiltinObject.Create(_StrLeft));
  builtins.Add('right', TBuiltinObject.Create(_StrRight));
  builtins.Add('strtonumberdef', TBuiltinObject.Create(_StrToNumDef));
  builtins.Add('numtostr', TBuiltinObject.Create(_NumToStr));
  builtins.Add('formatnumber', TBuiltinObject.Create(_FormatFloat));

  for i := Low(MODULES) to High(MODULES) do
  begin
    L:=TLexer.Create(MODULES[i][1]);
    try
      P := TParser.Create(L);
      try
        builtedmodule.Add(MODULES[i][0], P.ParseProgram);
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
