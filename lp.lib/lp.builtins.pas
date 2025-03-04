unit lp.builtins;

interface

uses  classes, SysUtils, Generics.Collections, Variants
  , lp.environment
  , lp.evaluator;

implementation

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
  Result := nil;
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
var
  i: Integer;
begin
  Result := nil;
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

procedure init;
begin
  builtins.Add('len', TBuiltinObject.Create(_Len));
  builtins.Add('first', TBuiltinObject.Create(_ArrayFirst));
  builtins.Add('last', TBuiltinObject.Create(_ArrayLast));
  builtins.Add('rest', TBuiltinObject.Create(_ArrayRest));
  builtins.Add('push', TBuiltinObject.Create(_ArrayPush));
  builtins.Add('println', TBuiltinObject.Create(_PrintLn));
end;

initialization
  init;

end.
