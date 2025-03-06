unit lp.evaluator;

interface

uses classes, SysUtils, Generics.Collections, Variants

  , lp.environment
  , lp.parser;

type
  TGarbageCollector = class

  end;

  TEvaluator = class
    FGarbageCollector:TGarbageCollector;
    function evalProgram(node: TASTNode; env: TEnvironment): TEvalObject;
    function evalStatements(Statements :TList<TASTStatement>; env: TEnvironment): TEvalObject;
    function evalBlockStatements(node :TASTBlockStatement; env: TEnvironment): TEvalObject;
    function evalBangOperatorExpression(right: TEvalObject):TEvalObject;
    function evalMinusPrefixOperatorExpression(right: TEvalObject): TEvalObject;
    function evalPrefixExpression(Op:string; right:TEvalObject):TEvalObject;
    function evalIntegerInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalStringInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalBooleanInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalInfixExpression(Op:string; left, right: TEvalObject):TEvalObject;
    function evalIfExpression(node:TASTIfExpression; env :TEnvironment):TEvalObject;
    function evalWhileExpression(node:TASTWhileExpression; env :TEnvironment):TEvalObject;
    function evalForExpression(node:TASTForExpression; env :TEnvironment):TEvalObject;
    function evalIdentifier(node: TASTIdentifier; env :TEnvironment):TEvalObject;
    function evalHashLiteral(node:TASTHashLiteral; env :TEnvironment):TEvalObject;
    function evalExpressions(exps:TList<TASTExpression>; env:TEnvironment):TList<TEvalObject>;
    function evalArrayIndexExpression(AArray, AIndex: TEvalObject):TEvalObject;
    function assignArrayIndexExpression(AArray, AIndex, AValue: TEvalObject):TEvalObject;
    function evalHashIndexExpression(AHash, AIndex: TEvalObject):TEvalObject;
    function assignHashIndexExpression(AHash, AIndex, AValue: TEvalObject):TEvalObject;
    function evalIndexExpression(left, index:TEvalObject):TEvalObject;
    function extendFunctionEnv(fn: TFunctionObject; args: TList<TEvalObject>):TEnvironment;
    function unwrapReturnValue(obj:TEvalObject):TEvalObject;
    function applyFunction(fn: TEvalObject; args: TList<TEvalObject>):TEvalObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Eval(node: TASTNode; env: TEnvironment): TEvalObject;
  end;

var
  builtins: TDictionary<string,TBuiltinObject>;


implementation

function nativeBoolToBooleanObject(input:Boolean): TBooleanObject;
begin
  Result := TBooleanObject.Create(input);
end;

function isError(obj: TEvalObject):Boolean;
begin
  Result := False;
	if (obj <> nil) then
		Result := (obj.ObjectType() = ERROR_OBJ);
end;

function isTruthy(obj:TEvalObject): Boolean;
begin
  Result := True;
  if obj is TNullObject then Result := false
  else
  if obj is TBooleanObject then Result := TBooleanObject(obj).Value;
end;

function isTruthyWithError(obj:TEvalObject; var ErrObj:TEvalObject): Boolean;
begin
  Result := True;
  if obj is TNullObject then Result := false
  else
  if obj is TBooleanObject then Result := TBooleanObject(obj).Value
  else
  if obj is TErrorObject then
  begin
    Result := false;
    ErrObj := obj;
  end;
end;


function TEvaluator.evalProgram(node: TASTNode; env: TEnvironment): TEvalObject;
var
  i: Integer;
  R: TEvalObject;
begin
  Result := evalStatements(TASTProgram(node).FStatements, env);
  if Result is TReturnValueObject then
    Result := TReturnValueObject(R).Value;
end;

function TEvaluator.evalBlockStatements(node :TASTBlockStatement; env: TEnvironment): TEvalObject;
begin
  Result := evalStatements(TASTBlockStatement(node).FStatements, env);
end;

function TEvaluator.evalBangOperatorExpression(right: TEvalObject):TEvalObject;
begin
  if (right is TNullObject) then
    Result := TBooleanObject.Create(True)
  else
  if (right is TBooleanObject) then
    Result := TBooleanObject.Create(NOT TBooleanObject(right).Value)
  else
    Result :=  TBooleanObject.Create(False);
end;

function TEvaluator.evalMinusPrefixOperatorExpression(right: TEvalObject): TEvalObject;
begin

  if (right.ObjectType <> NUMBER_OBJ) then
    Result := TErrorObject.newError('unknown operator: -%s', [right.ObjectType])
  else
    Result := TNumberObject.Create(-TNumberObject(right).Value);
end;

function TEvaluator.evalPrefixExpression(Op:string; right:TEvalObject):TEvalObject;
begin
  if Op='!' then Result := evalBangOperatorExpression(right)
  else
  if Op='-' then Result := evalMinusPrefixOperatorExpression(right)
  else
    Result := TErrorObject.newError('unknown operator: %s%s', [Op, right.ObjectType])
end;

function TEvaluator.evalIntegerInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
var
  LVal,
  RVal:Double;
begin
  LVal:= TNumberObject(left).Value;
  RVal:= TNumberObject(right).Value;
  if Op='+' then Result:= TNumberObject.Create(LVal + RVal)
  else
  if Op='-' then Result:= TNumberObject.Create(LVal - RVal)
  else
  if Op='*' then Result:= TNumberObject.Create(LVal * RVal)
  else
  if Op='/' then Result:= TNumberObject.Create(LVal / RVal)
  else
  if Op='<' then Result:= nativeBoolToBooleanObject(LVal < RVal)
  else
  if Op='<=' then Result:= nativeBoolToBooleanObject(LVal <= RVal)
  else
  if Op='>' then Result:= nativeBoolToBooleanObject(LVal > RVal)
  else
  if Op='>=' then Result:= nativeBoolToBooleanObject(LVal >= RVal)
  else
  if Op='==' then Result:= nativeBoolToBooleanObject(LVal = RVal)
  else
  if Op='!=' then Result:= nativeBoolToBooleanObject(LVal <> RVal)
  else
    Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType])
end;

function TEvaluator.evalStatements(Statements: TList<TASTStatement>;  env: TEnvironment): TEvalObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Statements.Count-1 do
  begin
    Result := Eval(Statements[i], env);
    if (Result<>nil) then
      if (Result.ObjectType = RETURN_VALUE_OBJ) or (Result.ObjectType = ERROR_OBJ) then
        Break;
  end;
end;

function TEvaluator.evalStringInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
var
  LVal,
  RVal:string;
begin
  LVal:= TStringObject(left).Value;
  RVal:= TStringObject(right).Value;
  if Op='+' then Result:= TStringObject.Create(LVal + RVal)
  else
  if Op='<' then Result:= nativeBoolToBooleanObject(LVal < RVal)
  else
  if Op='<=' then Result:= nativeBoolToBooleanObject(LVal <= RVal)
  else
  if Op='>' then Result:= nativeBoolToBooleanObject(LVal > RVal)
  else
  if Op='>=' then Result:= nativeBoolToBooleanObject(LVal >= RVal)
  else
  if Op='==' then Result:= nativeBoolToBooleanObject(LVal = RVal)
  else
  if Op='!=' then Result:= nativeBoolToBooleanObject(LVal <> RVal)
  else
    Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType])
end;

function TEvaluator.evalBooleanInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
var
  LVal,
  RVal:Boolean;
begin
  LVal:= TBooleanObject(left).Value;
  RVal:= TBooleanObject(right).Value;
  if Op='==' then Result:= nativeBoolToBooleanObject(LVal = RVal)
  else
  if Op='!=' then Result:= nativeBoolToBooleanObject(LVal <> RVal)
  else
  if Op='&&' then Result:= nativeBoolToBooleanObject(LVal and RVal)
  else
  if Op='||' then Result:= nativeBoolToBooleanObject(LVal or RVal)
  else
    Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType])
end;

function TEvaluator.evalInfixExpression(Op:string; left, right: TEvalObject):TEvalObject;
begin
  if ((left.ObjectType=NUMBER_OBJ) and (right.ObjectType=NUMBER_OBJ)) then
    Result := evalIntegerInfixExpression(Op, left, right)
  else
  if ((left.ObjectType=STRING_OBJ) and (right.ObjectType=STRING_OBJ)) then
    Result := evalStringInfixExpression(Op, left, right)
  else
  if ((left.ObjectType=BOOLEAN_OBJ) and (right.ObjectType=BOOLEAN_OBJ)) then
    Result := evalBooleanInfixExpression(Op, left, right)
  else
  if (Op='==') then
    Result := nativeBoolToBooleanObject(left = right)
  else
  if (Op='!=') then
    Result := nativeBoolToBooleanObject(left <> right)
  else
  if (left.ObjectType<>right.ObjectType) then
		Result := TErrorObject.newError('type mismatch: %s %s %s', [left.ObjectType, Op, right.ObjectType])
  else
		Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType])
end;

function TEvaluator.evalIfExpression(node:TASTIfExpression; env :TEnvironment):TEvalObject;
begin
  Result := Eval(TASTIfExpression(node).Condition, env);
  if NOT isError(Result) then
  begin
    if isTruthy(Result) then
      Result := Eval(TASTIfExpression(node).Consequence, env)
    else
    if (TASTIfExpression(node).Alternative<> nil) then
      Result := Eval(TASTIfExpression(node).Alternative, env)
    else
      Result := TNullObject.Create;
  end;
end;

function TEvaluator.evalWhileExpression(node:TASTWhileExpression; env :TEnvironment):TEvalObject;
begin
  Result := nil;
  while isTruthyWithError(Eval(TASTWhileExpression(node).Condition, env), Result) do
    Result := Eval(TASTWhileExpression(node).Body, env)
end;

function TEvaluator.evalForExpression(node:TASTForExpression; env :TEnvironment):TEvalObject;
var
  ForEnv: TEnvironment;
begin
  ForEnv:= TEnvironment.Create(env);
  try
    Eval(node.Init, ForEnv);
    while isTruthyWithError(Eval(TASTForExpression(node).Condition, ForEnv), Result) do
    begin
      Result := Eval(TASTForExpression(node).Body, ForEnv);
      Eval(TASTForExpression(node).Expression, ForEnv);
    end;
  finally
    ForEnv.Free;
  end;
end;


function TEvaluator.evalIdentifier(node: TASTIdentifier; env :TEnvironment):TEvalObject;
begin
  Result:=nil;

  if NOT env.GetValue(node.Value, Result) then
  begin
    if builtins.ContainsKey(node.Value) then
      Result := builtins[node.Value]
    else
      Result := TErrorObject.newError('identifier not found: %s ',[node.Value]);
  end;
end;

function TEvaluator.evalHashLiteral(node:TASTHashLiteral; env :TEnvironment):TEvalObject;
var
  k,v: TEvalObject;
  h:THashkey;
  pair: TPair<TASTExpression,TASTExpression>;
begin
  Result := THashObject.Create;
  THashObject(Result).Pairs := TDictionary<THashkey,THashPair>.create;

  for pair in node.Pairs do
  begin
    k := Eval(pair.Key, env);
    if isError(k) then
      Exit(k);

    h:= THashkey.fromObject(k);
    if h.ObjectType=ERROR_OBJ then
      Exit(TErrorObject.newError('unusable as hash key: %s', [k.ObjectType]));

    v := Eval(pair.Value, env);
    if isError(v) then
      Exit(v);

    THashObject(Result).Pairs.Add(h, THashPair.Create(k,v));
  end;
end;


function TEvaluator.evalExpressions(exps:TList<TASTExpression>; env:TEnvironment):TList<TEvalObject>;
var
  i:Integer;
  val:TEvalObject;
begin
  Result := TList<TEvalObject>.Create;
  for i := 0 to exps.Count-1 do
  begin
    val := Eval(exps[i], env);
    if isError(val) then
    begin
      Result.Clear;
      Result.Add(val);
      Break;
    end;
    Result.Add(val);
  end;
end;

function TEvaluator.evalArrayIndexExpression(AArray, AIndex: TEvalObject):TEvalObject;
var
  idx, max: Integer;
begin
  idx := Trunc(TNumberObject(AIndex).Value);
  max := TArrayObject(AArray).Elements.Count - 1;

  if ((idx<0) or (idx>max)) then
    Result := TNullObject.Create
  else
    Result := TArrayObject(AArray).Elements[idx];
end;

function TEvaluator.assignArrayIndexExpression(AArray, AIndex, AValue: TEvalObject):TEvalObject;
var
  idx, max: Integer;
begin
  idx := Trunc(TNumberObject(AIndex).Value);
  max := TArrayObject(AArray).Elements.Count - 1;

  if ((idx<0) or (idx>max)) then
    Exit(TErrorObject.newError('unusable index for array: %s', [TNumberObject(AIndex).Inspect]))
  else
  begin
    TArrayObject(AArray).Elements[idx] := AValue;
    Result := AArray;
  end;
end;

function TEvaluator.evalHashIndexExpression(AHash, AIndex: TEvalObject):TEvalObject;
var
  HO:THashObject;
  HK:THashkey;
begin
  HO := AHash as THashObject;
  HK := THashkey.fromObject(AIndex);
  if HK.ObjectType=ERROR_OBJ then
    Exit(TErrorObject.newError('unusable as hash key: %s', [AIndex.ObjectType]));

  if HO.Pairs.ContainsKey(HK) then
    Result := HO.Pairs[HK].Value
  else
    Result := TNullObject.Create;
end;

function TEvaluator.assignHashIndexExpression(AHash, AIndex, AValue: TEvalObject):TEvalObject;
var
  HO:THashObject;
  HK:THashkey;
begin
  HO := AHash as THashObject;
  HK := THashkey.fromObject(AIndex);
  if HK.ObjectType=ERROR_OBJ then
    Exit(TErrorObject.newError('unusable as hash key: %s', [AIndex.ObjectType]));

  if HO.Pairs.ContainsKey(HK) then
    HO.Pairs[HK].Value := AValue
  else
    HO.Pairs.Add(HK,THashPair.Create(AIndex, AValue));

  Result := AHash;
end;

constructor TEvaluator.Create;
begin
  FGarbageCollector:= TGarbageCollector.Create;
end;

destructor TEvaluator.Destroy;
begin
  FGarbageCollector.Free;
  inherited;
end;

function TEvaluator.evalIndexExpression(left, index:TEvalObject):TEvalObject;
begin
  if (left.ObjectType=ARRAY_OBJ) and (index.ObjectType=NUMBER_OBJ) then
    Result := evalArrayIndexExpression(left, index)
  else
  if (left.ObjectType=HASH_OBJ) then
    Result := evalHashIndexExpression(left, index)
  else
    Result := TErrorObject.newError('index operator not supported: %s', [left.ObjectType]);
end;

function TEvaluator.extendFunctionEnv(fn: TFunctionObject; args: TList<TEvalObject>):TEnvironment;
var
  i: Integer;
begin
  Result := TEnvironment.Create(fn.Env);
  for i := 0 to fn.Parameters.Count-1 do
    Result.SetValue(fn.Parameters[i].Value,args[i].Clone);
end;

function TEvaluator.unwrapReturnValue(obj:TEvalObject):TEvalObject;
begin
  Result := nil;
  if (obj is TReturnValueObject) then
    if (TReturnValueObject(obj).Value<>nil) then
       Result := TReturnValueObject(obj).Value;

  if Result=nil then
    Result := obj;
end;


function TEvaluator.applyFunction(fn: TEvalObject; args: TList<TEvalObject>):TEvalObject;
var
  FEnv:TEnvironment;
begin
  if (fn is TFunctionObject) then
  begin
    FEnv := extendFunctionEnv(fn as TFunctionObject, args);
    try
      Result := unwrapReturnValue(Eval(TFunctionObject(fn).Body, FEnv));
    finally
      FEnv.Free;
    end;
  end
  else
  if (fn is TBuiltinObject) then
    Result := TBuiltinObject(fn).BuiltinFunction(args)
  else
    Result := TErrorObject.newError('not a function: %s', [fn.ObjectType])
end;


function TEvaluator.Eval(node: TASTNode; env: TEnvironment): TEvalObject;
var
  val,enobj,index:TEvalObject;
  args:TList<TEvalObject>;
begin
  Result := nil;
  if node is TASTProgram then
    Result := evalProgram(node, env)
  else
  if node is TASTExpressionStatement then
    Result := Eval(TASTExpressionStatement(node).Expression, env)
  else
  if node is TASTNumberLiteral then
    Result := TNumberObject.Create(TASTNumberLiteral(node).Value)
  else
  if node is TASTBoolean then
    Result := nativeBoolToBooleanObject(TASTBoolean(node).Value)
  else
  if node is TASTStringLiteral then
    Result := TStringObject.Create(TASTStringLiteral(node).Value)
  else
  if node is TASTArrayLiteral then
  begin
    Result := TArrayObject.Create;
    TArrayObject(Result).Elements := evalExpressions(TASTArrayLiteral(node).Elements, env);
    if ((TArrayObject(Result).Elements.Count=1) and (isError(TArrayObject(Result).Elements[0]))) then
      Result := TArrayObject(Result).Elements[0];
  end
  else
  if node is TASTHashLiteral then
    Result := evalHashLiteral(TASTHashLiteral(node), env)
  else
  if node is TASTIndexExpression then
  begin
    val := Eval(TASTIndexExpression(node).Left, env);
    if NOT isError(val) then
    begin
      index := Eval(TASTIndexExpression(node).Index, env);
      if NOT isError(val) then
        Result := evalIndexExpression(val, index);
    end;
  end
  else
  if node is TASTPrefixExpression then
  begin
    Result := Eval(TASTPrefixExpression(node).Right, env);
		if NOT isError(Result) then
  		Result := evalPrefixExpression(TASTPrefixExpression(node).Op, Result)
  end
  else
  if node is TASTInfixExpression then
  begin
    Result := Eval(TASTInfixExpression(node).Left, env);
    if NOT isError(Result)  then
    begin
      val := Eval(TASTInfixExpression(node).Right, env);
      if isError(Result)  then
        Result := val
      else
        Result := evalInfixExpression(TASTInfixExpression(node).Op, Result, val);
    end;
  end
  else
  if node is TASTBlockStatement then
    Result := evalBlockStatements(node as TASTBlockStatement, env)
  else
  if node is TASTIfExpression then
    Result := evalIfExpression(node as TASTIfExpression, env)
  else
  if node is TASTWhileExpression then
    Result := evalWhileExpression(node as TASTWhileExpression, env)
  else
  if node is TASTForExpression then
    Result := evalForExpression(node as TASTForExpression, env)
  else
  if node is TASTReturnStatement then
  begin
    Result := Eval(TASTReturnStatement(node).ReturnValue, env);
		if NOT isError(Result) then
      Result := TReturnValueObject.Create(Result);
  end
  else
  if node is TASTLetStatement then
  begin
    val := Eval(TASTLetStatement(node).Expression, env);
    if NOT isError(val) then
    begin
      if TASTLetStatement(node).Name is TASTIdentifier then
        env.SetValue(TASTIdentifier(TASTLetStatement(node).Name).Value, val)
      else
      if TASTLetStatement(node).Name is TASTIndexExpression then
      begin
        enobj := Eval(TASTIndexExpression(TASTLetStatement(node).Name).Left, env);
        if NOT isError(val) then
        begin
          index := Eval(TASTIndexExpression(TASTLetStatement(node).Name).Index, env);
          if NOT isError(val) then
          begin
            if (enobj.ObjectType=ARRAY_OBJ) and (index.ObjectType=NUMBER_OBJ) then
              Result := assignArrayIndexExpression(enobj, index, val)
            else
            if (enobj.ObjectType=HASH_OBJ) then
              Result := assignHashIndexExpression(enobj, index, val)
            else
              Result := TErrorObject.newError('index operator not supported: %s', [enobj.ObjectType]);
          end
          else Result := val;
        end
        else Result := val;
      end;
    end
    else Result := val;
  end
  else
  if node is TASTIdentifier then
    Result := evalIdentifier(node as TASTIdentifier, env)
  else
  if node is TASTFunctionLiteral then
    Result := TFunctionObject.Create(TASTFunctionLiteral(node).Parameters,TASTFunctionLiteral(node).Body, env)
  else
  if node is TASTCallExpression then
  begin
    Result := Eval(TASTCallExpression(node).Funct, env);
    if NOT isError(Result) then
    begin
		  args := evalExpressions(TASTCallExpression(node).Args, env);
      if ((args.Count=1) and (isError(args[0]))) then
        Result := args[0]
      else
        Result := applyFunction(Result, args);
    end;
  end;
end;



procedure init;
begin
  builtins := TDictionary<string,TBuiltinObject>.Create;
end;

procedure deinit;
begin
  builtins.Free;
end;


initialization
  init;
finalization
  deinit;

end.

