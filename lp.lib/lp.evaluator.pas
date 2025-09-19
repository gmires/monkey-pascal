unit lp.evaluator;

interface

uses classes, SysUtils, Generics.Collections, Variants, Math, SyncObjs

  , lp.environment
  , lp.parser;

type
  TEvaluator = class;

  TGarbageCollector = class
    FLock: TCriticalSection;
    FObjects: TList<TEvalObject>;
    function GetElemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function  Add(AObject: TEvalObject):TEvalObject;
    procedure Mark(AObject: TEvalObject); overload;
    procedure Mark(AEnvironment: TEnvironment); overload;
    procedure Sweep;

    property ElemCount:Integer read GetElemCount;
  end;

  TEvalNotifierEvent = function(AModule:string; ALine, APos: Integer; AEnvironment:TEnvironment; AEval:TEvaluator; var AContinue:Boolean):Boolean of object;

  TEvaluator = class
    FGCCounter: Word;
    FGarbageCollector:TGarbageCollector;
    FFunctEnv: TList<TEnvironment>;
    FFunctLck: TCriticalSection;
    FModules: TStringList;
    FEvalNotifierEvent: TEvalNotifierEvent;
    function evalProgram(node: TASTNode; env: TEnvironment): TEvalObject;
    function evalStatements(Statements :TList<TASTStatement>; env: TEnvironment): TEvalObject;
    function evalBlockStatements(node :TASTBlockStatement; env: TEnvironment): TEvalObject;
    function evalBangOperatorExpression(right: TEvalObject):TEvalObject;
    function evalMinusPrefixOperatorExpression(right: TEvalObject): TEvalObject;
    function evalPrefixExpression(Op:string; right:TEvalObject):TEvalObject;
    function evalPostfixExpression(Op:string; left:TEvalObject):TEvalObject;
    function evalIntegerInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalStringInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalDateTimeInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalBooleanInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
    function evalInfixExpression(Op:string; left, right: TEvalObject):TEvalObject;
    function evalIfExpression(node:TASTIfExpression; env :TEnvironment):TEvalObject;
    function evalSwitchExpression(node:TASTSwitchExpression; env :TEnvironment):TEvalObject;
    function evalTernaryExpression(node:TASTTernaryExpression; env :TEnvironment):TEvalObject;
    function evalWhileExpression(node:TASTWhileExpression; env :TEnvironment):TEvalObject;
    function evalForExpression(node:TASTForExpression; env :TEnvironment):TEvalObject;
    function evalForEachExpression(node:TASTForEachExpression; env :TEnvironment):TEvalObject;
    function evalIdentifier(node: TASTIdentifier; env :TEnvironment):TEvalObject;
    function evalHashLiteral(node:TASTHashLiteral; env :TEnvironment):TEvalObject;
    function evalExpressions(exps:TList<TASTExpression>; env:TEnvironment):TList<TEvalObject>;
    function evalIndexExpression(left, index:TEvalObject):TEvalObject;
    function evalMethodCallExpression(node:TASTMethodCallExpression; env: TEnvironment):TEvalObject;
    function evalMethodCall(AObject: TEvalObject; node:TASTCallExpression; env: TEnvironment):TEvalObject;
    function extendFunctionEnv(fn: TFunctionObject; args: TList<TEvalObject>; env:TEnvironment):TEnvironment;
    function unwrapReturnValue(obj:TEvalObject):TEvalObject;
    function applyFunction(fn: TEvalObject; args: TList<TEvalObject>; env:TEnvironment):TEvalObject;
    function assignExpression(node: TASTNode; AValue: TEvalObject; env: TEnvironment):TEvalObject;
    // -- utils function -- //
    function  CreateErrorObj(const AFormat: string; const Args: array of const): TErrorObject;
    function  CreateNullObj: TNullObject;
    function  AddEnv(AEnv:TEnvironment):TEnvironment;
    procedure DelEnv(AEnv:TEnvironment);

    function OnEvalNotifier(AModule:string; ALine, APos: Integer; AEnvironment:TEnvironment; var AContinue:Boolean):Boolean;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function  Run(node: TASTNode; env: TEnvironment): TEvalObject;
    function  Eval(node: TASTNode; env: TEnvironment): TEvalObject;

    property Gc: TGarbageCollector read FGarbageCollector;
    property EvalNotifierEvent: TEvalNotifierEvent read FEvalNotifierEvent write FEvalNotifierEvent;
  end;

var
  builtins: TDictionary<string,TBuiltinObject>;
  builtedmodules: TObjectDictionary<string,TASTProgram>;
  GCCounterWait: Integer = 100000; // default value is 100, but is very low

procedure ClearProjectModule;

implementation

procedure ClearProjectModule;
var
  sModule:string;
begin
  for sModule in builtedmodules.Keys do
  begin
    if (Pos('main.',sModule)>0) then
      builtedmodules.Remove(sModule);
  end;
end;

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
begin
  Result := evalStatements(TASTProgram(node).FStatements, env);
  if Result is TReturnValueObject then
    Result := TReturnValueObject(Result).Value;
end;

function TEvaluator.evalBlockStatements(node :TASTBlockStatement; env: TEnvironment): TEvalObject;
begin
  Result := evalStatements(TASTBlockStatement(node).FStatements, env);
end;

function TEvaluator.evalStatements(Statements: TList<TASTStatement>; env: TEnvironment): TEvalObject;
var
  i: Integer;
  Continue: Boolean;
begin
  Result := nil;
  for i := 0 to Statements.Count-1 do
  begin
    Continue := True;
    OnEvalNotifier(Statements[i].Token.Module, Statements[i].Token.Line, Statements[i].Token.Coln, env, Continue);
    if NOT Continue then
      Exit(CreateErrorObj('interrupted by user',[]));

    Result := Eval(Statements[i], env);

    if (Result<>nil) then
    begin
      Gc.Mark(Result);
      if (Result.ObjectType = RETURN_VALUE_OBJ) then
      begin
        Gc.Add(TReturnValueObject(Result).Value);
        Gc.Mark(TReturnValueObject(Result).Value);
      end;

      Inc(FGCCounter);
      if (FGCCounter>=GCCounterWait) then
      begin
        Gc.Mark(env);
        GC.Sweep;
        FGCCounter := 0;
      end;

      if ((Result.ObjectType = RETURN_VALUE_OBJ)
      or (Result.ObjectType = ERROR_OBJ)
      or (Result.ObjectType = LOOP_OBJ)) then
        Break;
    end;
  end;
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

function TEvaluator.evalMethodCall(AObject: TEvalObject; node: TASTCallExpression; env: TEnvironment): TEvalObject;
var
  args:TList<TEvalObject>;
  FEnv:TEnvironment;
  method: string;
  index:TEvalObject;
begin
  args:= evalExpressions(node.Args, env);
  try
    if ((args.Count=1) and (isError(args[0]))) then
      Result := args[0]
    else
    begin
      method := (node.Funct as TASTIdentifier).Value;
      Result := AObject.MethodCall(method, args, env);
      if (Result is TFunctionObject) then
      begin
        FEnv := AddEnv(extendFunctionEnv(Result as TFunctionObject, args, env));
        try
          FEnv.SetOrCreateValue('self', AObject, false);
          Result := unwrapReturnValue(Eval(TFunctionObject(Result).Body, FEnv));
        finally
          Gc.Mark(FEnv);
          DelEnv(FEnv);
        end;
      end
      else
      if Result=nil then
        Result := CreateErrorObj('Error call object method <%s> on <%s>', [method, AObject.ObjectType])
      else
        Gc.Add(Result);
    end;
  finally
    for index in args do
      index.DecRefCount;
    args.Free;
  end;
end;

function TEvaluator.evalMethodCallExpression(node: TASTMethodCallExpression; env: TEnvironment): TEvalObject;
var
  index: TEvalObject;
begin
  Result:= Eval(node.Objc, env);
  if NOT isError(Result) then
  begin
    if (node.Call is TASTCallExpression) then
    begin
      Result := evalMethodCall(Result, node.Call as TASTCallExpression, env);
    end
    else
    if (node.Call is TASTIdentifier) then
      Result:= Gc.Add(Result.GetIdentifer((node.Call as TASTIdentifier).Value, nil))
    else
    if (node.Call is TASTIndexExpression) then
    begin
      if (((node.Call as TASTIndexExpression).Left) is TASTIdentifier) then
      begin
        index:= Eval((node.Call as TASTIndexExpression).Index, env);
        if NOT isError(index) then
          Result:= Gc.Add(Result.GetIdentifer(TASTIdentifier((node.Call as TASTIndexExpression).Left).Value, index))
        else
          Result := index;
      end
      else Result := CreateErrorObj('Index Expression not an indentifier got=%s',[node.Call.toString]);
    end
    else
    if (node.Call is TASTMethodCallExpression) then
    begin
      Result:= evalMethodCallExpression(node as TASTMethodCallExpression, env);
    end
    else Result := CreateErrorObj('Type of Expression error ',[Result.ObjectType]);
  end;
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
    Result := TErrorObject.newError('unknown operator: %s%s', [Op, right.ObjectType]);

  Gc.Add(Result);
end;

function TEvaluator.evalPostfixExpression(Op: string;  left: TEvalObject): TEvalObject;
begin
  if left.ObjectType=NUMBER_OBJ then
  begin
    if Op='++' then Result := TNumberObject.Create(TNumberObject(left).Value+1)
    else
    if Op='--' then Result := TNumberObject.Create(TNumberObject(left).Value-1)
    else
      Result := TErrorObject.newError('unknown operator: %s%s', [left.ObjectType, Op]);
  end
  else Result := TErrorObject.newError('unknown type operator: %s%s', [left.ObjectType, Op]);

  Gc.Add(Result);
end;

function TEvaluator.evalIntegerInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
var
  LVal,
  RVal:Double;
  Step,
  Len,i,IVal:Integer;
begin
  LVal:= TNumberObject(left).Value;
  RVal:= TNumberObject(right).Value;
  if Op='+'  then Result:= TNumberObject.Create(LVal + RVal)
  else
  if Op='-'  then Result:= TNumberObject.Create(LVal - RVal)
  else
  if Op='*'  then Result:= TNumberObject.Create(LVal * RVal)
  else
  if Op='/'  then Result:= TNumberObject.Create(LVal / RVal)
  else
  if Op='<'  then Result:= nativeBoolToBooleanObject(LVal < RVal)
  else
  if Op='%'  then Result:= TNumberObject.Create(Trunc(LVal) mod Trunc(RVal))
  else
  if Op='<=' then Result:= nativeBoolToBooleanObject(LVal <= RVal)
  else
  if Op='>'  then Result:= nativeBoolToBooleanObject(LVal > RVal)
  else
  if Op='>=' then Result:= nativeBoolToBooleanObject(LVal >= RVal)
  else
  if Op='==' then Result:= nativeBoolToBooleanObject(LVal = RVal)
  else
  if Op='!=' then Result:= nativeBoolToBooleanObject(LVal <> RVal)
  else
  if Op='+=' then Result:= TNumberObject.Create(LVal + RVal)
  else
  if Op='-=' then Result:= TNumberObject.Create(LVal - RVal)
  else
  if Op='*=' then Result:= TNumberObject.Create(LVal * RVal)
  else
  if Op='/=' then Result:= TNumberObject.Create(LVal / RVal)
  else
  if Op='%=' then Result:= TNumberObject.Create(Trunc(LVal) mod Trunc(RVal))
  else
  if Op='**' then Result:= TNumberObject.Create(Power(LVal, RVal))
  else
  if Op='..' then
  begin
    { -- operator range begin -- }
    IVal:= Trunc(Abs(LVal));
    Len := Trunc(Abs(RVal-LVal)) + 1;
    Step:= 1;
    if (RVal<LVal) then
      Step:=-Step;

    if Len>0 then
    begin
      Result:= TArrayObject.Create;
      TArrayObject(Result).Elements := TList<TEvalObject>.Create;
      for i := 0 to Len-1 do
      begin
        TArrayObject(Result).Elements.Add(TNumberObject.Create(IVal));
        IVal := IVal + Step;
      end;
    end
    else Result := TErrorObject.newError('invalid len of range = 0', []);
    { -- operator range end -- }
  end
  else Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType]);
end;

function TEvaluator.evalStringInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
var
  LVal,
  RVal:string;
begin
  LVal:= TStringObject(left).Value;
  RVal:= TStringObject(right).Value;
  if Op='+'  then Result:= TStringObject.Create(LVal + RVal)
  else
  if Op='+=' then Result:= TStringObject.Create(LVal + RVal)
  else
  if Op='<'  then Result:= nativeBoolToBooleanObject(LVal < RVal)
  else
  if Op='<=' then Result:= nativeBoolToBooleanObject(LVal <= RVal)
  else
  if Op='>'  then Result:= nativeBoolToBooleanObject(LVal > RVal)
  else
  if Op='>=' then Result:= nativeBoolToBooleanObject(LVal >= RVal)
  else
  if Op='==' then Result:= nativeBoolToBooleanObject(LVal = RVal)
  else
  if Op='!=' then Result:= nativeBoolToBooleanObject(LVal <> RVal)
  else
    Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType])
end;

function TEvaluator.evalSwitchExpression(node: TASTSwitchExpression; env: TEnvironment): TEvalObject;
var
  svalue,cvalue:TEvalObject;
  ccase:TASTCaseExpression;
  xvalue:TASTExpression;
begin
  Result:= nil;
  svalue:= Eval(node.Value, env);

  for ccase in node.Choises do
    if NOT ccase.Default then
    begin
      for xvalue in ccase.Values do
      begin
        cvalue := Eval(xvalue, env);
        if ((cvalue.ObjectType=svalue.ObjectType) and (cvalue.Inspect=svalue.Inspect)) then
        begin
          Result := Eval(ccase.Body, env);
          Exit(Result);
        end;
      end;
    end;

  if Result=nil then
    for ccase in node.Choises do
      if ccase.Default then
      begin
        Result := Eval(ccase.Body, env);
        Break;
      end;

end;

function TEvaluator.evalTernaryExpression(node: TASTTernaryExpression; env: TEnvironment): TEvalObject;
begin
  Result := nil;
  if isTruthyWithError(Eval(node.Condition,env), Result) then
    Result := Eval(node.IfTrue,env)
  else
  begin
    if NOT isError(Result) then
      Result := Eval(node.IfFalse,env);
  end;
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

function TEvaluator.evalDateTimeInfixExpression(Op: string; left, right: TEvalObject): TEvalObject;
var
  LVal,
  RVal:TDateTime;
begin
  LVal:= TDateTimeObject(left).Value;
  RVal:= TDateTimeObject(right).Value;
  if Op='<'  then Result:= nativeBoolToBooleanObject(LVal < RVal)
  else
  if Op='<=' then Result:= nativeBoolToBooleanObject(LVal <= RVal)
  else
  if Op='>'  then Result:= nativeBoolToBooleanObject(LVal > RVal)
  else
  if Op='>=' then Result:= nativeBoolToBooleanObject(LVal >= RVal)
  else
  if Op='==' then Result:= nativeBoolToBooleanObject(LVal = RVal)
  else
  if Op='!=' then Result:= nativeBoolToBooleanObject(LVal <> RVal)
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
  if ((left.ObjectType=DATETIME_OBJ) and (right.ObjectType=DATETIME_OBJ)) then
    Result := evalDateTimeInfixExpression(Op, left, right)
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
		Result := TErrorObject.newError('unknown operator: %s %s %s', [left.ObjectType, Op, right.ObjectType]);

  Gc.Add(Result);
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
      Result := CreateNullObj;
  end;
end;

function TEvaluator.evalWhileExpression(node:TASTWhileExpression; env :TEnvironment):TEvalObject;
begin
  Result := nil;
  while isTruthyWithError(Eval(TASTWhileExpression(node).Condition, env), Result) do
  begin
    Result := Eval(TASTWhileExpression(node).Body, env);
    if Assigned(Result) then
      if isError(Result) then
        Break
      else
      if ((Result.ObjectType=RETURN_VALUE_OBJ) and isError(TReturnValueObject(Result).Value)) then
      begin
        Result := TReturnValueObject(Result).Value;
        Break;
      end
      else
      if ((Result.ObjectType=LOOP_OBJ) and (TLoopObject(Result).LoopType=LOOP_TYPE_BREAK)) then
        Break;
  end;
end;

function TEvaluator.evalForEachExpression(node: TASTForEachExpression;  env: TEnvironment): TEvalObject;
var
  iterable,
  current,
  ret:TEvalObject;
  FEachEnv:TEnvironment;
  L:TStringList;
begin
  Result:= nil;
  iterable:= Eval(TASTForEachExpression(node).Expression, env);
  if NOT iterable.isIterable then
  begin
    Result := CreateErrorObj('%s object doesn''t implement the Iterable interface', [iterable.ObjectType]);
    Exit(Result);
  end
  else
  begin
    L:=TStringList.Create;
    try
      L.Add(TASTForEachExpression(node).ident);
      if TASTForEachExpression(node).index<>'' then
        L.Add(TASTForEachExpression(node).index);

      FEachEnv:= AddEnv(TEnvironment.Create(env, L));
      try
        iterable.Reset;
        current := gc.Add(iterable.Next);

        while Assigned(current) do
        begin
          FEachEnv.SetOrCreateValue(TASTForEachExpression(node).ident, current, false);
          if TASTForEachExpression(node).index<>'' then
            FEachEnv.SetOrCreateValue(TASTForEachExpression(node).index, gc.Add(iterable.CurrentIndex), false);

          ret := Eval(TASTForEachExpression(node).Body, FEachEnv);
          if Assigned(ret) then
            if isError(ret) then
            begin
              Result := ret;
              Break;
            end
            else
            if ((ret.ObjectType=RETURN_VALUE_OBJ) and isError(TReturnValueObject(ret).Value)) then
            begin
              Result := TReturnValueObject(ret).Value;
              Break;
            end
            else
            if ((ret.ObjectType=LOOP_OBJ) and (TLoopObject(ret).LoopType=LOOP_TYPE_BREAK)) then
              Break;

          current := gc.Add(iterable.Next);
        end;
      finally
        DelEnv(FEachEnv);
      end;

    finally
      L.Free;
    end;
  end;
end;

function TEvaluator.evalForExpression(node:TASTForExpression; env :TEnvironment):TEvalObject;
begin
  while isTruthyWithError(Eval(TASTForExpression(node).Condition, env), Result) do
  begin
    Result := Eval(TASTForExpression(node).Body, env);
    if Assigned(Result) then
      if isError(Result) then
        Break
      else
      if ((Result.ObjectType=RETURN_VALUE_OBJ) and isError(TReturnValueObject(Result).Value)) then
      begin
        Result := TReturnValueObject(Result).Value;
        Break;
      end
      else
      if ((Result.ObjectType=LOOP_OBJ) and (TLoopObject(Result).LoopType=LOOP_TYPE_BREAK)) then
        Break;

    Eval(TASTForExpression(node).Expression, env);
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
      Result := CreateErrorObj('identifier not found: %s ',[node.Value]);
  end;
end;

function TEvaluator.evalHashLiteral(node:TASTHashLiteral; env :TEnvironment):TEvalObject;
var
  k,v: TEvalObject;
  h:THashkey;
  pair: TPair<TASTExpression,TASTExpression>;
begin
  Result := THashObject.Create;

  for pair in node.Pairs do
  begin
    k := Eval(pair.Key, env);
    if isError(k) then
    begin
      FreeAndNil(Result);
      Exit(k);
    end;

    h:= THashkey.fromObject(k);
    if h.ObjectType=ERROR_OBJ then
    begin
      FreeAndNil(k);
      FreeAndNil(h);
      FreeAndNil(Result);
      Exit(CreateErrorObj('unusable as hash key: %s', [k.ObjectType]));
    end;

    v := Eval(pair.Value, env);
    if isError(v) or (v = nil) then
    begin
      FreeAndNil(k);
      FreeAndNil(h);
      FreeAndNil(Result);
      Exit(v);
    end;

    THashObject(Result).Pairs.Add(h, THashPair.Create(k,v));
  end;
end;


function TEvaluator.evalExpressions(exps:TList<TASTExpression>; env:TEnvironment):TList<TEvalObject>;
var
  i:Integer;
  val:TEvalObject;
begin
  Result := TList<TEvalObject>.Create;
  if Assigned(exps) then
    for i := 0 to exps.Count-1 do
    begin
      val := Eval(exps[i], env);
      if isError(val) then
      begin
        Result.Clear;
        Result.Add(val);
        Break;
      end;
      val.IncRefCount; { -- ref for arg -- }
      Result.Add(val);
    end;
end;

function TEvaluator.assignExpression(node: TASTNode; AValue: TEvalObject; env: TEnvironment): TEvalObject;
var
  enobj,
  index: TEvalObject;
  N,P:TASTNode;
  Q:TQueue<TASTNode>;
begin
  if node is TASTIdentifier then
    Result := env.SetValue(TASTIdentifier(node).Value, AValue)
  else
  if node is TASTIndexExpression then
  begin
    enobj := Eval(TASTIndexExpression(node).Left, env);
    if NOT isError(AValue) then
    begin
      index := Eval(TASTIndexExpression(node).Index, env);
      if NOT isError(AValue) then
        Result := Gc.Add(enobj.Setindex(index, AValue))
      else
        Result := AValue;
    end
    else Result := AValue;
  end
  else
  if node is TASTMethodCallExpression then
  begin
    Q:= TQueue<TASTNode>.Create;
    try
      N:= (node as TASTMethodCallExpression).Objc;
      while (N is TASTMethodCallExpression) do
      begin
        Q.Enqueue((N as TASTMethodCallExpression).Call);
        N:= (N as TASTMethodCallExpression).Objc;
      end;

      enobj := Eval(N, env);
      if NOT isError(enobj) then
      begin
        while (Q.Count<>0) do
        begin
          P:= Q.Dequeue;
          if (P is TASTIdentifier) then
            enobj := enobj.GetIdentifer((P as TASTIdentifier).Value, nil)
          else
          if (P is TASTIndexExpression) then
          begin
            index := Eval((P as TASTIndexExpression).index,env);
            enobj := enobj.GetIdentifer(((P as TASTIndexExpression).Left as TASTIdentifier).Value, index);
          end
          else
          if (P is TASTCallExpression) then
            enobj := evalMethodCall(enobj, P as TASTCallExpression, env)
          else
            enobj := CreateErrorObj('Object Type <%s> not supported for dot operator',[P.toString]);

          if isError(enobj) then
          begin
            Result := enobj;
            Break;
          end;
        end;

        if NOT isError(enobj) then
        begin
          P:= (node as TASTMethodCallExpression).Call;
          if (P is TASTIdentifier) then
            Result := enobj.SetIdentifer(TASTIdentifier(P).Value, AValue, nil)
          else
          if (P is TASTIndexExpression) then
          begin
            index := Eval((P as TASTIndexExpression).index, env);
            Result:= enobj.SetIdentifer(((P as TASTIndexExpression).Left as TASTIdentifier).Value, AValue, index);
          end
          else Result := CreateErrorObj('Object Type <%s> not supported for dot assign operator',[P.toString]);
        end
        else Result := enobj;

      end
      else Result := enobj;
    finally
      Q.Free;
    end;
  end
  else Result := AValue;
end;

constructor TEvaluator.Create;
begin
  FGCCounter := 0;
  FGarbageCollector:= TGarbageCollector.Create;
  FFunctEnv := TList<TEnvironment>.Create;
  FFunctLck:= TCriticalSection.Create;
  FModules := TStringList.Create;
end;

function TEvaluator.CreateErrorObj(const AFormat: string; const Args: array of const): TErrorObject;
begin
  Result := TErrorObject.newError(AFormat, Args);
  Gc.Add(Result);
end;

function TEvaluator.CreateNullObj: TNullObject;
begin
  Result := TNullObject.Create;
  Gc.Add(Result);
end;

destructor TEvaluator.Destroy;
var
  current: TEnvironment;
begin
  FGarbageCollector.Free;

  for current in FFunctEnv do
    current.Free;

  FFunctLck.Free;
  FFunctEnv.Free;
  FModules.Free;
  inherited;
end;

function TEvaluator.evalIndexExpression(left, index:TEvalObject):TEvalObject;
begin
  Result := Gc.Add(left.GetIndex(index));
end;

function TEvaluator.extendFunctionEnv(fn: TFunctionObject; args: TList<TEvalObject>; env:TEnvironment):TEnvironment;
var
  i: Integer;
begin
  Result := TEnvironment.Create(env);
  for i := 0 to fn.Parameters.Count-1 do
    // -- Result.SetOrCreateValue(fn.Parameters[i].Value, Gc.Add(args[i].Clone), False);
    Result.SetOrCreateValue(fn.Parameters[i].Value, Gc.Add(args[i].Reference), False);
end;

function TEvaluator.OnEvalNotifier(AModule: string; ALine, APos: Integer;  AEnvironment: TEnvironment; var AContinue:Boolean): Boolean;
begin
  Result := Assigned(FEvalNotifierEvent);
  if Result then
    Result := FEvalNotifierEvent(AModule, ALine, APos, AEnvironment, Self, AContinue);
end;

procedure TEvaluator.DelEnv(AEnv: TEnvironment);
begin
  if Assigned(AEnv) then
  try
    FFunctLck.Acquire;
    if FFunctEnv.IndexOf(AEnv)>=0 then
      FFunctEnv.Remove(AEnv);

    AEnv.Free;
  finally
    FFunctLck.Release;
  end;
end;

function TEvaluator.Run(node: TASTNode; env: TEnvironment): TEvalObject;
var
  sModule:string;
begin
  for sModule in builtedmodules.Keys do
    if (FModules.IndexOf(sModule)<0) then
    begin
      Eval(builtedmodules[sModule], env);
      FModules.Add(sModule);
    end;

  Result := Eval(node, env);

  for sModule in builtedmodules.Keys do
  begin
    if (Pos('main.',sModule)>0) then
    begin
      builtedmodules[sModule].Free;
      builtedmodules.Remove(sModule);
    end;
  end;
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


function TEvaluator.AddEnv(AEnv: TEnvironment):TEnvironment;
begin
  Result := AEnv;
  try
    FFunctLck.Acquire;
    if FFunctEnv.IndexOf(AEnv)<0 then
      FFunctEnv.Add(AEnv);
  finally
    FFunctLck.Release;
  end;
end;

function TEvaluator.applyFunction(fn: TEvalObject; args: TList<TEvalObject>; env:TEnvironment):TEvalObject;
var
  FEnv:TEnvironment;
  CurrOb:TEvalObject;
begin
  if (fn is TFunctionObject) then
  begin
    FEnv := AddEnv(extendFunctionEnv(fn as TFunctionObject, args, env));
    try
      Result := unwrapReturnValue(Eval(TFunctionObject(fn).Body, FEnv));
    finally
      Gc.Mark(FEnv);
      DelEnv(FEnv);
    end;
  end
  else
  if (fn is TBuiltinObject) then
  begin
    Result := Gc.Add(TBuiltinObject(fn).BuiltinFunction(env, args));
  end
  else Result := CreateErrorObj('not a function: %s', [fn.ObjectType])
end;


function TEvaluator.Eval(node: TASTNode; env: TEnvironment): TEvalObject;
var
  val,index:TEvalObject;
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
    Result := Gc.Add(TNumberObject.Create(TASTNumberLiteral(node).Value))
  else
  if node is TASTBoolean then
    Result := Gc.Add(nativeBoolToBooleanObject(TASTBoolean(node).Value))
  else
  if node is TASTStringLiteral then
    Result := Gc.Add(TStringObject.Create(TASTStringLiteral(node).Value))
  else
  if node is TASTNullLiteral then
    Result := Gc.Add(TNullObject.Create)
  else
  if node is TASTLoopStatement then
    Result := Gc.Add(TLoopObject.Create(TASTLoopStatement(node).LoopType))
  else
  if node is TASTArrayLiteral then
  begin
    Result := TArrayObject.Create;
    TArrayObject(Result).Elements := evalExpressions(TASTArrayLiteral(node).Elements, env);
    Gc.Add(Result);
    for index in TArrayObject(Result).Elements do
      index.DecRefCount;
    if ((TArrayObject(Result).Elements.Count=1) and (isError(TArrayObject(Result).Elements[0]))) then
      Result := TArrayObject(Result).Elements[0];
  end
  else
  if node is TASTHashLiteral then
    Result := Gc.Add(evalHashLiteral(TASTHashLiteral(node), env))
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
  if node is TASTPostfixExpression then
  begin
    Result := Eval(TASTPostfixExpression(node).Left, env);
		if NOT isError(Result) then
    begin
  		Result := evalPostfixExpression(TASTPostfixExpression(node).Op, Result);
      if NOT isError(Result) then
        Result := assignExpression(TASTPostfixExpression(node).Left, Result, env);
    end;
  end
  else
  if node is TASTPrefixExpression then
  begin
    Result := Eval(TASTPrefixExpression(node).Right, env);
		if NOT isError(Result) then
  		Result := evalPrefixExpression(TASTPrefixExpression(node).Op, Result);
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
  if node is TASTSwitchExpression then
    Result := evalSwitchExpression(node as TASTSwitchExpression, env)
  else
  if node is TASTIfExpression then
    Result := evalIfExpression(node as TASTIfExpression, env)
  else
  if node is TASTTernaryExpression then
    Result := evalTernaryExpression(node as TASTTernaryExpression, env)
  else
  if node is TASTWhileExpression then
    Result := evalWhileExpression(node as TASTWhileExpression, env)
  else
  if node is TASTForExpression then
    Result := evalForExpression(node as TASTForExpression, env)
  else
  if node is TASTForEachExpression then
    Result := evalForEachExpression(node as TASTForEachExpression, env)
  else
  if node is TASTReturnStatement then
  begin
    Result := Eval(TASTReturnStatement(node).ReturnValue, env);
		if NOT isError(Result) then
      Result := Gc.Add(TReturnValueObject.Create(env.AddRef(Result)));
  end
  else
  if node is TASTImportStatement then
  begin
    if builtedmodules.ContainsKey(TASTImportStatement(node).Module) then
    begin
      if (FModules.IndexOf(TASTImportStatement(node).Module)<0) then
        Result := Eval(builtedmodules[TASTImportStatement(node).Module], env)
    end
    else Result := CreateErrorObj('module %s not found.',[TASTImportStatement(node).Module]);
  end
  else
  if node is TASTConstStatement then
  begin
    val := Eval(TASTConstStatement(node).Expression, env);
    if NOT isError(val) then
      if TASTConstStatement(node).Name is TASTIdentifier then
        Result := env.SetOrCreateValue(TASTIdentifier(TASTConstStatement(node).Name).Value, val, True);
  end
  else
  if node is TASTLetStatement then
  begin
    val := Eval(TASTLetStatement(node).Expression, env);
    if NOT isError(val) then
    begin
      if TASTLetStatement(node).Name is TASTIdentifier then
        Result := env.SetOrCreateValue(TASTIdentifier(TASTLetStatement(node).Name).Value, val, False);
    end
    else Result := val;
  end
  else
  if node is TASTAssignExpression then
  begin
    val := Eval(TASTAssignExpression(node).Expression, env);
    if NOT isError(val) then
    begin
      if (TASTAssignExpression(node).Op='+=')
      or (TASTAssignExpression(node).Op='-=')
      or (TASTAssignExpression(node).Op='*=')
      or (TASTAssignExpression(node).Op='/=')
      or (TASTAssignExpression(node).Op='%=') then
      begin
        val := evalInfixExpression(TASTAssignExpression(node).Op, Eval(TASTAssignExpression(node).Name, env), val);
      end;

      if NOT isError(val) then
      begin
        Result := assignExpression(TASTAssignExpression(node).Name, val, env);
      end
      else Result := val;

    end
    else Result := val;
  end
  else
  if node is TASTIdentifier then
    Result := evalIdentifier(node as TASTIdentifier, env)
  else
  if node is TASTMethodCallExpression then
    Result := evalMethodCallExpression(node as TASTMethodCallExpression, env)
  else
  if node is TASTFunctionDefineStatement then
  begin
    Result := Eval(TASTFunctionDefineStatement(node).Funct, env);
    if NOT isError(Result) then
      Result := env.SetOrCreateValue(TASTFunctionDefineStatement(node).Indent, Result, True);
  end
  else
  if node is TASTClosureLiteral then
  begin
    Result := Eval(TASTClosureLiteral(node).Funct, env);
    if NOT isError(Result) then
      Result := Gc.Add(TClosureObject.Create(Result as TFunctionObject, env))
  end
  else
  if node is TASTFunctionLiteral then
    Result := Gc.Add(TFunctionObject.Create(TASTFunctionLiteral(node).Parameters,TASTFunctionLiteral(node).Body))
  else
  if node is TASTCallExpression then
  begin
    Result := Eval(TASTCallExpression(node).Funct, env);
    if NOT isError(Result) then
    begin
		  args := evalExpressions(TASTCallExpression(node).Args, env);
      try
        if ((args.Count=1) and (isError(args[0]))) then
          Result := args[0]
        else
        if Result is TClosureObject then
          Result := applyFunction(TClosureObject(Result).Funct, args, TClosureObject(Result).Env)
        else
          Result := applyFunction(Result, args, env);
      finally
        for index in args do
          index.DecRefCount;
        args.Free;
      end;
    end;
  end;
end;

{ TGarbageCollector }

function TGarbageCollector.Add(AObject: TEvalObject):TEvalObject;
var
  Element: TEvalObject;
  Hkey: THashkey;
begin
  Result := AObject;
  if Assigned(AObject) then
//  if NOT AObject.GcManualFree then
  begin
    FLock.Acquire;
    try
      if FObjects.IndexOf(AObject)<0 then
        FObjects.Add(AObject);
    finally
      FLock.Release;
    end;
    if AObject.ObjectType=ARRAY_OBJ then
    begin
      if Assigned(TArrayObject(AObject).Elements) then
        for Element in TArrayObject(AObject).Elements do
          Add(Element);
    end
    else
    if AObject.ObjectType=HASH_OBJ then
    begin
      if Assigned(THashObject(AObject).Pairs) then
        for HKey in THashObject(AObject).Pairs.Keys do
        begin
          Add(THashObject(AObject).Pairs[HKey].Key);
          Add(THashObject(AObject).Pairs[HKey].Value);
        end;
    end
    else
    if AObject.ObjectType=RETURN_VALUE_OBJ then
      Add(TReturnValueObject(AObject).Value);
  end;
end;

constructor TGarbageCollector.Create;
begin
  FLock:= TCriticalSection.Create;
  FObjects:= TList<TEvalObject>.Create;
end;

destructor TGarbageCollector.Destroy;
var
  i: Integer;
  P: Pointer;
begin
  P:=nil;
  FLock.Acquire;
  try
    FObjects.Sort;
    for i := 0 to FObjects.Count-1 do
    try
      if P<>FObjects[i] then
      if NOT (FObjects[i].GcManualFree) then
      begin
        P:=FObjects[i];
        FObjects[i].Free;
      end;

    except
      Continue;
    end;
    FObjects.Clear;
  finally
    FLock.Release;
  end;

  FreeAndNil(FObjects);
  FreeAndNil(FLock);
  inherited;
end;

function TGarbageCollector.GetElemCount: Integer;
begin
  Result := FObjects.Count;
end;

procedure TGarbageCollector.Mark(AObject: TEvalObject);
var
  Element: TEvalObject;
  HKey: THashkey;
begin
  if Assigned(AObject) then
    if NOT AObject.GcManualFree then
    begin
      if NOT (AObject.GcMark) then
      begin
        AObject.GcMark := True;
        AObject.MarkChild;
      end;

      { -- standard object marking -- }
      if AObject.ObjectType=ARRAY_OBJ then
      begin
        if Assigned(TArrayObject(AObject).Elements) then
          for Element in TArrayObject(AObject).Elements do
            Mark(Element);
      end
      else
      if AObject.ObjectType=HASH_OBJ then
      begin
        if Assigned(THashObject(AObject).Pairs) then
          for HKey in THashObject(AObject).Pairs.Keys do
          begin
            Mark(THashObject(AObject).Pairs[HKey].Key);
            Mark(THashObject(AObject).Pairs[HKey].Value);
          end;
      end
      else
      if AObject.ObjectType=RETURN_VALUE_OBJ then
        Mark(TReturnValueObject(AObject).Value)
      else
      if AObject.ObjectType=CLOSURE_OBJ then
        Mark(TClosureObject(AObject).Env);
      { -- standard object marking -- }
    end;
end;

procedure TGarbageCollector.Mark(AEnvironment: TEnvironment);
var
  element: string;
begin
  for element in AEnvironment.Store.Keys do
    Mark(AEnvironment.Store[element]);

  if (AEnvironment.Outer<>nil) then
    Mark(AEnvironment.Outer);
end;

procedure TGarbageCollector.Sweep;
var
  i:Integer;
  current:TEvalObject;
begin
  try
    FLock. Acquire;
    for current in FObjects do
      if (NOT current.GcMark and NOT current.GcManualFree and (current.GcRefCount<=0)) then
      begin
        FObjects.Remove(current);
        current.Free;
      end;
    for i := 0 to FObjects.Count-1 do
      FObjects[i].GcMark := False;
  finally
    FLock.Release;
  end;
end;

/////-------------------------------------------

procedure init;
begin
  builtins := TDictionary<string,TBuiltinObject>.Create;
  builtedmodules := TObjectDictionary<string,TASTProgram>.Create([doOwnsValues]);
end;

procedure deinit;
begin
  builtins.Free;
  builtedmodules.Free;
end;

initialization
  init;
finalization
  deinit;

end.

