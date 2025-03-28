unit lp.environment;

interface

uses classes, SysUtils, Generics.Collections, Variants, StrUtils
  ,lp.parser
  ,lp.utils;


const
	NUMBER_OBJ       = 'NUMBER';
	BOOLEAN_OBJ      = 'BOOLEAN';
	STRING_OBJ       = 'STRING';
	NULL_OBJ         = 'NULL';
	RETURN_VALUE_OBJ = 'RETURN_VALUE';
	ERROR_OBJ        = 'ERROR';
	FUNCTION_OBJ     = 'FUNCTION';
	BUILTIN_OBJ      = 'BUILTIN';
	ARRAY_OBJ        = 'ARRAY';
	HASH_OBJ         = 'HASH';
	LOOP_OBJ         = 'LOOP';
	CLOSURE_OBJ      = 'CLOSURE';

const
  LOOP_TYPE_BREAK  = 'break';
  LOOP_TYPE_CONTINUE  = 'continue';

type
  TEvalObject = class;
  TEnvironment = class;
  TBuiltinFunction = function(args: TList<TEvalObject>): TEvalObject;

  TEnvironment = class
    FStore: TDictionary<string,TEvalObject>;
    FConst: TStringList;
    FOuter: TEnvironment;
    FAllowedIdent: TStringList;
  public
    constructor Create; overload;
    constructor Create(AOuter:TEnvironment); overload;
    constructor Create(AOuter:TEnvironment; AllowedIdent:TStringList); overload;
    destructor Destroy; override;
    property Store: TDictionary<string,TEvalObject> read FStore;
    property Outer: TEnvironment read FOuter write FOuter;
    function GetValue(name:string; var value:TEvalObject ):Boolean;
    function SetValue(name:string; value:TEvalObject ):TEvalObject;
    function SetOrCreateValue(name:string; value:TEvalObject; IsConst:Boolean ):TEvalObject;

    function Clone:TEnvironment;
  end;

  // ---- Object --------------------
  TEvalObjectType = string;

  TEvalObject = class
  public
    IIndex: Integer;
    function ObjectType:TEvalObjectType; virtual;
    function Inspect:string; virtual;
    function Clone:TEvalObject; virtual;

    { -- support for object method call -- }
    function MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; virtual;
    function MethodInline(method:string; env: TEnvironment):TEvalObject; virtual;

    { -- support for object with index (es. array or map Array[index], map[index]) -- }
    function GetIndex(Index:TEvalObject):TEvalObject; virtual;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; virtual;

    { -- support for object with property identifier (es. object.property := value) -- }
    function GetIdentifer(name:string; Index:TEvalObject=nil):TEvalObject; virtual;
    function SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject=nil):TEvalObject; virtual;

    { -- support for interble value, index object element (es. foreach v,i in object ) -- }
    function  isIterable:Boolean; virtual;
    function  Next:TEvalObject; virtual;
    function  CurrentIndex:TEvalObject; virtual;
    procedure Reset; virtual;

  public
    { ** for GC (mark and sweep) ** }
    //GcNext:TEvalObject;
    GcMark:Boolean;
    GcManualFree:Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  THashkey = class
  public
    ObjectType: TEvalObjectType;
    FValueStr: string;
    FValueNum: Double;
    FValueBool: Boolean;
  public
    class function fromObject(Obj: TEvalObject):THashkey;
    constructor Create(Ref: TEvalObjectType); overload;
    constructor Create(Ref: TEvalObject); overload;
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
  end;

  TNumberObject = class(TEvalObject)
  public
    Value: Double;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
    function MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; override;
  public
    constructor Create(AValue: Double);
    function toInt:Integer;
  end;

  TBooleanObject = class(TEvalObject)
  public
    Value: Boolean;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AValue: Boolean);
  end;

  TStringObject = class(TEvalObject)
  public
    Value: string;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;

    function MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; override;

    function GetIndex(Index:TEvalObject):TEvalObject; override;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
    function CurrentIndex:TEvalObject; override;
  public
    constructor Create(AValue: string);
  end;

  TNullObject = class(TEvalObject)
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  end;

  TLoopObject = class(TEvalObject)
  public
    LoopType: string;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(ALoopType: string);
  end;

  TReturnValueObject = class(TEvalObject)
  public
    Value: TEvalObject;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create; overload;
    constructor Create(AValue: TEvalObject); overload;
    destructor Destroy; override;
  end;

  TErrorObject = class(TEvalObject)
  public
    ErrMessage: string;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AErrMessage: string);
    class function newError(const AFormat: string; const Args: Array of const):TErrorObject;
  end;

  TFunctionObject = class(TEvalObject)
  public
    Parameters: TList<TASTIdentifier>;
	  Body: TASTBlockStatement;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AParameters: TList<TASTIdentifier>; ABody: TASTBlockStatement);
    destructor Destroy; override;
  end;

  TClosureObject = class(TEvalObject)
  public
    Env: TEnvironment;
    Funct:TFunctionObject;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AFunct: TFunctionObject; AEnv:TEnvironment);
    destructor Destroy; override;
  end;

  TBuiltinObject = class(TEvalObject)
  public
    BuiltinFunction: TBuiltinFunction;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(ABuiltinFunction: TBuiltinFunction);
  end;

  TArrayObject = class(TEvalObject)
  public
    Elements: TList<TEvalObject>;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;

    function MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; override;

    function GetIndex(Index:TEvalObject):TEvalObject; override;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
    function CurrentIndex:TEvalObject; override;
  public
    constructor Create;
    constructor CreateWithElements;
    destructor Destroy; override;
  end;

  THashPair = class
  public
    Key: TEvalObject;
    Value: TEvalObject;
  public
    constructor Create(AKey, AValue:TEvalObject);
    destructor Destroy; override;
  end;

  THashObject = class(TEvalObject)
  public
    Pairs: TDictionary<THashkey,THashPair>;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;

    function MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; override;

    function GetIndex(Index:TEvalObject):TEvalObject; override;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; override;

    function GetIdentifer(name:string; Index:TEvalObject=nil):TEvalObject; override;
    function SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject=nil):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
    function CurrentIndex:TEvalObject; override;
  public
    destructor Destroy; override;
  end;

implementation

function QuoteIfString(value:TEvalObject):string;
begin
  if value.ObjectType=STRING_OBJ then
    Result := '"' + value.Inspect + '"'
  else
    Result := value.Inspect;
end;


{ TEvalObject }

function TEvalObject.Clone: TEvalObject;
begin
  Result := nil; { virtual }
end;

constructor TEvalObject.Create;
begin
  //GcNext := nil;
  GcManualFree := False;
  GcMark := False;
  Reset;
end;

function TEvalObject.CurrentIndex: TEvalObject;
begin
  Result := nil;
end;

destructor TEvalObject.Destroy;
begin
    //
  inherited;
end;

function TEvalObject.GetIdentifer(name: string; Index:TEvalObject): TEvalObject;
begin
  Result := TErrorObject.newError('get identifier not supported in object type %s',[ObjectType]);
end;

function TEvalObject.GetIndex(Index: TEvalObject): TEvalObject;
begin
  Result := TErrorObject.newError('get index not supported in object type %s',[ObjectType]);
end;

function TEvalObject.Inspect: string;
begin
  Result := '';
end;

function TEvalObject.isIterable: Boolean;
begin
  Result := false;
end;

function TEvalObject.MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  if (method='tostring') then
    Result := TStringObject.Create(Inspect)
  else
  if (method='type') then
    Result := TStringObject.Create(ObjectType)
  else
  if (method='clone') then
    Result := Clone
  else
    Result := MethodInline(ObjectType+'.'+method, env);

  if Result=nil then
    TErrorObject.newError('method %s not found in object type %s',[method, ObjectType]);
end;

function TEvalObject.MethodInline(method: string; env: TEnvironment): TEvalObject;
begin
  Result:= nil;
  env.GetValue(method, Result);
end;

function TEvalObject.Next: TEvalObject;
begin
  Result := nil;
end;

function TEvalObject.ObjectType: TEvalObjectType;
begin
  Result := '';
end;

procedure TEvalObject.Reset;
begin
  IIndex:=0;
end;

function TEvalObject.SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject): TEvalObject;
begin
  Result := TErrorObject.newError('set identifier not supported in object type %s',[ObjectType]);
end;

function TEvalObject.Setindex(Index, value: TEvalObject): TEvalObject;
begin
  Result := TErrorObject.newError('set index not supported in object type %s',[ObjectType]);
end;

{ TNumberObject }

function TNumberObject.Clone: TEvalObject;
begin
  Result := TNumberObject.Create(Value);
end;

constructor TNumberObject.Create(AValue: Double);
begin
  inherited Create;
  Value := AValue;
end;

function TNumberObject.Inspect: string;
begin
  Result :=  StringReplace(FloatToStr(Value),',','.',[rfReplaceAll])
end;

function TNumberObject.MethodCall(method: string; args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  if (method='format') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>STRING_OBJ) then
      Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
    else
      Result := TStringObject.Create(FormatFloat(TStringObject(args[0]).Value, Value));
  end
  else Result := MethodInline(ObjectType+'.'+method, env);

  if (Result=nil) then
    Result := inherited MethodCall(method, args, env);
end;

function TNumberObject.ObjectType: TEvalObjectType;
begin
  Result := NUMBER_OBJ;
end;

function TNumberObject.toInt: Integer;
begin
  Result := Trunc(Value);
end;

{ TBooleanObject }

function TBooleanObject.Clone: TEvalObject;
begin
  Result := TBooleanObject.Create(Value);
end;

constructor TBooleanObject.Create(AValue: Boolean);
begin
  inherited Create;
  Value := AValue;
end;

function TBooleanObject.Inspect: string;
begin
  if Value then Result := 'true' else Result := 'false';
end;

function TBooleanObject.ObjectType: TEvalObjectType;
begin
  Result := BOOLEAN_OBJ;
end;

{ TReturnValueObject }

constructor TReturnValueObject.Create;
begin
  inherited Create;
  Value := nil;
end;

function TReturnValueObject.Clone: TEvalObject;
begin
  Result := TReturnValueObject.Create(Value.Clone);
end;

constructor TReturnValueObject.Create(AValue: TEvalObject);
begin
  Create;
  Value := AValue.Clone;
end;

destructor TReturnValueObject.Destroy;
begin
  inherited;
end;

function TReturnValueObject.Inspect: string;
begin
  Result := Value.Inspect;
end;

function TReturnValueObject.ObjectType: TEvalObjectType;
begin
  Result := RETURN_VALUE_OBJ;
end;

{ TErrorObject }

function TErrorObject.Clone: TEvalObject;
begin
  Result := TErrorObject.Create(ErrMessage);
end;

constructor TErrorObject.Create(AErrMessage: string);
begin
  inherited Create;
  ErrMessage := AErrMessage;
end;

function TErrorObject.Inspect: string;
begin
  Result := 'ERROR: ' + ErrMessage;
end;

class function TErrorObject.newError(const AFormat: string; const Args: array of const): TErrorObject;
begin
  Result := TErrorObject.Create(Format(AFormat, Args));
end;

function TErrorObject.ObjectType: TEvalObjectType;
begin
  Result := ERROR_OBJ;
end;

{ TFunctionObject }

function TFunctionObject.Clone: TEvalObject;
begin
  Result := TFunctionObject.Create(Parameters, Body);
end;

constructor TFunctionObject.Create(AParameters: TList<TASTIdentifier>; ABody: TASTBlockStatement);
var
  i: Integer;
begin
  inherited Create;
  Parameters := TList<TASTIdentifier>.create;
  if Assigned(AParameters) then
    for i := 0 to AParameters.Count-1 do
      Parameters.Add(AParameters[i].Clone as TASTIdentifier);

  Body:= ABody.Clone as TASTBlockStatement;
end;

destructor TFunctionObject.Destroy;
var
  i: Integer;
begin
  for i := 0 to Parameters.Count-1 do
    Parameters[i].Free;
  Parameters.Free;
  Body.Free;
  inherited;
end;

function TFunctionObject.Inspect: string;
var
  i:Integer;
begin
  Result := 'function(';
  if (Parameters.Count>0) then
  begin
    for i := 0 to Parameters.Count-1 do
      Result := Result + Parameters[i].Value+',';

    Result := Copy(Result,1,Length(Result)-1);
  end;
  Result := Result+')';
end;

function TFunctionObject.ObjectType: TEvalObjectType;
begin
  Result := FUNCTION_OBJ;
end;

{ TEnvironment }

constructor TEnvironment.Create;
begin
  FStore := TDictionary<string,TEvalObject>.Create;
  FConst := TStringList.Create;
  FAllowedIdent:= TStringList.Create;
  FOuter := nil;
end;

constructor TEnvironment.Create(AOuter: TEnvironment);
begin
  Create;
  FOuter := AOuter;
end;

function TEnvironment.Clone: TEnvironment;
var
  key :string;
begin
  Result := TEnvironment.Create;

  for key in FStore.Keys do
    Result.Store.Add(key,FStore[key]);

  for key in FConst do
    Result.FConst.Add(key);

  for key in FAllowedIdent do
    Result.FAllowedIdent.Add(key);

  Result.Outer := Outer;
end;

constructor TEnvironment.Create(AOuter: TEnvironment; AllowedIdent: TStringList);
begin
  Create(AOuter);
  FAllowedIdent.Text := AllowedIdent.Text;
end;

destructor TEnvironment.Destroy;
//var
//  key :string;
begin
{
  for key in FStore.Keys do
    FStore[key].Free;  }

  FStore.Free;
  FConst.Free;
  FAllowedIdent.Free;
  inherited;
end;

function TEnvironment.GetValue(name: string; var value: TEvalObject): Boolean;
begin
  Result := Store.TryGetValue(name, value);
  if NOT Result and Assigned(Outer) then
    Result := Outer.GetValue(name, value);
end;

function TEnvironment.SetOrCreateValue(name: string; value: TEvalObject; IsConst:Boolean): TEvalObject;

  function isAllowed:Boolean;
  begin
    Result := (FAllowedIdent.Count = 0);
    if NOT Result then
      Result := FAllowedIdent.IndexOf(name)>=0;
  end;

begin
  Result := value;
  if FStore.ContainsKey(name) then
  begin
    if FConst.IndexOf(name)<0 then
      FStore[name] := Result
    else
      Result := TErrorObject.newError('identifier : %s is const, READONLY value',[name]);
  end
  else
  begin
    if isAllowed then
    begin
      FStore.Add(name, Result);
      if IsConst then
        FConst.Add(name);
    end
    else
    if Assigned(Outer) then
      Outer.SetOrCreateValue(name, value, IsConst);
  end;
end;

function TEnvironment.SetValue(name: string; value: TEvalObject): TEvalObject;
begin
  Result := value;
  if FStore.ContainsKey(name) then
  begin
    if FConst.IndexOf(name)<0 then
      FStore[name] := Result
    else
      Result := TErrorObject.newError('identifier : %s is const, not modifier value',[name]);
  end
  else
  if FOuter<>nil then
    Result := FOuter.SetValue(name, value)
  else
    Result := TErrorObject.newError('identifier not found: %s ',[name]);
end;

{ TNullObject }

function TNullObject.Clone: TEvalObject;
begin
  Result := TNullObject.Create;
end;

function TNullObject.Inspect: string;
begin
  Result := 'Null';
end;

function TNullObject.ObjectType: TEvalObjectType;
begin
  Result := NULL_OBJ;
end;

{ TStringObject }

function TStringObject.Clone: TEvalObject;
begin
  Result := TStringObject.Create(Value);
end;

constructor TStringObject.Create(AValue: string);
begin
  inherited Create;
  Value := AValue;
end;

function TStringObject.CurrentIndex: TEvalObject;
begin
  Result := TNumberObject.Create(IIndex+1);
end;

function TStringObject.GetIndex(Index: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  begin
    if ((TNumberObject(Index).toInt<=0) or (TNumberObject(Index).toInt>Length(Value))) then
      Result := TNullObject.Create
    else
      Result := TStringObject.Create(Value[TNumberObject(Index).toInt]);
  end;
end;

function TStringObject.Inspect: string;
begin
  Result := Value;
end;

function TStringObject.isIterable: Boolean;
begin
  Result := True;
end;

function TStringObject.MethodCall(method: string; args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  j: char;
  S,S1: string;
begin
  if (method='tonumber') then
  begin
    if (args.Count>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, max=1', [args.Count])
    else
    if ((args.Count>0) and (args[0].ObjectType<>NUMBER_OBJ)) then
      Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[0].ObjectType])
    else
      if args.Count=1 then
        Result := TNumberObject.Create(StrToFloatDef(Value, TNumberObject(args[0]).Value))
      else
        Result := TNumberObject.Create(StrToFloatDef(Value,0));
  end
  else
  if (method='trim') then
    Result := TStringObject.Create(Trim(Value))
  else
  if (method='rtrim') then
    Result := TStringObject.Create(TrimRight(Value))
  else
  if (method='ltrim') then
    Result := TStringObject.Create(TrimLeft(Value))
  else
  if (method='len') then
    Result := TNumberObject.Create(Length(Value))
  else
  if (method='copy') then
  begin
    if (args.Count<>2) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
    else
    if (args[0].ObjectType<>NUMBER_OBJ) then
      Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[1].ObjectType])
    else
    if (args[1].ObjectType<>NUMBER_OBJ) then
      Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[2].ObjectType])
    else
      Result := TStringObject.Create(Copy(Value,TNumberObject(args[0]).toInt,TNumberObject(args[1]).toInt));
  end
  else
  if ((method='left') or (method='right')) then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>NUMBER_OBJ) then
      Result := TErrorObject.newError('argument must be NUMBER, got %s', [args[0].ObjectType])
    else
    begin
      if (method='left') then
        Result := TStringObject.Create(LeftStr(Value, TNumberObject(args[0]).toInt))
      else
        Result := TStringObject.Create(RightStr(Value, TNumberObject(args[0]).toInt))
    end;
  end
  else
  if (method='split') then
  begin
    j:=',';
    if (args.Count>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, max=2', [args.Count])
    else
    if ((args.Count=1) and (args[0].ObjectType<>STRING_OBJ)) then
      Result := TErrorObject.newError('argument to `first` must be STRING, got %s', [args[0].ObjectType])
    else
    begin
      if ((args.Count=1) and (TStringObject(args[0]).Value<>'')) then
        j:= TStringObject(args[0]).Value[1];

      Result := TArrayObject.Create;
      TArrayObject(Result).Elements := TList<TEvalObject>.Create;

      S:= Value;
      While (S<>'') do
      begin
        S1:= StrSplit(S,j);
        TArrayObject(Result).Elements.Add(TStringObject.Create(S1));
      end;
    end;
  end
  else Result := MethodInline(ObjectType+'.'+method, env);

  if (Result=nil) then
    Result := inherited MethodCall(method, args, env);
end;

function TStringObject.Next: TEvalObject;
begin
  Result := nil;
  if IIndex<length(Value) then
  begin
    Result := TStringObject.Create(Value[IIndex+1]);
    Inc(IIndex);
  end;

end;

function TStringObject.ObjectType: TEvalObjectType;
begin
  Result := STRING_OBJ;
end;

function TStringObject.Setindex(Index, value: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  if value.ObjectType<>STRING_OBJ then
    Result := TErrorObject.newError('value type <%s> not supported: %s', [value.ObjectType, ObjectType])
  else
  begin
    if ((TNumberObject(Index).toInt<=0) or (TNumberObject(Index).toInt>length(Self.value))) then
      Exit( TErrorObject.newError('unusable index for string: %s', [TNumberObject(Index).Inspect]))
    else
    if (TStringObject(value).Value='') then
      Exit( TErrorObject.newError('unusable value for string: %s', [TStringObject(value).Inspect]))
    else
    begin
      Self.value[TNumberObject(Index).toInt] := TStringObject(value).Value[1];
      Result := Self;
    end;
  end;
end;

{ TBuiltinObject }

function TBuiltinObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TBuiltinObject.Create(ABuiltinFunction: TBuiltinFunction);
begin
  inherited Create;
  BuiltinFunction := ABuiltinFunction;
end;

function TBuiltinObject.Inspect: string;
begin
  Result := 'builtin function()'
end;

function TBuiltinObject.ObjectType: TEvalObjectType;
begin
  Result := BUILTIN_OBJ;
end;


{ TArrayObject }

function TArrayObject.Clone: TEvalObject;
var
  i: Integer;
begin
  Result := TArrayObject.Create;
  TArrayObject(Result).Elements := TList<TEvalObject>.Create;
  for i := 0 to Elements.Count -1 do
    TArrayObject(Result).Elements.Add(Elements[i].Clone);
end;

constructor TArrayObject.Create;
begin
  inherited Create;
end;

constructor TArrayObject.CreateWithElements;
begin
  Create;
  Elements := TList<TEvalObject>.Create;
end;

function TArrayObject.CurrentIndex: TEvalObject;
begin
  Result := TNumberObject.Create(IIndex-1);
end;

destructor TArrayObject.Destroy;
begin
  Elements.Clear;
  FreeAndNil(Elements);
  inherited;
end;

function TArrayObject.GetIndex(Index: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  begin
    if ((TNumberObject(Index).toInt<0) or (TNumberObject(Index).toInt>Elements.Count - 1)) then
      Result := TNullObject.Create
    else
      Result := Elements[TNumberObject(Index).toInt];
  end;
end;

function TArrayObject.Inspect: string;
var
  i: Integer;
begin
  Result := 'Array [';
  if (Elements<>nil) then
    if (Elements.Count>0) then
    begin
      for i := 0 to Elements.Count-1 do
        Result := Result + QuoteIfString(Elements[i])+ ',';

      Result:= Copy(Result,1,length(Result)-1);
    end;

  Result := Result + ']';
end;

function TArrayObject.isIterable: Boolean;
begin
  Result := True;
end;

function TArrayObject.MethodCall(method: string; args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  j,S: string;
  i: Integer;
begin
  if (method='size') then
    Result := TNumberObject.Create(Elements.Count)
  else
  if (method='first') then
  begin
    if (args.Count>0) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=0', [args.Count])
    else
    if Elements.Count>0 then
      Result := Elements[0].Clone
    else
      Result := TNullObject.Create;
  end
  else
  if (method='last') then
  begin
    if (args.Count>0) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=0', [args.Count])
    else
    if Elements.Count>0 then
      Result := Elements[Elements.Count-1].Clone
    else
      Result := TNullObject.Create;
  end
  else
  if (method='rest') then
  begin
    if (args.Count>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=0', [args.Count])
    else
    if (Elements.Count>0) then
    begin
      Result := TArrayObject.Create;
      TArrayObject(Result).Elements := TList<TEvalObject>.Create;
      for i := 1 to Elements.Count-1 do
        TArrayObject(Result).Elements.Add(Elements[i].Clone);
    end
    else Result := TNullObject.Create;
  end
  else
  if (method='push') then
  begin
    if (args.Count>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    begin
      Result := Clone;
      TArrayObject(Result).Elements.Add(args[0].Clone);
    end;
  end
  else
  if (method='insert') then
  begin
    if (args.Count<>2) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=2', [args.Count])
    else
    if args[1].ObjectType<>NUMBER_OBJ then
      Result := TErrorObject.newError('argument to `second` must be NUMBER (index of array), got %s', [args[1].ObjectType])
    else
    begin
      i := TNumberObject(args[1]).toInt;
      if ((i<0) or (i>Elements.Count)) then
        Result:= TErrorObject.newError('index out of range, 0 to %d', [Elements.Count])
      else
      begin
        Result := Clone;
        TArrayObject(Result).Elements.Insert(i, args[0].Clone);
      end;
    end;
  end
  else
  if (method='join') then
  begin
    j:=',';
    if (args.Count>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, max=1', [args.Count])
    else
    if (args.Count=1) and (args[0].ObjectType<>STRING_OBJ) then
      Result := TErrorObject.newError('argument to `second` must be STRING, got %s', [args[0].ObjectType])
    else
    begin
      if (args.Count=1) then
        j:= TStringObject(args[0]).Value;
      if Elements.Count>0 then
      begin
        S:='';
        for i := 0 to Elements.Count-1 do
          S:=S+Elements[i].Inspect+j;
        S:= Copy(S,1,Length(S)-1);
        Result := TStringObject.Create(S);
      end
      else Result := TNullObject.Create;
    end;
  end
  else
  if (method='delete') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if args[0].ObjectType<>NUMBER_OBJ then
      Result := TErrorObject.newError('invalid index type for ARRAY , got %s', [args[0].ObjectType])
    else
    begin
      Result := TArrayObject.Create;
      TArrayObject(Result).Elements := TList<TEvalObject>.Create;
      for i := 0 to Elements.Count-1 do
        if (i<>TNumberObject(args[0]).toInt) then
          TArrayObject(Result).Elements.Add(Elements[i].Clone);
    end;
  end
  else Result := MethodInline(ObjectType+'.'+method, env);

  if (Result=nil) then
    Result := inherited MethodCall(method, args, env);
end;

function TArrayObject.Next: TEvalObject;
begin
  Result := nil;

  if Assigned(Elements) then
    if (IIndex<Elements.Count) then
    begin
      Result := Elements[IIndex];
      Inc(IIndex);
    end;
end;

function TArrayObject.ObjectType: TEvalObjectType;
begin
  Result := ARRAY_OBJ;
end;

function TArrayObject.Setindex(Index, value: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  begin
    if ((TNumberObject(Index).toInt<0) or (TNumberObject(Index).toInt>Elements.Count - 1)) then
      Exit( TErrorObject.newError('unusable index for array: %s', [TNumberObject(Index).Inspect]))
    else
    begin
      Elements[TNumberObject(Index).toInt] := Value;
      Result := Self;
    end;
  end;
end;

{ THashkey }

constructor THashkey.Create(Ref: TEvalObject);
begin
  FValueStr := '';
  FValueNum := -1;
  FValueBool:= false;

  ObjectType:= Ref.ObjectType;
  if (ObjectType=NUMBER_OBJ) then
    FValueNum := TNumberObject(Ref).Value
  else
  if (ObjectType=STRING_OBJ) then
    FValueStr := TStringObject(Ref).Value
  else
  if (ObjectType=BOOLEAN_OBJ) then
    FValueBool := TBooleanObject(Ref).Value
  else
    ObjectType := ERROR_OBJ;
end;

constructor THashkey.Create(Ref: TEvalObjectType);
begin
  ObjectType:= Ref;
end;

function THashkey.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj is THashkey) and (THashkey(Obj).ObjectType=ObjectType);

  if Result then
    if (ObjectType=NUMBER_OBJ) then
      Result := FValueNum = THashkey(Obj).FValueNum
    else
    if (ObjectType=STRING_OBJ) then
      Result := FValueStr = THashkey(Obj).FValueStr
    else
    if (ObjectType=BOOLEAN_OBJ) then
      Result := FValueBool = THashkey(Obj).FValueBool;
end;

class function THashkey.fromObject(Obj: TEvalObject): THashkey;
begin
  Result := THashkey.Create(Obj);
end;

function THashkey.GetHashCode: Integer;
begin
  Result := 1;
end;

{ THashPair }

constructor THashPair.Create(AKey, AValue: TEvalObject);
begin
  Key := AKey;
  Value := AValue;
end;

destructor THashPair.Destroy;
begin
//  FreeAndNil(Key);
//  FreeAndNil(Value);
  inherited;
end;

{ THashObject }

function THashObject.Clone: TEvalObject;
var
  key:THashkey;
  k,v:TEvalObject;
begin
  Result := THashObject.Create;
  THashObject(Result).Pairs := TDictionary<THashkey,THashPair>.create;
  for key in Pairs.Keys do
  begin
    k := Pairs[key].Key.Clone;
    v := Pairs[key].Value.Clone;
    THashObject(Result).Pairs.Add(THashkey.fromObject(k), THashPair.Create(k,v));
  end;
end;

function THashObject.CurrentIndex: TEvalObject;
begin
  Result := Pairs.ToArray[IIndex-1].Value.Key;
end;

destructor THashObject.Destroy;
var
  hkey: THashkey;
begin
  for hkey in Pairs.Keys do
  begin
    Pairs[hkey].Free;
    hkey.Free;
  end;
  Pairs.Clear;
  FreeAndNil(Pairs);
  inherited;
end;

function THashObject.GetIdentifer(name: string; Index: TEvalObject): TEvalObject;
var
  HK:THashkey;
begin
  HK := THashkey.Create(STRING_OBJ);
  HK.FValueStr := name;

  if Pairs.ContainsKey(HK) then
    Result := Pairs[HK].Value
  else
    Result := TNullObject.Create;

  FreeAndNil(HK);
end;

function THashObject.GetIndex(Index: TEvalObject): TEvalObject;
var
  HK:THashkey;
begin
  HK := THashkey.fromObject(Index);
  if HK.ObjectType=ERROR_OBJ then
    Exit(TErrorObject.newError('unusable as hash key: %s', [Index.ObjectType]));

  if Pairs.ContainsKey(HK) then
    Result := Pairs[HK].Value
  else
    Result := TNullObject.Create;

  FreeAndNil(HK);
end;

function THashObject.Inspect: string;
var
  key: THashkey;
  pair: THashPair;
begin
  Result := 'HashMap {';
  if Pairs.Count>0 then
  begin
    for key in Pairs.Keys do
    begin
      pair := Pairs[key];
      Result := Result + QuoteIfString(pair.Key) + ':' + QuoteIfString(pair.Value) + ',';
    end;
    Result := Copy(Result,1,length(Result)-1);
  end;
  Result := Result + '}';
end;

function THashObject.isIterable: Boolean;
begin
  Result := True;
end;

function THashObject.MethodCall(method: string; args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  hkey: THashkey;
begin
  if method='size' then
    Result := TNumberObject.Create(Pairs.Count)
  else
  if method='keys' then
  begin
    if (args.Count<>0) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=0', [args.Count])
    else
    begin
      Result := TArrayObject.Create;
      TArrayObject(Result).Elements := TList<TEvalObject>.Create;
      for hkey in Pairs.Keys do
        if (hkey.ObjectType=STRING_OBJ) then
          TArrayObject(Result).Elements.Add(TStringObject.Create(hkey.FValueStr))
        else
        if (hkey.ObjectType=NUMBER_OBJ) then
          TArrayObject(Result).Elements.Add(TNumberObject.Create(hkey.FValueNum))
        else
        if (hkey.ObjectType=BOOLEAN_OBJ) then
          TArrayObject(Result).Elements.Add(TBooleanObject.Create(hkey.FValueBool));
    end;
  end
  else
  if method='delete' then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    begin
      hkey := THashkey.fromObject(args[0]);
      try
        if hkey.ObjectType=ERROR_OBJ then
          Result := TErrorObject.newError('invalid index type for HashMap , got %s', [args[0].ObjectType])
        else
        begin
          Result := Clone as THashObject;
          if THashObject(Result).Pairs.ContainsKey(hkey) then
          begin
            THashObject(Result).Pairs[hkey].Free;
            THashObject(Result).Pairs.Remove(hkey);
          end;
        end;
      finally
        hkey.Free;
      end;
    end;
  end
  else Result := MethodInline(ObjectType+'.'+method, env);

  if (Result=nil) then
    Result := inherited MethodCall(method, args, env);
end;

function THashObject.Next: TEvalObject;
begin
  Result := nil;

  if Assigned(Pairs) then
    if (IIndex<Pairs.Count) then
    begin
      Result := Pairs.ToArray[IIndex].Value.Value;
      Inc(IIndex);
    end;

end;

function THashObject.ObjectType: TEvalObjectType;
begin
  Result := HASH_OBJ;
end;

function THashObject.SetIdentifer(name: string; value,  Index: TEvalObject): TEvalObject;
var
  HK:THashkey;
begin
  HK := THashkey.Create(STRING_OBJ);
  HK.FValueStr := name;

  if Pairs.ContainsKey(HK) then
  begin
    Pairs[HK].Value := Value;
    FreeAndNil(HK);
  end
  else Pairs.Add(HK,THashPair.Create(TStringObject.Create(name), Value));

  Result := Self;
end;

function THashObject.Setindex(Index, value: TEvalObject): TEvalObject;
var
  HK:THashkey;
begin
  HK := THashkey.fromObject(Index);
  if HK.ObjectType=ERROR_OBJ then
  begin
    FreeAndNil(HK);
    Exit( TErrorObject.newError('unusable as hash key: %s', [Index.ObjectType]));
  end;

  if Pairs.ContainsKey(HK) then
  begin
    Pairs[HK].Value := Value;
    FreeAndNil(HK);
  end
  else Pairs.Add(HK,THashPair.Create(Index, Value));

  Result := Self;
end;

{ TLoopObject }

function TLoopObject.Clone: TEvalObject;
begin
  Result := TLoopObject.Create(LoopType);
end;

constructor TLoopObject.Create(ALoopType: string);
begin
  LoopType := ALoopType;
end;

function TLoopObject.Inspect: string;
begin
  Result := 'Loop Object ' + LoopType;
end;

function TLoopObject.ObjectType: TEvalObjectType;
begin
  Result := LOOP_OBJ;
end;

{ TClosureObject }

function TClosureObject.Clone: TEvalObject;
begin
  Result := TClosureObject.Create(Funct.Clone as TFunctionObject, Env);
end;

constructor TClosureObject.Create(AFunct: TFunctionObject; AEnv: TEnvironment);
begin
  inherited Create;
  Funct:=AFunct;
  Env := AEnv.Clone;
end;

destructor TClosureObject.Destroy;
begin
  if Assigned(Env) then Env.Free;
  inherited;
end;

function TClosureObject.Inspect: string;
begin
  Result := '<closure> ' + Funct.Inspect;
end;

function TClosureObject.ObjectType: TEvalObjectType;
begin
  Result := CLOSURE_OBJ;
end;

end.
