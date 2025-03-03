unit lp.environment;

interface

uses classes, SysUtils, Generics.Collections, Variants
  ,lp.parser;


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

type
  TEvalObject = class;
  TBuiltinFunction = function(args: TList<TEvalObject>): TEvalObject;

  TEnvironment = class
    FStore: TDictionary<string,TEvalObject>;
    FOuter: TEnvironment;
  public
    constructor Create; overload;
    constructor Create(AOuter:TEnvironment); overload;
    destructor Destroy; override;
    property Store: TDictionary<string,TEvalObject> read FStore;
    property Outer: TEnvironment read FOuter write FOuter;
    function GetValue(name:string; var value:TEvalObject ):Boolean;
    function SetValue(name:string; value:TEvalObject ):TEvalObject;
  end;

  // ---- Object --------------------

  TEvalObjectType = string;

  TEvalObject = class
  public
    function ObjetcType:TEvalObjectType; virtual;
    function Inspect:string; virtual;
    function Clone:TEvalObject; virtual;
  end;

  TNumberObject = class(TEvalObject)
  public
    Value: Double;
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AValue: Double);
  end;

  TBooleanObject = class(TEvalObject)
  public
    Value: Boolean;
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AValue: Boolean);
  end;

  TStringObject = class(TEvalObject)
  public
    Value: string;
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AValue: string);
  end;

  TNullObject = class(TEvalObject)
  public
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  end;

  TReturnValueObject = class(TEvalObject)
  public
    Value: TEvalObject;
    function ObjetcType:TEvalObjectType; override;
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
    function ObjetcType:TEvalObjectType; override;
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
	  Env: TEnvironment;
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(AParameters: TList<TASTIdentifier>; ABody: TASTBlockStatement; AEnv: TEnvironment);
    destructor Destroy; override;
  end;

  TBuiltinObject = class(TEvalObject)
  public
    BuiltinFunction: TBuiltinFunction;
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create(ABuiltinFunction: TBuiltinFunction);
  end;

  TArrayObject = class(TEvalObject)
  public
    Elements: TList<TEvalObject>;
    function ObjetcType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    destructor Destroy; override;
  end;

implementation

{ TEvalObject }

function TEvalObject.Clone: TEvalObject;
begin
  Result := nil; { virtual }
end;

function TEvalObject.Inspect: string;
begin
  Result := '';
end;

function TEvalObject.ObjetcType: TEvalObjectType;
begin
  Result := '';
end;

{ TNumberObject }

function TNumberObject.Clone: TEvalObject;
begin
  Result := TNumberObject.Create(Value);
end;

constructor TNumberObject.Create(AValue: Double);
begin
  Value := AValue;
end;

function TNumberObject.Inspect: string;
begin
  Result := FloatToStr(Value)
end;

function TNumberObject.ObjetcType: TEvalObjectType;
begin
  Result := NUMBER_OBJ;
end;

{ TBooleanObject }

function TBooleanObject.Clone: TEvalObject;
begin
  Result := TBooleanObject.Create(Value);
end;

constructor TBooleanObject.Create(AValue: Boolean);
begin
  Value := AValue;
end;

function TBooleanObject.Inspect: string;
begin
  if Value then Result := 'True' else Result := 'False';
end;

function TBooleanObject.ObjetcType: TEvalObjectType;
begin
  Result := BOOLEAN_OBJ;
end;

{ TReturnValueObject }

constructor TReturnValueObject.Create;
begin
  Value := nil;
end;

function TReturnValueObject.Clone: TEvalObject;
begin
  Result := TReturnValueObject.Create(Value.Clone);
end;

constructor TReturnValueObject.Create(AValue: TEvalObject);
begin
  Create;
  if AValue is TNumberObject  then Value := TNumberObject.Create(TNumberObject(AValue).Value)
  else
  if AValue is TBooleanObject then Value := TBooleanObject.Create(TBooleanObject(AValue).Value)
  else
  if AValue is TStringObject then Value := TStringObject.Create(TStringObject(AValue).Value)
  else
  if AValue is TNumberObject then Value := TNullObject.Create
  else
  if AValue is TErrorObject then Value := TErrorObject.Create(TErrorObject(AValue).ErrMessage)
  else
  if AValue is TFunctionObject then
    Value := TFunctionObject.Create(TFunctionObject(AValue).Parameters
      ,TFunctionObject(AValue).Body
      ,TFunctionObject(AValue).Env);
end;

destructor TReturnValueObject.Destroy;
begin
{  if (Value<>nil) then
    FreeAndNil(Value);}
  inherited;
end;

function TReturnValueObject.Inspect: string;
begin
  Result := Value.Inspect;
end;

function TReturnValueObject.ObjetcType: TEvalObjectType;
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

function TErrorObject.ObjetcType: TEvalObjectType;
begin
  Result := ERROR_OBJ;
end;

{ TFunctionObject }

function TFunctionObject.Clone: TEvalObject;
begin
  Result := TFunctionObject.Create(Parameters, Body, Env);
end;

constructor TFunctionObject.Create(AParameters: TList<TASTIdentifier>;
  ABody: TASTBlockStatement; AEnv: TEnvironment);
var
  i: Integer;
begin
  Parameters := TList<TASTIdentifier>.create;
  for i := 0 to AParameters.Count-1 do
    Parameters.Add(AParameters[i].Clone as TASTIdentifier);

  Body := ABody.Clone as TASTBlockStatement;
  Env := AEnv;
end;

destructor TFunctionObject.Destroy;
begin

  inherited;
end;

function TFunctionObject.Inspect: string;
begin
  Result := '';
end;

function TFunctionObject.ObjetcType: TEvalObjectType;
begin
  Result := FUNCTION_OBJ;
end;

{ TEnvironment }

constructor TEnvironment.Create;
begin
  FStore := TDictionary<string,TEvalObject>.Create;
  FOuter := nil;
end;

constructor TEnvironment.Create(AOuter: TEnvironment);
begin
  Create;
  FOuter := AOuter;
end;

destructor TEnvironment.Destroy;
var
  key :string;
begin
  for key in FStore.Keys do
    FStore[key].Free;
  FStore.Free;
  if (FOuter<>nil) then FOuter.Free;
  inherited;
end;

function TEnvironment.GetValue(name: string; var value: TEvalObject): Boolean;
begin
  Result := FStore.TryGetValue(name, value);
  if NOT Result and Assigned(Outer) then
    Result := Outer.GetValue(name, value);
end;

function TEnvironment.SetValue(name: string; value: TEvalObject): TEvalObject;
begin
  Result := value;
  if FStore.ContainsKey(name) then
    FStore[name] := Result
  else
    FStore.Add(name, Result);
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

function TNullObject.ObjetcType: TEvalObjectType;
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
  Value := AValue;
end;

function TStringObject.Inspect: string;
begin
  Result := Value;
end;

function TStringObject.ObjetcType: TEvalObjectType;
begin
  Result := STRING_OBJ;
end;

{ TBuiltinObject }

function TBuiltinObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TBuiltinObject.Create(ABuiltinFunction: TBuiltinFunction);
begin
  BuiltinFunction := ABuiltinFunction;
end;

function TBuiltinObject.Inspect: string;
begin
  Result := 'builtin function'
end;

function TBuiltinObject.ObjetcType: TEvalObjectType;
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

destructor TArrayObject.Destroy;
begin

  inherited;
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
        Result := Result + Elements[i].Inspect+ ',';

      Result:= Copy(Result,1,length(Result)-1);
    end;

  Result := Result + ']';
end;

function TArrayObject.ObjetcType: TEvalObjectType;
begin
  Result := ARRAY_OBJ;
end;

end.
