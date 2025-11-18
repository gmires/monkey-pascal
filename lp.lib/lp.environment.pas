unit lp.environment;

interface

{$I lp.inc}

uses classes, SysUtils, Generics.Collections, Variants, StrUtils, DateUtils
  ,System.NetEncoding, Math
  ,{$IFDEF LPI_D28} JSON {$ELSE} DBXJSON {$ENDIF}
  ,lp.parser
  ,lp.utils
  ,lp.base64;


const
  ALL_OBJ          = 'OBJECT';
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
	DATETIME_OBJ     = 'DATETIME';

const
  LOOP_TYPE_BREAK  = 'break';
  LOOP_TYPE_CONTINUE  = 'continue';

type
  TEvalObject = class;
  TEnvironment = class;
  TBuiltinFunction = function(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;

  TEnvironment = class
    FStore: TDictionary<string,TEvalObject>;
    FConst: TStringList;
    FOuter: TEnvironment;
    FAllowedIdent: TStringList;
    FReturnRef: TList<TEvalObject>;
  public
    constructor Create; overload;
    constructor Create(AOuter:TEnvironment); overload;
    constructor Create(AOuter:TEnvironment; AllowedIdent:TStringList); overload;
    destructor Destroy; override;
    property Store: TDictionary<string,TEvalObject> read FStore;
    property ReturnRef: TList<TEvalObject> read FReturnRef write FReturnRef;
    property Outer: TEnvironment read FOuter write FOuter;
    function GetValue(name:string; var value:TEvalObject ):Boolean;
    function SetValue(name:string; value:TEvalObject ):TEvalObject;
    function SetOrCreateValue(name:string; value:TEvalObject; IsConst:Boolean ):TEvalObject;
    function GetFunctionObject(AObjectType:string):TDictionary<string, TEvalObject>;

    function AddRef(AObject:TEvalObject):TEvalObject;

    function Clone:TEnvironment;
  end;

  // ---- Object --------------------

  TEvalObjectType = string;

  TMethodFunct = function(args: TList<TEvalObject>; env: TEnvironment):TEvalObject of object;
  TMethodDescr = class(TObject)
  public
    ArgMin: Integer;
    ArgMax: Integer;
    ArgType: TArray<string>;
    Funct: TMethodFunct;
    ShortDescription:string;
  public
    constructor Create(AArgMin: Integer; AArgMax: Integer; AArgType: TArray<string>; AFunct: TMethodFunct; AShortDescription:string='');
  end;

  TIdentifierGetFunct = function(Index:TEvalObject=nil):TEvalObject of object;
  TIdentifierSetFunct = function(Index:TEvalObject; value:TEvalObject):TEvalObject of object;

  TIdentifierDescr = class(TObject)
  public
    IType: TArray<string>;
    Getter: TIdentifierGetFunct;
    Setter: TIdentifierSetFunct;
    IIndexType: TArray<string>;
    ShortDescription:string;
  private
    function GetIsReadOnly: Boolean;
  public
    constructor Create(AIType: TArray<string>; AIIndexType: TArray<string>; AGetter: TIdentifierGetFunct; ASetter: TIdentifierSetFunct=nil; AShortDescription:string='');
    property isReadOnly: Boolean read GetIsReadOnly;
  end;

  TEvalObject = class
  public
    IIndex: Integer;
    function ObjectType:TEvalObjectType; virtual;
    function Inspect:string; virtual;
    function Clone:TEvalObject; virtual;
    function Reference:TEvalObject; virtual;

    { -- support for object method call -- }
    function  MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; virtual;
    function  MethodInline(method:string; env: TEnvironment):TEvalObject; virtual;

    { -- support for object with index (es. array or map Array[index], map[index]) -- }
    function GetIndex(Index:TEvalObject):TEvalObject; virtual;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; virtual;

    { -- support for object with property identifier (es. object.property := value) -- }
    function GetIdentifer(name:string; Index:TEvalObject=nil):TEvalObject; virtual;
    function SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject=nil):TEvalObject; virtual;

    { -- support for interable value, index object element (es. foreach v,i in object ) -- }
    function  isIterable:Boolean; virtual;
    function  Next:TEvalObject; virtual;
    function  CurrentIndex:TEvalObject; virtual;
    procedure Reset; virtual;

    procedure IncRefCount;
    procedure DecRefCount;

    function toJSON:TJSONValue; virtual;
    function toJSONString:TEvalObject; virtual;
  protected
    Methods: TObjectDictionary<string, TMethodDescr>;
    function  m_tostring(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_type(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_clone(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_ref(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_help(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_toJSON(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; virtual;
  protected
    Identifiers: TObjectDictionary<string, TIdentifierDescr>;
    procedure IdentifierInit; virtual;
  public
    { ** for GC (mark and sweep) ** }
    GcNext:TEvalObject;
    GcRefCount:Integer;
    GcMark:Boolean;
    GcGarbage:Integer;
    GcManualFree:Boolean;

    procedure MarkChild; virtual;

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
    function Reference:TEvalObject; override;
  protected
    function toJSON:TJSONValue; override;
  protected
    function  m_format(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_round(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_trunc(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_round_to(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  public
    constructor Create(AValue: Double);
    destructor Destroy; override;
    function toInt:Integer;
  end;

  TBooleanObject = class(TEvalObject)
  public
    Value: Boolean;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
    function Reference:TEvalObject; override;
  protected
    function toJSON:TJSONValue; override;
  public
    constructor Create(AValue: Boolean);
    constructor CreateTrue;
    constructor CreateFalse;
    destructor Destroy; override;
  end;

  TStringObject = class(TEvalObject)
  public
    Value: string;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
    function Reference:TEvalObject; override;

    function GetIndex(Index:TEvalObject):TEvalObject; override;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
    function CurrentIndex:TEvalObject; override;
  protected
    function toJSON:TJSONValue; override;
  protected
    function  m_tonumber(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_trim(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_rtrim(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_ltrim(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_len(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_copy(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_left(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_right(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_split(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_split_at_len(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_lower(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_upper(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_contains(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_startwith(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_endwith(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_empty(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_to_base64(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_from_base64(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    function  m_url_encode(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_url_decode(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    function  m_html_encode(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_html_decode(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    function  m_concat(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_index_of(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_pad_start(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_pad_end(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_repeat(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_replace(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_to_char_code(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_char_code_at(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    procedure MethodInit; override;
  public
    constructor Create(AValue: string);
    class function CreateFormat(const AFormat: string; const Args: Array of const):TStringObject;
    destructor Destroy; override;
  end;

  TNullObject = class(TEvalObject)
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
    function Reference:TEvalObject; override;
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

    function GetIndex(Index:TEvalObject):TEvalObject; override;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
    function CurrentIndex:TEvalObject; override;
  protected
    function toJSON:TJSONValue; override;
  protected
    function  m_size(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_first(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_last(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_rest(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_push(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_insert(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_join(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_delete(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
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

    function GetObject(name:string):TEvalObject; //** utility function for return real object (not copy) in integrations **//
    function ChkObject(name:string; OType:TEvalObjectType):Boolean; overload;//** utility function return if exists and type specified  **//
    function ChkObject(name:string; OType:TEvalObjectType; var OObject:TEvalObject):Boolean; overload;//** utility function return if exists and type specified  **//
    function GetIdentifer(name:string; Index:TEvalObject=nil):TEvalObject; override;
    function SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject=nil):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
    function CurrentIndex:TEvalObject; override;
  protected
    function toJSON:TJSONValue; override;
  protected
    function  m_size(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_keys(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_delete(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDateTimeObject = class(TEvalObject)
  public
    Value: TDateTime;
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
    function Reference:TEvalObject; override;
  protected
    function toJSON:TJSONValue; override;
  protected
    function  i_get_date(Index:TEvalObject=nil):TEvalObject;
    function  i_get_year(Index:TEvalObject=nil):TEvalObject;
    function  i_get_month(Index:TEvalObject=nil):TEvalObject;
    function  i_get_day(Index:TEvalObject=nil):TEvalObject;
    function  i_get_hour(Index:TEvalObject=nil):TEvalObject;
    function  i_get_minute(Index:TEvalObject=nil):TEvalObject;
    function  i_get_second(Index:TEvalObject=nil):TEvalObject;
    function  i_get_millisecond(Index:TEvalObject=nil):TEvalObject;
    procedure IdentifierInit; override;
  protected
    function  m_valid(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_encode(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_decode(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_format(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    function  m_years_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_months_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_weeks_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_days_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_hours_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_minutes_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_seconds_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_milliseconds_between(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    function  m_years_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_months_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_weeks_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_days_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_hours_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_minutes_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_seconds_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_milliseconds_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;

    procedure MethodInit; override;
  public
    constructor Create; overload;
    constructor Create(AValue:TDateTime); overload;
    destructor Destroy; override;
  end;

function  ArgumentValidate(args: TList<TEvalObject>; ArgMin, ArgMax: Integer; ArgType: array of string):TEvalObject;
procedure EvalObjectSetManualFree(Value:TEvalObject);
procedure EvalObjectFree(Value:TEvalObject);

implementation

function QuoteIfString(value:TEvalObject):string;
begin
  if value.ObjectType=STRING_OBJ then
    Result := '"' + value.Inspect + '"'
  else
    Result := value.Inspect;
end;

function ArgumentValidate(args: TList<TEvalObject>; ArgMin, ArgMax: Integer; ArgType: array of string):TEvalObject;
var
  i:Integer;
begin
  Result := nil;
  if (args.Count<ArgMin) or (args.Count>ArgMax) then
    Result := TErrorObject.newError('wrong number of arguments. got=%d, min=%d, max=%d', [args.Count, ArgMin, ArgMax])
  else
  begin
    for i := 0 to args.Count-1 do
      if ((ArgType[i]<>ALL_OBJ) and (args[i].ObjectType<>ArgType[i])) then
      begin
        Result := TErrorObject.newError('argument must be %s, got %s', [ArgType[i], args[i].ObjectType]);
        Break;
      end;
  end;
end;

procedure EvalObjectSetManualFree(Value:TEvalObject);
var
  H:THashkey;
  C:TEvalObject;
begin
  Value.GcManualFree := True;
  if Value is TArrayObject then
  begin
    for c in TArrayObject(Value).Elements do
      EvalObjectSetManualFree(C);
  end
  else
  if Value is THashObject then
  begin
    for H in THashObject(Value).Pairs.Keys do
    begin
      EvalObjectSetManualFree(THashObject(Value).Pairs[H].Key);
      EvalObjectSetManualFree(THashObject(Value).Pairs[H].Value);
    end;
  end;
end;

procedure EvalObjectFree(Value:TEvalObject);
var
  H:THashkey;
  C:TEvalObject;
begin
  if Value is TArrayObject then
  begin
    for c in TArrayObject(Value).Elements do
      EvalObjectFree(C);
  end
  else
  if Value is THashObject then
  begin
    for H in THashObject(Value).Pairs.Keys do
    begin
      EvalObjectFree(THashObject(Value).Pairs[H].Key);
      EvalObjectFree(THashObject(Value).Pairs[H].Value);
    end;
  end;
  Value.Free;
end;


{ TEvalObject }

procedure TEvalObject.IdentifierInit;
begin

end;

procedure TEvalObject.IncRefCount;
begin
  Inc(GcRefCount);
end;

function TEvalObject.Clone: TEvalObject;
begin
  Result := nil; { virtual }
end;

constructor TEvalObject.Create;
begin
  // -- garbage collector -- //
  GcNext := nil;
  GcRefCount := 0;
  GcManualFree := False;
  GcMark := True;
  GcGarbage:= 0;
  // -- garbage collector -- //

  Reset;
  Methods := TObjectDictionary<string, TMethodDescr>.Create([doOwnsValues]);
  MethodInit;
  Identifiers:= TObjectDictionary<string, TIdentifierDescr>.Create([doOwnsValues]);
  IdentifierInit;
end;

function TEvalObject.CurrentIndex: TEvalObject;
begin
  Result := nil;
end;

procedure TEvalObject.DecRefCount;
begin
  Dec(GcRefCount);
  if GcRefCount<0 then
    GcRefCount:=0;
end;

destructor TEvalObject.Destroy;
var
  current:string;
begin

//  for current in Methods.Keys do
//    Methods[current].Free;
//    Methods.ExtractPair(current).Value.Free;
  Methods.Free;

//  for current in Identifiers.Keys do
//    Identifiers[current].Free;
//    Identifiers.ExtractPair(current).Value.Free;
  Identifiers.Free;

  inherited;
end;

function TEvalObject.GetIdentifer(name: string; Index:TEvalObject): TEvalObject;

  function CheckIndexType(i_descr: TIdentifierDescr):TEvalObject;
  var
    i:Integer;
  begin
    Result := nil;
    if Assigned(Index) then
    begin
      if (Length(i_descr.IIndexType)=0) then
        Result := TErrorObject.newError('index not supported in identifier "'+name+'" in object type %s',[ObjectType])
      else
      begin
        for i := Low(i_descr.IIndexType) to High(i_descr.IIndexType) do
          if i_descr.IIndexType[i]=Index.ObjectType then
            Exit;

        Result := TErrorObject.newError('index Type %s not supported in identifier "'+name+'"',[Index.ObjectType])
      end;
    end;
  end;

begin
  if Identifiers.ContainsKey(name) then
  begin
    Result:= CheckIndexType(Identifiers[name]);
    if (Result=nil) then
      Result :=Identifiers[name].Getter(Index)
  end
  else Result := TErrorObject.newError('identifier "'+name+'" not supported in object type %s',[ObjectType]);
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

procedure TEvalObject.MarkChild;
begin
  { -- virtual method -- }
end;

function TEvalObject.MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  M:TMethodDescr;
begin
  if Methods.ContainsKey(method) then
  begin
    M:=Methods[method];
    Result := ArgumentValidate(args, M.ArgMin, M.ArgMax, M.ArgType);
    if Result = nil then
      Result := M.Funct(args,env);
  end
  else
  begin
    Result := MethodInline(ObjectType+'.'+method, env);
    if ((Result=nil) and (ObjectType<>'')) then
      Result := MethodInline(ALL_OBJ+'.'+method, env);
  end;

  if Result=nil then
    TErrorObject.newError('method %s not found in object type %s',[method, ObjectType]);
end;

procedure TEvalObject.MethodInit;
begin
  Methods.Add('tostring', TMethodDescr.Create(0, 0, [], m_tostring));
  Methods.Add('toJSON', TMethodDescr.Create(0, 0, [], m_toJSON));
  Methods.Add('type', TMethodDescr.Create(0, 0, [], m_type));
  Methods.Add('clone', TMethodDescr.Create(0, 0, [], m_clone));
  Methods.Add('ref', TMethodDescr.Create(0, 0, [], m_ref));
  Methods.Add('help', TMethodDescr.Create(0, 1, [STRING_OBJ], m_help));
end;

function TEvalObject.MethodInline(method: string; env: TEnvironment): TEvalObject;
begin
  Result:= nil;
  env.GetValue(method, Result);
end;

function TEvalObject.m_clone(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := Clone;
end;

function TEvalObject.m_help(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  S,current,
  method:string;
  M:TMethodDescr;
  X:TIdentifierDescr;
  i, y: Integer;
  InlineMethods: TDictionary<string, TEvalObject>;
  E:TEvalObject;
begin
  if args.Count=0 then
  begin
    S:='Builded Method List ----------- '#13#10;
    for current in  Methods.Keys do
      S:=S+' - '+current+IfThen(Methods[current].ShortDescription<>'',' = '+LowerCase(Methods[current].ShortDescription),'')+#13#10;

    S:=S+'Inline Method List ----------- '#13#10;

    InlineMethods := env.GetFunctionObject(ObjectType);
    try
      for current in InlineMethods.Keys do
        S:=S+' - '+current+#13#10;
    finally
      InlineMethods.Free;
    end;

    InlineMethods := env.GetFunctionObject(ALL_OBJ);
    try
      for current in InlineMethods.Keys do
        S:=S+' - '+current+#13#10;
    finally
      InlineMethods.Free;
    end;

    S:=S+'Identifiers List ----------- '#13#10;
    for current in Identifiers.Keys do
      S:=S+' - '+current+IfThen(Identifiers[current].isReadOnly,' (readonly) ','')+IfThen(Identifiers[current].ShortDescription<>'',' = '+LowerCase(Identifiers[current].ShortDescription),'')+#13#10;

    S:=S+'use help("method") for details. '#13#10;

    Result := TStringObject.Create(S);
  end
  else
  begin
    E:=nil;
    method:=TStringObject(args[0]).Value;
    if Methods.TryGetValue(method, M ) then
    begin
      S:='Describe method '+method+' paramenters ---- '#13#10;
      if M.ShortDescription<>'' then
        S:=S+'# '+M.ShortDescription+#13#10;
      for i := M.ArgMin to M.ArgMax do
      begin
        S:=S+' - '+method+'(';
        for y := 0 to i-1 do
        begin
          S:=S+M.ArgType[y];
          if y<i-1 then S:=S+',';
        end;
        S:=S+')'+#13#10
      end;

      Result:= TStringObject.Create(S);
    end
    else
    if env.GetValue(method, E) then
    begin
      if (E is TFunctionObject) then
        Result:= TStringObject.Create(TFunctionObject(e).Inspect);
    end
    else
    if Identifiers.TryGetValue(method, X) then
    begin
      S:='Describe identifier '+method+' ---- '#13#10;
      if X.ShortDescription<>'' then
        S:=S+'# '+X.ShortDescription+#13#10;
      S:=S+' - type = ';
      for i := 0 to Length(X.IType)-1 do
        S:=S+X.IType[i]+IfThen(i<Length(X.IType)-1,' or ','');
      S:=S+#13#10;
      if Length(X.IIndexType)>0 then
      begin
        S:=S+' - indexed = ';
        for i := 0 to Length(X.IIndexType)-1 do
          S:=S+X.IIndexType[i]+IfThen(i<Length(X.IIndexType)-1,' or ','');
        S:=S+#13#10;
      end;

      S:=S+' - readonly = '+IfThen(X.isReadOnly,'yes','no')+#13#10;

      Result:= TStringObject.Create(S);
    end
    else Result:= TStringObject.CreateFormat('method %s not found in object type %s',[method, ObjectType]);
  end;
end;

function TEvalObject.m_ref(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := Reference;
end;

function TEvalObject.m_toJSON(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := toJSONString;
end;

function TEvalObject.m_tostring(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(Inspect);
end;

function TEvalObject.m_type(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(ObjectType);
end;

function TEvalObject.Next: TEvalObject;
begin
  Result := nil;
end;

function TEvalObject.ObjectType: TEvalObjectType;
begin
  Result := '';
end;

function TEvalObject.Reference: TEvalObject;
begin
  Result := Self;
end;

procedure TEvalObject.Reset;
begin
  IIndex:=0;
end;

function TEvalObject.SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject): TEvalObject;
var
  I: TIdentifierDescr;
begin
  if Identifiers.ContainsKey(name) then
  begin
     I := Identifiers[name];
     if NOT I.isReadOnly then
     begin
       Result := I.Setter(Index, value);
       if Result=nil then
         Result := I.Getter(Index);
     end
     else
      Result := TErrorObject.newError('Identifier %s is readonly',[name]);
  end
  else Result := TErrorObject.newError('identifier "'+name+'"not supported in object type %s',[ObjectType]);
end;

function TEvalObject.Setindex(Index, value: TEvalObject): TEvalObject;
begin
  Result := TErrorObject.newError('set index not supported in object type %s',[ObjectType]);
end;

function TEvalObject.toJSON: TJSONValue;
begin
  Result := TJSONNull.Create;
end;

function TEvalObject.toJSONString: TEvalObject;
var
  V:TJSONValue;
begin
  Result:= nil;

  V:= toJSON;
  try
    Result := TStringObject.Create(V.ToJSON);
  finally
    V.Free;
  end;
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

destructor TNumberObject.Destroy;
begin
  inherited;
end;

function TNumberObject.Inspect: string;
begin
  Result :=  StringReplace(FloatToStr(Value),',','.',[rfReplaceAll])
end;

procedure TNumberObject.MethodInit;
begin
  inherited;
  Methods.Add('format', TMethodDescr.Create(1, 1, [STRING_OBJ], m_format));
  Methods.Add('round', TMethodDescr.Create(0, 0, [], m_round));
  Methods.Add('trunc', TMethodDescr.Create(0, 0, [], m_trunc));
  Methods.Add('roundTo', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_round_to));
end;

function TNumberObject.m_format(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(FormatFloat(TStringObject(args[0]).Value, Value));
end;

function TNumberObject.m_round(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(Round(Value));
end;

function TNumberObject.m_round_to(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(RoundTo(Value, TNumberObject(args[0]).toInt));
end;

function TNumberObject.m_trunc(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(Trunc(Value));
end;

function TNumberObject.ObjectType: TEvalObjectType;
begin
  Result := NUMBER_OBJ;
end;

function TNumberObject.Reference: TEvalObject;
begin
  Result := Clone;
end;

function TNumberObject.toInt: Integer;
begin
  Result := Trunc(Value);
end;

function TNumberObject.toJSON: TJSONValue;
begin
  Result := TJSONNumber.Create(Value);
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

constructor TBooleanObject.CreateFalse;
begin
  Create(False);
end;

constructor TBooleanObject.CreateTrue;
begin
  Create(True);
end;

destructor TBooleanObject.Destroy;
begin
  inherited;
end;

function TBooleanObject.Inspect: string;
begin
  if Value then Result := 'true' else Result := 'false';
end;

function TBooleanObject.ObjectType: TEvalObjectType;
begin
  Result := BOOLEAN_OBJ;
end;

function TBooleanObject.Reference: TEvalObject;
begin
  Result := Clone;
end;

function TBooleanObject.toJSON: TJSONValue;
begin
  Result := TJSONBool.Create(Value);
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
  Value := AValue;// AValue.Clone;
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
  FAllowedIdent := TStringList.Create;
  FReturnRef:= TList<TEvalObject>.Create;
  FOuter := nil;
end;

constructor TEnvironment.Create(AOuter: TEnvironment);
begin
  Create;
  FOuter := AOuter;
end;

function TEnvironment.AddRef(AObject: TEvalObject): TEvalObject;
begin
  Result := AObject;
  if (Assigned(Outer) and Assigned(AObject)) then
  begin
    AObject.IncRefCount;
    Outer.ReturnRef.Add(AObject);
  end;
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
var
  current: TEvalObject;
begin
  for current in FReturnRef do
    current.DecRefCount;

  FReturnRef.Free;

  FStore.Free;
  FConst.Free;
  FAllowedIdent.Free;
  inherited;
end;

function TEnvironment.GetFunctionObject(AObjectType: string): TDictionary<string, TEvalObject>;
var
  current:string;
  CurrentStore:TDictionary<string, TEvalObject>;
begin
  Result := TDictionary<string, TEvalObject>.Create;
  CurrentStore := FStore;
  while (True) do
  begin
    for current in FStore.Keys do
      if Pos(AObjectType+'.', current)>0 then
        Result.Add(current, FStore[current]);

    if FOuter<>nil then
      CurrentStore := FOuter.Store
    else
      Break;
  end;
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
    begin
      if (FStore[name]<>value) then
      begin
        FStore[name].GcRefCount := 0;
        FStore[name].GcMark := False;
        Result.GcManualFree := FStore[name].GcManualFree;
        FStore[name].GcManualFree := False;
        FStore[name] := Result;
      end;
    end
    else Result := TErrorObject.newError('identifier : %s is const, READONLY value',[name]);
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
    begin
      if (FStore[name]<>value) then
      begin
        FStore[name].GcRefCount := 0;
        FStore[name].GcMark := False;
        FStore[name].GcManualFree := False;
        Result.GcManualFree:= FStore[name].GcManualFree;
        FStore[name] := Result
      end;
    end
    else Result := TErrorObject.newError('identifier : %s is const, not modifier value',[name]);
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

function TNullObject.Reference: TEvalObject;
begin
  Result := Clone;
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

destructor TStringObject.Destroy;
begin
  inherited;
end;

class function TStringObject.CreateFormat(const AFormat: string; const Args: array of const): TStringObject;
begin
  Result := TStringObject.Create(Format(AFormat, Args));
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

procedure TStringObject.MethodInit;
begin
  inherited;
  Methods.Add('tonumber', TMethodDescr.Create(0, 1, [NUMBER_OBJ], m_tonumber));
  Methods.Add('trim', TMethodDescr.Create(0, 0, [], m_trim));
  Methods.Add('rtrim', TMethodDescr.Create(0, 0, [], m_rtrim));
  Methods.Add('ltrim', TMethodDescr.Create(0, 0, [], m_ltrim));
  Methods.Add('len', TMethodDescr.Create(0, 0, [], m_len));
  Methods.Add('copy', TMethodDescr.Create(2, 2, [NUMBER_OBJ, NUMBER_OBJ], m_copy));
  Methods.Add('left', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_left));
  Methods.Add('right', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_right));
  Methods.Add('split', TMethodDescr.Create(0, 1, [STRING_OBJ], m_split));
  Methods.Add('splitAtLength', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_split_at_len));
  Methods.Add('lower', TMethodDescr.Create(0, 0, [], m_lower));
  Methods.Add('upper', TMethodDescr.Create(0, 0, [], m_upper));
  Methods.Add('contains', TMethodDescr.Create(1, 1, [STRING_OBJ], m_contains));
  Methods.Add('concat', TMethodDescr.Create(1, 1, [STRING_OBJ], m_concat));
  Methods.Add('indexof', TMethodDescr.Create(1, 1, [STRING_OBJ], m_index_of));
  Methods.Add('startWith', TMethodDescr.Create(1, 1, [STRING_OBJ], m_startwith));
  Methods.Add('endWith', TMethodDescr.Create(1, 1, [STRING_OBJ], m_endwith));
  Methods.Add('empty', TMethodDescr.Create(0, 0, [], m_empty));
  Methods.Add('toBase64', TMethodDescr.Create(0, 0, [], m_to_base64));
  Methods.Add('fromBase64', TMethodDescr.Create(0, 0, [], m_from_base64));
  Methods.Add('toEncodedUrl', TMethodDescr.Create(0, 0, [], m_url_encode));
  Methods.Add('toDecodedUrl', TMethodDescr.Create(0, 0, [], m_url_decode));
  Methods.Add('toEncodedHtml', TMethodDescr.Create(0, 0, [], m_html_encode));
  Methods.Add('toDecodedHtml', TMethodDescr.Create(0, 0, [], m_html_decode));

  Methods.Add('padStart', TMethodDescr.Create(1, 2, [NUMBER_OBJ, STRING_OBJ], m_pad_start));
  Methods.Add('padEnd', TMethodDescr.Create(1, 2, [NUMBER_OBJ, STRING_OBJ], m_pad_end));
  Methods.Add('repeat', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_repeat));
  Methods.Add('replace', TMethodDescr.Create(2, 2, [STRING_OBJ,STRING_OBJ], m_replace));
  Methods.Add('toCharCode', TMethodDescr.Create(0, 0, [], m_to_char_code));
  Methods.Add('charCodeAt', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_char_code_at));
end;

function TStringObject.m_char_code_at(args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
var
  P:Integer;
begin
  P := TNumberObject(args[0]).toInt;
  if ((P>0) and (P<=Length(Value))) then
    Result := TNumberObject.Create(Ord(Value[P]))
  else
    Result := TNullObject.Create;
end;

function TStringObject.m_concat(args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(Value + TStringObject(args[0]).Value);
end;

function TStringObject.m_contains(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  S:string;
begin
  S:= TStringObject(args[0]).Value;
  if S='' then
    Result := TBooleanObject.CreateFalse
  else
    Result := TBooleanObject.Create(Pos(S,Value)>0);
end;

function TStringObject.m_copy(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(Copy(Value,TNumberObject(args[0]).toInt,TNumberObject(args[1]).toInt));
end;

function TStringObject.m_empty(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TBooleanObject.Create(Length(Value)=0);
end;

function TStringObject.m_endwith(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  S:string;
begin
  S:= TStringObject(args[0]).Value;
  if S='' then
    Result := TBooleanObject.CreateFalse
  else
    Result := TBooleanObject.Create(EndsText(S, Value));
end;

function TStringObject.m_from_base64(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(DecodeString(Value));
end;

function TStringObject.m_html_decode(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(TNetEncoding.HTML.Decode(Value));
end;

function TStringObject.m_html_encode(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(TNetEncoding.HTML.Encode(Value));
end;

function TStringObject.m_index_of(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(Pos(TStringObject(args[0]).Value, Value));
end;

function TStringObject.m_left(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(LeftStr(Value, TNumberObject(args[0]).toInt));
end;

function TStringObject.m_len(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(Length(Value));
end;

function TStringObject.m_lower(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(LowerCase(Value));
end;

function TStringObject.m_ltrim(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(TrimLeft(Value));
end;

function TStringObject.m_pad_end(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  C:Char;
begin
  C:=' ';
  if (args.Count=2) then
    if (TStringObject(args[1]).Value<>'') then
      C:= TStringObject(args[1]).Value[1];

  Result := TStringObject.Create(StrSpace(Value, TNumberObject(args[0]).toInt, C))
end;

function TStringObject.m_pad_start(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  C:Char;
begin
  C:=' ';
  if (args.Count=2) then
    if (TStringObject(args[1]).Value<>'') then
      C:= TStringObject(args[1]).Value[1];

  Result := TStringObject.Create(StrSpace(Value, -TNumberObject(args[0]).toInt, C))
end;

function TStringObject.m_repeat(args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
var
  i,R:Integer;
  S:string;
begin
  S:='';
  R:=TNumberObject(args[0]).toInt;
  for i := 1 to R do
    S:=S+Value;

  Result := TStringObject.Create(S);
end;

function TStringObject.m_replace(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(StringReplace(Value,TStringObject(args[0]).Value,TStringObject(args[1]).Value,[rfReplaceAll]));
end;

function TStringObject.m_right(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(RightStr(Value, TNumberObject(args[0]).toInt));
end;

function TStringObject.m_rtrim(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(TrimRight(Value));
end;

function TStringObject.m_split(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
const
  SEPARATOR = ',';
var
  J:Char;
  S,S1:string;
begin
  J:=SEPARATOR;
  if ((args.Count=1) and (TStringObject(args[0]).Value<>'')) then
    j:= TStringObject(args[0]).Value[1];

  Result := TArrayObject.Create;
  TArrayObject(Result).Elements := TList<TEvalObject>.Create;

  S:= Value;
  While (S<>'') do
  begin
    S1:= StrSplit(S,J);
    TArrayObject(Result).Elements.Add(TStringObject.Create(S1));
  end;
end;

function TStringObject.m_split_at_len(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  L:TStringList;
  Len,i:Integer;
  S0,S1,S2:String;
begin
  Result := TArrayObject.Create;
  TArrayObject(Result).Elements := TList<TEvalObject>.Create;

  Len:=TNumberObject(args[0]).toInt;
  L:=TStringList.Create;
  try
    L.Text := TNetEncoding.HTML.Decode(Value);

    for i := 0 to L.Count-1 do
    begin
      S0 := L[i];
      While (S0<>'') do
      begin
        S1 := StrSplit(S0,' ');
        if (Length(S2+' '+S1)>Len) then
        begin
          TArrayObject(Result).Elements.Add(TStringObject.Create(S2));
          S2:=S1;
        end
        else S2 := S2+' '+S1;
      end;
      if (S2<>'') then
      begin
        TArrayObject(Result).Elements.Add(TStringObject.Create(S2));
        S2:='';
      end;
    end;
    if (S2<>'') then
      TArrayObject(Result).Elements.Add(TStringObject.Create(S2));
  finally
    L.Free;
  end;
end;

function TStringObject.m_startwith(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  S:string;
begin
  S:= TStringObject(args[0]).Value;
  if S='' then
    Result := TBooleanObject.CreateFalse
  else
    Result := TBooleanObject.Create(StartsText(S, Value));
end;

function TStringObject.m_tonumber(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  if args.Count=1 then
    Result := TNumberObject.Create(StrToFloatDef(Value, TNumberObject(args[0]).Value))
  else
    Result := TNumberObject.Create(StrToFloatDef(Value,0));
end;

function TStringObject.m_to_base64(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(EncodeString(Value));
end;

function TStringObject.m_to_char_code(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  i:Integer;
begin
  Result := TArrayObject.CreateWithElements;
  for i := 1 to Length(Value) do
    TArrayObject(Result).Elements.Add(TNumberObject.Create(Ord( Value[i])) );
end;

function TStringObject.m_trim(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(Trim(Value));
end;

function TStringObject.m_upper(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(UpperCase(Value));
end;

function TStringObject.m_url_decode(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(TNetEncoding.URL.Decode(Value));
end;

function TStringObject.m_url_encode(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(TNetEncoding.URL.Encode(Value));
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

function TStringObject.Reference: TEvalObject;
begin
  Result := Clone;
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

function TStringObject.toJSON: TJSONValue;
begin
  Result := TJSONString.Create(Value);
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
      Result := Elements[TNumberObject(Index).toInt].Reference;
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

procedure TArrayObject.MethodInit;
begin
  inherited;
  Methods.Add('size', TMethodDescr.Create(0, 0, [], m_size, 'get numbers of elements of array.'));
  Methods.Add('first', TMethodDescr.Create(0, 0, [], m_first, 'get first element of array.'));
  Methods.Add('last', TMethodDescr.Create(0, 0, [], m_last, 'get first element of array.'));
  Methods.Add('rest', TMethodDescr.Create(0, 0, [], m_rest));
  Methods.Add('push', TMethodDescr.Create(1, 1, [ALL_OBJ], m_push));
  Methods.Add('insert', TMethodDescr.Create(2, 2, [ALL_OBJ, NUMBER_OBJ], m_insert));
  Methods.Add('join', TMethodDescr.Create(0, 1, [STRING_OBJ], m_join));
  Methods.Add('delete', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_delete));
end;

function TArrayObject.m_delete(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  i:Integer;
begin
  i := TNumberObject(args[0]).toInt;
  if ((i>=0) and (i<Self.Elements.Count)) then
  begin
    Elements[i].GcManualFree := False;
    Elements[i].GcRefCount := 0;
    Elements.Delete(i);
    Result := TNumberObject.Create(Elements.Count);
  end
  else Result:= TErrorObject.newError('index out of range, 0 to %d', [Elements.Count])
end;

function TArrayObject.m_first(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  if Elements.Count>0 then
    Result := Elements[0].Reference
  else
    Result := TNullObject.Create;
end;

function TArrayObject.m_insert(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  i:Integer;
begin
  i := TNumberObject(args[1]).toInt;
  if ((i<0) or (i>Elements.Count)) then
    Result:= TErrorObject.newError('index out of range, 0 to %d', [Elements.Count])
  else
  begin
    Self.Elements.Insert(i, args[0].Reference);
    Result := TNumberObject.Create(Elements.Count);
  end;
end;

function TArrayObject.m_join(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
const
  SEPARATOR = ',';
var
  J,S:string;
  i:Integer;
begin
  J:=SEPARATOR;
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

function TArrayObject.m_last(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  if Elements.Count>0 then
    Result := Elements[Elements.Count-1].Reference
  else
    Result := TNullObject.Create;
end;

function TArrayObject.m_push(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Elements.Add(args[0].Reference);
  Result := TNumberObject.Create(Elements.Count);
end;

function TArrayObject.m_rest(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  i: Integer;
begin
  if (Elements.Count>0) then
  begin
    Elements[0].GcManualFree := False;
    Elements[0].GcRefCount := 0;
    Elements.Delete(0);
  end;
  Result := TNumberObject.Create(Elements.Count);
end;

function TArrayObject.m_size(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(Elements.Count);
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
      Value.GcManualFree:= Elements[TNumberObject(Index).toInt].GcManualFree;
      Elements[TNumberObject(Index).toInt].GcManualFree := False;
      Elements[TNumberObject(Index).toInt] := Value.Reference;
      Result := Self;
    end;
  end;
end;

function TArrayObject.toJSON: TJSONValue;
var
  E:TEvalObject;
begin
  Result := TJSONArray.Create;
  for E in Elements do
    TJSONArray(Result).AddElement(E.toJSON);
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
      Result := FValueBool = THashkey(Obj).FValueBool
    else
      Result := False;
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

function THashObject.ChkObject(name: string; OType: TEvalObjectType): Boolean;
var
  OObject:TEvalObject;
begin
  Result := ChkObject(name, OType, OObject);
end;

function THashObject.ChkObject(name: string; OType: TEvalObjectType; var OObject: TEvalObject): Boolean;
var
  HK:THashkey;
begin
  OObject := nil;

  HK := THashkey.Create(STRING_OBJ);
  HK.FValueStr := name;

  Result := Pairs.ContainsKey(HK);
  if Result then
  begin
    Result := (Pairs[HK].Value.ObjectType=OType);
    if Result then
      OObject := Pairs[HK].Value;
  end;

  FreeAndNil(HK);
end;

function THashObject.Clone: TEvalObject;
var
  key:THashkey;
  k,v:TEvalObject;
begin
  Result := THashObject.Create;
  for key in Pairs.Keys do
  begin
    k := Pairs[key].Key.Clone;
    v := Pairs[key].Value.Clone;
    THashObject(Result).Pairs.Add(THashkey.fromObject(k), THashPair.Create(k,v));
  end;
end;

constructor THashObject.Create;
begin
  inherited Create;
  Pairs :=  TDictionary<THashkey,THashPair>.create;
end;

function THashObject.CurrentIndex: TEvalObject;
begin
  Result := Pairs.ToArray[IIndex-1].Value.Key;
end;

destructor THashObject.Destroy;
var
  hkey: THashkey;
  hpair: THashPair;
begin
  for hpair in Pairs.Values do
    hpair.Free;
  for hkey in Pairs.Keys do
    hkey.Free;

{  for hkey in Pairs.Keys do
  begin
    Pairs[hkey].Free;
    hkey.Free;
  end;}
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
    Result := Pairs[HK].Value.Reference
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
    Result := Pairs[HK].Value.Reference
  else
    Result := TNullObject.Create;

  FreeAndNil(HK);
end;

function THashObject.GetObject(name: string): TEvalObject;
var
  HK:THashkey;
begin
  Result := nil;

  HK := THashkey.Create(STRING_OBJ);
  HK.FValueStr := name;

  if Pairs.ContainsKey(HK) then
    Result := Pairs[HK].Value;

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
begin
  Result := GetIdentifer(method);
  if NOT (Result is TFunctionObject) then
    Result := inherited MethodCall(method, args, env);
end;

procedure THashObject.MethodInit;
begin
  inherited;
  Methods.Add('size', TMethodDescr.Create(0, 0, [], m_size));
  Methods.Add('keys', TMethodDescr.Create(0, 0, [], m_keys));
  Methods.Add('delete', TMethodDescr.Create(1, 1, [ALL_OBJ], m_delete));
end;

function THashObject.m_delete(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  hkey: THashkey;
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

function THashObject.m_keys(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  hkey: THashkey;
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

function THashObject.m_size(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(Pairs.Count);
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
    Value.GcManualFree := Pairs[HK].Value.GcManualFree;
    Pairs[HK].Value.GcManualFree := false;
    Pairs[HK].Value := Value.Reference;
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
    Value.GcManualFree := Pairs[HK].Value.GcManualFree;
    Pairs[HK].Value.GcManualFree := false;
    Pairs[HK].Value := Value.Reference;
    FreeAndNil(HK);
  end
  else Pairs.Add(HK,THashPair.Create(Index, Value));

  Result := Self;
end;

function THashObject.toJSON: TJSONValue;
var
  k:THashkey;
begin
  Result := TJSONObject.Create;
  for k in Pairs.Keys do
    if K.ObjectType=STRING_OBJ then
      TJSONObject(Result).AddPair(k.FValueStr, Pairs[k].Value.toJSON);
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

{ TMethodDescr }

constructor TMethodDescr.Create(AArgMin, AArgMax: Integer; AArgType: TArray<string>; AFunct: TMethodFunct; AShortDescription:string='');
begin
  ArgMin := AArgMin;
  ArgMax := AArgMax;
  ArgType:= AArgType;
  Funct := AFunct;
  ShortDescription := AShortDescription;
end;

{ TIdentifierDescr }

constructor TIdentifierDescr.Create(AIType: TArray<string>; AIIndexType: TArray<string>; AGetter: TIdentifierGetFunct; ASetter: TIdentifierSetFunct; AShortDescription: string);
begin
  IType:= AIType;
  Getter:= AGetter;
  Setter:= ASetter;
  IIndexType:= AIIndexType;
  ShortDescription:=AShortDescription;
end;

function TIdentifierDescr.GetIsReadOnly: Boolean;
begin
  Result := NOT Assigned(Setter);
end;

{ TDateTimeObject }

function TDateTimeObject.Clone: TEvalObject;
begin
  Result := TDateTimeObject.Create(Value);
end;

constructor TDateTimeObject.Create;
begin
  inherited Create;
  Value := Now;
end;

constructor TDateTimeObject.Create(AValue: TDateTime);
begin
  Create;
  Value := AValue;
end;

destructor TDateTimeObject.Destroy;
begin

  inherited;
end;

procedure TDateTimeObject.IdentifierInit;
begin
  inherited;
  Identifiers.Add('date', TIdentifierDescr.Create([DATETIME_OBJ],[], i_get_date, nil,'return only date part of datetime object'));
  Identifiers.Add('year', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_year, nil,'return Year of datetime object'));
  Identifiers.Add('month', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_month, nil,'return Month of datetime object'));
  Identifiers.Add('day', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_day, nil,'return Day of datetime object'));
  Identifiers.Add('hour', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_hour, nil,'return Hour of datetime object'));
  Identifiers.Add('minute', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_minute, nil,'return Minute of datetime object'));
  Identifiers.Add('second', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_second, nil,'return Second of datetime object'));
  Identifiers.Add('millisec', TIdentifierDescr.Create([NUMBER_OBJ],[], i_get_millisecond, nil,'return Millisecond of datetime object'));
end;

function TDateTimeObject.Inspect: string;
begin
  Result := DateToStr(Value);
end;

function TDateTimeObject.i_get_date(Index: TEvalObject): TEvalObject;
begin
  Result := TDateTimeObject.Create(Trunc(Value));
end;

function TDateTimeObject.i_get_day(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(DayOf(Value));
end;

function TDateTimeObject.i_get_hour(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(HourOf(Value));
end;

function TDateTimeObject.i_get_millisecond(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(MilliSecondOf(Value));
end;

function TDateTimeObject.i_get_minute(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(MinuteOf(Value));
end;

function TDateTimeObject.i_get_month(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(MonthOf(Value));
end;

function TDateTimeObject.i_get_second(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(SecondOf(Value));
end;

function TDateTimeObject.i_get_year(Index: TEvalObject): TEvalObject;
begin
  Result := TNumberObject.Create(YearOf(Value));
end;

procedure TDateTimeObject.MethodInit;
begin
  inherited;
  Methods.Add('valid', TMethodDescr.Create(0, 0, [], m_valid,'return is a valid date time object.'));
  Methods.Add('encode', TMethodDescr.Create(3, 7, [NUMBER_OBJ,NUMBER_OBJ,NUMBER_OBJ,NUMBER_OBJ,NUMBER_OBJ,NUMBER_OBJ,NUMBER_OBJ ], m_encode,'Date from min Year, Month, Day (optional hour, min, sec, millis), return is valid'));
  Methods.Add('decode', TMethodDescr.Create(0, 0, [], m_decode,'return an array on numbers [Year, Month, Day, Hour, Minute, Second, Millisecond]'));
  Methods.Add('format', TMethodDescr.Create(1, 1, [STRING_OBJ], m_format,'return formatted string datetime.'));

  Methods.Add('yearsBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_years_between,'return Years between passed date time object.'));
  Methods.Add('monthsBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_months_between,'return Months between passed date time object.'));
  Methods.Add('weeksBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_weeks_between,'return Weeks between passed date time object.'));
  Methods.Add('daysBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_days_between,'return Days between passed date time object.'));
  Methods.Add('hoursBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_hours_between,'return Hours between passed date time object.'));
  Methods.Add('minutesBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_minutes_between,'return Minutes between passed date time object.'));
  Methods.Add('secondsBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_seconds_between,'return Seconds between passed date time object.'));
  Methods.Add('millisBetween', TMethodDescr.Create(1, 1, [DATETIME_OBJ], m_milliseconds_between,'return MilliSeconds between passed date time object.'));

  Methods.Add('addYears', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_years_add,'add Years to date time object.'));
  Methods.Add('addMonths', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_months_add,'add Months to date time object.'));
  Methods.Add('addWeeks', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_weeks_add,'add Weeks to date time object.'));
  Methods.Add('addDays', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_days_add,'add Days to date time object.'));
  Methods.Add('addHours', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_hours_add,'add Hours to date time object.'));
  Methods.Add('addMinutes', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_minutes_add,'add Minutes to passed date time object.'));
  Methods.Add('addSeconds', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_seconds_add,'add Seconds to passed date time object.'));
  Methods.Add('addMillis', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_milliseconds_add,'add MilliSeconds to date time object.'));
end;

function TDateTimeObject.m_days_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncDay(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_days_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(DaysBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_decode(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(Value
    , AYear, AMonth, ADay
    , AHour, AMinute, ASecond, AMilliSecond);
  Result := TArrayObject.CreateWithElements;

  TArrayObject(Result).Elements.Add(TNumberObject.Create(AYear));
  TArrayObject(Result).Elements.Add(TNumberObject.Create(AMonth));
  TArrayObject(Result).Elements.Add(TNumberObject.Create(ADay));
  TArrayObject(Result).Elements.Add(TNumberObject.Create(AHour));
  TArrayObject(Result).Elements.Add(TNumberObject.Create(AMinute));
  TArrayObject(Result).Elements.Add(TNumberObject.Create(ASecond));
  TArrayObject(Result).Elements.Add(TNumberObject.Create(AMilliSecond));
end;

function TDateTimeObject.m_encode(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
const
  DP_SIZE = 7;
var
  DPart: Array[0..DP_SIZE-1] of Word;
  i:Integer;
begin
  for i := 0 to DP_SIZE-1 do DPart[i] := 0;
  for i := 0 to args.Count-1 do DPart[i] := TNumberObject(args[i]).toInt;

  Result := TBooleanObject.Create(IsValidDateTime(DPart[0],DPart[1],DPart[2],DPart[3],DPart[4],DPart[5],DPart[6]));
  if TBooleanObject(Result).Value then
    Value := EncodeDateTime(DPart[0],DPart[1],DPart[2],DPart[3],DPart[4],DPart[5],DPart[6]);
end;

function TDateTimeObject.m_format(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(FormatDateTime(TStringObject(args[0]).Value, Value));
end;

function TDateTimeObject.m_hours_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncHour(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_hours_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(HoursBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_milliseconds_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncMilliSecond(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_milliseconds_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(MilliSecondsBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_minutes_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncMinute(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_minutes_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(MinutesBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_months_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncMonth(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_months_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(MonthsBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_seconds_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncSecond(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_seconds_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(SecondsBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_valid(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(Value
    , AYear, AMonth, ADay
    , AHour, AMinute, ASecond, AMilliSecond);

  Result := TBooleanObject.Create(IsValidDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond));
end;

function TDateTimeObject.m_weeks_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncWeek(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_weeks_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(WeeksBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.m_years_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Value := IncYear(Value, TNumberObject(args[0]).toInt);
  Result:= Self;
end;

function TDateTimeObject.m_years_between(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(YearsBetween(Value, TDateTimeObject(args[0]).Value));
end;

function TDateTimeObject.ObjectType: TEvalObjectType;
begin
  Result := DATETIME_OBJ;
end;

function TDateTimeObject.Reference: TEvalObject;
begin
  Result := Clone;
end;

function TDateTimeObject.toJSON: TJSONValue;
begin
  Result := TJSONString.Create(DateToISO8601(Value));
end;

end.
