unit lp.advobject;

interface

uses classes, SysUtils, Generics.Collections, Variants, StrUtils
  , lp.environment
  , lp.evaluator;

const
	STRING_LIST_OBJ = 'STRING_LIST';

type
  TStringListObject = class(TEvalObject)
  private
    InnetList: TStringList;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;

    function MethodCall(method:string; args: TList<TEvalObject>; env: TEnvironment):TEvalObject; override;

    function GetIdentifer(name:string; Index:TEvalObject=nil):TEvalObject; override;
    function SetIdentifer(name:string; value:TEvalObject; Index:TEvalObject=nil):TEvalObject; override;

    function GetIndex(Index:TEvalObject):TEvalObject; override;
    function Setindex(Index:TEvalObject; value:TEvalObject):TEvalObject; override;

    function isIterable:Boolean; override;
    function Next:TEvalObject; override;
  public
    constructor Create; overload;
    constructor Create(AStrings: TStringList); overload;
    destructor Destroy; override;
  end;

implementation

function _StringListConstructor(args: TList<TEvalObject>): TEvalObject;
begin
  Result := TStringListObject.Create;
end;

procedure init;
begin
  builtins.Add('StringList', TBuiltinObject.Create(_StringListConstructor));
end;

{ TStringListObject }

function TStringListObject.Clone: TEvalObject;
begin
  Result := TStringListObject.Create;
end;

constructor TStringListObject.Create;
begin
  InnetList:= TStringList.Create;
end;

constructor TStringListObject.Create(AStrings: TStringList);
begin
  Create;
  InnetList.Assign(AStrings);
end;

destructor TStringListObject.Destroy;
begin
  InnetList.Free;
  inherited;
end;

function TStringListObject.GetIdentifer(name: string; Index: TEvalObject): TEvalObject;
begin
  if name='duplicates' then
    Result := TBooleanObject.Create(InnetList.Duplicates = dupAccept )
  else
  if name='sorted' then
    Result := TBooleanObject.Create(InnetList.Sorted)
  else
  if name='casesentitive' then
    Result := TBooleanObject.Create(InnetList.CaseSensitive)
  else
  if name='text' then
    Result := TStringObject.Create(InnetList.Text)
  else
    Result:= inherited GetIdentifer(name, Index);
end;

function TStringListObject.GetIndex(Index: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  begin
    if ((TNumberObject(Index).toInt<0) or (TNumberObject(Index).toInt>InnetList.Count - 1)) then
      Result := TNullObject.Create
    else
      Result := TStringObject.Create(InnetList[TNumberObject(Index).toInt]);
  end;
end;

function TStringListObject.Inspect: string;
begin
  Result := 'StringList$'+ IntToHex(Integer(@InnetList), 6) +' (' + IntToStr(InnetList.Count) + ')';
end;

function TStringListObject.isIterable: Boolean;
begin
  Result := True;
end;

function TStringListObject.MethodCall(method: string; args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
var
  S: string;
begin
  if (method='add') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>STRING_OBJ) then
      Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
    else
      Result := TNumberObject.Create(InnetList.Add(TStringObject(args[0]).Value)) ;
  end
  else
  if (method='indexof') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>STRING_OBJ) then
      Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
    else
      Result := TNumberObject.Create(InnetList.IndexOf(TStringObject(args[0]).Value)) ;
  end
  else
  if (method='delete') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>NUMBER_OBJ) then
      Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
    else
    begin
      if ((TNumberObject(args[0]).toInt<0) or (TNumberObject(args[0]).toInt>InnetList.Count-1)) then
        Result := TErrorObject.newError('index oput of range , %s', [args[0].Inspect])
      else
      begin
        InnetList.Delete(TNumberObject(args[0]).toInt);
        Result := Self;
      end;
    end;
  end
  else
  if (method='loadfromfile') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>STRING_OBJ) then
      Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
    else
    begin
      InnetList.LoadFromFile(TStringObject(args[0]).Value);
      Result := Self;
    end;
  end
  else
  if (method='savetofile') then
  begin
    if (args.Count<>1) then
      Result := TErrorObject.newError('wrong number of arguments. got=%d, want=1', [args.Count])
    else
    if (args[0].ObjectType<>STRING_OBJ) then
      Result := TErrorObject.newError('argument must be STRING, got %s', [args[0].ObjectType])
    else
    begin
      InnetList.SaveToFile(TStringObject(args[0]).Value);
      Result := Self;
    end;
  end
  else
  if (method='toarray') then
  begin
    Result := TArrayObject.Create;
    TArrayObject(Result).Elements := TList<TEvalObject>.Create;
    for S in InnetList do
      TArrayObject(Result).Elements.Add(TStringObject.Create(S));
  end
  else
  if (method='clear') then
  begin
    InnetList.Clear;
    Result := Self;
  end
  else
  if (method='size') then
    Result := TNumberObject.Create(InnetList.Count)
  else Result := MethodInline(ObjectType+'.'+method, env);

  if (Result=nil) then
    Result := inherited MethodCall(method, args, env);
end;


function TStringListObject.Next: TEvalObject;
begin
  Result := nil;
  if (IIndex<InnetList.Count) then
  begin
    Result := TStringObject.Create(InnetList[IIndex]);
    Inc(IIndex);
  end;
end;

function TStringListObject.ObjectType: TEvalObjectType;
begin
  Result := STRING_LIST_OBJ;
end;

function TStringListObject.SetIdentifer(name: string; value, Index: TEvalObject): TEvalObject;
begin
  if ((name='duplicates') or (name='sorted') or (name='casesentitive')) then
  begin
    if value.ObjectType<>BOOLEAN_OBJ then
      Result := TErrorObject.newError('value type <%s> not usable for <'+name+'> property',[value.ObjectType])
    else
    begin
      if (name='duplicates') then
      begin
        if TBooleanObject(value).Value then
          InnetList.Duplicates := dupAccept
        else
          InnetList.Duplicates := dupIgnore;
      end
      else
      if (name='sorted') then
        InnetList.Sorted := TBooleanObject(value).Value
      else
      if (name='casesentitive') then
        InnetList.CaseSensitive := TBooleanObject(value).Value;

      Result :=GetIdentifer(name);
    end;
  end
  else
  if (name='text') then
  begin
    if value.ObjectType<>STRING_OBJ then
      Result := TErrorObject.newError('value type <%s> not usable for <'+name+'> property',[value.ObjectType])
    else
    begin
      InnetList.Text := TStringObject(value).Value;
      Result :=GetIdentifer(name);
    end;
  end
  else Result:= inherited SetIdentifer(name, value, Index);
end;

function TStringListObject.Setindex(Index, value: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  if value.ObjectType<>STRING_OBJ then
    Result := TErrorObject.newError('value type <%s> not supported: %s', [value.ObjectType, ObjectType])
  else
  begin
    if ((TNumberObject(Index).toInt<0) or (TNumberObject(Index).toInt>InnetList.Count - 1)) then
      Exit( TErrorObject.newError('unusable index for array: %s', [TNumberObject(Index).Inspect]))
    else
    begin
      InnetList[TNumberObject(Index).toInt] := TStringObject(Value).Value;
      Result := Self;
    end;
  end;
end;

initialization
  init;

end.
