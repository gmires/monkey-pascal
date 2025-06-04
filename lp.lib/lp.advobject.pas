unit lp.advobject;

interface

{$I lp.inc}

uses classes, SysUtils, Generics.Collections, Variants, StrUtils
  , lp.environment
  , lp.evaluator;

const
	STRING_LIST_OBJ = 'STRING_LIST';
	FILE_SYSTEM_OBJ = 'FILE_SYSTEM';

type
  TStringListObject = class(TEvalObject)
  private
    InnetList: TStringList;
  protected
    function  m_add(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_indexof(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_delete(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_loadfromfile(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_savetofile(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_toarray(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_clear(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_size(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;

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

  TFileSystemObject = class(TEvalObject)
  protected
    function  m_dirExists(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  FileSystemObject:TFileSystemObject = nil;

implementation

function _StringListConstructor(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  Result := TStringListObject.Create;
end;

function _FileSystemConstructor(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  if NOT Assigned(FileSystemObject) then
    FileSystemObject := TFileSystemObject.Create;

  Result := FileSystemObject;
end;

procedure init;
begin
  builtins.Add('StringList', TBuiltinObject.Create(_StringListConstructor));
  builtins.Add('FS', TBuiltinObject.Create(_FileSystemConstructor));
end;

procedure deinit;
begin
  if Assigned(FileSystemObject) then FileSystemObject.Free;
end;


{ TStringListObject }

function TStringListObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TStringListObject.Create;
begin
  inherited Create;
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
  Result := 'StringList$'+ IntToHex(Integer(Pointer(InnetList)), 6) +' (' + IntToStr(InnetList.Count) + ' rows)';
end;

function TStringListObject.isIterable: Boolean;
begin
  Result := True;
end;

procedure TStringListObject.MethodInit;
begin
  inherited;
  Methods.Add('add', TMethodDescr.Create(1, 1, [STRING_OBJ], m_add));
  Methods.Add('indexof', TMethodDescr.Create(1, 1, [STRING_OBJ], m_indexof));
  Methods.Add('delete', TMethodDescr.Create(1, 1, [NUMBER_OBJ], m_delete));
  Methods.Add('loadfromfile', TMethodDescr.Create(1, 1, [STRING_OBJ], m_loadfromfile));
  Methods.Add('savetofile', TMethodDescr.Create(1, 1, [STRING_OBJ], m_savetofile));
  Methods.Add('toarray', TMethodDescr.Create(0, 0, [], m_toarray));
  Methods.Add('clear', TMethodDescr.Create(0, 0, [], m_clear));
  Methods.Add('size', TMethodDescr.Create(0, 0, [], m_size));
end;

function TStringListObject.m_add(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(InnetList.Add(TStringObject(args[0]).Value)) ;
end;

function TStringListObject.m_clear(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  InnetList.Clear;
  Result := Self;
end;

function TStringListObject.m_delete(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  if ((TNumberObject(args[0]).toInt<0) or (TNumberObject(args[0]).toInt>InnetList.Count-1)) then
    Result := TErrorObject.newError('index oput of range , %s', [args[0].Inspect])
  else
  begin
    InnetList.Delete(TNumberObject(args[0]).toInt);
    Result := Self;
  end;
end;

function TStringListObject.m_indexof(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(InnetList.IndexOf(TStringObject(args[0]).Value)) ;
end;

function TStringListObject.m_loadfromfile(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  InnetList.LoadFromFile(TStringObject(args[0]).Value);
  Result := Self;
end;

function TStringListObject.m_savetofile(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  InnetList.SaveToFile(TStringObject(args[0]).Value);
  Result := Self;
end;

function TStringListObject.m_size(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TNumberObject.Create(InnetList.Count)
end;

function TStringListObject.m_toarray(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  S:String;
begin
  Result := TArrayObject.Create;
  TArrayObject(Result).Elements := TList<TEvalObject>.Create;
  for S in InnetList do
    TArrayObject(Result).Elements.Add(TStringObject.Create(S));
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

{ TFileSystemObject }

function TFileSystemObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TFileSystemObject.Create;
begin
  inherited Create;
  GcManualFree := True;
end;

destructor TFileSystemObject.Destroy;
begin
  inherited;
end;

function TFileSystemObject.m_dirExists(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TBooleanObject.Create(DirectoryExists((TStringObject(args[0]).Value)));
end;

function TFileSystemObject.Inspect: string;
begin
  Result := 'FileSystem$'+ IntToHex(Integer(Pointer(Self)), 6);
end;

procedure TFileSystemObject.MethodInit;
begin
  inherited;
  Methods.Add('dirExist', TMethodDescr.Create(1, 1, [STRING_OBJ], m_dirExists));
end;

function TFileSystemObject.ObjectType: TEvalObjectType;
begin
  Result := FILE_SYSTEM_OBJ;
end;

initialization
  init;
finalization
  deinit;

end.
