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
  protected
    function  i_get_duplicates(Index:TEvalObject=nil):TEvalObject;
    function  i_set_duplicates(Index:TEvalObject; value:TEvalObject):TEvalObject;
    function  i_get_sorted(Index:TEvalObject=nil):TEvalObject;
    function  i_set_sorted(Index:TEvalObject; value:TEvalObject):TEvalObject;
    function  i_get_casesentitive(Index:TEvalObject=nil):TEvalObject;
    function  i_set_casesentitive(Index:TEvalObject; value:TEvalObject):TEvalObject;
    function  i_get_text(Index:TEvalObject=nil):TEvalObject;
    function  i_set_text(Index:TEvalObject; value:TEvalObject):TEvalObject;
    procedure IdentifierInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;

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

procedure TStringListObject.IdentifierInit;
begin
  inherited;
  Identifiers.Add('duplicates', TIdentifierDescr.Create(BOOLEAN_OBJ, i_get_duplicates, i_set_duplicates));
  Identifiers.Add('sorted', TIdentifierDescr.Create(BOOLEAN_OBJ, i_get_sorted, i_set_sorted));
  Identifiers.Add('casesentitive', TIdentifierDescr.Create(BOOLEAN_OBJ, i_get_casesentitive, i_set_casesentitive));
  Identifiers.Add('text', TIdentifierDescr.Create(STRING_OBJ, i_get_text, i_set_text));
end;

function TStringListObject.Inspect: string;
begin
  Result := 'StringList$'+ IntToHex(Integer(Pointer(InnetList)), 6) +' (' + IntToStr(InnetList.Count) + ' rows)';
end;

function TStringListObject.isIterable: Boolean;
begin
  Result := True;
end;

function TStringListObject.i_get_casesentitive(Index: TEvalObject): TEvalObject;
begin
  Result := TBooleanObject.Create(InnetList.CaseSensitive);
end;

function TStringListObject.i_get_duplicates(Index: TEvalObject): TEvalObject;
begin
  Result := TBooleanObject.Create(InnetList.Duplicates = dupAccept );
end;

function TStringListObject.i_get_sorted(Index: TEvalObject): TEvalObject;
begin
  Result := TBooleanObject.Create(InnetList.Sorted);
end;

function TStringListObject.i_get_text(Index: TEvalObject): TEvalObject;
begin
  Result := TStringObject.Create(InnetList.Text);
end;

function TStringListObject.i_set_casesentitive(Index, value: TEvalObject): TEvalObject;
begin
  Result := nil;
  InnetList.CaseSensitive := TBooleanObject(value).Value;
end;

function TStringListObject.i_set_duplicates(Index, value: TEvalObject): TEvalObject;
begin
  Result := nil;
  if TBooleanObject(value).Value then
    InnetList.Duplicates := dupAccept
  else
    InnetList.Duplicates := dupIgnore;
end;

function TStringListObject.i_set_sorted(Index, value: TEvalObject): TEvalObject;
begin
  Result := nil;
  InnetList.Sorted := TBooleanObject(value).Value
end;

function TStringListObject.i_set_text(Index, value: TEvalObject): TEvalObject;
begin
  Result := nil;
  InnetList.Text := TStringObject(value).Value;
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
