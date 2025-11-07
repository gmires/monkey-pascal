unit lp.advobject;

interface

{$I lp.inc}

uses classes, SysUtils, Generics.Collections, Variants, StrUtils, System.IOUtils
  ,{$IFDEF LPI_D28} JSON {$ELSE} DBXJSON {$ENDIF}
  , lp.environment
  , lp.evaluator;

const
	STRING_LIST_OBJ = 'STRING_LIST';
	FILE_SYSTEM_OBJ = 'FILE_SYSTEM';
	DIRECTORY_OBJ = 'DIRECTORY';
	FILE_OBJ = 'FILE';
	PATH_OBJ = 'PATH';
	JSON_OBJ = 'JSON';

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

  TDirectoryObject = class(TEvalObject)
  protected
    // -- directory helper
    function  m_Exists(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Create(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Delete(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Copy(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Empty(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_List(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TFileObject = class(TEvalObject)
  protected
    function  m_Exists(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Create(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Delete(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Copy(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TPathObject = class(TEvalObject)
  protected
    function  m_ExtractFileName(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_ExtractFileExt(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_Join(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    procedure MethodInit; override;
  protected
    function  i_get_pathdelimiter(Index:TEvalObject=nil):TEvalObject;
    procedure IdentifierInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSystemObject = class(TEvalObject)
    FDirectory: TDirectoryObject;
    FFile: TFileObject;
    FPath: TPathObject;
  protected
    function  i_get_directory(Index:TEvalObject=nil):TEvalObject;
    function  i_get_file(Index:TEvalObject=nil):TEvalObject;
    function  i_get_path(Index:TEvalObject=nil):TEvalObject;
    function  i_get_args(Index:TEvalObject=nil):TEvalObject;
    function  i_get_dirname(Index:TEvalObject=nil):TEvalObject;
    function  i_get_filename(Index:TEvalObject=nil):TEvalObject;
    procedure IdentifierInit; override;
  public
    function ObjectType:TEvalObjectType; override;
    function Inspect:string; override;
    function Clone:TEvalObject; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TJSONLIBObject = class(TEvalObject)
  protected
    function  m_stringify(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_parse(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
    function  m_validate(args: TList<TEvalObject>; env: TEnvironment):TEvalObject;
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
  FileSystemObject:TSystemObject = nil;
  JSONLIBObject:TJSONLIBObject = nil;

implementation

function _StringListConstructor(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  Result := TStringListObject.Create;
end;

function _SystemConstructor(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  if NOT Assigned(FileSystemObject) then
    FileSystemObject := TSystemObject.Create;

  Result := FileSystemObject;
end;

function _JSONLIBConstructor(env:TEnvironment; args: TList<TEvalObject>): TEvalObject;
begin
  if NOT Assigned(JSONLIBObject) then
    JSONLIBObject := TJSONLIBObject.Create;

  Result := JSONLIBObject;
end;

procedure init;
begin
  builtins.Add('StringList', TBuiltinObject.Create(_StringListConstructor));
  builtins.Add('System', TBuiltinObject.Create(_SystemConstructor));
  builtins.Add('JSON', TBuiltinObject.Create(_JSONLIBConstructor));
end;

procedure deinit;
begin
  if Assigned(FileSystemObject) then FileSystemObject.Free;
  if Assigned(JSONLIBObject) then JSONLIBObject.Free;
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
  Identifiers.Add('duplicates',TIdentifierDescr.Create([BOOLEAN_OBJ],[], i_get_duplicates, i_set_duplicates));
  Identifiers.Add('sorted', TIdentifierDescr.Create([BOOLEAN_OBJ],[], i_get_sorted, i_set_sorted));
  Identifiers.Add('casesentitive', TIdentifierDescr.Create([BOOLEAN_OBJ],[], i_get_casesentitive, i_set_casesentitive));
  Identifiers.Add('text', TIdentifierDescr.Create([STRING_OBJ],[], i_get_text, i_set_text));
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

{ TSystemObject }

function TSystemObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TSystemObject.Create;
begin
  inherited Create;
  GcManualFree := True;
  FDirectory:= TDirectoryObject.Create;
  FDirectory.GcManualFree := GcManualFree;
  FFile:= TFileObject.Create;
  FFile.GcManualFree := GcManualFree;
  FPath:= TPathObject.Create;
  FPath.GcManualFree := GcManualFree;
end;

destructor TSystemObject.Destroy;
begin
  FPath.Free;
  FFile.Free;
  FDirectory.Free;
  inherited;
end;

procedure TSystemObject.IdentifierInit;
begin
  inherited;
  Identifiers.Add('Directory', TIdentifierDescr.Create([DIRECTORY_OBJ],[], i_get_directory));
  Identifiers.Add('File', TIdentifierDescr.Create([FILE_OBJ],[], i_get_file));
  Identifiers.Add('Path', TIdentifierDescr.Create([PATH_OBJ],[], i_get_path));
  Identifiers.Add('args', TIdentifierDescr.Create([ARRAY_OBJ, STRING_OBJ] ,[NUMBER_OBJ], i_get_args));
  Identifiers.Add('dirname', TIdentifierDescr.Create([STRING_OBJ] ,[], i_get_dirname));
  Identifiers.Add('filename', TIdentifierDescr.Create([STRING_OBJ] ,[], i_get_filename));
end;

function TSystemObject.Inspect: string;
begin
  Result := 'System$'+ IntToHex(Integer(Pointer(Self)), 6);
end;

function TSystemObject.i_get_args(Index: TEvalObject): TEvalObject;
var
  i:Integer;
begin
  if Assigned(Index) then
  begin
    i := TNumberObject(Index).toInt;
    if (i>=0) and (i<=ParamCount) then
      Result := TStringObject.Create(ParamStr(i))
    else
      Result := TNullObject.Create;
  end
  else
  begin
    Result := TArrayObject.CreateWithElements;
    for i := 0 to ParamCount do
      TArrayObject(Result).Elements.Add(TStringObject.Create(ParamStr(i)));
  end;
end;

function TSystemObject.i_get_directory(Index: TEvalObject): TEvalObject;
begin
  Result := FDirectory;
end;

function TSystemObject.i_get_dirname(Index: TEvalObject): TEvalObject;
begin
  Result := TStringObject.Create(ExcludeTrailingBackslash(ExtractFilePath(ParamStr(0))));
end;

function TSystemObject.i_get_file(Index: TEvalObject): TEvalObject;
begin
  Result := FFile;
end;

function TSystemObject.i_get_filename(Index: TEvalObject): TEvalObject;
begin
  Result:=nil;
  if ParamCount>0 then
    if (LowerCase(RightStr(ParamStr(1),4))='.lpi') then
     Result := TStringObject.Create(ExcludeTrailingBackslash(ExtractFilePath(ExpandFileName(ParamStr(1)))));

  if Result=nil then
    Result := i_get_dirname;
end;

function TSystemObject.i_get_path(Index: TEvalObject): TEvalObject;
begin
  Result := FPath;
end;

function TSystemObject.ObjectType: TEvalObjectType;
begin
  Result := FILE_SYSTEM_OBJ;
end;

{ TDirectoryObject }

function TDirectoryObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TDirectoryObject.Create;
begin
  inherited Create;
end;

destructor TDirectoryObject.Destroy;
begin

  inherited;
end;

function TDirectoryObject.Inspect: string;
begin
  Result := 'Directory$'+ IntToHex(Integer(Pointer(Self)), 6);
end;

procedure TDirectoryObject.MethodInit;
begin
  inherited;
  Methods.Add('Exist', TMethodDescr.Create(1, 1, [STRING_OBJ], m_Exists));
  Methods.Add('Create', TMethodDescr.Create(1, 2, [STRING_OBJ, BOOLEAN_OBJ], m_Create,' with 2nd params boolean = True, create recursive.'));
  Methods.Add('Delete', TMethodDescr.Create(1, 2, [STRING_OBJ, BOOLEAN_OBJ], m_Delete));
  Methods.Add('Copy', TMethodDescr.Create(2, 2, [STRING_OBJ, STRING_OBJ], m_Copy));
  Methods.Add('Empty', TMethodDescr.Create(1, 1, [STRING_OBJ], m_Empty));
  Methods.Add('List', TMethodDescr.Create(1, 1, [STRING_OBJ], m_List));
end;

function TDirectoryObject.m_Copy(args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
begin
  try
    TDirectory.Copy(TStringObject(args[0]).Value, TStringObject(args[1]).Value);
    Result := TBooleanObject.CreateTrue;
  except
    On E:Exception do
      Result := TErrorObject.Create(E.Message);
  end;
end;

function TDirectoryObject.m_Create(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  try
    TDirectory.CreateDirectory(TStringObject(args[0]).Value);
    Result := TBooleanObject.CreateTrue;
  except
    On E:Exception do
      Result := TErrorObject.Create(E.Message);
  end;
end;

function TDirectoryObject.m_Delete(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  try
    if (args.Count=2) then
      TDirectory.Delete(TStringObject(args[0]).Value, TBooleanObject(args[1]).Value)
    else
      TDirectory.Delete(TStringObject(args[0]).Value, False);
    Result := TBooleanObject.CreateTrue;
  except
    On E:Exception do
      Result := TErrorObject.Create(E.Message);
  end;
end;

function TDirectoryObject.m_Empty(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TBooleanObject.Create(TDirectory.IsEmpty(TStringObject(args[0]).Value));
end;

function TDirectoryObject.m_Exists(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TBooleanObject.Create(TDirectory.Exists(TStringObject(args[0]).Value));
end;

function TDirectoryObject.m_List(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  sr: TSearchRec;
  H: THashObject;
begin
  Result := TArrayObject.CreateWithElements;
  if SysUtils.FindFirst(TStringObject(args[0]).Value, faAnyFile, sr) = 0 then
  begin
    repeat
      H:=THashObject.Create;
      // -- set file attribuites --
      H.SetIdentifer('name', TStringObject.Create(sr.Name));
      H.SetIdentifer('size', TNumberObject.Create(sr.Size));
      H.SetIdentifer('type', TStringObject.Create(IfThen((sr.Attr and faDirectory <> 0),'DIR','FILE')));
      H.SetIdentifer('time', TNumberObject.Create(FileDateToDateTime(sr.Time)));
      // -- set file attribuites --
      TArrayObject(Result).Elements.Add(H);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;
function TDirectoryObject.ObjectType: TEvalObjectType;
begin
  Result := DIRECTORY_OBJ;
end;

{ TFileObject }

function TFileObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TFileObject.Create;
begin
  inherited Create;
end;

destructor TFileObject.Destroy;
begin
  inherited;
end;

function TFileObject.Inspect: string;
begin
  Result := 'File$'+ IntToHex(Integer(Pointer(Self)), 6);
end;

procedure TFileObject.MethodInit;
begin
  inherited;
  Methods.Add('Exist', TMethodDescr.Create(1, 2, [STRING_OBJ, BOOLEAN_OBJ], m_Exists));
  Methods.Add('Copy', TMethodDescr.Create(2, 3, [STRING_OBJ, STRING_OBJ, BOOLEAN_OBJ], m_Copy));
  Methods.Add('Delete', TMethodDescr.Create(1, 1, [STRING_OBJ], m_Delete));
  Methods.Add('Create', TMethodDescr.Create(1, 2, [STRING_OBJ, STRING_OBJ], m_Create));
end;

function TFileObject.m_Copy(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  try
    if args.Count=3 then
      TFile.Copy(TStringObject(args[0]).Value, TStringObject(args[1]).Value, TBooleanObject(args[1]).Value)
    else
      TFile.Copy(TStringObject(args[0]).Value, TStringObject(args[1]).Value);
    Result := TBooleanObject.CreateTrue;
  except
    On E:Exception do
      Result := TErrorObject.Create(E.Message);
  end;
end;

function TFileObject.m_Create(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  Content:string;
begin
  Content := '';
  try
    if args.Count=2 then
    begin
      Content := TStringObject(args[1]).Value;
      Content := StringReplace(Content,'\n',#13,[rfReplaceAll]);
      Content := StringReplace(Content,'\r',#10,[rfReplaceAll]);
    end;

    TFile.AppendAllText(TStringObject(args[0]).Value, Content);
    Result := m_Exists(args, env);
  except
    On E:Exception do
      Result := TErrorObject.Create(E.Message);
  end;
end;

function TFileObject.m_Delete(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  try
    TFile.Delete(TStringObject(args[0]).Value);
    Result := TBooleanObject.CreateTrue;
  except
    On E:Exception do
      Result := TErrorObject.Create(E.Message);
  end;
end;

function TFileObject.m_Exists(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TBooleanObject.Create(TFile.Exists(TStringObject(args[0]).Value));
end;

function TFileObject.ObjectType: TEvalObjectType;
begin
  Result := FILE_OBJ;
end;

{ TPathObject }

function TPathObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TPathObject.Create;
begin
  inherited Create;
end;

destructor TPathObject.Destroy;
begin
  inherited;
end;

procedure TPathObject.IdentifierInit;
begin
  inherited;
  Identifiers.Add('delimiter', TIdentifierDescr.Create([STRING_OBJ],[], i_get_pathdelimiter));
end;

function TPathObject.Inspect: string;
begin
  Result := 'Path$'+ IntToHex(Integer(Pointer(Self)), 6);
end;

function TPathObject.i_get_pathdelimiter(Index: TEvalObject): TEvalObject;
begin
  Result := TStringObject.Create(PathDelim);
end;

procedure TPathObject.MethodInit;
begin
  inherited;
  Methods.Add('ExtractFileName', TMethodDescr.Create(1, 1, [STRING_OBJ], m_ExtractFileName));
  Methods.Add('ExtractFileExt', TMethodDescr.Create(1, 1, [STRING_OBJ], m_ExtractFileExt));
  Methods.Add('join', TMethodDescr.Create(1, 1, [ARRAY_OBJ], m_Join));
end;

function TPathObject.m_ExtractFileExt(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(ExtractFileExt(TStringObject(args[0]).Value));
end;

function TPathObject.m_ExtractFileName(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := TStringObject.Create(ExtractFileName(TStringObject(args[0]).Value));
end;

function TPathObject.m_Join(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
var
  current:TEvalObject;
  n_args: TList<TEvalObject>;
  p_deli: TStringObject;
begin
  Result := nil;
  for current in TArrayObject(args[0]).Elements do
    if (current.ObjectType<>STRING_OBJ) then
    begin
      Result := TErrorObject.newError('Object Type %s not supported in join method, use only %s',[current.ObjectType, STRING_OBJ]);
      Break;
    end;

  if (Result=nil) then
  begin
    n_args := TList<TEvalObject>.Create;
    p_deli := TStringObject.Create(PathDelim);
    try
      n_args.Add(p_deli);
      Result := TArrayObject(args[0]).MethodCall('join', n_args, env);
    finally
      p_deli.Free;
      n_args.Free;
    end;
  end;
end;

function TPathObject.ObjectType: TEvalObjectType;
begin
  Result := PATH_OBJ;
end;

{ TJSONLIBObject }

function TJSONLIBObject.Clone: TEvalObject;
begin
  Result := Self;
end;

constructor TJSONLIBObject.Create;
begin
  inherited Create;
  GcManualFree := True;
end;

destructor TJSONLIBObject.Destroy;
begin

  inherited;
end;

function TJSONLIBObject.Inspect: string;
begin
  Result := 'Json$'+ IntToHex(Integer(Pointer(Self)), 6);
end;

procedure TJSONLIBObject.MethodInit;
begin
  inherited;
  Methods.Add('stringify', TMethodDescr.Create(1, 1, [ALL_OBJ], m_stringify,'return string json rappresentation of objects.'));
  Methods.Add('parse', TMethodDescr.Create(1, 1, [STRING_OBJ], m_parse,'return object rappresentation of json string.'));
  Methods.Add('validate', TMethodDescr.Create(1, 1, [STRING_OBJ], m_validate,'validate json string rappresentation.'));
end;

function JSONInnerParse(O:TJSONValue):TEvalObject;
var
  i:Integer;
begin
  Result := nil;
  if O is TJSONNumber then
    Result := TNumberObject.Create(TJSONNumber(O).AsDouble)
  else
  if O is TJSONBool then
    Result := TBooleanObject.Create(TJSONBool(O).AsBoolean)
  else
  if O is TJSONNull then
    Result := TNullObject.Create
  else
  if O is TJSONString then
    Result := TStringObject.Create(TJSONString(O).Value)
  else
  if O is TJSONObject then
  begin
    Result := THashObject.Create;
    for i := 0 to TJSONObject(O).Count-1 do
      THashObject(Result).SetIdentifer(TJSONObject(O).Get(i).JsonString.Value, JSONInnerParse(TJSONObject(O).Get(i).JsonValue));
  end
  else
  if O is TJSONArray then
  begin
    Result := TArrayObject.CreateWithElements;
    for i := 0 to TJSONArray(O).Size-1 do
      TArrayObject(Result).Elements.Add(JSONInnerParse(TJSONArray(O).Get(i)));
  end;

  if (Result=nil) then Result := TNullObject.Create;
end;

function TJSONLIBObject.m_parse(args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
var
  O:TJSONValue;
begin
  O:=TJSONObject.ParseJSONValue(TStringObject(args[0]).Value);
  try
    if Assigned(O) then
      Result := JSONInnerParse(O)
    else
      Result := TNullObject.Create;
  finally
    if Assigned(O) then O.Free;
  end;
end;

function TJSONLIBObject.m_stringify(args: TList<TEvalObject>; env: TEnvironment): TEvalObject;
begin
  Result := args[0].toJSONString;
end;

function TJSONLIBObject.m_validate(args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
var
  O:TJSONValue;
begin
  O:=TJSONObject.ParseJSONValue(TStringObject(args[0]).Value);
  try
     Result := TBooleanObject.Create(Assigned(O));
  finally
    if Assigned(O) then O.Free;
  end;
end;

function TJSONLIBObject.ObjectType: TEvalObjectType;
begin
  Result := JSON_OBJ;
end;

initialization
  init;
finalization
  deinit;

end.
