# monkey pascal interpreter with Garbage Collector mark-and-sweep
Code for “Writing an Interpreter in Go” by Thorsten Ball in Pascal.

# Array
```
let q = [{"CFGVAL":"XXX","CFGMOD":"LPI","CFGKEY":"MOD01"},{"CFGVAL":"XXX","CFGMOD":"LPI","CFGKEY":"MOD02"}];
```

# HashmMap
```
let q = {"CFGVAL":"XXX","CFGMOD":"LPI","CFGKEY":"MOD01"};
```

# Loop While, For, ForEach
```
let q = [{"CFGVAL":"XXX","CFGMOD":"LPI","CFGKEY":"MOD01"},{"CFGVAL":"XXX","CFGMOD":"LPI","CFGKEY":"MOD02"}];
let c = q.size();

let i = 0;
let u = [];
while(i<c){
  let q[i]["CFGVAL"] = "YYYZZZ";
  let u = u.push(q[i]);
  let i = i +1;
};

println(u);

let a = ["a","b"];
let i = 0;
for(i<a.size(), i++){
  println(a[i]);
};

foreach i,v in 1..10 {
  println("at index " + i.tostring() + " value = " + v.tostring() );
};

foreach i,v in {"nome":"giacomo","cognome":"mola"} {
  println("key = " + i + ", with value = " + v);
};

foreach i,v in "string" {
  println("at index " + i.tostring() + " value = " + v );
};

```

# Loop Control
```
let i = 0;
for(i<10; i++){
  if (i>5) { break; };
  println(i);
};

let i = 0;
for(i<10; i++){
  if (i==5) { continue; };
  println(i);
};

foreach v in [1,2,3,4,5,6,"aaaa",8,9] {
  if (v=3) {
    continue;
  } else {
    if (v.type()="STRING"){
      break;
    };
  };
};

```

# Conditional if and switch and Ternary Operator (?)
```
if (a<b){
  return a;
} else {
  return b;
};

switch (a) {
  case "A", "B" {
    ....
  }
  case "C" {
    ....
  }
  default {
    ....
  }
};

foreach i,v in {"name":"james","cognome":"mola", "age": 42} {
  println(i + " = " + (v.type()=="NUMBER" ? v.tostring() : v) );
};

```


# Closure
```
let closure = fn(x) {
  return fn(y) {
    return (y + x);
  }
};

let b = closure(5);
println(b(5));
println(b(10));
println(b(15));
```

# Anonymous function inline
```
let q = [1, 2, 3, 6, 1, 3, 45, 4];

let filter = fn(x, f) {
  let r = [];
  for(let i = 0;i < x.size(); i := i+1){
    if(f(x[i])){
      r := r.push(x[i]);
    };
  };
  return r;
};

println(filter(q,fn(x) { return x<10; }));
```

# Object function definiton
```
function ARRAY.sort = fn(f) {
  if (self.size() <= 1){
    return self;
  } else {
    let left = [];
    let right = [];
    let i = 1;
    while (i < self.size()){
      if (f(self[i], self[0]) ) {
        left := left.push(self[i]);
      } else {
        right:= right.push(self[i]);
      };
      i := i+1;
    };
    return left.sort(f).concat([self[0], right.sort(f)]); 
  };
};

[3,2,5,7,1].sort(fn(x,y) { return x<y; });
```

# expandability
with builtins function
```pascal
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

builtins.Add('println', TBuiltinObject.Create(_PrintLn));

```

with builtins new object
```pascal
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
    inherited GetIdentifer(name, Index);
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
  Result := 'StringList$'+ IntToHex(Integer(@InnetList), 6) +' (' + InnetList.Text + ')';
end;

function TStringListObject.isIterable: Boolean;
begin
  Result := True;
end;

function TStringListObject.MethodCall(method: string; args: TList<TEvalObject>;  env: TEnvironment): TEvalObject;
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
  if (method='savefromfile') then
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
  else inherited SetIdentifer(name, value, Index);
end;

function TStringListObject.Setindex(Index, value: TEvalObject): TEvalObject;
begin
  if Index.ObjectType<>NUMBER_OBJ then
    Result := TErrorObject.newError('index type <%s> not supported: %s', [Index.ObjectType, ObjectType])
  else
  if value.ObjectType<>NUMBER_OBJ then
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
```

> Souce inline code testing
![Alt text](resources/SRC.png)

> AST inspector
![Alt text](resources/AST.png)

> REPL
![Alt text](resources/REPL.png)
