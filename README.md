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
let c = len(q);

let i = 0;
let u = [];
while(i<c){
  let q[i]["CFGVAL"] = "YYYZZZ";
  let u = push(u, q[i]);
  let i = i +1;
};

println(u);

let a = ["a","b"];
let i = 0;
for(i<len(a), i++){
  println(a[i]);
};

foreach i,v in 1..10 {
  println("at index " + numtostr(i) + " value = " + numtostr(v) );
};

foreach i,v in {"nome":"giacomo","cognome":"mola"} {
  println("key = " + i + ", with value = " + v);
};

foreach i,v in "string" {
  println("at index " + numtostr(i) + " value = " + v );
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
    if (typeof(v)="STRING"){
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
  println(i + " = " + (typeof(v)=="NUMBER" ? numtostr(v) : v) );
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
  for(let i = 0;i < len(x); i := i+1){
    if(f(x[i])){
      r := push(r, x[i]);
    };
  };
  return r;
};

println(filter(q,fn(x) { return x<10; }));
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

> Souce inline code testing
![Alt text](resources/SRC.png)

> AST inspector
![Alt text](resources/AST.png)

> REPL
![Alt text](resources/REPL.png)
