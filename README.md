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

# Loop and conditional
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
