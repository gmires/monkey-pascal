# Linguaggio Monkey (dialetto del progetto)

Questa sezione descrive il linguaggio Monkey così come supportato dall’interprete in questo repository.

> Nota: molti esempi sono presi/adattati dal `README.md` del progetto.

## Tipi (panoramica)

Tipicamente Monkey supporta:
- `NUMBER` (numeri)
- `STRING`
- `BOOLEAN`
- `NULL`
- `ARRAY`
- `HASH` (mappa chiave/valore)
- `FUNCTION` (funzioni/closure)

Nel progetto sono presenti anche oggetti “avanzati” esposti tramite builtins (es. `StringList`, `System`, `JSON`): vedi [Builtins ed estensioni](builtins-and-extensions.md).

## Variabili

```monkey
let x = 10;
let s = "ciao";
```

## Funzioni e closure

### Funzione
```monkey
let add = fn(a, b) { return a + b; };
println(add(1, 2));
```

### Closure
```monkey
let closure = fn(x) {
  return fn(y) {
    return (y + x);
  }
};

let b = closure(5);
println(b(5));
println(b(10));
```

## Array

```monkey
let q = [1,2,3];
println(q[0]);
```

## HashMap

```monkey
let h = {"name":"giacomo","age":42};
println(h["name"]);
```

## Controllo di flusso

### if
```monkey
if (a<b){
  return a;
} else {
  return b;
};
```

### switch
```monkey
switch (a) {
  case "A", "B" {
    println("A or B");
  }
  case "C" {
    println("C");
  }
  default {
    println("other");
  }
};
```

### Operatore ternario `?`
```monkey
foreach i,v in {"name":"james","age": 42} {
  println(i + " = " + (v.type()=="NUMBER" ? v.tostring() : v) );
};
```

## Cicli

### while
```monkey
let i = 0;
while(i<10){
  println(i);
  let i = i + 1;
};
```

### for
```monkey
let a = ["a","b"];
let i = 0;

for(i<a.size(), i++){
  println(a[i]);
};
```

### foreach

Su range:
```monkey
foreach i,v in 1..10 {
  println("at index " + i.tostring() + " value = " + v.tostring() );
};
```

Su hash:
```monkey
foreach k,v in {"nome":"giacomo","cognome":"mola"} {
  println("key = " + k + ", with value = " + v);
};
```

Su string:
```monkey
foreach i,v in "string" {
  println("at index " + i.tostring() + " value = " + v );
};
```

## Controllo dei loop: `break` / `continue`

```monkey
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
```

## Funzioni anonime inline

```monkey
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

## Metodi su oggetti (esempio: definizione su ARRAY)

```monkey
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
      i := i+1
    };
    return left.sort(f).concat([self[0], right.sort(f)]);
  };
};

[3,2,5,7,1].sort(fn(x,y) { return x<y; });
```
