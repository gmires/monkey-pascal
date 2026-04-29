# Analisi Vulnerabilità — SS-LPI Monkey Interpreter

> Data analisi: 2026-04-29  
> Branch: main  
> Modello: claude-sonnet-4-6

---

## Legenda Criticità

| Livello | Significato |
|---------|-------------|
| 🔴 CRITICA | Crash certo, corruzione memoria, DoS garantito, path traversal arbitrario |
| 🟠 ALTA | Bug logici gravi, nil dereference, premature collection, errori silenziati |
| 🟡 MEDIA | Comportamento scorretto, overflow, semantic bug |
| 🟢 BASSA | Design smell, convenzioni, validazioni deboli |

---

## 🔴 CRITICHE

### C-1 — Lexer: ciclo infinito su commento non chiuso (DoS)
**File:** `lp.lib/lp.lexer.pas:356–365`

```pascal
function TLexer.SkipComment:Boolean;
begin
  Result := ((ch='/') and (PeekChar='*'));
  if Result then
  begin
     while NOT ((ch='*') and (PeekChar='/')) do ReadChar; // <-- loop infinito
     ReadChar;
     ReadChar;
  end;
end;
```

Quando un commento `/* ... ` non è mai chiuso, il lexer raggiunge `ch=#0` e `PeekChar=#0`. La condizione `NOT ((#0='*') and (#0='/'))` è sempre `True`, e `ReadChar` non avanza più (è già a fine input). Il processo si blocca in un loop infinito consumando il 100% della CPU.

**Fix:** aggiungere il controllo di EOF nella condizione del while:
```pascal
while NOT (((ch='*') and (PeekChar='/')) or (ch=#0)) do ReadChar;
if ch=#0 then Exit; // commento non chiuso: trattare come errore o EOF
```

---

### C-2 — Evaluator: divisione per zero non gestita
**File:** `lp.lib/lp.evaluator.pas:324,328,346,348`

```pascal
if Op='/'  then Result:= TNumberObject.Create(LVal / RVal)   // EZeroDivide se RVal=0
if Op='%'  then Result:= TNumberObject.Create(Trunc(LVal) mod Trunc(RVal)) // SIGSEGV/EDivByZero
if Op='/=' then Result:= TNumberObject.Create(LVal / RVal)
if Op='%=' then Result:= TNumberObject.Create(Trunc(LVal) mod Trunc(RVal))
```

Per l'operatore `%` con `Trunc(RVal)=0`, Delphi solleva un `EDivByZero` non catturato che termina il processo. Per `/`, il comportamento dipende dal flag FPU (può produrre `+Inf`/`NaN` o un'eccezione).

**Fix:**
```pascal
if Op='/'  then
begin
  if RVal = 0 then Exit(TErrorObject.newError('division by zero', []))
  else Result := TNumberObject.Create(LVal / RVal)
end
if Op='%'  then
begin
  if Trunc(RVal) = 0 then Exit(TErrorObject.newError('modulo by zero', []))
  else Result := TNumberObject.Create(Trunc(LVal) mod Trunc(RVal))
end
```

---

### C-3 — File system: nessuna validazione dei percorsi (path traversal)
**File:** `lp.lib/lp.advobject.pas:334,341,531,541,554,564,580,632,645,666,677`

Ogni metodo del file system accetta percorsi arbitrari passati dallo script senza alcuna sanitizzazione:

```pascal
InnetList.LoadFromFile(TStringObject(args[0]).Value);    // ../../sensibile
InnetList.SaveToFile(TStringObject(args[0]).Value);
TDirectory.Delete(TStringObject(args[0]).Value, True);   // rm -rf /
TDirectory.Copy(src, dst);
TFile.Copy(src, dst, overwrite);
TFile.Delete(path);
TFile.AppendAllText(path, content);
```

Uno script Monkey eseguito tramite l'interprete può leggere, scrivere, cancellare o sovrascrivere qualsiasi file accessibile al processo. `Directory.Delete(path, true)` effettua una cancellazione ricorsiva senza limiti.

**Fix minimo:** definire una working directory consentita e rifiutare percorsi che la escono:
```pascal
function IsSafePath(const APath, ABaseDir: string): Boolean;
var
  Expanded: string;
begin
  Expanded := ExpandFileName(APath);
  Result := StartsStr(IncludeTrailingBackslash(ABaseDir), 
                      IncludeTrailingBackslash(Expanded));
end;
```

---

### C-4 — GC: use-after-free in `evalHashLiteral`
**File:** `lp.lib/lp.evaluator.pas:668–675`

```pascal
h:= THashkey.fromObject(k);
if h.ObjectType=ERROR_OBJ then
begin
  FreeAndNil(k);   // k viene liberato...
  FreeAndNil(h);
  FreeAndNil(Result);
  Exit(CreateErrorObj('unusable as hash key: %s', [k.ObjectType])); // ...ma qui viene letto!
end;
```

`k` è già stato aggiunto alla linked list del GC tramite la precedente chiamata a `Eval()`. Chiamare `FreeAndNil(k)` lo libera mentre il GC ha ancora un puntatore ad esso (dangling pointer nel GC). Inoltre `k.ObjectType` viene letto dopo `FreeAndNil(k)` — use-after-free garantito.

**Fix:** non liberare manualmente oggetti che il GC traccia; propagare l'errore senza chiamare Free:
```pascal
h:= THashkey.fromObject(k);
if h.ObjectType=ERROR_OBJ then
begin
  h.Free;
  FreeAndNil(Result);
  Exit(CreateErrorObj('unusable as hash key: %s', [k.ObjectType]));
end;
```

---

### C-5 — Evaluator: ricorsione infinita in `evalMethodCallExpression`
**File:** `lp.lib/lp.evaluator.pas:267–271`

```pascal
if (node.Call is TASTMethodCallExpression) then
begin
  Result:= evalMethodCallExpression(node as TASTMethodCallExpression, env); // BUG: passa node, non node.Call
end
```

Quando `node.Call` è un `TASTMethodCallExpression` (catena di chiamate tipo `a.b().c()`), il codice chiama ricorsivamente `evalMethodCallExpression` passando `node` invece di `node.Call`. Il risultato è una ricorsione infinita che termina con un `EStackOverflow`.

**Fix:**
```pascal
if (node.Call is TASTMethodCallExpression) then
begin
  Result:= evalMethodCallExpression(node.Call as TASTMethodCallExpression, env);
end
```

---

### C-6 — GC: lock commentato — thread safety assente
**File:** `lp.lib/lp.evaluator.pas:1148–1190`

```pascal
function TGarbageCollector.Add(AObject: TEvalObject):TEvalObject;
begin
  try
//    FLock.Acquire;   // <-- disabilitato
    ...
  finally
//    FLock.Release;
  end;
end;
```

`FLock: TCriticalSection` viene creato nel costruttore del GC ma non viene mai acquisito. La linked list `FHead` e il contatore `Count` sono condivisi tra i thread del debugger (`lpi.debugger.pas`) e il thread dell'evaluator. Modifiche concorrenti alla lista possono corrompere i puntatori `GcNext` producendo loop infiniti nel Sweep o crash non deterministici.

**Fix:** riabilitare il lock in `Add` e in `Sweep`. In alternativa, garantire che il GC venga acceduto solo dal thread dell'evaluator.

---

## 🟠 ALTE

### A-1 — `MethodCall`: errore "method not found" non restituito (nil silenzioso)
**File:** `lp.lib/lp.environment.pas:673–675`

```pascal
if Result=nil then
  TErrorObject.newError('method %s not found in object type %s',[method, ObjectType]);
  // Result rimane nil!
```

La chiamata a `TErrorObject.newError` crea un oggetto ma non lo assegna a `Result`. Quando un metodo non esiste sull'oggetto, la funzione ritorna `nil`. I chiamanti non si aspettano `nil` da `MethodCall` e tentano di chiamare `.ObjectType` su di esso, producendo un access violation.

**Fix:**
```pascal
if Result=nil then
  Result := TErrorObject.newError('method %s not found in object type %s',[method, ObjectType]);
```

---

### A-2 — GC: `TClosureObject.Funct` non marcato durante il Mark
**File:** `lp.lib/lp.evaluator.pas:1264–1267`

```pascal
if AObject.ObjectType=CLOSURE_OBJ then
  Mark(TClosureObject(AObject).Env);   // solo l'environment, non la funzione!
```

Quando un `TClosureObject` è vivo e viene marcato, il GC marca l'environment catturato (`Env`) ma non il `TFunctionObject` interno (`Funct`). Il `TFunctionObject` è stato aggiunto al GC tramite `Gc.Add` al momento della creazione del closure. Se non ci sono altri riferimenti ad esso, viene erroneamente liberato durante il Sweep con il closure ancora vivo, causando un dangling pointer.

**Fix:**
```pascal
if AObject.ObjectType=CLOSURE_OBJ then
begin
  Mark(TClosureObject(AObject).Env);
  Mark(TClosureObject(AObject).Funct);  // aggiungere questa riga
end;
```

---

### A-3 — Evaluator: check errore sull'operando sbagliato nell'infix expression
**File:** `lp.lib/lp.evaluator.pas:1009–1016`

```pascal
Result := Eval(TASTInfixExpression(node).Left, env);
if NOT isError(Result) then
begin
  val := Eval(TASTInfixExpression(node).Right, env);
  if isError(Result) then   // BUG: dovrebbe essere isError(val)
    Result := val
  else
    Result := evalInfixExpression(..., Result, val);
end;
```

Il check a riga 1012 testa `isError(Result)` (l'operando sinistro), ma al riga 1009 si era già verificato che `NOT isError(Result)`. Il check è quindi sempre `False`. Se l'operando destro (`val`) è un errore, non viene intercettato e viene passato a `evalInfixExpression`, che può crashare tentando operazioni su un `TErrorObject`.

**Fix:** `if isError(val) then Result := val`

---

### A-4 — Cast non verificati nei setter degli identifier
**File:** `lp.lib/lp.advobject.pas:267–291`

```pascal
function TStringListObject.i_set_casesentitive(Index, value: TEvalObject): TEvalObject;
begin
  Result := nil;
  InnetList.CaseSensitive := TBooleanObject(value).Value;  // cast diretto senza check
end;

function TStringListObject.i_set_text(Index, value: TEvalObject): TEvalObject;
begin
  Result := nil;
  InnetList.Text := TStringObject(value).Value;  // cast diretto senza check
end;
```

Se lo script passa un tipo sbagliato (es. un numero invece di un booleano), il cast produce un access violation. Il sistema `TIdentifierDescr` ha il campo `IType` per dichiarare i tipi accettati, ma la validazione non viene applicata prima di chiamare il setter.

**Fix:** aggiungere una validazione del tipo in `SetIdentifer` prima di invocare il setter, o almeno un check esplicito in ogni setter.

---

## 🟡 MEDIE

### M-1 — Range operator `..`: nessun limite di dimensione (OOM)
**File:** `lp.lib/lp.evaluator.pas:355–372`

```pascal
IVal:= Trunc(Abs(LVal));
Len := Trunc(Abs(RVal-LVal)) + 1;  // può essere miliardi
...
for i := 0 to Len-1 do
  TArrayObject(Result).Elements.Add(TNumberObject.Create(IVal));
```

`Len` è un `Integer` a 32 bit: con `LVal=-2e9` e `RVal=2e9` l'espressione `Trunc(Abs(RVal-LVal))` overflowa. Con range ragionevoli ma grandi (es. `0..10000000`) l'allocazione di milioni di `TNumberObject` esaurisce la memoria.

**Fix:** imporre un limite massimo (es. 1.000.000 elementi) e restituire un errore se superato.

---

### M-2 — `TFileObject.m_Create`: usa `AppendAllText` invece di creare/sovrascrivere
**File:** `lp.lib/lp.advobject.pas:645–664`

```pascal
TFile.AppendAllText(TStringObject(args[0]).Value, Content);
```

Il metodo si chiama `Create` ma usa `AppendAllText`, che aggiunge contenuto a un file esistente anziché crearlo ex-novo o sovrascriverlo. Semantica incoerente con il nome.

**Fix:** usare `TFile.WriteAllText` oppure documentare esplicitamente il comportamento di append.

---

### M-3 — Escape `\n` e `\r` invertiti in `m_Create`
**File:** `lp.lib/lp.advobject.pas:653–657`

```pascal
Content := StringReplace(Content,'\n',#13,[rfReplaceAll]);  // sbagliato: \n = LF = #10
Content := StringReplace(Content,'\r',#10,[rfReplaceAll]);  // sbagliato: \r = CR = #13
```

`\n` (newline, LF) è il carattere `#10`, non `#13`. `\r` (carriage return, CR) è `#13`, non `#10`. Sono invertiti, producendo file con sequenze di a capo incorrette.

**Fix:**
```pascal
Content := StringReplace(Content,'\n',#10,[rfReplaceAll]);
Content := StringReplace(Content,'\r',#13,[rfReplaceAll]);
```

---

### M-4 — GC Destroy: eccezioni silenziate mascherano double-free
**File:** `lp.lib/lp.evaluator.pas:1211–1219`

```pascal
try
  tmp.Free;
except
  Inc(ErrCount);  // ErrCount non viene mai usato né loggato
end;
```

Le eccezioni durante la distruzione del GC vengono catturate e conteggiate in `ErrCount`, che però non viene mai esaminato né loggato. Un double-free o un access violation durante la pulizia passano inosservati. 

**Fix:** loggare o rilanciare (in debug), oppure rimuovere il try/except e correggere le cause a monte.

---

### M-5 — `ArgumentValidate`: accesso fuori bounds dell'array ArgType
**File:** `lp.lib/lp.environment.pas:476–492`

```pascal
for i := 0 to args.Count-1 do
  if ((ArgType[i]<>ALL_OBJ) and (args[i].ObjectType<>ArgType[i])) then
```

Se `args.Count > Length(ArgType)`, l'accesso a `ArgType[i]` è out-of-bounds. In Delphi con `{$R+}` questo solleva un `ERangeError`; senza range check, legge memoria arbitraria. Questo può succedere se un metodo è dichiarato con `ArgMax > Length(ArgType)` per errore.

**Fix:** aggiungere `if i < Length(ArgType) then` prima del check sul tipo.

---

## 🟢 BASSE

### B-1 — Weak extension check in `i_get_filename`
**File:** `lp.lib/lp.advobject.pas:476–482`

```pascal
if (LowerCase(RightStr(ParamStr(1),4))='.lpi') then
```

Controlla solo gli ultimi 4 caratteri del percorso. Un file chiamato `evil.notlpi` passerebbe se qualcuno chiama `ParamStr(1)` con quel suffisso. Usare `ExtractFileExt` per un controllo più robusto.

---

### B-2 — `GcGarbage` usato come flag booleano ma dichiarato Integer
**File:** `lp.lib/lp.environment.pas:145`, `lp.evaluator.pas:1159–1161`

```pascal
GcGarbage:Integer;
...
if Result.GcGarbage=0 then
  Inc(Result.GcGarbage);
```

Il campo è un `Integer` ma viene usato come flag on/off (0 = non nel GC, 1 = nel GC). Il nome "GcGarbage" è fuorviante: il campo indica che l'oggetto è **già tracciato** dal GC, non che è garbage. Rinominare in `GcTracked: Boolean` migliorerebbe la leggibilità e rimuoverebbe l'ambiguità.

---

### B-3 — `TStringListObject.Clone` e `TSystemObject.Clone` ritornano `Self`
**File:** `lp.lib/lp.advobject.pas:192–195, 397–400`

```pascal
function TStringListObject.Clone: TEvalObject;
begin
  Result := Self;  // non è un vero clone
end;
```

Chiamare `.clone()` su questi oggetti dallo script non produce una copia indipendente ma restituisce lo stesso oggetto. Modifiche sul "clone" si riflettono sull'originale. Comportamento incoerente con gli altri tipi.

---

## Riepilogo

| ID | File | Criticità | Categoria |
|----|------|-----------|-----------|
| C-1 | lp.lexer.pas:356 | 🔴 CRITICA | DoS — loop infinito su `/*` non chiuso |
| C-2 | lp.evaluator.pas:324,328 | 🔴 CRITICA | Crash — divisione per zero `/` e `%` |
| C-3 | lp.advobject.pas:334+ | 🔴 CRITICA | Security — path traversal/file system illimitato |
| C-4 | lp.evaluator.pas:668 | 🔴 CRITICA | Memoria — use-after-free in evalHashLiteral |
| C-5 | lp.evaluator.pas:269 | 🔴 CRITICA | Crash — ricorsione infinita su method chain |
| C-6 | lp.evaluator.pas:1154 | 🔴 CRITICA | Thread — GC lock completamente disabilitato |
| A-1 | lp.environment.pas:674 | 🟠 ALTA | Bug — nil non assegnato, nil dereference downstream |
| A-2 | lp.evaluator.pas:1265 | 🟠 ALTA | GC — TFunctionObject non marcato nel closure |
| A-3 | lp.evaluator.pas:1012 | 🟠 ALTA | Bug logico — check errore su variabile sbagliata |
| A-4 | lp.advobject.pas:267+ | 🟠 ALTA | Safety — cast non verificati negli identifier setter |
| M-1 | lp.evaluator.pas:355 | 🟡 MEDIA | DoS soft — range operator senza limite di dimensione |
| M-2 | lp.advobject.pas:658 | 🟡 MEDIA | Bug — AppendAllText vs WriteAllText |
| M-3 | lp.advobject.pas:653 | 🟡 MEDIA | Bug — \n e \r invertiti |
| M-4 | lp.evaluator.pas:1217 | 🟡 MEDIA | Debug — eccezioni silenziate nel GC Destroy |
| M-5 | lp.environment.pas:485 | 🟡 MEDIA | Safety — ArgType accesso fuori bounds possibile |
| B-1 | lp.advobject.pas:477 | 🟢 BASSA | Weak extension check |
| B-2 | lp.environment.pas:145 | 🟢 BASSA | Naming misleading per GcGarbage |
| B-3 | lp.advobject.pas:192 | 🟢 BASSA | Clone() non clona davvero |

---

**Priorità di fix suggerita:** C-1 e C-2 (crash immediati/DoS), C-3 (sicurezza), C-4 e C-5 (corruzione memoria), A-1 (nil silenzioso molto frequente).
