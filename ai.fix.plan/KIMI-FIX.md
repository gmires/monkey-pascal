# Analisi Interprete Monkey (Delphi) — Fix e Migliorie

> **Nota**: questo documento descrive esclusivamente i problemi riscontrati e le relative correzioni da applicare. Nessuna modifica al codice sorgente è stata eseguita.

---

## Indice

1. [Parsing (`lp.lexer.pas` + `lp.parser.pas`)](#1-parsing)
2. [Garbage Collector (`lp.evaluator.pas` + `lp.environment.pas`)](#2-garbage-collector)
3. [Architetturali / Strutturali](#3-architetturali--strutturali)
4. [Tabella riassuntiva](#4-tabella-riassuntiva)

---

## 1. Parsing

### 🔴 Critico – Bug di casting in `ParseNullLiteral`

**File:** `lp.parser.pas`  
**Riga:** ~1074

```pascal
TASTNumberLiteral(Result).Token := CurrToken.Clone;
```

**Problema:** il cast è sbagliato (`TASTNumberLiteral` invece di `TASTNullLiteral`). Questo causa **memory corruption** (scrittura fuori dai bounds dell'oggetto `TASTNullLiteral`) e Access Violation imprevedibili.

**Fix:**
```pascal
TASTNullLiteral(Result).Token := CurrToken.Clone;
```

---

### 🔴 Critico – Bug nel lexer per gli escape nelle stringhe

**File:** `lp.lexer.pas`  
**Riga:** ~351

```pascal
while specialchar or NOT CharInSet(ch, ['"', #0]) do ReadChar;
```

**Problema:** `specialchar` chiama già `ReadChar` internamente (riga 291), ma il `while` ne chiama un altro. Per ogni sequenza di escape vengono consumati **due caratteri** invece di uno. Esempio: `"a\nb"` viene letto come `"a"` + newline + salto di un carattere.

**Fix:** rimuovere il `ReadChar` interno a `specialchar` e lasciare che sia il `while` a fare l'avanzamento, oppure gestire esplicitamente l'escape senza doppio `ReadChar`.

---

### 🟠 Medio – `TASTFunctionDefineStatement.Clone` usa `Create` invece di `Clone`

**File:** `lp.parser.pas`  
**Riga:** ~2205

```pascal
TASTFunctionDefineStatement(Result).Funct := Funct.Create as TASTFunctionLiteral;
```

**Problema:** `Funct.Create` crea un'istanza vuota di `TASTFunctionLiteral`, non un clone. Si perde tutto il body e i parametri.

**Fix:**
```pascal
TASTFunctionDefineStatement(Result).Funct := Funct.Clone as TASTFunctionLiteral;
```

---

### 🟠 Medio – `TASTHashLiteral` usa identità oggetto come chiave

**File:** `lp.parser.pas`  
**Riga:** ~114-123

```pascal
FPairs:TDictionary<TASTExpression, TASTExpression>;
```

**Problema:** `TDictionary<TASTExpression, TASTExpression>` si basa su `GetHashCode`/`Equals` di `TObject` (identità di istanza). Due nodi `TASTNumberLiteral` con lo stesso valore numerico sono considerati chiavi **diverse**. Questo rende l'hash literal semanticamente errato: non può esistere una chiave `1: "a"` perché la ricerca per chiave fallirebbe.

**Fix:** introdurre una classe wrapper `TASTHashKey` che implementi `GetHashCode`/`Equals` basati sul valore semantico del nodo AST, oppure usare una lista di coppie anziché un dizionario.

---

### 🟠 Medio – `ReadNumber` accetta numeri malformati

**File:** `lp.lexer.pas`  
**Riga:** ~267-280

```pascal
while CharInSet(ch, ['0'..'9', '.']) and NOT dotdot do ReadChar;
```

**Problema:** non viene contato il numero di punti decimali. Stringhe come `1.2.3` vengono accettate come singolo token numerico, causando poi `StrToFloatDef(...,0)` che restituisce `1.2` troncando silenziosamente.

**Fix:** contare i punti decimali e interrompere al secondo punto (che deve diventare token separato o generare errore lexer).

---

### 🟡 Basso – Commenti `/* */` non chiusi causano loop silenzioso

**File:** `lp.lexer.pas`  
**Riga:** ~356-365

```pascal
while NOT ((ch='*') and (PeekChar='/')) do ReadChar;
```

**Problema:** se il file termina senza `*/`, il lexer consuma tutto fino a `#0` senza segnalare errore.

**Fix:** segnalare `ttILLEGAL` con messaggio "unterminated comment" quando si incontra `#0` durante lo skip del commento.

---

### 🟡 Basso – Error recovery assente nel parser

**File:** `lp.parser.pas`

**Problema:** il parser accumula errori in `FErrors` ma non tenta il **panic mode recovery** (es. sincronizzarsi su `ttSEMICOLON` o `ttRBRACE`). Un singolo errore di sintassi fa tornare `nil` a catena, generando parsing incompleto e potenziali AV nei chiamanti.

**Fix:** implementare `synchronize` dopo un errore per saltare token fino a un punto di recovery noto.

---

### 🟡 Basso – Limitazione artificiale su ternary annidate

**File:** `lp.parser.pas`  
**Riga:** ~1235

```pascal
if NOT (FInTernary) then
  ...
else AddError('nested ternary expressions are illegal');
```

**Problema:** il parser blocca esplicitamente le ternary annidate.

**Fix:** rimuovere `FInTernary` e gestire correttamente la precedenza/associatività dell'operatore ternario (è un operatore right-associative, basta usare precedenza appropriata in Pratt parsing).

---

## 2. Garbage Collector

### 🔴 Critico – `GcMark` inizializzato a `True` causa memory leak permanente

**File:** `lp.environment.pas`  
**Riga:** ~562

```pascal
constructor TEvalObject.Create;
begin
  ...
  GcMark := True;   // <-- BUG
  ...
end;
```

**Problema:** dopo una `Sweep`, gli oggetti sopravvissuti hanno `GcMark = False`. Gli oggetti **nuovi** allocati dopo la sweep hanno `GcMark = True`. Se diventano non raggiungibili, nella prossima `Sweep` la condizione:

```pascal
if (NOT node.GcNext.GcMark and ...)
```

è **falsa**, quindi **non vengono mai deallocati**. Questo è un **memory leak progressivo** che cresce monotonicamente dopo ogni ciclo di GC.

**Fix:** inizializzare `GcMark := False` in `TEvalObject.Create`. Gli oggetti devono essere marcati esplicitamente da `Mark` per sopravvivere.

---

### 🔴 Critico – `TGarbageCollector.Add` non gestisce riferimenti circolari

**File:** `lp.evaluator.pas`  
**Riga:** ~1148-1190

```pascal
if Result.ObjectType=ARRAY_OBJ then
  for Element in TArrayObject(Result).Elements do
    Add(Element);
// idem per HASH_OBJ, RETURN_VALUE_OBJ
```

**Problema:** se un `TArrayObject` contiene se stesso (o due array si referenziano a vicenda, fattibile in un linguaggio dinamico), `Add` entra in **ricorsione infinita** → stack overflow.

**Fix:** introdurre un parametro `Visited: TList<TEvalObject>` (o un flag temporaneo su `TEvalObject`, es. `GcAdding`) per tracciare gli oggetti già visitati durante una singola chiamata `Add`.

---

### 🔴 Critico – Lock del GC disabilitato

**File:** `lp.evaluator.pas`  
**Riga:** ~1154

```pascal
try
//    FLock.Acquire;
```

**Problema:** `TGarbageCollector.Add` e `Mark`/`Sweep` sono **non thread-safe**. Se l'interprete viene usato con thread multipli (o anche solo con eventi asincroni), la linked list `FHead.GcNext` si corrompe.

**Fix:** decommentare `FLock.Acquire`/`FLock.Release` in `Add`, `Mark` e `Sweep`. Oppure, per prestazioni migliori, usare `TSpinLock` o una coda lock-free per gli oggetti da aggiungere.

---

### 🟠 Medio – `Mark` non marca `TEnvironment.ReturnRef`

**File:** `lp.evaluator.pas`  
**Riga:** ~1271-1280

```pascal
procedure TGarbageCollector.Mark(AEnvironment: TEnvironment);
begin
  for element in AEnvironment.Store.Keys do
    Mark(AEnvironment.Store[element]);
  if (AEnvironment.Outer<>nil) then
    Mark(AEnvironment.Outer);
end;
```

**Problema:** `TEnvironment` ha una lista `FReturnRef` (riga 45) che contiene oggetti con `IncRefCount` incrementato manualmente (usata da `AddRef`). Se questi oggetti non sono anche nello `Store`, **non vengono marcati** e possono essere sweepati, rendendo i riferimenti dangling.

**Fix:** aggiungere nel `Mark(AEnvironment)`:
```pascal
for current in AEnvironment.ReturnRef do
  Mark(current);
```

---

### 🟠 Medio – `evalHashLiteral` con leak su errore a metà

**File:** `lp.evaluator.pas`  
**Riga:** ~651-688

Quando `evalHashLiteral` incontra un errore dopo aver inserito alcune coppie, fa:
```pascal
FreeAndNil(k);
FreeAndNil(h);
FreeAndNil(Result);
```

**Problema:** `THashObject.Destroy` libera i `THashPair` ma **non libera Key/Value interni** (il destructor di `THashPair` è commentato, riga 1982-1986). Le coppie già inserite vengono perse nella linked list GC senza essere marcati correttamente, causando leak.

**Fix:** nel destructor `THashPair.Destroy`, decommentare `FreeAndNil(Key)` / `FreeAndNil(Value)`, oppure gestire esplicitamente il cleanup in `evalHashLiteral` iterando sulle coppie già inserite prima di `FreeAndNil(Result)`.

---

### 🟠 Medio – `TEnvironment.SetOrCreateValue` manipola direttamente flag GC

**File:** `lp.environment.pas`  
**Riga:** ~1218-1224

```pascal
FStore[name].GcRefCount := 0;
FStore[name].GcMark := False;
FStore[name].GcManualFree := False;
```

**Problema:** sovrascrivere un valore nello store azzera forzatamente i flag del vecchio oggetto, ma se qualcun altro (altro environment, closure, o array) ha un riferimento a quel vecchio oggetto, quel riferimento diventa **dangling** dopo la prossima `Sweep`.

**Fix:** non manipolare manualmente `GcRefCount`/`GcMark`. Lasciare che il sistema ibrido reference-counting + mark-and-sweep gestisca la vita dell'oggetto. Se serve un reference counting robusto, implementare `Retain`/`Release` espliciti.

---

### 🟡 Basso – `Sweep` è O(n) con singola linked list senza generazioni

**File:** `lp.evaluator.pas`  
**Riga:** ~1282-1304

```pascal
node:= FHead;
while (node.GcNext<>nil) do
  ...
```

**Problema:** tutti gli oggetti sono in un'unica lista concatenata. La sweep è lineare su **tutti** gli oggetti allocati dall'inizio.

**Fix:** introdurre **generazioni** (young/old) o almeno una struttura che permetta sweep incrementale. Gli oggetti "sopravvissuti" a diverse sweep possono essere spostati in una lista meno frequentemente controllata.

---

### 🟡 Basso – `FGCCounter` è `Word` ma `GCCounterWait` è `Integer`

**File:** `lp.evaluator.pas`  
**Riga:** ~30, ~85

```pascal
FGCCounter: Word;          // max 65535
GCCounterWait: Integer = 10000;
```

**Problema:** se un utente imposta `GCCounterWait > 65535`, `FGCCounter` overflowa e il GC viene triggerato in modo erratico.

**Fix:** cambiare `FGCCounter` in `Integer` o `Cardinal`.

---

### 🟡 Basso – `nativeBoolToBooleanObject` non passa per `Gc.Add`

**File:** `lp.evaluator.pas`  
**Riga:** ~102-105

```pascal
function nativeBoolToBooleanObject(input:Boolean): TBooleanObject;
begin
  Result := TBooleanObject.Create(input);
end;
```

**Problema:** l'oggetto viene creato ma non è automaticamente nella lista GC. Finché tutti i chiamanti fanno `Gc.Add` dopo, va bene, ma è fragile.

**Fix:** fare in modo che `nativeBoolToBooleanObject` aggiunga automaticamente l'oggetto al GC, oppure eliminare completamente la funzione e usare sempre `Gc.Add(TBooleanObject.Create(...))`.

---

## 3. Architetturali / Strutturali

### 🟠 Medio – AST e oggetti runtime sono troppo accoppiati

**File:** `lp.environment.pas`

**Problema:** `TFunctionObject` (runtime) contiene direttamente `TList<TASTIdentifier>` e `TASTBlockStatement` (AST). Questo significa che l'AST deve restare in memoria per tutta la vita della closure, impedendo di liberare l'AST originale dopo la compilazione.

**Fix:** separare completamente AST e oggetti runtime. La fase di `Eval` dovrebbe trasformare `TASTFunctionLiteral` in una struttura runtime dedicata (`TFunctionBody`, `TParamList`) che non dipenda dall'AST.

---

### 🟡 Basso – `TASTHashLiteral` usa `TDictionary` con chiavi `TASTExpression`

**File:** `lp.parser.pas`

**Problema:** oltre al problema semantico, questo crea problemi nel `Destroy` (iterare e modificare `TDictionary` nello stesso ciclo). Il codice commentato (riga 1817-1820) testimonia che l'autore ha già avuto difficoltà.

**Fix:** usare `TList<TPair<TASTExpression,TASTExpression>>` o una struttura custom.

---

## 4. Tabella riassuntiva

| Priorità | File | Riga | Problema | Tipo |
|----------|------|------|----------|------|
| 🔴 | `lp.parser.pas` | ~1074 | Cast errato in `ParseNullLiteral` | Bug/Crash |
| 🔴 | `lp.lexer.pas` | ~351 | Doppio `ReadChar` su string escape | Bug/Corruzione |
| 🔴 | `lp.environment.pas` | ~562 | `GcMark := True` nel ctor → leak | Bug/Leak |
| 🔴 | `lp.evaluator.pas` | ~1148-1190 | Ricorsione infinita in `GC.Add` su circolari | Bug/StackOverflow |
| 🔴 | `lp.evaluator.pas` | ~1154 | Lock GC commentato | Bug/ThreadSafety |
| 🟠 | `lp.parser.pas` | ~2205 | `Clone` errato in `TASTFunctionDefineStatement` | Bug/Logica |
| 🟠 | `lp.parser.pas` | ~114 | `TDictionary` con chiavi AST per identità | Bug/Semantico |
| 🟠 | `lp.evaluator.pas` | ~1271 | `Mark` ignora `ReturnRef` | Bug/Leak |
| 🟠 | `lp.evaluator.pas` | ~651-688 | Leak in `evalHashLiteral` su errore | Bug/Leak |
| 🟠 | `lp.environment.pas` | ~1218 | Manipolazione diretta flag GC | Bug/Dangling |
| 🟡 | `lp.lexer.pas` | ~267 | Numeri malformati accettati | Robustness |
| 🟡 | `lp.lexer.pas` | ~356 | Commenti non chiusi silenziosi | Robustness |
| 🟡 | `lp.evaluator.pas` | ~30,85 | Tipo `Word` vs `Integer` per GC counter | Robustness |
| 🟡 | `lp.parser.pas` | ~1235 | Ternary non annidabili | Limitazione |
| 🟡 | `lp.evaluator.pas` | ~102-105 | `nativeBoolToBooleanObject` non in GC | Fragilità |
| 🟠 | `lp.environment.pas` | vari | AST accoppiato a oggetti runtime | Architettura |
| 🟡 | `lp.parser.pas` | ~114 | `TDictionary` problematico in `TASTHashLiteral` | Architettura |

---

*Generato il: 28 Aprile 2026*
