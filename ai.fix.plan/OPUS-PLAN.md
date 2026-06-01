# Analisi anomalie LPI (interprete Monkey in Delphi) + piano di fix

## Context

LPI è un interprete del linguaggio Monkey scritto in Object Pascal con GC
mark-and-sweep ibrido (con qualche segnale di reference counting), debugger e
IDE. L'utente sospetta che il GC non funzioni in modo ottimale, soprattutto
per i return objects e le closure. Per evitare crash, oggi tutti gli oggetti
partono "raggiungibili" (`GcMark := True`) — un workaround che maschera bug
sottostanti invece di risolverli. Obiettivo: trovare le anomalie reali e
proporre un piano di fix incrementale **a parità di API pubblica** (nessun
nuovo flag/campo/metodo), così da poter applicarlo in modo conservativo.

I file analizzati (verificati direttamente, non solo via agent):

- `lp.lib/lp.environment.pas` — TEvalObject + sottoclassi (Function, Closure,
  Return, Hash, Array, ...) + TEnvironment + THashkey.
- `lp.lib/lp.evaluator.pas` — TEvaluator + TGarbageCollector (Add/Mark/Sweep).
- `lp.lib/lp.builtins.pas`, `lp.lib/lp.advobject.pas` — builtin e oggetti
  estesi (StringList, System, Path, JSON, ...).

---

## Findings prioritizzati

Legenda gravità:
- **CRIT** = corruzione memoria / use-after-free / crash latente
- **ALTA** = bug logico o leak certo
- **MEDIA** = leak intermittente, fragilità, dipende da pattern d'uso
- **BASSA** = stilistico / coerenza

### A. Garbage Collector

| # | File:line | Gravità | Problema |
|---|---|---|---|
| A1 | `lp.environment.pas:562` | **ALTA** | `GcMark := True` in `TEvalObject.Create`. Tutti gli oggetti partono già marcati. Sweep richiede `GcMark=False` → ogni oggetto sopravvive almeno 1 ciclo. È il workaround che maschera i bug sotto. |
| A2 | `lp.evaluator.pas:1170-1188` (`TGarbageCollector.Add`) | **CRIT** | `Add` registra ricorsivamente solo `ARRAY_OBJ`, `HASH_OBJ`, `RETURN_VALUE_OBJ`. **`CLOSURE_OBJ` non è coperto**: il `Env` clonato della closure e i suoi store-entry non vengono registrati. Se contengono oggetti non già in GC → leak; se sono nel GC → OK ma non garantito. |
| A3 | `lp.evaluator.pas:1268-1269` (`TGarbageCollector.Mark`) | **CRIT** | `Mark(CLOSURE_OBJ)` marca solo `Env`, **non marca `Funct`**. Se nessun altro path mantiene viva la `TFunctionObject`, la prossima Sweep la libera mentre la closure è ancora vivente → use-after-free sul `Body`/`Parameters` alla prossima chiamata. |
| A4 | `lp.evaluator.pas:1291` (`TGarbageCollector.Sweep`) | **ALTA** | Condizione di libera: `NOT GcMark AND NOT GcManualFree AND GcRefCount<=0`. Il `RefCount<=0` come **AND** invece di OR su `GcMark` rende il refcount un "ancoraggio" che blocca per sempre la sweep. Combinato con A8 (refcount mai decrementato in alcuni path) → leak permanente. |
| A5 | `lp.evaluator.pas:1244` + sottoclassi | MEDIA | `MarkChild` virtuale è dichiarato e chiamato, ma le sottoclassi (TArrayObject, THashObject, TReturnValueObject, TClosureObject) NON lo overridano. Tutta la logica di marking ricorsivo è inline nel `Mark` di TGarbageCollector con `if ObjectType=...`. Questo impedisce di marcare correttamente oggetti custom (es. `TStringListObject` o futuri builtin) che possano contenere riferimenti. |
| A6 | `lp.evaluator.pas:30` (`FGCCounter: Word`) + `:85` (`GCCounterWait = 10000`) | MEDIA | `Word` ha max 65535: se `GCCounterWait` venisse alzato sopra 65535 → overflow silenzioso e il GC non parte mai. Inoltre il commento `// default value is 100` contraddice il valore reale `10000`. |
| A7 | `lp.evaluator.pas:822-835` (`TEvaluator.Destroy`) | MEDIA | Nessuna ultima `Sweep` prima di `FGarbageCollector.Free`. Il destructor del GC libera comunque tutti i non-`GcManualFree`, ma l'ordine non è garantito rispetto a `FFunctEnv` → eventuali `Env.Free` toccano oggetti già liberati. |
| A8 | `lp.environment.pas:546-549, 578-583` + `lp.evaluator.pas:710` | ALTA | Il refcount è incrementato per **gli argomenti di funzione** e per i return value (via `env.AddRef`), ma è scarsamente decrementato. `evalExpressions` chiama `val.IncRefCount` su ogni arg; la `DecRefCount` corrispondente è in `TEnvironment.Destroy` solo per la `ReturnRef` list. Risultato: oggetti che entrano come argomento ma muoiono insieme all'environment chiamato hanno `GcRefCount>0` persistente. Combinato con A4: **mai liberati**. |
| A9 | `lp.environment.pas:1220-1221, 1252-1253` (`SetOrCreateValue`) | **CRIT** | Quando una variabile viene riassegnata, il vecchio oggetto è azzerato `GcRefCount := 0; GcMark := False`. Se quel vecchio oggetto era catturato da una closure non ancora marcata in questo ciclo, viene liberato alla prossima sweep mentre la closure ne è ancora dipendente → use-after-free. |

### B. Closure / Environment / Return

| # | File:line | Gravità | Problema |
|---|---|---|---|
| B1 | `lp.environment.pas:2290-2295` (`TClosureObject.Create`) | ALTA | `Env := AEnv.Clone`. Clone del TEnvironment è **shallow**: copia le entry dello store per riferimento, copia `Outer` per puntatore. La closure possiede un **nuovo TEnvironment** ma con `Outer` non-owned. |
| B2 | `lp.environment.pas:1137-1153` (`TEnvironment.Clone`) | ALTA | `Result.Outer := Outer;` — l'Outer è solo puntatore. Se la catena outer attraversa un env temporaneo (es. quello di una funzione che ritorna una closure annidata) e quell'env viene `Free`d in `DelEnv`, la closure ha un **dangling Outer**. Scenario reale: `fn(x){return fn(y){return fn(z){return x+y+z}}}`. |
| B3 | `lp.environment.pas:2297-2301` (`TClosureObject.Destroy`) | MEDIA | Libera `Env` (il dict clonato) ma non `Funct`. Coerente con B4: la `TFunctionObject` è solo riferimento, ma nessun marker la tiene viva (vedi A3). |
| B4 | `lp.environment.pas:1016-1019` (`TReturnValueObject.Destroy`) | MEDIA | Destructor vuoto, `Value` non liberato dal destructor stesso. Funzionalmente OK perché `Value` è registrato nel GC, ma fragile se Value non è ancora in GC (es. dato proveniente da un builtin che dimentica `Gc.Add`). |
| B5 | `lp.evaluator.pas:1047` (`Result := Gc.Add(TReturnValueObject.Create(env.AddRef(Result)))`) | ALTA | Triplo book-keeping sullo stesso oggetto: (1) `env.AddRef` incrementa refcount e aggiunge alla `Outer.ReturnRef` list; (2) `TReturnValueObject.Create` lo wrappa; (3) `Gc.Add` lo registra di nuovo via il ramo `RETURN_VALUE_OBJ`. Il refcount aumenta ad ogni return value innestato e non scende mai durante l'esecuzione. |
| B6 | `lp.environment.pas:1127-1135` (`AddRef`) | MEDIA | Aggiunge a `Outer.ReturnRef` solo se `Outer<>nil`. Se la funzione è chiamata dal `GlobalEnv` (Outer=nil), il refcount NON è incrementato (nessun anchor). Il return value sopravvive solo perché `GcMark=True` di default (A1). |
| B7 | `lp.evaluator.pas:842-850` (`extendFunctionEnv`) + `:1137` (apply su Closure) | ALTA | `Result := TEnvironment.Create(env);` per le closure passa `env = TClosureObject.Env`. Il nuovo FEnv ha `Outer := Env-clonata-dalla-closure`. Quando la chiamata termina, `DelEnv(FEnv)` libera FEnv ma non gli oggetti nel suo Store (gli args con `Reference`). Va bene se sono in GC, ma se la closure muore mentre FEnv è ancora vivo (cosa impossibile nel single-thread attuale) → dangling. |
| B8 | `lp.environment.pas:1161-1174` (`TEnvironment.Destroy`) | MEDIA | `FStore.Free` libera solo il dict (TDictionary non-owning). Gli oggetti restano in GC, OK; ma se l'env era una closure-env clonata, e la sweep successiva li libera, le altre closure che condividono i puntatori (sibling) cadono. Effetto di B1. |
| B9 | `lp.environment.pas:1061-1064` (`TFunctionObject.Clone`) | BASSA | Crea un nuovo TFunctionObject clonando Parameters e Body, ma nessun environment. Per come è usato (`TClosureObject.Clone`), perde semantica della cattura. Non critico oggi (raramente clonata), ma fonte di bug futuri. |

### C. Bug logici evaluator

| # | File:line | Gravità | Problema |
|---|---|---|---|
| C1 | `lp.evaluator.pas:731` (`assignExpression`) | **ALTA** | `if NOT isError(AValue) then` ripetuto due volte. La seconda dovrebbe essere `isError(index)`. Se `index` è un errore (es. expr che valuta a ERROR_OBJ), il check fallisce e si chiama `Setindex(index, AValue)` con un index errore → crash o silent failure. |
| C2 | `lp.evaluator.pas:615-635` (`evalForExpression`) | ALTA | `Result` non inizializzato prima del `while isTruthyWithError(..., Result)`. È var, ma `isTruthyWithError` può non sempre scrivere `Result` → `Result` ritornato dal `function` è indeterminato → AV nel caller che fa `if Result.ObjectType=...`. |
| C3 | `lp.evaluator.pas:674-685` (`evalHashLiteral`) | **CRIT** | `FreeAndNil(k)` su `k := Eval(pair.Key, env)`. `k` viene tipicamente da Eval che lo registra nel GC. Free manuale → dangling pointer nella linked list del GC → la sweep successiva tocca memoria liberata. Idem `FreeAndNil(h)` (h è THashkey, classe Delphi: Free in sé è OK ma h non è stata aggiunta a Pairs e non c'è nessuno che la libera in caso di success path se Pairs ha ownership → ok in caso errore). Cattivo design generale. |
| C4 | `lp.evaluator.pas:562` (`evalForEachExpression`, riga circa) | ALTA | `iterable := Eval(...)`; subito `iterable.isIterable`. Se `Eval` ritorna `nil`, AV. Manca `if Assigned(iterable) and iterable.isIterable then`. |
| C5 | `lp.evaluator.pas:651-691` (`evalHashLiteral`) | MEDIA | `Result := THashObject.Create;` non è seguito da `Gc.Add(Result)`. Dipende dal caller per la registrazione (l'evaluator usa `Gc.Add` ai chiamanti più alti). Se per qualche refactor il caller smette di farlo → leak silenzioso. Pattern fragile. |

### D. Anomalie builtin / advobject

| # | File:line | Gravità | Problema |
|---|---|---|---|
| D1 | `lp.advobject.pas:810-822` (`TFileObject.m_Copy`) | **ALTA** | Branch `args.Count=3` usa `TBooleanObject(args[1]).Value` come terzo argomento (overwrite flag) invece di `args[2]`. Risultato: l'overwrite flag è in realtà il **path destinazione** castato a boolean → comportamento errato/AV. |
| D2 | `lp.advobject.pas` (vari Clone) | MEDIA | `Clone` ritorna `Self` in `TStringListObject` (riga 372-375), `TSystemObject` (577-580), `TDirectoryObject` (675-678), `TFileObject` (781-784), `TPathObject` (868-871), `TJSONLIBObject` (952-955). Coerente con `GcManualFree := True`, ma rompe la semantica di Clone: modifiche al "clone" toccano l'originale. |
| D3 | `lp.advobject.pas:758-773` (`TDirectoryObject.m_List`) | MEDIA | `H := THashObject.Create;` e `TStringObject.Create(...)` dentro al loop non sono `Gc.Add`-ati esplicitamente. Vengono catturati indirettamente quando il caller `Gc.Add(Result)` (perché `Result` è ARRAY → ramo A2 fa Add ricorsivo). Funzionale, ma fragile e dipende da A2 (incompleto). |
| D4 | `lp.advobject.pas:531-539` (`TStringListObject.m_toarray`) | MEDIA | Stesso pattern: `Result := TArrayObject.Create;` e contenuti non Gc.Add-ati. Dipende dal caller. |
| D5 | `lp.advobject.pas:917-943` (`TPathObject.m_Join`) | BASSA | Crea `p_deli := TStringObject.Create(PathDelim)` manuale e fa `p_deli.Free`. Pattern OK con try/finally, ma incoerente con il resto del codice GC-based. |
| D6 | `lp.advobject.pas:23` ecc. (`InnetList`) | BASSA | Typo: probabilmente "Inner". Solo leggibilità. |

---

## Piano di fix (a parità di API)

I fix sono ordinati per dipendenza logica e rischio. Ogni fase è un commit
separato verificabile in isolamento. **Nessuna fase introduce nuovi campi,
metodi pubblici o cambia firme** — solo modifiche al corpo dei metodi.

### Fase 0 — Bug logici puntuali (basso rischio, alto valore)

Sono i bug più facili da fissare e che probabilmente causano già crash
osservabili. Falli prima per ridurre il "rumore" durante il refactor del GC.

- **C1** `assignExpression` riga 731 → `if NOT isError(index) then`.
- **C2** `evalForExpression` riga 617: aggiungere `Result := nil;` come prima
  riga del `begin`.
- **C3** `evalHashLiteral`: rimuovere tutti i `FreeAndNil(k)`. `k` è del GC.
  Per `h` (THashkey) — è creata localmente: se non viene aggiunta a `Pairs`
  (path d'errore), `h.Free` è corretto. Lasciare `FreeAndNil(h)` solo nei
  rami d'errore prima dell'`Add`. Rimuovere `FreeAndNil(Result)` (anche
  Result è gestito dal GC se il caller lo registra; meglio lasciar fare al
  GC). Verificare i caller di evalHashLiteral che facciano `Gc.Add(Result)`.
- **C4** `evalForEachExpression`: aggiungere guard `if (iterable=nil) or NOT iterable.isIterable then ...`.
- **C5** `evalHashLiteral`: aggiungere `Gc.Add(Result)` subito dopo `Result := THashObject.Create;` (idempotente grazie a `GcGarbage` check).
- **D1** `TFileObject.m_Copy`: cambiare `TBooleanObject(args[1]).Value` →
  `TBooleanObject(args[2]).Value` nel ramo `args.Count=3`.

**Verifica**: eseguire l'IDE e provare uno script come:
```
let h = {"a": 1/0, "b": 2};   // chiave/value errati per esercitare C3
let i = 0;
for(; i<3; i := i+1){ println(i); };  // C2
let a[xxx] = 5;               // C1 (index expr errato)
foreach v in nullobj { ... }; // C4
```
e gli script di `README.md` per regressione.

### Fase 1 — GC: fix correttezza di Mark / Add (medio rischio)

Mantenere flag e API identici, ma chiudere i gap di copertura:

- **A2** `TGarbageCollector.Add`: aggiungere un ramo `if ObjectType=CLOSURE_OBJ then`:
  - registrare ricorsivamente i value di `TClosureObject(Result).Env.Store`
    (chiavi sono `string`, non oggetti).
  - registrare `TClosureObject(Result).Funct` (chiama `Add` su di esso —
    TFunctionObject contiene solo AST non-GC, quindi è no-op sui figli).
  - non scendere su `Env.Outer` (l'outer environment non è un oggetto GC).
- **A3** `TGarbageCollector.Mark(AObject)`: nel ramo CLOSURE_OBJ, marcare
  anche `Funct` oltre a `Env`.
- **A5** opzionale (no nuova API): lasciare `MarkChild` virtuale come oggi e
  spostare le ramificazioni `if ObjectType=...` nel `MarkChild` delle
  rispettive sottoclassi (TArrayObject, THashObject, TReturnValueObject,
  TClosureObject). Riduce il rischio futuro per oggetti custom. Se vuoi
  restare strettamente "no cambi di design", **skippare A5**.

**Verifica**: prima/dopo il fix, eseguire uno script con molte closure
annidate (`fn(x){fn(y){fn(z){return x+y+z}}}` chiamato in loop) e contare
`Gc.Count` periodicamente. Deve calare dopo le sweep, non crescere lineare.

### Fase 2 — GC: comportamento di Sweep / refcount (medio rischio)

Il vero cuore. Cambi solo nei corpi di Sweep e nei punti che toccano refcount:

- **A1** rimuovere il workaround `GcMark := True` in `TEvalObject.Create` →
  metterlo a `False`. Da fare **DOPO** A2/A3 (altrimenti gli oggetti orfani
  vengono liberati troppo presto). Dopo questo cambio, ogni oggetto creato
  deve essere o (a) registrato nel GC tramite `Gc.Add` E raggiungibile da un
  root (env corrente), oppure (b) `GcManualFree:=True`.
- **A4** `Sweep`: rivedere la condizione. Con A1 e A2/A3 a posto, il
  refcount non deve essere un ancoraggio ortogonale. Proposta: liberare se
  `NOT GcMark AND NOT GcManualFree` e basta. Il refcount diventa diagnostic
  o si rimuove dalla condizione. Mantieni `GcRefCount` come campo per non
  rompere API.
- **A8** `evalExpressions`: l'`IncRefCount` su arg è ridondante una volta
  che A1+A2+A3 sono attivi (gli arg sono raggiungibili via env del callee
  durante Eval). Rimuovere `val.IncRefCount` riga 710 e la `DecRefCount`
  corrispondente. Conservare se serve per esecuzione multithread futura.
- **A9** `SetOrCreateValue` righe 1220-1221, 1252-1253: rimuovere
  `FStore[name].GcRefCount := 0; FStore[name].GcMark := False;`. Il vecchio
  oggetto non deve essere "forzato a morire": lascia decidere alla mark/sweep.
  Se è ancora referenziato da una closure, viene marcato; se no, sweep lo
  prende.
- **B5/B6** `env.AddRef` + return value: con A1+A4 a posto, l'`AddRef`
  diventa ridondante (il return value sale via `unwrapReturnValue` e viene
  legato all'env del chiamante che è ancora vivo). Suggerimento: trasformare
  `AddRef` in pass-through (`Result := AObject`) e neutralizzare la lista
  `Outer.ReturnRef`. **Da fare per ultimo** e con regressione attenta.

**Verifica**: tutti gli script di `README.md`, in particolare il blocco
"Closure" con `let b = closure(5); println(b(5)); println(b(10)); println(b(15));`.
Deve sempre stampare 10, 15, 20 senza AV. Far girare con `Gc.Count` stampato
periodicamente per essere certi che la sweep funzioni.

### Fase 3 — Closure / Environment / Return ownership (medio rischio)

A questo punto il GC è solido. Restano le ownership ambigue:

- **B1/B2** scelta: la `TEnvironment.Clone` deve restare shallow (non si può
  fare deep senza cambiare API). MA il problema dell'`Outer` dangling esiste
  solo per closure annidate dove l'env-outer è quello di una funzione che
  ritorna. Mitigazione conservativa: in `TClosureObject.Create`, dopo
  `Env := AEnv.Clone`, "**appiattire**" lo store: cammina la catena
  `Outer` e copia (per puntatore) tutte le entry mancanti dentro
  `Env.Store`, poi `Env.Outer := nil`. Costo: una copia extra per closure
  creata; beneficio: nessun dangling Outer mai.
- **B3** `TClosureObject.Destroy`: lasciare invariato.
- **B4** `TReturnValueObject.Destroy`: lasciare invariato (Value è in GC).
- **B7** `extendFunctionEnv`: invariato.

**Verifica**:
```
let make3 = fn(x){ return fn(y){ return fn(z){ return x+y+z }; }; };
let add12 = make3(10)(2);
println(add12(3));   // 15
```
Deve funzionare senza AV anche dopo molti GC cycle. Inoltre script con
ricorsione profonda di closure e con `let f = fn(){...}; let f = fn(){...};`
(riassegnazione) per esercitare A9.

### Fase 4 — Builtin / advobject (basso rischio)

Sono indipendenti dal resto, possono andare in qualsiasi momento:

- **D1** già in Fase 0.
- **D2** `Clone returns Self`: scelta consapevole per gli oggetti
  `GcManualFree`. Documentare con un commento di una riga (l'unica eccezione
  alla regola "no comments WHAT"). Niente cambio di codice.
- **D3/D4** `TDirectoryObject.m_List` / `TStringListObject.m_toarray`: dopo
  che Fase 1 ha completato A2 con il ramo CLOSURE_OBJ, il pattern attuale
  funziona già grazie al ramo ARRAY_OBJ ricorsivo di Add. Nessun cambio
  necessario. Verificare che il caller faccia `Gc.Add(Result)`.
- **D5** `TPathObject.m_Join`: invariato (pattern try/finally è safe).
- **D6** `InnetList` typo: rinominare in `FInnerList` con replace_all.

### Fase 5 — Hardening minore

- **A6** `FGCCounter: Word` → cambiare a `Integer` per evitare overflow
  futuri se `GCCounterWait` venisse alzato. Cambio di un tipo locale, non
  rompe API. Allineare il commento `// default value is 100` al valore reale.
- **A7** `TEvaluator.Destroy`: prima di `FGarbageCollector.Free`, chiamare
  `FGarbageCollector.Sweep` con tutti gli env già marcati / liberare prima
  `FFunctEnv` poi il GC. Riduce gli ordini di Free non deterministici.

---

## File critici da modificare

Tutte le modifiche restano in **3 file**:

- `lp.lib/lp.environment.pas` — A1, A8 (Inc/Dec), A9, B3, B4, D6, e i refcount.
- `lp.lib/lp.evaluator.pas` — A2, A3, A4, A6, A7, A8, B5, B7, C1–C5.
- `lp.lib/lp.advobject.pas` — D1, D2 (commento), D3/D4 (verifica).

Nessun cambio in `lp.lexer.pas`, `lp.parser.pas`, `lp.token.pas`,
`lp.utils.pas`, `lp.base64.pas`, `lp.builtins.pas`, ide/repl/interpreter.

---

## Funzioni e utility da riusare

- `TGarbageCollector.Add` con il check `GcGarbage=0` è già idempotente:
  posso chiamarlo liberamente anche su oggetti già registrati senza doppia
  iscrizione.
- `TEvalObject.Reference` (`lp.environment.pas:103, 174, 195, 211, 268, 418`):
  già usato in `extendFunctionEnv` per passare arg "by reference". Continuare
  ad usarlo invece di Clone per evitare deep copy.
- `unwrapReturnValue` (`lp.evaluator.pas:891`): già scarta TReturnValueObject
  in modo difensivo, riusarla ovunque si maneggi un risultato di funzione.
- `isError` (`lp.evaluator.pas:140` ca.): usarlo coerentemente — è la
  funzione mancante nella verifica errata di C1.

---

## Verifica end-to-end

1. Compilare `LPInterpreter.dproj` e `LPIde.dproj` dopo ogni fase.
2. Eseguire come smoke test gli script in `README.md` (Array, HashMap, Loop,
   Closure, Anonymous function, Object function `ARRAY.sort`).
3. Script di stress GC:
   ```
   let i = 0;
   while (i < 100000) {
     let f = fn(x){ return fn(y){ return x+y; }; };
     let g = f(i);
     g(i+1);
     let i = i+1;
   };
   ```
   La memoria del processo deve restare stabile (non crescita lineare).
   Usare il Memory Profiler dell'IDE o un loop con report `Gc.Count`.
4. Test specifici per i bug fixati:
   - C1: `let a = [1,2,3]; let a[undefined_var] = 5;` deve restituire errore,
     non AV.
   - C2: ciclo `for` con condizione che diventa subito falsa: il Result
     iniziale non deve essere garbage.
   - C3: hash con chiavi che sono espressioni in errore.
   - C4: `foreach x in null` deve restituire errore, non AV.
   - D1: `System.File.Copy("a","b",true)` deve effettivamente passare `true`
     come overwrite.
5. Eseguire il debugger IDE (`lp.ide.debugger.pas`) per assicurarsi che il
   memory inspector non mostri oggetti dangling.
