# Specifiche oggetti — Base (lp.lib)

Questa pagina descrive le API degli **oggetti base** dell’interprete.
Gli oggetti runtime derivano da `TEvalObject` (unit: `lp.lib/lp.environment.pas`) e dichiarano:

- **Methods** tramite `Methods.Add(name, TMethodDescr.Create(ArgMin, ArgMax, ArgType[], Funct, ShortDescription?))`
- **Identifiers** tramite `Identifiers.Add(name, TIdentifierDescr.Create(IType[], IIndexType[], Getter, Setter?, ShortDescription?))`

## Riferimenti
- Sorgente: `lp.lib/lp.environment.pas`
  - `TEvalObject.MethodInit` (metodi comuni)
  - classi: `TNumberObject`, `TBooleanObject`, `TStringObject`, `TNullObject`, `TArrayObject`, `THashObject`, `TDateTimeObject`, ecc.

---

## OBJECT (`TEvalObject`)
Fonte: `lp.lib/lp.environment.pas` righe ~677-685.

### Methods (comuni a tutti)
- `tostring() -> STRING`
  - Arg: `0..0`, types: `[]`
- `toJSON() -> STRING`
  - Arg: `0..0`, types: `[]`
- `type() -> STRING`
  - Arg: `0..0`, types: `[]`
- `clone() -> OBJECT`
  - Arg: `0..0`, types: `[]`
- `ref() -> OBJECT`
  - Arg: `0..0`, types: `[]`
- `help(methodOrIdentifier?: STRING) -> STRING`
  - Arg: `0..1`, types: `[STRING]`
  - Output:
    - senza argomenti: lista metodi/inline methods/identifiers (con eventuale `ShortDescription`)
    - con argomento: descrizione firma del metodo o dettagli identifier

### Identifiers
- nessuno (default `IdentifierInit` vuoto)

---

## NUMBER (`TNumberObject`)
Fonte: `lp.lib/lp.environment.pas` righe ~900-907.

### Methods
- `format(fmt: STRING) -> STRING`
  - Arg: `1..1`, types: `[STRING]`
- `round() -> NUMBER`
  - Arg: `0..0`, types: `[]`
- `trunc() -> NUMBER`
  - Arg: `0..0`, types: `[]`
- `roundTo(decimals: NUMBER) -> NUMBER`
  - Arg: `1..1`, types: `[NUMBER]`

### Identifiers
- nessuno

---

## BOOLEAN (`TBooleanObject`)
Fonte: `lp.lib/lp.environment.pas` (non risultano `MethodInit/IdentifierInit` override nello snippet estratto).

### Methods
- solo quelli comuni di `TEvalObject` (tostring, toJSON, type, clone, ref, help)

### Identifiers
- nessuno

---

## STRING (`TStringObject`)
Fonte: `lp.lib/lp.environment.pas` (la classe dichiara `MethodInit; override;` e include molte `m_*`).

### Methods
> La lista completa dipende da `TStringObject.MethodInit`, che registra i metodi tramite `Methods.Add(...)`.
> Dal sorgente sono dichiarati (almeno) i seguenti handler, quindi ci si aspetta metodi con questi nomi:

- `tonumber() -> NUMBER` *(nome atteso: `tonumber`, handler `m_tonumber`)*
- `trim() -> STRING`
- `rtrim() -> STRING`
- `ltrim() -> STRING`
- `len() -> NUMBER`
- `copy(start: NUMBER, count: NUMBER) -> STRING`
- `left(n: NUMBER) -> STRING`
- `right(n: NUMBER) -> STRING`
- `split(sep: STRING) -> ARRAY`
- `splitAtLen(len: NUMBER) -> ARRAY` *(nome atteso: `splitAtLen`)*
- `lower() -> STRING`
- `upper() -> STRING`
- `contains(substr: STRING) -> BOOLEAN`
- `startwith(prefix: STRING) -> BOOLEAN`
- `endwith(suffix: STRING) -> BOOLEAN`
- `empty() -> BOOLEAN`
- `toBase64() -> STRING`
- `fromBase64() -> STRING`
- `urlEncode() -> STRING`
- `urlDecode() -> STRING`
- `htmlEncode() -> STRING`
- `htmlDecode() -> STRING`
- `concat(other: STRING) -> STRING`
- `indexOf(substr: STRING) -> NUMBER`
- `padStart(len: NUMBER, ch?: STRING) -> STRING`
- `padEnd(len: NUMBER, ch?: STRING) -> STRING`
- `repeat(n: NUMBER) -> STRING`
- `replace(search: STRING, replace: STRING) -> STRING`
- `toCharCode() -> ARRAY|?` *(dipende da implementazione; handler `m_to_char_code`)*
- `charCodeAt(pos: NUMBER) -> NUMBER|NULL`

**ArgMin/ArgMax/ArgType/ShortDescription**: da `TStringObject.MethodInit` (non incluso nello snippet estratto).  
Suggerimento: usa la ricerca GitHub `repo:gmires/monkey-pascal "TStringObject.MethodInit"` per catturare tutte le righe `Methods.Add`.

### Identifiers
- non risultano `IdentifierInit` override nello snippet estratto ⇒ nessuna proprietà specifica (salvo presenza altrove)

---

## NULL (`TNullObject`)
Fonte: `lp.lib/lp.environment.pas` (nessun `MethodInit/IdentifierInit` override visibile nello snippet estratto).

### Methods
- solo metodi comuni di `TEvalObject`

### Identifiers
- nessuno

---

## ARRAY (`TArrayObject`)
Fonte: `lp.lib/lp.environment.pas` (handler elencati nella classe).

### Methods
- `size() -> NUMBER`
  - handler: `m_size`
- `first() -> OBJECT|NULL`
  - handler: `m_first`
- `last() -> OBJECT|NULL`
  - handler: `m_last`
- `rest() -> ARRAY`
  - handler: `m_rest`
- `push(value: OBJECT) -> ARRAY`
  - handler: `m_push`
- `insert(index: NUMBER, value: OBJECT) -> ARRAY`
  - handler: `m_insert`
- `join(delimiter: STRING) -> STRING` *(atteso, perché esiste handler `m_join`)*
- `delete(index: NUMBER) -> ARRAY`
  - handler: `m_delete`

**ArgMin/ArgMax/ArgType/ShortDescription**: da `TArrayObject.MethodInit` (non incluso nello snippet estratto).

### Identifiers
- non risultano `IdentifierInit` override nello snippet estratto ⇒ nessuna proprietà specifica

---

## HASH (`THashObject`)
Fonte: `lp.lib/lp.environment.pas` (handler elencati nella classe).

### Methods
- `size() -> NUMBER`
  - handler: `m_size`
- `keys() -> ARRAY`
  - handler: `m_keys`
- `delete(key: STRING|NUMBER|BOOLEAN|...) -> HASH`
  - handler: `m_delete`

**ArgMin/ArgMax/ArgType/ShortDescription**: da `THashObject.MethodInit` (non incluso nello snippet estratto).

### Identifiers (proprietà)
`THashObject` override `GetIdentifer/SetIdentifer`, quindi supporta proprietà dinamiche in stile:
- `h.name` come sugar per `h["name"]` (implementazione dipendente dal codice interno)

Non risultano però `Identifiers.Add(...)` statici (da verificare nel sorgente completo).

---

## DATETIME (`TDateTimeObject`)
Fonte: `lp.lib/lp.environment.pas` (override `IdentifierInit` + `MethodInit` dichiarati).

### Identifiers (registrati in `IdentifierInit`)
Proprietà attese (getter presenti):
- `date`
- `year`
- `month`
- `day`
- `hour`
- `minute`
- `second`
- `millisecond`

**Tipi e descrizioni**: da `TDateTimeObject.IdentifierInit` (non incluso nello snippet estratto).

### Methods (registrati in `MethodInit`)
Handler presenti (metodi attesi):
- `valid(...)`
- `encode(...)`
- `decode(...)`
- `format(...)`
- `yearsBetween(dt)`, `monthsBetween(dt)`, `weeksBetween(dt)`, `daysBetween(dt)`, `hoursBetween(dt)`, `minutesBetween(dt)`, `secondsBetween(dt)`, `millisecondsBetween(dt)`
- `yearsAdd(n)`, `monthsAdd(n)`, `weeksAdd(n)`, `daysAdd(n)`, `hoursAdd(n)`, `minutesAdd(n)`, `secondsAdd(n)`, `millisecondsAdd(n)`

**ArgMin/ArgMax/ArgType/ShortDescription**: da `TDateTimeObject.MethodInit` (non incluso nello snippet estratto).

---

## Altri oggetti base (senza Methods/Identifiers proprie nello snippet)
- `FUNCTION` (`TFunctionObject`) – nessuna lista Methods/Identifiers osservata nello snippet
- `CLOSURE` (`TClosureObject`) – nessuna lista Methods/Identifiers osservata nello snippet
- `BUILTIN` (`TBuiltinObject`) – nessuna lista Methods/Identifiers osservata nello snippet
- `ERROR` (`TErrorObject`) – nessuna lista Methods/Identifiers osservata nello snippet
- `RETURN_VALUE` (`TReturnValueObject`) – nessuna lista Methods/Identifiers osservata nello snippet
- `LOOP` (`TLoopObject`) – nessuna lista Methods/Identifiers osservata nello snippet

> Nota: questi oggetti ereditano comunque i metodi comuni di `TEvalObject`.
