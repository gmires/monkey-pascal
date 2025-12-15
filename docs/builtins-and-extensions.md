# Builtins ed estensioni

Questa guida spiega come estendere l’interprete aggiungendo:
- **funzioni built-in** (es. `println`)
- **nuovi oggetti runtime** (es. `System`, `JSON`, `StringList`)

## Builtin function: esempio `println`

Esempio (dal README del progetto):

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

### Linee guida
- Validare numero e tipo degli argomenti (se necessario)
- Restituire un `TEvalObject` coerente (spesso `NULL`/nil/oggetto dedicato)
- Registrare la builtin nel registro globale (`builtins.Add(name, ...)`)

## Nuovi oggetti runtime: `StringList`, `System`, `JSON`

Nel README è mostrata una unit `lp.advobject` che registra nuovi costruttori built-in:

- `StringList()` → ritorna un oggetto `StringListObject`
- `System()` → ritorna un singleton `SystemObject` con sotto-oggetti `Directory`, `File`, `Path`
- `JSON()` → ritorna un singleton `JSONLIBObject` con metodi `stringify` e `parse`

Esempio di registrazione (dal README):

```pascal
procedure init;
begin
  builtins.Add('StringList', TBuiltinObject.Create(_StringListConstructor));
  builtins.Add('System', TBuiltinObject.Create(_SystemConstructor));
  builtins.Add('JSON', TBuiltinObject.Create(_JSONLIBConstructor));
end;
```

### Pattern consigliato
1. Definisci una classe che estende l’oggetto base runtime (es. `TEvalObject`)
2. Implementa:
   - `ObjectType`
   - `Inspect`
   - (opzionale) `Clone`
3. In `MethodInit` registra i metodi invocabili dal linguaggio (es. `size()`, `add()`, `parse()`)
4. In `IdentifierInit` registra properties/identificatori (es. `System.Directory`, `Path.delimiter`)
5. Registra il costruttore come builtin

## Iterabilità (`foreach`)

Alcuni oggetti possono supportare `foreach` implementando:
- `isIterable: Boolean`
- `Next: TEvalObject`

## Best practice
- Documentare sempre:
  - firma e tipi accettati
  - comportamento in caso di errore (ritorno `ErrorObject` vs eccezione)
  - eventuali side-effect (I/O filesystem, ecc.)
- Se un oggetto è singleton e “manual free”, definire chiaramente init/deinit.
