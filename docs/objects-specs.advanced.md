# Specifiche oggetti — Avanzati (lp.advobject)

Questa pagina descrive gli oggetti “avanzati” aggiunti tramite builtins (unit: `lp.lib/lp.advobject.pas`).

## Builtin constructors
In `lp.lib/lp.advobject.pas` vengono registrati:
- `StringList()` → `TStringListObject` (`STRING_LIST_OBJ`)
- `System()` → singleton `TSystemObject` (`FILE_SYSTEM_OBJ`)
- `JSON()` → singleton `TJSONLIBObject` (`JSON_OBJ`)

---

## STRING_LIST (`TStringListObject`)

### ObjectType
- `STRING_LIST`

### Identifiers (proprietà)
Fonte: `IdentifierInit` righe ~228-235.

- `duplicates: BOOLEAN` (read/write)
- `sorted: BOOLEAN` (read/write)
- `casesentitive: BOOLEAN` (read/write)
- `text: STRING` (read/write)

### Methods
Fonte: `MethodInit` righe ~294-305.

- `add(value: STRING) -> NUMBER`
  - Arg: `1..1`, types: `[STRING]`
- `indexof(value: STRING) -> NUMBER`
  - Arg: `1..1`, types: `[STRING]`
- `delete(index: NUMBER) -> STRING_LIST`
  - Arg: `1..1`, types: `[NUMBER]`
- `loadfromfile(path: STRING) -> STRING_LIST`
  - Arg: `1..1`, types: `[STRING]`
- `savetofile(path: STRING) -> STRING_LIST`
  - Arg: `1..1`, types: `[STRING]`
- `toarray() -> ARRAY`
  - Arg: `0..0`, types: `[]`
- `clear() -> STRING_LIST`
  - Arg: `0..0`, types: `[]`
- `size() -> NUMBER`
  - Arg: `0..0`, types: `[]`

### Note
- supporta indicizzazione:
  - get: `sl[i]` (NUMBER) → `STRING | NULL`
  - set: `sl[i] = "..."` (STRING)
- iterabile (`foreach`) tramite `Next()`.

---

## FILE_SYSTEM (`TSystemObject`)

### ObjectType
- `FILE_SYSTEM`

### Identifiers
Fonte: `IdentifierInit` righe ~422-431.

- `Directory: DIRECTORY` (read-only)
- `File: FILE` (read-only)
- `Path: PATH` (read-only)
- `args: ARRAY | STRING` *(indicizzabile con NUMBER)*
  - senza index: ritorna `ARRAY` con tutti i parametri
  - con index: ritorna `STRING` o `NULL`
- `dirname: STRING` (read-only)
- `filename: STRING` (read-only)

### Methods
- nessuno (usa oggetti figli)

---

## DIRECTORY (`TDirectoryObject`)

### ObjectType
- `DIRECTORY`

### Methods
Fonte: `MethodInit` righe ~517-526.

- `Exist(path: STRING) -> BOOLEAN`
  - Arg: `1..1`, types: `[STRING]`
- `Create(path: STRING, recursive?: BOOLEAN) -> BOOLEAN`
  - Arg: `1..2`, types: `[STRING, BOOLEAN]`
  - Descrizione: `with 2nd params boolean = True, create recursive.`
- `Delete(path: STRING, recursive?: BOOLEAN) -> BOOLEAN`
  - Arg: `1..2`, types: `[STRING, BOOLEAN]`
- `Copy(src: STRING, dst: STRING) -> BOOLEAN`
  - Arg: `2..2`, types: `[STRING, STRING]`
- `Empty(path: STRING) -> BOOLEAN`
  - Arg: `1..1`, types: `[STRING]`
- `List(path: STRING) -> ARRAY`
  - Arg: `1..1`, types: `[STRING]`
  - Restituisce array di hash con: `name`, `size`, `type`, `time`

### Identifiers
- nessuno

---

## FILE (`TFileObject`)

### ObjectType
- `FILE`

### Methods
Fonte: `MethodInit` righe ~622-629.

- `Exist(path: STRING, unused?: BOOLEAN) -> BOOLEAN`
  - Arg: `1..2`, types: `[STRING, BOOLEAN]`
  - Nota: l’implementazione usa solo `args[0]` (il boolean non sembra usato)
- `Copy(src: STRING, dst: STRING, overwrite?: BOOLEAN) -> BOOLEAN`
  - Arg: `2..3`, types: `[STRING, STRING, BOOLEAN]`
- `Delete(path: STRING) -> BOOLEAN`
  - Arg: `1..1`, types: `[STRING]`
- `Create(path: STRING, content?: STRING) -> BOOLEAN`
  - Arg: `1..2`, types: `[STRING, STRING]`
  - Nota: converte `\\n` e `\\r` in CR/LF prima di scrivere

### Identifiers
- nessuno

---

## PATH (`TPathObject`)

### ObjectType
- `PATH`

### Identifiers
Fonte: `IdentifierInit` righe ~704-708.

- `delimiter: STRING` (read-only)

### Methods
Fonte: `MethodInit` righe ~720-726.

- `ExtractFileName(path: STRING) -> STRING`
  - Arg: `1..1`, types: `[STRING]`
- `ExtractFileExt(path: STRING) -> STRING`
  - Arg: `1..1`, types: `[STRING]`
- `join(parts: ARRAY) -> STRING`
  - Arg: `1..1`, types: `[ARRAY]`
  - Nota: valida che `parts` contenga solo `STRING`, poi delega a `ARRAY.join(delimiter)`.

---

## JSON (`TJSONLIBObject`)

### ObjectType
- `JSON`

### Methods
Fonte: `MethodInit` righe ~795-801.

- `stringify(value: OBJECT) -> STRING`
  - Arg: `1..1`, types: `[OBJECT]`
  - Descrizione: `return string json rappresentation of objects.`
- `parse(json: STRING) -> OBJECT`
  - Arg: `1..1`, types: `[STRING]`
  - Descrizione: `return object rappresentation of json string.`
- `validate(json: STRING) -> BOOLEAN`
  - Arg: `1..1`, types: `[STRING]`
  - Descrizione: `validate json string rappresentation.`

### Identifiers
- nessuno
