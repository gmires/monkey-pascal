# Architettura dell’interprete

Questa pagina descrive l’architettura ad alto livello dell’interprete Monkey in Pascal.

## Componenti principali

A livello concettuale, un interprete Monkey segue il classico flusso:

1. **Lexer** (tokenizzazione)
2. **Parser** (costruzione dell’AST)
3. **AST** (representation del programma)
4. **Evaluator** (esecuzione/valutazione dell’AST)
5. **Environment** (scope/variabili/closure)
6. **Runtime objects** (valori a runtime: numeri, stringhe, array, hash, funzioni, builtins, ecc.)
7. **Garbage Collector** (mark-and-sweep)
8. **Debugger** (breakpoint/step/ispezione)

Nel repo sono presenti progetti separati:
- Interprete UI: `lp.interpreter/LPInterpreter.dpr`
- REPL: `lp.repl/LPRepl.dpr`
- IDE: `lp.ide/LPIde.dpr`

## Dati a runtime: oggetti e tipo

Il runtime espone il concetto di `EvalObject` (nomenclatura tipica: oggetti valutati dall’evaluator).
Ogni oggetto:
- dichiara un `ObjectType`
- espone una rappresentazione testuale (`Inspect`)
- può essere “iterabile” (per `foreach`)
- può esporre **metodi** e **identifiers** (property-like), registrati con pattern del tipo:
  - `Methods.Add(...)`
  - `Identifiers.Add(...)`
  - override di `MethodInit` / `IdentifierInit`

Questo modello è fondamentale per:
- builtins (funzioni globali)
- oggetti avanzati (es. `System`, `JSON`, `StringList`)
- metodi su tipi base (`ARRAY.sort`, ecc.)

## Environment e scope

L’`Environment` gestisce:
- binding `let name = value`
- scope annidati (funzioni/closure)
- lookup/assegnazione

Le closure catturano l’environment esterno (scope parent).

## Garbage Collector (mark-and-sweep)

Il progetto dichiara esplicitamente un GC mark-and-sweep.

Concetti tipici:
- **root set**: riferimenti “raggiungibili” dall’environment globale, stack di valutazione/evaluator, oggetti globali (builtins singletons), ecc.
- **mark phase**: visita degli oggetti raggiungibili e marcatura
- **sweep phase**: deallocazione degli oggetti non marcati

Nota dal codice (esempio visto in oggetti avanzati):
- alcuni oggetti possono essere assegnati come “non gestiti automaticamente” dal GC (es. `GcManualFree := True`) e diventare singletons (es. `System`, `JSON`) che vengono liberati su deinit.

## Debugger

Nel progetto ci sono sorgenti dedicati al debugger:
- `lp.interpreter/lpi.debugger.pas` (+ `.dfm`)
- `lp.ide/lp.ide.debugger.pas` (+ `.dfm`)

La UI del debugger tipicamente consente:
- esecuzione step-by-step
- breakpoint
- watch di variabili e stack frames
- navigazione sorgente

Per dettagli operativi vedi: [IDE e Debugger](ide-and-debugger.md).
