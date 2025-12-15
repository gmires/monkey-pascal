# Documentazione — Monkey Pascal (Delphi 11)

Questa cartella contiene la documentazione del progetto **Monkey Interpreter in Pascal**.

## Requisiti

- **Embarcadero Delphi 11 (Alexandria)**

## Progetti principali

Nel repository sono presenti più progetti (Delphi):

- **IDE (LPI)**: `lp.ide/LPIde.dproj` (entry: `lp.ide/LPIde.dpr`)
- **Interprete**: `lp.interpreter/LPInterpreter.dproj` (entry: `lp.interpreter/LPInterpreter.dpr`)
- **REPL**: `lp.repl/LPRepl.dproj` (entry: `lp.repl/LPRepl.dpr`)
- **Group project**: `LPI.groupproj` (per aprire l’intera suite in Delphi)

## Avvio rapido (Delphi 11)

1. Apri Delphi 11.
2. Apri il group project: `LPI.groupproj`.
3. Imposta come *Startup Project* uno dei seguenti:
   - `LPIde` (IDE)
   - `LPInterpreter` (interprete)
   - `LPRepl` (REPL)
4. Compila ed esegui (Run).

## Indice dei contenuti

- [Manuale del linguaggio](language.md)
- [Architettura dell’interprete](architecture.md)
- [Builtins ed estensioni](builtins-and-extensions.md)
- [IDE e Debugger](ide-and-debugger.md)
- [Architettura oggetti di base](objects-specs.base.md)
- [Architettura oggetti avanzati](objects-specs.advanced.md)

## Note

Se vuoi includere anche istruzioni di build da riga di comando (MSBuild/Build Tools), dimmi se compili su Windows e se usi toolchain standard di Delphi 11: posso aggiungere la sezione “Build CLI”.
