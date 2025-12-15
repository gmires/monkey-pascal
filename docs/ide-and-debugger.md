# IDE (LPI) e Debugger

Questa pagina descrive i moduli IDE/debugger presenti nel repository.

## Progetti correlati

- IDE: `lp.ide/LPIde.dpr` / `lp.ide/LPIde.dproj`
- Debugger IDE:
  - `lp.ide/lp.ide.debugger.pas`
  - `lp.ide/lp.ide.debugger.dfm`
- Interpreter UI + debugger:
  - `lp.interpreter/lpi.main.pas` / `.dfm`
  - `lp.interpreter/lpi.debugger.pas` / `.dfm`

## IDE LPI (funzioni tipiche)

In base ai file presenti, l’IDE include:
- editor sorgente (modulo `lp.ide.module.source.*`)
- integrazione con l’interprete per eseguire script/progetti
- finestra debugger (step, breakpoint, stato runtime)

## Debugger (comandi tipici da documentare)

Checklist per completare la doc:
- breakpoint: aggiunta/rimozione
- step into / step over / continue / stop
- stack trace e scope variables
- watch expressions
- output console/log

## Materiale utile
- screenshot in `resources/` (es. `resources/IDE.png`)
- esempi in `examples/` (es. `examples/project-first.lpi`)
