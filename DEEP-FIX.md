# DEEP-FIX.md — Analisi Approfondita di Tutto il Codice

> **Interprete Monkey (Delphi)** - Tutti i bug, le criticità e le vulnerabilità trovati in ogni file del progetto.

---

## 1. Lexer (`lp.lexer.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| L1 | :red_circle: | ~291+351 | **Doppio ReadChar su escape nelle stringhe** | specialchar chiama ReadChar (per consumare il carattere dopo \), ma anche il while principale chiama ReadChar. Ogni escape salta un carattere extra. |
| L2 | :orange_circle: | ~267-280 | **ReadNumber non limita i punti decimali** | Stringhe come 1.2.3.4 sono accettate come singolo token. StrToFloatDef tronca silenziosamente. |
| L3 | :yellow_circle: | ~356-365 | **Commenti /* */ non chiusi** | Il loop while NOT ((ch='*') and (PeekChar='/')) non segnala errore se si raggiunge #0. |
| L4 | :yellow_circle: | ~46-54 | **Parametri inutilizzati** | isLetter(value) e isDigit(value) hanno un parametro value mai usato; lavorano su ch di classe. |
| L5 | :yellow_circle: | ~367-370 | **Backspace (#8) considerato whitespace** | SkipWhiteSpace rimuove #8. Non e whitespace standard. |
| L6 | :yellow_circle: | ~367-370 | **Ignora #9 (tab) nel whitespace** | Tab (#9) non nell'insieme [' ', #13, #10, #8]. I tab nel codice causano crash. |

---

## 2. Parser (`lp.parser.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| P1 | :red_circle: | ~1074 | **Cast errato in ParseNullLiteral** | TASTNumberLiteral(Result).Token := CurrToken.Clone; invece di TASTNullLiteral. Memory corruption certa. |
| P2 | :orange_circle: | ~2205 | **TASTFunctionDefineStatement.Clone usa Create invece di Clone** | Funct.Create as TASTFunctionLiteral crea istanza vuota, non clona. |
| P3 | :orange_circle: | ~114-123 | **TASTHashLiteral.FPairs come TDictionary<TASTExpression,TASTExpression>** | Basato su identita oggetto, non su uguaglianza semantica. |
| P4 | :orange_circle: | ~938-968 | **Leak in ParseHashLiteral su errore** | Se expectPeek(ttRBRACE) fallisce, le coppie gia inserite leakano. |
| P5 | :yellow_circle: | ~977-1007 | **ParseIfExpression non gestisce else if** | Ogni else richiede {} obbligatoriamente. |
| P6 | :yellow_circle: | ~1235 | **Ternary annidate vietate** | FInTernary flag impedisce nidificazione. |
| P7 | :yellow_circle: | ~1172-1228 | **ParseSwitchExpression confusa** | Supporta switch (e) e switch e ma il flusso e fragile. |
| P8 | :yellow_circle: | - | **Error recovery assente** | Nessun panic mode recovery. |

---

## 3. Token (`lp.token.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| T1 | :yellow_circle: | ~67,75-77 | **Commenti di documentazione errati** | ttMINUSMINUS commentato come "++" (dovrebbe "--"); ttBREAK, ttCONTINUE, ttFUNCTION_DEFINE commentati come "%=". |
| T2 | :yellow_circle: | ~108-175 | **Array TTypeStr disaccoppiato dall'enum** | Modifiche a TTokenType senza aggiornare TTypeStr causano crash. |

---

## 4. Evaluator & GC (`lp.evaluator.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| G1 | :red_circle: | ~1148-1190 | **Gc.Add non gestisce riferimenti circolari** | Ricorsione infinita su array autoreferenziali. |
| G2 | :red_circle: | ~1154 | **Lock GC disabilitato** | //FLock.Acquire commentato. Non thread-safe. |
| G3 | :red_circle: | ~502-506 | **== e != confrontano per identita** | left = right confronta puntatori Delphi, non il valore. |
| G4 | :orange_circle: | ~1271-1280 | **Mark(AEnvironment) ignora FReturnRef** | Oggetti in ReturnRef possono essere sweepati. |
| G5 | :orange_circle: | ~149-187 | **GC triggerato solo su statement con Result<>nil** | Se tutti gli statement restituiscono nil, GC mai eseguito. |
| G6 | :orange_circle: | ~651-688 | **evalHashLiteral leak su errore** | THashPair.Destroy non libera Key/Value (commentato). |
| G7 | :orange_circle: | ~269 | **evalMethodCall catena confusa** | Se node.Call e TASTMethodCallExpression, passa node invece di node.Call - ricorsione infinita. |
| G8 | :yellow_circle: | ~30,85 | **FGCCounter: Word ma GCCounterWait: Integer** | Overflow se GCCounterWait > 65535. |
| G9 | :yellow_circle: | ~1282-1304 | **Sweep O(n) senza generazioni** | Degrado prestazionale. |
| G10 | :yellow_circle: | ~102 | **nativeBoolToBooleanObject non in GC** | Fragile, se un chiamante dimentica Gc.Add, leak. |
| G11 | :yellow_circle: | ~707 | **IncRefCount senza controllo overflow** | Overflow fa percepire refcount <= 0 -> sweep oggetto in uso -> dangling. |
| G12 | :yellow_circle: | ~213 | **Cast diretto in evalMethodCall** | (node.Funct as TASTIdentifier).Value crasha se non e identifier. |

---

## 5. Environment & Object System (`lp.environment.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| E1 | :red_circle: | ~562 | **GcMark := True nel costruttore di TEvalObject** | Oggetti nuovi dopo Sweep non vengono mai deallocati. **Memory leak permanente.** |
| E2 | :orange_circle: | ~1982-1986 | **THashPair.Destroy commentato** | Key/Value non liberati. Leak su ogni hash literal. |
| E3 | :orange_circle: | ~1218-1224 | **SetOrCreateValue azzera flag GC del vecchio valore** | Dangling pointer per altri referenti. |
| E4 | :orange_circle: | ~1215,1247 | **Controllo FConst solo locale** | Const in environment outer non protette da sovrascrittura in inner. |
| E5 | :yellow_circle: | ~2018-2030 | **THashObject.Clone perde chiavi non NUMBER/STRING/BOOLEAN** | THashkey.fromObject setta ERROR_OBJ per altri tipi. |
| E6 | :yellow_circle: | ~2043-2061 | **Distruzione fragile di THashObject** | Se hpair.Free lancia eccezione, chiavi non liberate. |
| E7 | :yellow_circle: | ~2228-2249 | **THashObject.Setindex non verifica duplicati dopo ContainsKey** | HK leakato se Pairs.Add lancia eccezione per race. |
| E8 | :yellow_circle: | ~574-592 | **m_List non chiama FindClose su eccezioni** | Handle di ricerca leakato. |

---

## 6. Builtins (`lp.builtins.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| B1 | :yellow_circle: | ~91-131 | **Print, PrintLn, Wait ritornano nil** | Assegnare risultato a variabile -> AV su x.type(). |
| B2 | :yellow_circle: | ~229 | **Override builtin senza cleanup** | builtins['println'] := TBuiltinObject.Create(...) sovrascrive senza free del vecchio. |
| B3 | :yellow_circle: | ~113-122 | **ReadLn usa Readln su console** | Non funziona in contesto GUI. |

---

## 7. Advanced Objects (`lp.advobject.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| A1 | :orange_circle: | ~574-594 | **m_List crea THashObject senza Gc.Add** | Oggetti non marcati -> dangling dopo sweep. |
| A2 | :orange_circle: | ~803-850 | **JSONInnerParse crea oggetti senza Gc.Add** | Memory leak massivo su ogni JSON.parse(). |
| A3 | :orange_circle: | ~224,361-369 | **TStringListObject.Next/GetIndex senza Gc.Add** | Ogni foreach su StringList leakka. |
| A4 | :yellow_circle: | ~738-764 | **TPathObject.m_Join crea oggetti temporanei non tracciati** | p_deli non passato a Gc.Add. |
| A5 | :yellow_circle: | ~402-412 | **TSystemObject con GcManualFree=True** | Oggetti mai liberati automaticamente. |
| A6 | :yellow_circle: | ~654-655 | **Sostituzione \\n e \\r invertita** | \\n convertito in #13 (CR) invece di #10 (LF). |
| A7 | :yellow_circle: | ~771-776 | **TJSONLIBObject.Clone ritorna Self** | Viola contratto di Clone. |

---

## 8. REPL (`LPRepl.dpr`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| R1 | :orange_circle: | ~290-315 | **Double-free potenziale su Par** | GcManualFree=True + SetOrCreateValue + manual free in finally. Se qualcuno aggiunge Gc.Add, double-free. |
| R2 | :yellow_circle: | ~311-315 | **Par.Elements[i].Free manuale** | Elementi creati senza Gc.Add. Corretto ma fragile. |

---

## 9. IDE (`lp.ide.main.pas`, `lp.ide.module.source.pas`, `lp.ide.debugger.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| I1 | :red_circle: | ~432-454 | **GetNodeByModuleName loop infinito** | Se componente percorso non esiste, Result=nil e while(ModuleName<>'') non termina mai. **App freeze.** |
| I2 | :orange_circle: | ~861-891 | **RunProject - Par mai liberato** | try-finally di Par e vuoto (riga 891-892). Memory leak ogni esecuzione. |
| I3 | :orange_circle: | ~1099-1102 | **Override builtin senza cleanup** | Stesso problema di B2. |
| I4 | :yellow_circle: | ~536 | **MnuProjectOpenClick con CloseProject commentato** | Progetto corrente non chiuso aprendone un altro. |
| I5 | :yellow_circle: | ~1026-1038 | **UpdateNodes presuppone TLPModuleSourceFrame** | Cast non sicuro se altro controllo nel tab. |
| I6 | :yellow_circle: | ~75-115 | **LPIDebugger non verifica parametri nil** | AV se chiamata con AEnv/AEval nulli. |
| I7 | :yellow_circle: | ~181-193 | **Debugger mostra solo ultimo valore per chiave** | Variabili shadowed non visibili. |

---

## 10. Interpreter GUI (`lpi.main.pas`, `lpi.debugger.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| U1 | :yellow_circle: | ~361 | **Override builtin senza cleanup** | Stesso problema di B2. |
| U2 | :yellow_circle: | ~149-339 | **Describe non gestisce tutti i nodi AST** | Nodi non riconosciuti ignorati. |
| U3 | :yellow_circle: | ~60-79 | **Eval nel debugger non usa cache** | Lexer+Parser ricreati ogni valutazione. |

---

## 11. Components (`lp.edits.pas`)

| # | Priorità | Riga | Problema | Dettagli |
|---|----------|------|----------|----------|
| C1 | :orange_circle: | ~281,404 | **Integer(@R) su Win64 tronca puntatore a 32 bit** | SendMessage su Win64 con puntatore troncato -> **AV su memoria > 4GB**. |
| C2 | :yellow_circle: | ~458 | **TLPIListBox.WMPaint usa EM_GETFIRSTVISIBLELINE su ListBox** | Messaggio per EDIT/MEMO, non per ListBox. |
| C3 | :yellow_circle: | ~404 | **TLPIListBox.SetEditRect usa EM_SETRECT su ListBox** | Non supportato da TListBox. |

---

## 12. Tabella riassuntiva finale

### :red_circle: Critici (7)

| # | File | Riga | Problema |
|---|------|------|----------|
| | lp.parser.pas | ~1074 | Cast errato in ParseNullLiteral |
| | lp.lexer.pas | ~351 | Doppio ReadChar su escape nelle stringhe |
| | lp.environment.pas | ~562 | GcMark := True nel ctor -> memory leak permanente |
| | lp.evaluator.pas | ~1148-1190 | Ricorsione infinita in Gc.Add su strutture circolari |
| | lp.evaluator.pas | ~1154 | Lock GC commentato |
| | lp.evaluator.pas | ~502-506 | == confronta identita oggetto anziche valore |
| | lp.ide.main.pas | ~432-454 | Loop infinito in GetNodeByModuleName |

### :orange_circle: Medi (16)

| # | File | Riga | Problema |
|---|------|------|----------|
| | lp.parser.pas | ~2205 | Clone errato in TASTFunctionDefineStatement |
| | lp.parser.pas | ~114 | TDictionary con chiavi AST per identita |
| | lp.parser.pas | ~938 | Leak in ParseHashLiteral su errore |
| | lp.evaluator.pas | ~1271 | Mark ignora ReturnRef |
| | lp.evaluator.pas | ~651 | Leak in evalHashLiteral su errore |
| | lp.evaluator.pas | ~269 | evalMethodCall catena confusa (ricorsione) |
| | lp.environment.pas | ~1218 | Manipolazione flag GC in SetOrCreateValue |
| | lp.environment.pas | ~1982 | THashPair.Destroy commentato -> leak |
| | lp.advobject.pas | ~574-594 | m_List crea THashObject senza Gc.Add |
| | lp.advobject.pas | ~803-850 | JSONInnerParse crea oggetti senza Gc.Add |
| | lp.advobject.pas | ~224,361-369 | StringListObject.Next/GetIndex senza Gc.Add |
| | lp.ide.main.pas | ~861 | RunProject - Par mai liberato |
| | lp.components/lp.edits.pas | ~281,404 | Integer(@R) su Win64 tronca puntatore |
| | LPRepl.dpr | ~290 | Double-free potenziale su Par |
| | lp.lexer.pas | ~267 | Numeri malformati accettati |
| | lp.evaluator.pas | ~149 | GC non triggerato se statement restituiscono nil |

### :yellow_circle: Bassi (20+)

| # | File | Riga | Problema |
|---|------|------|----------|
| | lp.lexer.pas | ~356 | Commenti non chiusi silenziosi |
| | lp.lexer.pas | ~367 | Tab (#9) non considerato whitespace |
| | lp.lexer.pas | ~46 | Parametri inutilizzati in isLetter/isDigit |
| | lp.parser.pas | ~1235 | Ternary non annidabili |
| | lp.token.pas | ~108 | Array TTypeStr disaccoppiato dall'enum |
| | lp.token.pas | ~67,75 | Commenti documentazione errati |
| | lp.evaluator.pas | ~30,85 | Word vs Integer per GC counter |
| | lp.evaluator.pas | ~1282 | Sweep O(n) senza generazioni |
| | lp.evaluator.pas | ~102 | nativeBoolToBooleanObject non in GC |
| | lp.evaluator.pas | ~912 | applyFunction non gestisce risultato nil |
| | lp.environment.pas | ~2018 | THashObject.Clone perde chiavi non supportate |
| | lp.environment.pas | ~2043 | Distruzione fragile di THashObject |
| | lp.builtins.pas | ~91-131 | Builtin ritornano nil (AV potenziale) |
| | lp.advobject.pas | ~771 | Clone ritorna Self (viola contratto) |
| | lp.ide.main.pas | ~536 | MnuProjectOpenClick con CloseProject commentato |
| | lp.ide.main.pas | ~1026 | Cast non sicuro in UpdateNodes |
| | lp.ide.debugger.pas | ~75 | Nessun nil-check su parametri |
| | LPRepl.dpr | - | Nessuna eccezione gestita in StartProgram |
| | lp.components/lp.edits.pas | ~458 | EM_GETFIRSTVISIBLELINE su ListBox |
| | lp.components/lp.edits.pas | ~404 | EM_SETRECT su ListBox |

---

*Analisi completa del codebase in 17 unit + 4 DPR (~12.500 righe). Generato: 28 Aprile 2026.*
