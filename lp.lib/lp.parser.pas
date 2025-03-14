unit lp.parser;

interface

uses SysUtils, Classes, Generics.Collections
  , lp.token, lp.lexer;

type
  TEXPrecedence = (
	  LOWEST
	  , LOGICAL
    , TERNARY
	  , EQUALS
	  , LESSGREATER
	  , SUM
	  , PRODUCT
	  , PREFIX
	  , CALL
	  , DOTDOT
	  , INDEX
    , HIGHEST
  );

  TASTNode = class
  public
    function toString:string; virtual;
    function Clone:TASTNode; virtual;
  end;

  TASTStatement = class(TASTNode)
  public
    Token: TToken;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTExpression = class(TASTNode)
  public
    Token: TToken;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTProgram = class(TASTNode)
    FStatements:TList<TASTStatement>;
  public
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Statements:TList<TASTStatement> read FStatements;
  end;

  TASTNumberLiteral = class(TASTExpression)
  public
    Value: Double;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
  end;

  TASTStringLiteral = class(TASTExpression)
  public
    Value: string;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
  end;

  TASTNullLiteral = class(TASTExpression)
  public
    function toString:string; override;
    function Clone:TASTNode; override;
  public
  end;

  TASTArrayLiteral = class(TASTExpression)
    FElements:TList<TASTExpression>;
  public
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Elements:TList<TASTExpression> read FElements write FElements;
  end;

  TASTIndexExpression = class(TASTExpression)
  public
    Left: TASTExpression;
    Index: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTIdentifier = class(TASTExpression)
  public
    Value: string;
    function toString:string; override;
    function Clone:TASTNode; override;
  end;

  TASTHashLiteral = class(TASTExpression)
    FPairs:TDictionary<TASTExpression, TASTExpression>;
  public
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Pairs:TDictionary<TASTExpression, TASTExpression> read FPairs write FPairs;
  end;

  TASTBoolean = class(TASTExpression)
  public
    Value: Boolean;
    function toString:string; override;
    function Clone:TASTNode; override;
  end;

  TASTPrefixExpression = class(TASTExpression)
  public
    Op:string;
    Right: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTInfixExpression = class(TASTExpression)
  public
    Left: TASTExpression;
    Op:string;
    Right: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTPostfixExpression = class(TASTExpression)
  public
    Left: TASTExpression;
    Op:string;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTAssignExpression = class(TASTExpression)
  public
    Name: TASTExpression;
    Op: string;
    Expression: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTExpressionStatement = class(TASTStatement)
  public
    Expression: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTImportStatement = class(TASTStatement)
  public
    Module: string;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTLetStatement = class(TASTStatement)
  public
    Name: TASTExpression;
    Expression: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTConstStatement = class(TASTStatement)
  public
    Name: TASTExpression;
    Expression: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTReturnStatement = class(TASTStatement)
  public
    ReturnValue: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTBlockStatement = class(TASTStatement)
    FStatements:TList<TASTStatement>;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Statements:TList<TASTStatement> read FStatements;
  end;

  TASTIfExpression = class(TASTExpression)
  public
    Condition: TASTExpression;
    Consequence: TASTBlockStatement;
    Alternative: TASTBlockStatement;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTTernaryExpression = class(TASTExpression)
  public
    Condition: TASTExpression;
    IfTrue: TASTExpression;
    IfFalse: TASTExpression;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTCaseExpression = class(TASTExpression)
  public
    Values: TList<TASTExpression>;
    Default: Boolean;
    Body: TASTBlockStatement;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTSwitchExpression = class(TASTExpression)
  public
    Value: TASTExpression;
    Choises: TList<TASTCaseExpression>;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTWhileExpression = class(TASTExpression)
  public
    Condition: TASTExpression;
    Body: TASTBlockStatement;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTForExpression = class(TASTExpression)
  public
    Condition: TASTExpression;
    Expression: TASTNode;
    Body: TASTBlockStatement;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTFunctionLiteral = class(TASTExpression)
    Parameters:TList<TASTIdentifier>;
    Body: TASTBlockStatement;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TASTCallExpression = class(TASTExpression)
    Funct:TASTExpression;
    Args: TList<TASTExpression>;
    function toString:string; override;
    function Clone:TASTNode; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TParsePrefixExpression = function:TASTExpression of object;
  TParseInfixExpression = function(AExpression:TASTExpression):TASTExpression of object;
  TParsePostfixExpression = function(AExpression:TASTExpression):TASTExpression of object;

  TParser = class
    FInTernary:Boolean;
    FErrors: TStringList;
    Lexer: TLexer;
    PrecToken:TToken;
    CurrToken:TToken;
    PeekToken:TToken;
    Precedences: TDictionary<TTokenType,TEXPrecedence>;
    PrefixFuncts: TDictionary<TTokenType,TParsePrefixExpression>;
    InfixFuncts: TDictionary<TTokenType,TParseInfixExpression>;
    PostfixFuncts: TDictionary<TTokenType,TParsePostfixExpression>;
    procedure AddError(Value:string);
    procedure nextToken;
    function  peekTokenIs(TType:TTokenType): Boolean;
    function  currTokenIs(TType:TTokenType): Boolean;
    function  currPrecedence: TEXPrecedence;
    function  peekPrecedence: TEXPrecedence;
    function  expectPeek(TType:TTokenType):Boolean;
    // ---------
    function ParseStatement: TASTStatement;
    function ParseExpressionStatement: TASTExpressionStatement;
    function ParseLetStatement: TASTLetStatement;
    function ParseConstStatement: TASTConstStatement;
    function ParseImportStatement: TASTImportStatement;
    function ParseReturnStatement: TASTReturnStatement;
    function ParseBlockStatement: TASTBlockStatement;
    // ---------
    function ParseExpression(APrecedence:TEXPrecedence): TASTExpression;
    function ParseNumberLiteral:TASTExpression;
    function ParseStringLiteral:TASTExpression;
    function ParseNullLiteral:TASTExpression;
    function ParseArrayLiteral:TASTExpression;
    function ParseHashLiteral:TASTExpression;
    function ParseExpressionsList(endTokenType:TTokenType):TList<TASTExpression>;
    function ParseIndexExpression(left:TASTExpression):TASTExpression;
    function ParseTernaryExpression(condition:TASTExpression):TASTExpression;
    function ParsePrefixExpression:TASTExpression;
    function ParseInfixExpression(left:TASTExpression):TASTExpression;
    function ParsePostfixExpression(left:TASTExpression):TASTExpression;
    function ParseGroupExpression:TASTExpression;
    function ParseIdentifier:TASTExpression;
    function ParseBoolean:TASTExpression;
    function ParseIfExpression:TASTExpression;
    function ParseSwitchExpression:TASTExpression;
    function ParseWhileExpression:TASTExpression;
    function ParseForExpression:TASTExpression;
    function ParseFunctionLiteral:TASTExpression;
    function ParseFunctionParameters:TList<TASTIdentifier>;
    function ParseCallExpression(funct:TASTExpression):TASTExpression;
    function ParseAssignExpression(left:TASTExpression):TASTExpression;
//    function ParseCallArguments:TList<TASTExpression>;
    // -----------
  public
    constructor Create(ALexer:TLexer);
    destructor Destroy; override;

    function ParseProgram: TASTProgram;
    property Errors: TStringList read FErrors;
  end;

implementation

procedure FreeAndNilAssigned(var Obj);
begin
  if Assigned(TObject(Obj)) then
    FreeAndNil(Obj);
end;

{ TParser }

procedure TParser.AddError(Value: string);
begin
 FErrors.Add(Value);
end;

constructor TParser.Create(ALexer: TLexer);
begin
  FInTernary:= False;
  FErrors := TStringList.Create;

  Precedences:= TDictionary<TTokenType,TEXPrecedence>.Create;
  Precedences.Add(ttQUESTION, TERNARY);
  Precedences.Add(ttEQ, TEXPrecedence.EQUALS);
  Precedences.Add(ttNOT_EQ, TEXPrecedence.EQUALS);
  Precedences.Add(ttLT, LESSGREATER);
  Precedences.Add(ttGT, LESSGREATER);
  Precedences.Add(ttLE, LESSGREATER);
  Precedences.Add(ttGE, LESSGREATER);
  Precedences.Add(ttPLUS, SUM);
  Precedences.Add(ttPLUSASSIGN, SUM);
  Precedences.Add(ttMINUS, SUM);
  Precedences.Add(ttMINUSASSIGN, SUM);
  Precedences.Add(ttSLASH, PRODUCT);
  Precedences.Add(ttSLASHASSIGN, PRODUCT);
  Precedences.Add(ttASTERISK, PRODUCT);
  Precedences.Add(ttASTERISKASSIGN, PRODUCT);
  Precedences.Add(ttLPAREN, CALL);
  Precedences.Add(ttVARASSIGN, CALL);
  Precedences.Add(ttLBRACKET, INDEX);
  Precedences.Add(ttLOGICALAND, LOGICAL);
  Precedences.Add(ttLOGICALOR, LOGICAL);
  Precedences.Add(ttDOTDOT, DOTDOT);

  PrefixFuncts:= TDictionary<TTokenType,TParsePrefixExpression>.Create;
  PrefixFuncts.Add(ttIDENT, ParseIdentifier);
  PrefixFuncts.Add(ttNUMBER, ParseNumberLiteral);
  PrefixFuncts.Add(ttSTRING, ParseStringLiteral);
  PrefixFuncts.Add(ttNULL, ParseNullLiteral);
  PrefixFuncts.Add(ttBANG, ParsePrefixExpression);
  PrefixFuncts.Add(ttMINUS, ParsePrefixExpression);
  PrefixFuncts.Add(ttTRUE, ParseBoolean);
  PrefixFuncts.Add(ttFALSE, ParseBoolean);
  PrefixFuncts.Add(ttLPAREN, ParseGroupExpression);
	PrefixFuncts.Add(ttIF, ParseIfExpression);
	PrefixFuncts.Add(ttSWITCH, ParseSwitchExpression);
	PrefixFuncts.Add(ttFUNCTION, ParseFunctionLiteral);
	PrefixFuncts.Add(ttLBRACKET, ParseArrayLiteral);
	PrefixFuncts.Add(ttLBRACE, ParseHashLiteral);
	PrefixFuncts.Add(ttWHILE, ParseWhileExpression);
	PrefixFuncts.Add(ttFOR, ParseForExpression);

  InfixFuncts := TDictionary<TTokenType,TParseInfixExpression>.Create;
  InfixFuncts.Add(ttPLUS, ParseInfixExpression);
  InfixFuncts.Add(ttMINUS, ParseInfixExpression);
  InfixFuncts.Add(ttSLASH, ParseInfixExpression);
  InfixFuncts.Add(ttASTERISK, ParseInfixExpression);
	InfixFuncts.Add(ttEQ, ParseInfixExpression);
	InfixFuncts.Add(ttNOT_EQ, ParseInfixExpression);
	InfixFuncts.Add(ttLT, ParseInfixExpression);
	InfixFuncts.Add(ttGT, ParseInfixExpression);
	InfixFuncts.Add(ttLE, ParseInfixExpression);
	InfixFuncts.Add(ttGE, ParseInfixExpression);
	InfixFuncts.Add(ttLPAREN, ParseCallExpression);
	InfixFuncts.Add(ttLBRACKET, ParseIndexExpression);
	InfixFuncts.Add(ttLOGICALAND, ParseInfixExpression);
	InfixFuncts.Add(ttLOGICALOR, ParseInfixExpression);
	InfixFuncts.Add(ttVARASSIGN, ParseAssignExpression);
	InfixFuncts.Add(ttPLUSASSIGN, ParseAssignExpression);
	InfixFuncts.Add(ttMINUSASSIGN, ParseAssignExpression);
	InfixFuncts.Add(ttASTERISKASSIGN, ParseAssignExpression);
	InfixFuncts.Add(ttSLASHASSIGN, ParseAssignExpression);
	InfixFuncts.Add(ttQUESTION, ParseTernaryExpression);
	InfixFuncts.Add(ttDOTDOT, ParseInfixExpression);

  PostfixFuncts := TDictionary<TTokenType,TParsePostfixExpression>.Create;
	PostfixFuncts.Add(ttPLUSPLUS, ParsePostfixExpression);
	PostfixFuncts.Add(ttMINUSMINUS, ParsePostfixExpression);

  Lexer := ALexer;
  PrecToken:=nil;
  CurrToken:=nil;
  PeekToken:=nil;

  nextToken;
  nextToken;
end;

function TParser.currPrecedence: TEXPrecedence;
begin
  if Precedences.ContainsKey(CurrToken.TokenType) then
    Result := Precedences[CurrToken.TokenType]
  else
    Result := LOWEST;
end;

function TParser.currTokenIs(TType: TTokenType): Boolean;
begin
  Result := (CurrToken.TokenType = TType);
end;

destructor TParser.Destroy;
begin
  FreeAndNilAssigned(PrecToken);
  FreeAndNilAssigned(CurrToken);
  FreeAndNilAssigned(PeekToken);

  PrefixFuncts.Free;
  InfixFuncts.Free;
  PostfixFuncts.Free;
  Precedences.Free;

  FErrors.Free;
end;

function TParser.expectPeek(TType: TTokenType): Boolean;
begin
  Result := peekTokenIs(TType);
  if Result then
    nextToken
  else
    AddError('Error Token : ' + PeekToken.toString  + ', expected : ' + TokenTypeToStr(TType));
end;

procedure TParser.nextToken;
begin
  FreeAndNilAssigned(PrecToken);

  PrecToken := CurrToken;
  CurrToken := PeekToken;
  PeekToken := Lexer.NextToken;
end;

function TParser.ParseArrayLiteral: TASTExpression;
begin
  Result := TASTArrayLiteral.Create;
  Result.Token := CurrToken.Clone;

  TASTArrayLiteral(Result).Elements := ParseExpressionsList(ttRBRACKET);
end;

function TParser.ParseAssignExpression(left: TASTExpression): TASTExpression;
begin
  Result := TASTAssignExpression.Create;
  TASTAssignExpression(Result).Token := CurrToken.Clone;
  TASTAssignExpression(Result).Op  := CurrToken.Literal;
  TASTAssignExpression(Result).Name:= left;

	nextToken;

	TASTAssignExpression(Result).Expression := parseExpression(LOWEST);
end;

function TParser.ParseBlockStatement: TASTBlockStatement;
var
  stmt : TASTStatement;
begin
  Result := TASTBlockStatement.Create;
  Result.Token := CurrToken.Clone;

  nextToken;
  while ((CurrToken.TokenType<>ttRBRACE) and NOT (CurrToken.TokenType=ttEOF)) do
  begin
    stmt := ParseStatement;
    if (stmt<>nil) then
      Result.Statements.Add(stmt);

    nextToken;
  end;
end;

function TParser.ParseBoolean: TASTExpression;
begin
  Result := TASTBoolean.Create;
  Result.Token := CurrToken.Clone;
  TASTBoolean(Result).Value:= (CurrToken.TokenType = ttTRUE);
end;

(*
function TParser.ParseCallArguments: TList<TASTExpression>;
begin
  Result := TList<TASTExpression>.Create;

  if NOT peekTokenIs(ttRPAREN) then
  begin
    nextToken;
    Result.Add(ParseExpression(LOWEST));

    while peekTokenIs(ttCOMMA) do
    begin
      nextToken;
      nextToken;

      Result.Add(ParseExpression(LOWEST));
    end;

    expectPeek(ttRPAREN);
  end
  else nextToken;
end;
*)

function TParser.ParseCallExpression(funct: TASTExpression): TASTExpression;
begin
  Result := TASTCallExpression.Create;
  Result.Token := CurrToken.Clone;
  TASTCallExpression(Result).Funct := funct;
  TASTCallExpression(Result).Args := ParseExpressionsList(ttRPAREN); // -- ParseCallArguments;
end;

function TParser.ParseConstStatement: TASTConstStatement;
begin
  Result := TASTConstStatement.Create;
  Result.Token := CurrToken.Clone;
  if expectPeek(ttIDENT) then
  begin
    Result.Name := ParseExpression(LOWEST);
    if expectPeek(ttASSIGN) then
    begin
      nextToken;
      Result.Expression := ParseExpression(LOWEST);

      if peekTokenIs(ttSEMICOLON) then
        nextToken;
    end
    else FreeAndNil(Result);
  end
  else FreeAndNil(Result);
end;

function TParser.ParseExpression(APrecedence:TEXPrecedence): TASTExpression;
var
  prefixFn: TParsePrefixExpression;
  infixFn: TParseInfixExpression;
  postfixFn: TParsePostfixExpression;
begin
  Result:= nil;
  if PrefixFuncts.TryGetValue(CurrToken.TokenType, prefixFn) then
  begin
    Result := prefixFn();
    if PostfixFuncts.TryGetValue(PeekToken.TokenType, postfixFn) then
    begin
      nextToken;
      Result := postfixFn(Result);
    end;

    while NOT peekTokenIs(ttSEMICOLON) and (APrecedence<peekPrecedence) do
    begin
      if InfixFuncts.TryGetValue(PeekToken.TokenType, infixFn) then
      begin
        nextToken;
        Result := infixFn(Result);
      end
      else Break;
    end;

  end;
end;

function TParser.ParseExpressionsList(endTokenType: TTokenType): TList<TASTExpression>;
begin
  Result := TList<TASTExpression>.Create;

  if NOT peekTokenIs(endTokenType) then
  begin
    nextToken;
    Result.Add(ParseExpression(LOWEST));

    while peekTokenIs(ttCOMMA) do
    begin
      nextToken;
      nextToken;

      Result.Add(ParseExpression(LOWEST));
    end;

    if NOT expectPeek(endTokenType) then
      FreeAndNil(Result);
  end
  else nextToken;
end;

function TParser.ParseExpressionStatement: TASTExpressionStatement;
begin
  Result := TASTExpressionStatement.Create;
  Result.Token := CurrToken.Clone;
  Result.Expression := ParseExpression(LOWEST);

  if peekTokenIs(ttSEMICOLON) then
    nextToken;
end;

function TParser.ParseForExpression: TASTExpression;
begin
  Result := TASTForExpression.Create;
  Result.Token := CurrToken.Clone;

  if expectPeek(ttLPAREN) then
  begin
    nextToken;
    TASTForExpression(Result).Condition := ParseExpression(LOWEST);
    if expectPeek(ttSEMICOLON) then
    begin
      nextToken;
      TASTForExpression(Result).Expression := ParseExpression(LOWEST);

      if expectPeek(ttRPAREN) then
      begin
        if expectPeek(ttLBRACE) then
        begin
          TASTForExpression(Result).Body := ParseBlockStatement;
          if currTokenIs(ttRBRACE) then
          begin
            if NOT expectPeek(ttSEMICOLON) then
              FreeAndNilAssigned(Result);
          end
          else FreeAndNilAssigned(Result);
        end
        else FreeAndNilAssigned(Result);
      end
      else FreeAndNilAssigned(Result);
    end
    else FreeAndNilAssigned(Result);
  end;

end;

function TParser.ParseFunctionLiteral: TASTExpression;
begin
  Result := TASTFunctionLiteral.Create;
  Result.Token := CurrToken.Clone;

  if expectPeek(ttLPAREN) then
  begin
    TASTFunctionLiteral(Result).Parameters := ParseFunctionParameters;
    if expectPeek(ttLBRACE) then
      TASTFunctionLiteral(Result).Body := ParseBlockStatement
    else
      FreeAndNilAssigned(Result);
  end
  else FreeAndNilAssigned(Result);

end;

function TParser.ParseFunctionParameters: TList<TASTIdentifier>;
var
  Ident:TASTIdentifier;
begin
  Result := TList<TASTIdentifier>.Create;
  if NOT peekTokenIs(ttRPAREN) then
  begin
    nextToken;

    Ident := TASTIdentifier.Create;
    Ident.Token := CurrToken.Clone;
    Ident.Value := CurrToken.Literal;
    Result.Add(Ident);

    while peekTokenIs(ttCOMMA) do
    begin
      nextToken;
      nextToken;

      Ident := TASTIdentifier.Create;
      Ident.Token := CurrToken.Clone;
      Ident.Value := CurrToken.Literal;
      Result.Add(Ident);
    end;

    if NOT expectPeek(ttRPAREN) then
      FreeAndNilAssigned(Result);
  end
  else nextToken;
end;

function TParser.ParseGroupExpression: TASTExpression;
var
  Expr:TASTExpression;
begin
  Result := nil;
  nextToken;

  Expr := ParseExpression(LOWEST);
  if expectPeek(ttRPAREN) then
    Result := Expr
  else
    FreeAndNil(Expr);
end;

function TParser.ParseHashLiteral: TASTExpression;
var
  key, value: TASTExpression;
begin
  Result := TASTHashLiteral.Create;
  Result.Token := CurrToken.Clone;
  TASTHashLiteral(Result).FPairs := TDictionary<TASTExpression,TASTExpression>.Create;

  While NOT peekTokenIs(ttRBRACE) do
  begin
    nextToken;
    key := ParseExpression(LOWEST);
    if expectPeek(ttCOLON) then
    begin
      nextToken;
      value := ParseExpression(LOWEST);

      TASTHashLiteral(Result).FPairs.Add(key, value);

      if NOT peekTokenIs(ttRBRACE) and NOT expectPeek(ttCOMMA) then
        Break;
    end
    else
    begin
      FreeAndNil(key);
      FreeAndNil(Result);
    end;
  end;
  if NOT expectPeek(ttRBRACE) then
    FreeAndNil(Result);
end;

function TParser.ParseIdentifier: TASTExpression;
begin
  Result := TASTIdentifier.Create;
  Result.Token := CurrToken.Clone;
  TASTIdentifier(Result).Value:= CurrToken.Literal;
end;

function TParser.ParseIfExpression: TASTExpression;
begin
  Result := TASTIfExpression.Create;
  Result.Token := CurrToken.Clone;

  if expectPeek(ttLPAREN) then
  begin
    nextToken;
    TASTIfExpression(Result).Condition := ParseExpression(LOWEST);

    if expectPeek(ttRPAREN) then
    begin
      if expectPeek(ttLBRACE) then
      begin
        TASTIfExpression(Result).Consequence := ParseBlockStatement;
        if peekTokenIs(ttELSE) then
        begin
          nextToken;
          if expectPeek(ttLBRACE) then
            TASTIfExpression(Result).Alternative := ParseBlockStatement
          else
            FreeAndNilAssigned(Result);
        end;
      end
      else  FreeAndNilAssigned(Result);
    end
    else FreeAndNilAssigned(Result);
  end
  else FreeAndNilAssigned(Result);

end;

function TParser.ParseImportStatement: TASTImportStatement;
begin
  Result := TASTImportStatement.Create;
  Result.Token := CurrToken.Clone;

  if expectPeek(ttSTRING) then
  begin
    TASTImportStatement(Result).Module := CurrToken.Literal;
    if peekTokenIs(ttSEMICOLON) then
      nextToken;
  end
  else FreeAndNilAssigned(Result);
end;

function TParser.ParseIndexExpression(left: TASTExpression): TASTExpression;
begin
  Result := TASTIndexExpression.Create;
  Result.Token := CurrToken.Clone;
  TASTIndexExpression(Result).Left := left;

  nextToken;
  TASTIndexExpression(Result).Index := ParseExpression(LOWEST);

  if NOT expectPeek(ttRBRACKET) then
    FreeAndNil(Result);
end;

function TParser.ParseInfixExpression(left: TASTExpression): TASTExpression;
var
  precedence: TEXPrecedence;
begin
  Result := TASTInfixExpression.Create;
  TASTInfixExpression(Result).Token := CurrToken.Clone;
  TASTInfixExpression(Result).Op  := CurrToken.Literal;
  TASTInfixExpression(Result).Left:= left;

  precedence := currPrecedence;
	nextToken;

	TASTInfixExpression(Result).Right := parseExpression(precedence);
end;

function TParser.ParseLetStatement: TASTLetStatement;
begin
  Result := TASTLetStatement.Create;
  Result.Token := CurrToken.Clone;
  if expectPeek(ttIDENT) then
  begin
    Result.Name := ParseExpression(LOWEST);
    if expectPeek(ttASSIGN) then
    begin
      nextToken;
      Result.Expression := ParseExpression(LOWEST);

      if peekTokenIs(ttSEMICOLON) then
        nextToken;
    end
    else FreeAndNil(Result);
  end
  else FreeAndNil(Result);
end;

function TParser.ParseNullLiteral: TASTExpression;
begin
  Result:= TASTNullLiteral.Create;
  TASTNumberLiteral(Result).Token := CurrToken.Clone;
end;

function TParser.ParseNumberLiteral: TASTExpression;
begin
  Result:= TASTNumberLiteral.Create;
  TASTNumberLiteral(Result).Token := CurrToken.Clone;
  TASTNumberLiteral(Result).Value := StrToFloatDef(StringReplace(CurrToken.Literal,'.',FormatSettings.DecimalSeparator,[]),0);
end;

function TParser.ParsePostfixExpression(left:TASTExpression): TASTExpression;
begin
  Result:= TASTPostfixExpression.Create;
  Result.Token := CurrToken.Clone;
  TASTPostfixExpression(Result).Op := CurrToken.Literal;
  TASTPostfixExpression(Result).Left := left;
end;

function TParser.ParsePrefixExpression: TASTExpression;
begin
  Result := TASTPrefixExpression.Create;
  TASTPrefixExpression(Result).Token := CurrToken.Clone;
  TASTPrefixExpression(Result).Op := CurrToken.Literal;

  nextToken;

  TASTPrefixExpression(Result).Right := ParseExpression(PREFIX);
end;

function TParser.ParseProgram: TASTProgram;
var
  stmt: TASTStatement;
begin
  Errors.Clear;

  Result := TASTProgram.Create;

  While (CurrToken.TokenType <> ttEOF ) do
  begin
    stmt := ParseStatement;
    if (stmt <> nil) then
      Result.Statements.Add(stmt);

    nextToken;
  end;
end;

function TParser.ParseReturnStatement: TASTReturnStatement;
begin
  Result := TASTReturnStatement.Create;
  Result.Token := CurrToken.Clone;

  nextToken;

  Result.ReturnValue := ParseExpression(LOWEST);

  if peekTokenIs(ttSEMICOLON) then
    nextToken;
end;

function TParser.ParseStatement: TASTStatement;
begin
  case CurrToken.TokenType of
    ttLET:
      Result := ParseLetStatement;
    ttCONST:
      Result := ParseConstStatement;
    ttRETURN:
      Result := ParseReturnStatement;
    ttIMPORT:
      Result := ParseImportStatement;
  else
    Result := ParseExpressionStatement;
  end;
end;

function TParser.ParseStringLiteral: TASTExpression;
begin
  Result := TASTStringLiteral.Create;
  Result.Token := CurrToken.Clone;
  TASTStringLiteral(Result).Value := CurrToken.Literal;
end;

function TParser.ParseSwitchExpression: TASTExpression;
var
  expr: TASTCaseExpression;
begin
  Result := TASTSwitchExpression.Create;
  Result.Token := CurrToken.Clone;

  nextToken;

  TASTSwitchExpression(Result).Value := ParseExpression(LOWEST);
  if Assigned(TASTSwitchExpression(Result).Value) then
  begin
    if currTokenIs(ttRPAREN) then
      nextToken;

    if currTokenIs(ttLBRACE) then
    begin
      nextToken;
      while NOT currTokenIs(ttRBRACE) do
      begin
        expr := TASTCaseExpression.Create;
        expr.Token := CurrToken.Clone;

        if currTokenIs(ttDEFAULT) then
           expr.Default := True
        else
        if currTokenIs(ttCASE) then
        begin
          nextToken;
          if currTokenIs(ttDEFAULT) then
            expr.Default := True
          else
          begin
            expr.Values.Add(ParseExpression(LOWEST));
            while peekTokenIs(ttCOMMA) do
            begin
              nextToken;
              nextToken;
              expr.Values.Add(ParseExpression(LOWEST));
            end;
          end;
        end;

        if expectPeek(ttLBRACE) then
        begin
          expr.Body := ParseBlockStatement;
          nextToken;
      		TASTSwitchExpression(Result).Choises.Add(expr);
        end
        else FreeAndNilAssigned(Result);

      end;
    end
    else FreeAndNilAssigned(Result);
  end
  else FreeAndNilAssigned(Result);
end;

function TParser.ParseTernaryExpression(condition: TASTExpression): TASTExpression;
var
  precedence: TEXPrecedence;
begin
  Result := nil;
  if NOT (FInTernary) then
  begin
    FInTernary :=True;
    try
      Result := TASTTernaryExpression.Create;
      Result.Token := CurrToken.Clone;
      TASTTernaryExpression(Result).Condition := condition;

      nextToken;
      precedence := currPrecedence;
      TASTTernaryExpression(Result).IfTrue := ParseExpression(precedence);

      if expectPeek(ttCOLON)  then
      begin
        nextToken;
        TASTTernaryExpression(Result).IfFalse :=  ParseExpression(precedence);
      end
      else FreeAndNilAssigned(Result);
    finally
      FInTernary :=False;
    end;
  end
  else AddError('nested ternary expressions are illegal');
end;

function TParser.ParseWhileExpression: TASTExpression;
begin
  Result := TASTWhileExpression.Create;
  Result.Token := CurrToken.Clone;

  if expectPeek(ttLPAREN) then
  begin
    nextToken;
    TASTWhileExpression(Result).Condition := ParseExpression(LOWEST);

    if expectPeek(ttRPAREN) then
    begin
      if expectPeek(ttLBRACE) then
      begin
        TASTWhileExpression(Result).Body := ParseBlockStatement;
        if currTokenIs(ttRBRACE) then
        begin
          if NOT expectPeek(ttSEMICOLON) then
            FreeAndNilAssigned(Result);
        end
        else FreeAndNilAssigned(Result);
      end
      else FreeAndNilAssigned(Result);
    end
    else FreeAndNilAssigned(Result);
  end
  else FreeAndNilAssigned(Result);

end;

function TParser.peekPrecedence: TEXPrecedence;
begin
  if Precedences.ContainsKey(PeekToken.TokenType) then
    Result := Precedences[PeekToken.TokenType]
  else
    Result := LOWEST;
end;

function TParser.peekTokenIs(TType: TTokenType): Boolean;
begin
  Result := (PeekToken.TokenType = TType);
end;

{ TASTProgram }

function TASTProgram.Clone: TASTNode;
var
  i: Integer;
begin
  Result := TASTProgram.Create;
  for i := 0 to Statements.Count-1 do
    TASTProgram(Result).Statements.Add(Statements[i].Clone as TASTStatement);
end;

constructor TASTProgram.Create;
begin
  FStatements := TList<TASTStatement>.Create;
end;

destructor TASTProgram.Destroy;
var
  i: Integer;
begin
  for i := FStatements.Count-1 downto 0 do
    if FStatements[i]<>nil then
      FStatements[i].Free;
  FStatements.Free;
end;

{ TASTStatement }


constructor TASTStatement.Create;
begin
  Token := nil;
end;

destructor TASTStatement.Destroy;
begin
  FreeAndNilAssigned(Token);
  inherited;
end;

{ TASTExpression }

constructor TASTExpression.Create;
begin
  inherited;
  Token := nil;
end;

destructor TASTExpression.Destroy;
begin
  FreeAndNilAssigned(Token);
  inherited;
end;

{ TASTExpressionStatement }

function TASTExpressionStatement.Clone: TASTNode;
begin
  Result := TASTExpressionStatement.Create;
  TASTExpressionStatement(Result).Expression := Expression.Clone as TASTExpression;
end;

constructor TASTExpressionStatement.Create;
begin
  Expression := nil;
end;

destructor TASTExpressionStatement.Destroy;
begin
  FreeAndNilAssigned(Expression);
  inherited;
end;

function TASTExpressionStatement.toString: string;
begin
  Result := 'Expression Stmt';
end;

{ TASTNumberLiteral }

function TASTNumberLiteral.Clone: TASTNode;
begin
  Result := TASTNumberLiteral.Create;
  TASTNumberLiteral(Result).Value := Value;
end;

function TASTNumberLiteral.toString: string;
begin
  Result := 'Number = ' + FloatToStr(Value);
end;

{ TASTPrefixExpression }

function TASTPrefixExpression.Clone: TASTNode;
begin
  Result := TASTPrefixExpression.Create;
  TASTPrefixExpression(Result).Op := Op;
  TASTPrefixExpression(Result).Right := Right.Clone as TASTExpression;
end;

constructor TASTPrefixExpression.Create;
begin
  inherited;
  Op := '';
  Right := nil;
end;

destructor TASTPrefixExpression.Destroy;
begin
  FreeAndNilAssigned(Right);
  inherited;
end;

function TASTPrefixExpression.toString: string;
begin
  Result := 'Prefix Operator = ' + Op;
end;

{ TASTInfixExpression }

function TASTInfixExpression.Clone: TASTNode;
begin
  Result := TASTInfixExpression.Create;
  TASTInfixExpression(Result).Left := Left.Clone as TASTExpression;
  TASTInfixExpression(Result).Op   := Op;
  TASTInfixExpression(Result).Right:= Right.Clone as TASTExpression;
end;

constructor TASTInfixExpression.Create;
begin
  Op:='';;
  Left:=nil;
  Right:=nil;
end;

destructor TASTInfixExpression.Destroy;
begin
  FreeAndNilAssigned(Right);
  FreeAndNilAssigned(Left);
  inherited;
end;

function TASTInfixExpression.toString: string;
begin
  Result := 'Infix Operator = ' + op;
end;

{ TASTIdentifier }

function TASTIdentifier.Clone: TASTNode;
begin
  Result := TASTIdentifier.Create;
  TASTIdentifier(Result).Value := Value;
end;

function TASTIdentifier.toString: string;
begin
  Result := 'Identifier = ' + Value;
end;

{ TASTLetStatement }

function TASTLetStatement.Clone: TASTNode;
begin
  Result := TASTLetStatement.Create;
  TASTLetStatement(Result).Name := Name.Clone as TASTIdentifier;
  TASTLetStatement(Result).Expression := Expression.Clone as TASTExpression;
end;

constructor TASTLetStatement.Create;
begin
  Name := nil;
  Expression := nil;
end;

destructor TASTLetStatement.Destroy;
begin
  FreeAndNilAssigned(Expression);
  FreeAndNilAssigned(Name);
  inherited;
end;

function TASTLetStatement.toString: string;
begin
  Result := 'Let Stmt';
end;

{ TASTReturnStatement }

function TASTReturnStatement.Clone: TASTNode;
begin
  Result := TASTReturnStatement.Create;
  TASTReturnStatement(Result).ReturnValue := ReturnValue.Clone as TASTExpression;
end;

constructor TASTReturnStatement.Create;
begin
  ReturnValue := nil;
end;

destructor TASTReturnStatement.Destroy;
begin
  FreeAndNilAssigned(ReturnValue);
  inherited;
end;

function TASTReturnStatement.toString: string;
begin
  Result := 'Return Stmt';
end;

{ TASTBoolean }

function TASTBoolean.Clone: TASTNode;
begin
  Result := TASTBoolean.Create;
  TASTBoolean(Result).Value := Value;
end;

function TASTBoolean.toString: string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

{ TASTBlockStatement }

function TASTBlockStatement.Clone: TASTNode;
var
  i: Integer;
begin
  Result := TASTBlockStatement.Create;
  for i := 0 to Statements.Count-1 do
    TASTBlockStatement(Result).Statements.Add(Statements[i].Clone as TASTStatement);
end;

constructor TASTBlockStatement.Create;
begin
  FStatements := TList<TASTStatement>.Create;
end;

destructor TASTBlockStatement.Destroy;
var
  i: Integer;
begin
  for i := FStatements.Count-1 downto 0 do
    FStatements[i].Free;
  FStatements.Free;
  inherited;
end;

function TASTBlockStatement.toString: string;
begin
  Result := 'Block stmt';
end;

{ TASTIfExpression }

function TASTIfExpression.Clone: TASTNode;
begin
  Result := TASTIfExpression.Create;
  TASTIfExpression(Result).Condition := Condition.Clone as TASTExpression;
  TASTIfExpression(Result).Consequence := Consequence.Clone as TASTBlockStatement;
  if Assigned(Alternative) then
    TASTIfExpression(Result).Alternative := Alternative.Clone as TASTBlockStatement;
end;

constructor TASTIfExpression.Create;
begin
  Condition:=nil;
  Consequence:=nil;
  Alternative:=nil;
end;

destructor TASTIfExpression.Destroy;
begin
  FreeAndNilAssigned(Condition);
  FreeAndNilAssigned(Consequence);
  FreeAndNilAssigned(Alternative);
  inherited;
end;

function TASTIfExpression.toString: string;
begin
  Result := 'if';
end;

{ TASTFunctionLiteral }

function TASTFunctionLiteral.Clone: TASTNode;
var
  i:Integer;
begin
  Result := TASTFunctionLiteral.Create;
  TASTFunctionLiteral(Result).Body := Body.Clone as TASTBlockStatement;
  TASTFunctionLiteral(Result).Parameters := TList<TASTIdentifier>.Create;
  if Assigned(Parameters)  then
    for i := 0 to Parameters.Count-1 do
      TASTFunctionLiteral(Result).Parameters.Add(Parameters[i].Clone as TASTIdentifier);
end;

constructor TASTFunctionLiteral.Create;
begin
  Parameters := nil;
  Body := nil;
end;

destructor TASTFunctionLiteral.Destroy;
var
  i: Integer;
begin
  FreeAndNilAssigned(Body);
  if Parameters<>nil then
  begin
    for i := Parameters.Count-1 downto 0 do
      Parameters[i].Free;
    Parameters.Free;
  end;
  inherited;
end;

function TASTFunctionLiteral.toString: string;
begin
  Result := 'function';
end;

{ TASTNode }

function TASTNode.Clone: TASTNode;
begin
  Result := nil; { -- virtual -- }
end;

function TASTNode.toString: string;
begin
  Result := 'ASTNode'; { -- virtual -- }
end;

{ TASTCallExpression }

function TASTCallExpression.Clone: TASTNode;
var
  i: Integer;
begin
  Result := TASTCallExpression.Create;
  TASTCallExpression(Result).Funct:= Funct.Clone as TASTExpression;
  TASTCallExpression(Result).Args := TList<TASTExpression>.create;
  for i := 0 to Args.Count-1 do
    TASTCallExpression(Result).Args.Add(Args[i].Clone as TASTExpression);
end;

constructor TASTCallExpression.Create;
begin
  Funct := nil;
  Args := nil;
end;

destructor TASTCallExpression.Destroy;
var
  i: Integer;
begin
  FreeAndNilAssigned(Funct);
  if Args<>nil then
  begin
    for i := Args.Count-1 downto 0 do
      Args[i].Free;
    Args.Free;
  end;
  inherited;
end;

function TASTCallExpression.toString: string;
begin
  Result := 'call';
end;

{ TASTStringLiteral }

function TASTStringLiteral.Clone: TASTNode;
begin
  Result := TASTStringLiteral.Create;
  TASTStringLiteral(Result).Value := Value;
end;

function TASTStringLiteral.toString: string;
begin
  Result := 'String = ' + Value;
end;

{ TASTArrayLiteral }

function TASTArrayLiteral.Clone: TASTNode;
var
  i: Integer;
begin
  Result := TASTArrayLiteral.Create;
  TASTArrayLiteral(Result).Elements := TList<TASTExpression>.Create;
  for i := 0 to Elements.Count-1 do
    TASTArrayLiteral(Result).Elements.Add(Elements[i].Clone as TASTExpression);
end;

constructor TASTArrayLiteral.Create;
begin
  FElements := nil;
end;

destructor TASTArrayLiteral.Destroy;
var
  i: Integer;
begin
  if (FElements<>nil) then
  begin
    for i := FElements.Count-1 downto 0 do
      if FElements[i]<>nil then
        FElements[i].Free;
    FElements.Free;
  end;
  inherited;
end;

function TASTArrayLiteral.toString: string;
begin
  Result := 'Array';
  if Assigned(Elements) then
    Result := Result + ' size = ' + IntToStr(FElements.Count);
end;

{ TASTIndexExpression }

function TASTIndexExpression.Clone: TASTNode;
begin
  Result := TASTIndexExpression.Create;
  TASTIndexExpression(Result).Left := Left.Clone as TASTExpression;
  TASTIndexExpression(Result).Index:= Index.Clone as TASTExpression;
end;

constructor TASTIndexExpression.Create;
begin

end;

destructor TASTIndexExpression.Destroy;
begin
  FreeAndNilAssigned(Left);
  FreeAndNilAssigned(Index);
  inherited;
end;

function TASTIndexExpression.toString: string;
begin
  Result := 'Index[]';
end;

{ TASTHashLiteral }

function TASTHashLiteral.Clone: TASTNode;
var
  key:TASTExpression;
begin
  Result := TASTHashLiteral.Create;
  TASTHashLiteral(Result).FPairs := TDictionary<TASTExpression,TASTExpression>.Create;
  for key in Pairs.Keys do
    TASTHashLiteral(Result).Pairs.Add(key.Clone as TASTExpression, Pairs[key].Clone as TASTExpression);
end;

constructor TASTHashLiteral.Create;
begin
  FPairs := nil;
end;

destructor TASTHashLiteral.Destroy;
var
  key:TASTExpression;
begin
  if (FPairs<>nil) then
  begin
    for key in Pairs.Keys do
    begin
      Pairs[key].Free;
//      Pairs.Remove(key);
      key.Free
    end;
    FPairs.Free;
  end;
  inherited;
end;

function TASTHashLiteral.toString: string;
begin
  Result := 'HashMap';
end;

{ TASTWhileExpression }

function TASTWhileExpression.Clone: TASTNode;
begin
  Result := TASTWhileExpression.Create;
  TASTWhileExpression(Result).Condition := Condition.Clone as TASTExpression;
  TASTWhileExpression(Result).Body := Body.Clone as TASTBlockStatement;
end;

constructor TASTWhileExpression.Create;
begin
  Condition := nil;
  Body := nil;
end;

destructor TASTWhileExpression.Destroy;
begin
  FreeAndNilAssigned(Condition);
  FreeAndNilAssigned(Body);
  inherited;
end;

function TASTWhileExpression.toString: string;
begin
  Result := 'while';
end;

{ TASTForExpression }

function TASTForExpression.Clone: TASTNode;
begin
  Result := TASTForExpression.Create;
  TASTForExpression(Result).Condition := Condition.Clone as TASTExpression;
  TASTForExpression(Result).Expression:= Expression.Clone;
  TASTForExpression(Result).Body := Body.Clone as TASTBlockStatement;
end;

constructor TASTForExpression.Create;
begin
  Condition:= nil;
  Expression:= nil;
  Body:= nil;
end;

destructor TASTForExpression.Destroy;
begin
  FreeAndNilAssigned(Condition);
  FreeAndNilAssigned(Expression);
  FreeAndNilAssigned(Body);
  inherited;
end;

function TASTForExpression.toString: string;
begin
  Result := 'for';
end;

{ TASTAssignExpression }

function TASTAssignExpression.Clone: TASTNode;
begin
  Result := TASTAssignExpression.Create;
  TASTAssignExpression(Result).Name := Name.Clone as TASTExpression;
  TASTAssignExpression(Result).Op   := Op;
  TASTAssignExpression(Result).Expression:= Expression.Clone as TASTExpression;
end;

constructor TASTAssignExpression.Create;
begin
  Op := '';;
  Name:=nil;
  Expression:=nil;
end;

destructor TASTAssignExpression.Destroy;
begin
  FreeAndNilAssigned(Name);
  FreeAndNilAssigned(Expression);
  inherited;
end;

function TASTAssignExpression.toString: string;
begin
  Result := 'Assign expression ' + Op;
end;

{ TASTImportStatement }

function TASTImportStatement.Clone: TASTNode;
begin
  Result := TASTImportStatement.Create;
  TASTImportStatement(Result).Module := Module;
end;

constructor TASTImportStatement.Create;
begin
  Module:= '';
end;

destructor TASTImportStatement.Destroy;
begin
  inherited;
end;

function TASTImportStatement.toString: string;
begin
  Result := 'Import Stmt MODULE = ' + Module;
end;

{ TASTNullLiteral }

function TASTNullLiteral.Clone: TASTNode;
begin
  Result := TASTNullLiteral.Create;
end;

function TASTNullLiteral.toString: string;
begin
  Result := 'Null';
end;

{ TASTConstStatement }

function TASTConstStatement.Clone: TASTNode;
begin
  Result := TASTConstStatement.Create;
  TASTConstStatement(Result).Name := Name.Clone as TASTIdentifier;
  TASTConstStatement(Result).Expression := Expression.Clone as TASTExpression;
end;

constructor TASTConstStatement.Create;
begin
  Name := nil;
  Expression := nil;
end;

destructor TASTConstStatement.Destroy;
begin
  FreeAndNilAssigned(Expression);
  FreeAndNilAssigned(Name);
  inherited;
end;

function TASTConstStatement.toString: string;
begin
  Result := 'Const Stmt';
end;

{ TASTTernaryExpression }

function TASTTernaryExpression.Clone: TASTNode;
begin
  Result := TASTTernaryExpression.Create;
  TASTTernaryExpression(Result).Condition := Condition.Clone as TASTExpression;
  TASTTernaryExpression(Result).IfTrue  := IfTrue.Clone as TASTExpression;
  TASTTernaryExpression(Result).IfFalse := IfFalse.Clone as TASTExpression;
end;

constructor TASTTernaryExpression.Create;
begin
  Condition:=nil;
  IfTrue:=nil;
  IfFalse:=nil;
end;

destructor TASTTernaryExpression.Destroy;
begin
  FreeAndNilAssigned(Condition);
  FreeAndNilAssigned(IfTrue);
  FreeAndNilAssigned(IfFalse);
  inherited;
end;

function TASTTernaryExpression.toString: string;
begin
  Result := 'Ternary';
end;

{ TASTCaseExpression }

function TASTCaseExpression.Clone: TASTNode;
var
  current: TASTExpression;
begin
  Result := TASTCaseExpression.Create;
  TASTCaseExpression(Result).Default:= Default;
  TASTCaseExpression(Result).Body   := Body.Clone as TASTBlockStatement;
  for current in Values do
    TASTCaseExpression(Result).Values.Add(current.Clone as TASTExpression);
end;

constructor TASTCaseExpression.Create;
begin
  Values:= TList<TASTExpression>.Create;;
  Default:= False;
  Body:= nil;
end;

destructor TASTCaseExpression.Destroy;
var
  i:Integer;
begin
  for I := 0 to Values.Count-1 do
    Values[i].Free;
  Values.Free;
  FreeAndNilAssigned(Body);
  inherited;
end;

function TASTCaseExpression.toString: string;
begin
  if Default then
    Result := 'Case default'
  else
    Result := 'Case';
end;

{ TASTSwitchExpression }

function TASTSwitchExpression.Clone: TASTNode;
var
  current: TASTExpression;
begin
  Result := TASTSwitchExpression.Create;
  TASTSwitchExpression(Result).Value:= Value.Clone as TASTExpression;
  for current in Choises do
    TASTSwitchExpression(Result).Choises.Add(current.Clone as TASTCaseExpression);
end;

constructor TASTSwitchExpression.Create;
begin
  Value:= nil;
  Choises:= TList<TASTCaseExpression>.Create;
end;

destructor TASTSwitchExpression.Destroy;
var
  i:Integer;
begin
  for I := 0 to Choises.Count-1 do
    Choises[i].Free;
  Choises.Free;
  FreeAndNilAssigned(Value);
  inherited;
end;

function TASTSwitchExpression.toString: string;
begin
  Result := 'switch';
end;

{ TASTPostfixExpression }

function TASTPostfixExpression.Clone: TASTNode;
begin
  Result:= TASTPostfixExpression.Create;
  TASTPostfixExpression(Result).Op := Op;
  TASTPostfixExpression(Result).Left := Left.Clone as TASTExpression;
end;

constructor TASTPostfixExpression.Create;
begin
  Op := '';
end;

destructor TASTPostfixExpression.Destroy;
begin
  FreeAndNilAssigned(Left);
  inherited;
end;

function TASTPostfixExpression.toString: string;
begin
  Result := 'Postfix Operator = ' + op;
end;

end.
