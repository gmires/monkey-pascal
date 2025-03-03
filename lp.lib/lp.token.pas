unit lp.token;

interface

uses Classes, Generics.Collections;

type
  TTokenType = (
    // base
      ttILLEGAL
    , ttEOF
    , ttNUMBER
    // Operators
    , ttPLUS      // = "+"
    , ttMINUS     // = "-"
    , ttASTERISK  // = "*"
    , ttSLASH     // = "/"
    // others
    , ttLPAREN    // = "("
    , ttRPAREN    // = ")"
    , ttSEMICOLON // = ";"

    , ttLET // = "LET"
    , ttASSIGN // = "="
    , ttIDENT // = "IDENT"

    , ttFUNCTION // = "FUNCTION"
    , ttTRUE // = "TRUE"
    , ttFALSE // = "FALSE"
    , ttIF // = "IF"
    , ttELSE // = "ELSE"
    , ttRETURN // = "RETURN"
    , ttCOMMA    // = ","
    , ttLBRACE // = "{"
    , ttRBRACE // = "}"
    , ttBANG  //   = "!"
    , ttLT // = "<"
    , ttGT // = ">"
    , ttEQ //    = "=="
    , ttNOT_EQ // = "!="

    , ttSTRING // = """
    , ttLBRACKET // = "["
    , ttRBRACKET    // = "]"
    , ttCOLON    // = ":"
  );


  TToken = class
  public
    constructor create(ATokenType:TTokenType; ALiteral:string);
    function toString:string;
    function Clone:TToken;
  public
    TokenType:TTokenType;
    Literal:string;
  end;

function LookupIdent(value:string): TTokenType;

implementation

var
  keywords:TDictionary<string,TTokenType>;

function LookupIdent(value:string): TTokenType;
begin
  if NOT keywords.TryGetValue(value, Result) then
    Result := ttIDENT;
end;

{ TToken }

function TToken.Clone: TToken;
begin
  Result := TToken.create(TokenType, Literal);
end;

constructor TToken.create(ATokenType: TTokenType; ALiteral: string);
begin
  TokenType := ATokenType;
  Literal := ALiteral;
end;

function TToken.toString: string;
const
  TTypeStr: array[TTokenType] of string = (
    'ILLEGAL'
    , 'EOF'
    , 'NUMBER'
    , 'PLUS'
    , 'MINUS'
    , 'ASTERISK'
    , 'SLASH'
    , 'LPAREN'
    , 'RPAREN'
    , 'SEMICOLON'
    , 'LET'
    , 'ASSIGN'
    , 'IDENT'
    , 'FUNCTION'
    , 'TRUE'
    , 'FALSE'
    , 'IF'
    , 'ELSE'
    , 'RETURN'
    , 'COMMA'
    , 'LBRACE'
    , 'RBRACE'
    , 'BANG'
    , 'LT'
    , 'GT'
    , 'EQ'
    , 'NOT_EQ'
    , 'STRING'
    , 'LBRACKET'
    , 'RBRACKET'
    , 'COLON'
  );
begin
  Result := 'TK Type = ' + TTypeStr[TokenType] + ', literal value = ' + Literal;
end;

procedure init;
begin
  keywords := TDictionary<string,TTokenType>.Create;
  keywords.Add('fn', ttFUNCTION);
  keywords.Add('let', ttLET);
  keywords.Add('true', ttTRUE);
  keywords.Add('false', ttFALSE);
  keywords.Add('if', ttIF);
  keywords.Add('else', ttELSE);
  keywords.Add('return', ttRETURN);
end;

procedure deinit;
begin
  keywords.Free;
end;

initialization
  init;

finalization
  deinit;

end.
