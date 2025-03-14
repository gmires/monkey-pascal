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
    , ttLE // = "<="
    , ttGE // = ">="
    , ttEQ //    = "=="
    , ttNOT_EQ // = "!="

    , ttSTRING   // = """
    , ttLBRACKET // = "["
    , ttRBRACKET // = "]"
    , ttCOLON    // = ":"

    , ttLOGICALAND    // = "&&"
    , ttLOGICALOR    // = "||"
    , ttWHILE    // = "WHILE"
    , ttFOR    // = "FOR"
    , ttVARASSIGN // = ":="
    , ttIMPORT // = "IMPORT"
    // -- new token -- //
    , ttCONST // = "CONST"
    , ttNULL  // = "NULL"
    , ttPLUSASSIGN  // = "+="
    , ttASTERISKASSIGN  // = "*="
    , ttSLASHASSIGN // = "/="
    , ttMINUSASSIGN // = "-="
    , ttQUESTION // = "?"
    , ttSWITCH // = "SWITCH"
    , ttCASE // = "CASE"
    , ttDEFAULT // = "DEFAULT"
    , ttPLUSPLUS // = "++"
    , ttMINUSMINUS // = "++"
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
function TokenTypeToStr(value:TTokenType): string;

implementation

var
  keywords:TDictionary<string,TTokenType>;

function LookupIdent(value:string): TTokenType;
begin
  if NOT keywords.TryGetValue(value, Result) then
    Result := ttIDENT;
end;

function TokenTypeToStr(value:TTokenType): string;
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
    , 'LE'
    , 'GE'
    , 'EQ'
    , 'NOT_EQ'
    , 'STRING'
    , 'LBRACKET'
    , 'RBRACKET'
    , 'COLON'
    , 'LOGICALAND'
    , 'LOGICALOR'
    , 'WHILE'
    , 'FOR'
    , 'VARASSIGN'
    , 'IMPORT'
    , 'CONST'
    , 'NULL'
    , 'PLUSASSIGN'
    , 'ASTERISKASSIGN'
    , 'SLASHASSIGN'
    , 'MINUSASSIGN'
    , 'QUESTION'
    , 'SWITCH'
    , 'CASE'
    , 'DEFAULT'
    , 'PLUSPLUS'
    , 'MINUSMINUS'
  );
begin
  Result := TTypeStr[value];
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
begin
  Result := 'TK Type = ' + TokenTypeToStr(TokenType) + ', literal value = ' + Literal;
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
  keywords.Add('while', ttWHILE);
  keywords.Add('for', ttFOR);
  keywords.Add('import', ttIMPORT);
  keywords.Add('const', ttCONST);
  keywords.Add('null', ttNULL);
  keywords.Add('switch', ttSWITCH);
  keywords.Add('case', ttCASE);
  keywords.Add('default', ttDEFAULT);
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
