unit lp.lexer;

interface

uses SysUtils, lp.token;

type
  TLexer = class
    Input:string;
    Position: Integer;
    ReadPosition: Integer;
    ch: Char;
    function  isLetter(value:char):Boolean;
    function  isDigit(value:char):Boolean;
  private
    procedure ReadChar;
    function  PeekChar:Char;
    procedure SkipWhiteSpace;
    procedure SkipComment;
    function  ReadNumber: string;
    function  ReadIdentifier: string;
    function  ReadString: string;
  public
    constructor Create(AInput:string);
    function NextToken: TToken;
  end;

implementation

{ TLexer }

constructor TLexer.Create(AInput: string);
begin
  Input:=AInput;
  Position:=0;
  ReadPosition:=1;
  ReadChar;
end;

function TLexer.isDigit(value: char): Boolean;
begin
  Result := CharInSet(ch, ['0'..'9']);
end;

function TLexer.isLetter(value: char): Boolean;
begin
  Result := CharInSet(ch, ['a'..'z','A'..'Z','_']);
end;

function TLexer.NextToken: TToken;
begin
  Result := nil;
  SkipWhiteSpace;
  SkipComment;
  case ch of
     #0 : Result := TToken.create(ttEOF, '');
    '+' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttPLUSASSIGN, '+=');
      end
      else
      if (PeekChar='+') then
      begin
        ReadChar;
        Result := TToken.create(ttPLUSPLUS, '++');
      end
      else Result := TToken.create(ttPLUS, ch);
    end;
    '-' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttMINUSASSIGN, '-=');
      end
      else
      if (PeekChar='-') then
      begin
        ReadChar;
        Result := TToken.create(ttMINUSMINUS, '--');
      end
      else Result := TToken.create(ttMINUS, ch);
    end;
    '*' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttASTERISKASSIGN, '*=');
      end
      else
      if (PeekChar='*') then
      begin
        ReadChar;
        Result := TToken.create(ttPOWER, '**');
      end
      else Result := TToken.create(ttASTERISK, ch);
    end;
    '/' : begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttSLASHASSIGN, '/=');
      end
      else Result := TToken.create(ttSLASH, ch);
    end;
    ':' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttVARASSIGN, ':=');
      end
      else Result := TToken.create(ttCOLON, ch);
    end;
    '%' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttMODASSIGN, '%=');
      end
      else Result := TToken.create(ttMOD, ch);
    end;
    '.' :
    begin
      if (PeekChar='.') then
      begin
        ReadChar;
        Result := TToken.create(ttDOTDOT, '..');
      end
      else Result := TToken.create(ttDOT, ch);
    end;
    '(' : Result := TToken.create(ttLPAREN, ch);
    ')' : Result := TToken.create(ttRPAREN, ch);
    '{' : Result := TToken.create(ttLBRACE, ch);
    '}' : Result := TToken.create(ttRBRACE, ch);
    '[' : Result := TToken.create(ttLBRACKET, ch);
    ']' : Result := TToken.create(ttRBRACKET, ch);
    ';' : Result := TToken.create(ttSEMICOLON, ch);
    ',' : Result := TToken.create(ttCOMMA, ch);
    '?' : Result := TToken.create(ttQUESTION, ch);
    '=' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttEQ, '==');
      end
      else Result := TToken.create(ttASSIGN, ch);
    end;
    '!' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttBANG, '!=');
      end
      else Result := TToken.create(ttBANG, ch);
    end;
    '"' :
    begin
      ReadChar;
      Result := TToken.create(ttSTRING, ReadString);
    end;
    '&' :
    begin
      if (PeekChar='&') then
      begin
        ReadChar;
        Result := TToken.create(ttLOGICALAND, '&&');
      end
    end;
    '|' :
    begin
      if (PeekChar='|') then
      begin
        ReadChar;
        Result := TToken.create(ttLOGICALOR, '||');
      end
    end;
    '<' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttLE, '<=');
      end
      else Result := TToken.create(ttLT, ch);
    end;
    '>' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttGE, '>=');
      end
      else Result := TToken.create(ttGT, ch);
    end
  else
    if isDigit(ch) then
    begin
      Result := TToken.create(ttNUMBER, ReadNumber);
      Exit;
    end
    else
    if isLetter(ch) then
    begin
      Result :=  TToken.create(ttIDENT, ReadIdentifier);
      Result.TokenType := LookupIdent(Result.Literal);
      Exit;
    end;
  end;
  if Result=nil then
    Result := TToken.create(ttILLEGAL, '');

  ReadChar;
end;

function TLexer.PeekChar: Char;
begin
  if (readPosition > Length(Input)) then
		Result := #0
	else
		Result := input[readPosition];
end;

procedure TLexer.ReadChar;
begin
  if (readPosition > Length(input)) then
		ch := #0
	else
		ch := input[readPosition];

	position := readPosition;
  Inc(readPosition);
end;

function TLexer.ReadIdentifier: string;
var
  CurrentPos:Integer;
begin
	CurrentPos := Position;
  while (isLetter(ch)) do ReadChar;

	Result := Copy(Input, CurrentPos, Position-CurrentPos);
end;

function TLexer.ReadNumber: string;

  function dotdot: Boolean;
  begin
    Result := ((ch='.') and (PeekChar='.'));
  end;

var
  CurrentPos:Integer;
begin
  CurrentPos := Position;
  while CharInSet(ch, ['0'..'9', '.']) and NOT dotdot do ReadChar;
	Result := Copy(Input, CurrentPos, Position-CurrentPos);
end;

function TLexer.ReadString: string;
var
  CurrentPos:Integer;
begin
  CurrentPos := Position;
  while NOT CharInSet(ch, ['"', #0]) do ReadChar;

	Result := Copy(Input, CurrentPos, Position-CurrentPos);
end;

procedure TLexer.SkipComment;
begin
  if ((ch='/') and (PeekChar='*')) then
  begin
     while NOT ((ch='*') and (PeekChar='/')) do ReadChar;
     ReadChar;
     ReadChar;
  end;
end;

procedure TLexer.SkipWhiteSpace;
begin
  while CharInSet(ch, [' ', #13, #10, #8]) do ReadChar;
end;

end.
