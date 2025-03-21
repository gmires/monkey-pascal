unit lp.lexer;

interface

uses SysUtils, lp.token;

type
  TLexer = class
    Input:string;
    Position: Integer;
    ReadPosition: Integer;
    Line: Integer;
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
  Line:=1;
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
     #0 : Result := TToken.create(ttEOF, '', Line);
    '+' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttPLUSASSIGN, '+=', Line);
      end
      else
      if (PeekChar='+') then
      begin
        ReadChar;
        Result := TToken.create(ttPLUSPLUS, '++', Line);
      end
      else Result := TToken.create(ttPLUS, ch, Line);
    end;
    '-' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttMINUSASSIGN, '-=', Line);
      end
      else
      if (PeekChar='-') then
      begin
        ReadChar;
        Result := TToken.create(ttMINUSMINUS, '--', Line);
      end
      else Result := TToken.create(ttMINUS, ch, Line);
    end;
    '*' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttASTERISKASSIGN, '*=', Line);
      end
      else
      if (PeekChar='*') then
      begin
        ReadChar;
        Result := TToken.create(ttPOWER, '**', Line);
      end
      else Result := TToken.create(ttASTERISK, ch, Line);
    end;
    '/' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttSLASHASSIGN, '/=', Line);
      end
      else Result := TToken.create(ttSLASH, ch, Line);
    end;
    ':' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttVARASSIGN, ':=', Line);
      end
      else Result := TToken.create(ttCOLON, ch, Line);
    end;
    '%' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttMODASSIGN, '%=', Line);
      end
      else Result := TToken.create(ttMOD, ch, Line);
    end;
    '.' :
    begin
      if (PeekChar='.') then
      begin
        ReadChar;
        Result := TToken.create(ttDOTDOT, '..', Line);
      end
      else Result := TToken.create(ttDOT, ch, Line);
    end;
    '(' : Result := TToken.create(ttLPAREN, ch, Line);
    ')' : Result := TToken.create(ttRPAREN, ch, Line);
    '{' : Result := TToken.create(ttLBRACE, ch, Line);
    '}' : Result := TToken.create(ttRBRACE, ch, Line);
    '[' : Result := TToken.create(ttLBRACKET, ch, Line);
    ']' : Result := TToken.create(ttRBRACKET, ch, Line);
    ';' : Result := TToken.create(ttSEMICOLON, ch, Line);
    ',' : Result := TToken.create(ttCOMMA, ch, Line);
    '?' : Result := TToken.create(ttQUESTION, ch, Line);
    '=' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttEQ, '==', Line);
      end
      else Result := TToken.create(ttASSIGN, ch, Line);
    end;
    '!' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttNOT_EQ, '!=', Line);
      end
      else Result := TToken.create(ttBANG, ch, Line);
    end;
    '"' :
    begin
      ReadChar;
      Result := TToken.create(ttSTRING, ReadString, Line);
    end;
    '&' :
    begin
      if (PeekChar='&') then
      begin
        ReadChar;
        Result := TToken.create(ttLOGICALAND, '&&', Line);
      end
    end;
    '|' :
    begin
      if (PeekChar='|') then
      begin
        ReadChar;
        Result := TToken.create(ttLOGICALOR, '||', Line);
      end
    end;
    '<' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttLE, '<=', Line);
      end
      else Result := TToken.create(ttLT, ch, Line);
    end;
    '>' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttGE, '>=', Line);
      end
      else Result := TToken.create(ttGT, ch, Line);
    end
  else
    if isDigit(ch) then
    begin
      Result := TToken.create(ttNUMBER, ReadNumber, Line);
      Exit;
    end
    else
    if isLetter(ch) then
    begin
      Result :=  TToken.create(ttIDENT, ReadIdentifier, Line);
      Result.TokenType := LookupIdent(Result.Literal);
      Exit;
    end;
  end;
  if Result=nil then
    Result := TToken.create(ttILLEGAL, '', Line);

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
  while CharInSet(ch, [' ', #13, #10, #8]) do
  begin
    if ((ch=#13) and (PeekChar=#10)) then
      Inc(Line);

    ReadChar;
  end;
end;

end.
