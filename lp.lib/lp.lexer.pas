unit lp.lexer;

interface

uses SysUtils, lp.token;

type
  TLexer = class
    Module:string;
    Input:string;
    Position: Integer;
    ReadPosition: Integer;
    Line: Integer;
    Coln: Integer;
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
    constructor Create(AInput:string; AModule:string);
    function NextToken: TToken;
  end;

implementation

{ TLexer }

constructor TLexer.Create(AInput: string; AModule:string);
begin
  Module:=AModule;
  Input:=AInput;
  Position:=0;
  ReadPosition:=1;
  Line:=1;
  Coln:=1;
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
  SkipWhiteSpace;
  case ch of
     #0 : Result := TToken.create(ttEOF, '', Line, Coln, Module);
    '+' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttPLUSASSIGN, '+=', Line, Coln, Module);
      end
      else
      if (PeekChar='+') then
      begin
        ReadChar;
        Result := TToken.create(ttPLUSPLUS, '++', Line, Coln, Module);
      end
      else Result := TToken.create(ttPLUS, ch, Line, Coln, Module);
    end;
    '-' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttMINUSASSIGN, '-=', Line, Coln, Module);
      end
      else
      if (PeekChar='-') then
      begin
        ReadChar;
        Result := TToken.create(ttMINUSMINUS, '--', Line, Coln, Module);
      end
      else Result := TToken.create(ttMINUS, ch, Line, Coln, Module);
    end;
    '*' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttASTERISKASSIGN, '*=', Line, Coln, Module);
      end
      else
      if (PeekChar='*') then
      begin
        ReadChar;
        Result := TToken.create(ttPOWER, '**', Line, Coln, Module);
      end
      else Result := TToken.create(ttASTERISK, ch, Line, Coln, Module);
    end;
    '/' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttSLASHASSIGN, '/=', Line, Coln, Module);
      end
      else Result := TToken.create(ttSLASH, ch, Line, Coln, Module);
    end;
    ':' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttVARASSIGN, ':=', Line, Coln, Module);
      end
      else Result := TToken.create(ttCOLON, ch, Line, Coln, Module);
    end;
    '%' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttMODASSIGN, '%=', Line, Coln, Module);
      end
      else Result := TToken.create(ttMOD, ch, Line, Coln, Module);
    end;
    '.' :
    begin
      if (PeekChar='.') then
      begin
        ReadChar;
        Result := TToken.create(ttDOTDOT, '..', Line, Coln, Module);
      end
      else Result := TToken.create(ttDOT, ch, Line, Coln, Module);
    end;
    '(' : Result := TToken.create(ttLPAREN, ch, Line, Coln, Module);
    ')' : Result := TToken.create(ttRPAREN, ch, Line, Coln, Module);
    '{' : Result := TToken.create(ttLBRACE, ch, Line, Coln, Module);
    '}' : Result := TToken.create(ttRBRACE, ch, Line, Coln, Module);
    '[' : Result := TToken.create(ttLBRACKET, ch, Line, Coln, Module);
    ']' : Result := TToken.create(ttRBRACKET, ch, Line, Coln, Module);
    ';' : Result := TToken.create(ttSEMICOLON, ch, Line, Coln, Module);
    ',' : Result := TToken.create(ttCOMMA, ch, Line, Coln, Module);
    '?' : Result := TToken.create(ttQUESTION, ch, Line, Coln, Module);
    '=' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttEQ, '==', Line, Coln, Module);
      end
      else Result := TToken.create(ttASSIGN, ch, Line, Coln, Module);
    end;
    '!' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttNOT_EQ, '!=', Line, Coln, Module);
      end
      else Result := TToken.create(ttBANG, ch, Line, Coln, Module);
    end;
    '"' :
    begin
      ReadChar;
      Result := TToken.create(ttSTRING, ReadString, Line, Coln, Module);
    end;
    '&' :
    begin
      if (PeekChar='&') then
      begin
        ReadChar;
        Result := TToken.create(ttLOGICALAND, '&&', Line, Coln, Module);
      end
    end;
    '|' :
    begin
      if (PeekChar='|') then
      begin
        ReadChar;
        Result := TToken.create(ttLOGICALOR, '||', Line, Coln, Module);
      end
    end;
    '<' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttLE, '<=', Line, Coln, Module);
      end
      else Result := TToken.create(ttLT, ch, Line, Coln, Module);
    end;
    '>' :
    begin
      if (PeekChar='=') then
      begin
        ReadChar;
        Result := TToken.create(ttGE, '>=', Line, Coln, Module);
      end
      else Result := TToken.create(ttGT, ch, Line, Coln, Module);
    end
  else
    if isDigit(ch) then
    begin
      Result := TToken.create(ttNUMBER, ReadNumber, Line, Coln, Module);
      Exit;
    end
    else
    if isLetter(ch) then
    begin
      Result :=  TToken.create(ttIDENT, ReadIdentifier, Line, Coln, Module);
      Result.TokenType := LookupIdent(Result.Literal);
      Exit;
    end;
  end;
  if Result=nil then
    Result := TToken.create(ttILLEGAL, '', Line, Coln, Module);

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
  begin
		ch := input[readPosition];
    if (ch = #13) then
    begin
      Coln := 1;
      Inc(Line);
    end
    else Inc(Coln);
  end;

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
  while CharInSet(ch, [' ', #13, #10, #8]) do  ReadChar;
end;

end.
