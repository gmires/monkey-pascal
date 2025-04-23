unit lp.utils;

interface

{$I lp.inc}

uses {$IFDEF LPI_D28} JSON {$ELSE} DBXJSON {$ENDIF}
  , SysUtils, Classes, ZLib;

function  StrSplit(var S:String; C:Char):string;
function  StrSplitPlus(var S:String; C:string):string;

function  JSONToString(O:TJSONObject):string;

procedure JSONSaveToFile(O:TJSONObject; JFileName:string);
procedure JSONSaveToFileCompress(O:TJSONObject; JFileName:string);
function  JSONLoadToFile(JFileName:string):TJSONObject;
function  JSONLoadToCompress(JFileName:string):TJSONObject; overload;
function  JSONLoadToCompress(stream:TStream):TJSONObject; overload;

procedure CompressStream(inpStream, outStream: TStream);
procedure DecompressStream(inpStream, outStream: TStream);

implementation

function StrSplit(var S:String; C:Char):string;
var
  P:Integer;
  R:String;
begin
  P:=Pos(C,S);
  if P=0 then
  begin
    R:=S;
    S:='';
  end
  else
  begin
    R:=Copy(S,1,P-1);
    S:=Copy(S,P+1,length(S)-P);
  end;
  Result:=R;
end;


function StrSplitPlus(var S:String; C:string):string;
var
  P:Integer;
  R:String;
begin
  P:=Pos(C,S);
  if P=0 then
  begin
    R:=S;
    S:='';
  end
  else
  begin
    R:=Copy(S,1,P-1);
    S:=Copy(S,P+length(C),length(S)-P);
  end;
  Result:=R;
end;

function JSONToString(O:TJSONObject):string;
var
  Buffer:TBytes;
  n:Integer;
  S:RawByteString;
begin
  SetLength(Buffer, O.EstimatedByteSize);
  n:= O.ToBytes(Buffer,0);
  SetLength(Buffer, n);
  SetLength(S, n);

  Move(Buffer[0], s[1], n);

  Result := S;
end;

procedure JSONSaveToFile(O:TJSONObject; JFileName:string);
var
  L:TStringList;
begin
  L:=TStringList.Create;
  try
    L.Text := JSONToString(O);
    L.SaveToFile(JFileName);
  finally
    L.Free;
  end;
end;

procedure JSONSaveToFileCompress(O:TJSONObject; JFileName:string);
var
  L:TStringStream;
  C:TMemoryStream;
begin
  L:=TStringStream.Create;
  C:=TMemoryStream.Create;
  try
    L.WriteString(JSONToString(O));
    L.Position := 0;
    CompressStream(L,C);
    C.Position := 0;
    C.SaveToFile(JFileName);
  finally
    C.Free;
    L.Free;
  end;
end;

function JSONLoadToFile(JFileName:string):TJSONObject;
var
  S:TStringStream;
begin
  S:=TStringStream.Create;
  try
    S.LoadFromFile(JFileName);
    S.Position := 0;
    Result := TJSONObject.ParseJSONValue(S.DataString) as TJSONObject;
  finally
    S.Free;
  end;
end;

function JSONLoadToCompress(stream:TStream):TJSONObject;
var
  S:TStringStream;
begin
  S:=TStringStream.Create;
  try
    stream.Position := 0;
    DecompressStream(stream, S);
    S.Position := 0;
    Result := TJSONObject.ParseJSONValue(S.DataString) as TJSONObject;
  finally
    S.Free;
  end;
end;

function JSONLoadToCompress(JFileName:string):TJSONObject;
var
  C:TMemoryStream;
  S:TStringStream;
begin
  C:=TMemoryStream.Create;
  S:=TStringStream.Create;
  try
    C.LoadFromFile(JFileName);
    C.Position := 0;
    DecompressStream(C, S);
    S.Position := 0;
    Result := TJSONObject.ParseJSONValue(S.DataString) as TJSONObject;
  finally
    S.Free;
    C.Free;
  end;
end;


procedure CompressStream(inpStream, outStream: TStream);
var
  C: TCompressionStream;
begin
  C := TCompressionStream.Create(clMax,outStream);
  try
    C.CopyFrom(inpStream,0);
  finally
    C.Free;
  end;
end;

procedure DecompressStream(inpStream, outStream: TStream);
var
  D: TDecompressionStream;
  nSize: Integer;
  Buffer: array [0..4095] of byte;
begin
  D := TDecompressionStream.Create(inpStream);
  try
    nSize := 4096;
    while nSize > 0 do begin
      nSize := D.Read(Buffer[0], 4096);
      if nSize > 0 then outStream.Write(Buffer[0], nSize);
    end;
  finally
    D.Free;
  end;
end;

end.
