unit lp.utils;

interface

uses DBXJSON, SysUtils, Classes;

function  StrSplit(var S:String; C:Char):string;
function  StrSplitPlus(var S:String; C:string):string;

function  JSONToString(O:TJSONObject):string;
procedure JSONSaveToFile(O:TJSONObject; JFileName:string);

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



end.
