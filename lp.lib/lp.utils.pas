unit lp.utils;

interface

function StrSplit(var S:String; C:Char):string;
function StrSplitPlus(var S:String; C:string):string;

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


end.
