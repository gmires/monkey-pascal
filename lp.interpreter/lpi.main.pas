unit lpi.main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, xmldom, XMLIntf, msxmldom, XMLDoc, ComCtrls, StdCtrls

  ,Generics.Collections, ExtCtrls
  ,lp.lexer, lp.token, lp.parser, lp.environment , lp.evaluator, lp.builtins
  ;


type
  TLPMain = class(TForm)
    Panel1: TPanel;
    BtnDescribe: TButton;
    BtnRun: TButton;
    pcMain: TPageControl;
    TabSource: TTabSheet;
    TabAST: TTabSheet;
    Label2: TLabel;
    MSouce: TMemo;
    Label1: TLabel;
    List: TListBox;
    TVAST: TTreeView;
    Label3: TLabel;
    SplitLog: TSplitter;
    procedure BtnDescribeClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure Describe(node: TASTNode; SParent:TTreeNode);
  public
    { Public declarations }
  end;

var
  LPMain: TLPMain;
  MLog: TListBox;

implementation

{$R *.dfm}

function _PrintLn(args: TList<TEvalObject>): TEvalObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to args.Count-1 do
    MLog.Items.Add(args[i].Inspect);
end;


procedure TLPMain.BtnDescribeClick(Sender: TObject);
var
  L:TLexer;
  P:TParser;
  i:Integer;
begin
  TVAST.Items.Clear;

  L:=TLexer.Create(MSouce.Lines.Text);
  try
    P := TParser.Create(L);
    try
      Describe(P.ParseProgram, nil);
    finally
      P.Free;
    end;
  finally
    L.Free;
  end;

  for i := 0 to TVAST.Items.Count-1 do
    TVAST.Items[i].Expand(True);

  pcMain.TabIndex := 1;
end;

procedure TLPMain.BtnRunClick(Sender: TObject);
var
  L:TLexer;
  E:TEnvironment;
  P:TParser;
  O:TEvalObject;
  Prg:TASTProgram;
  Ev:TEvaluator;
begin
  L:=TLexer.Create(MSouce.Lines.Text);
  try
    P := TParser.Create(L);
    try
      E:=TEnvironment.Create;
      Ev:=TEvaluator.Create;
      try
        Prg:=P.ParseProgram;
        try
          if (P.Errors.Count>0) then
            List.Items.AddStrings(P.Errors)
          else
          begin
            O := Ev.Eval(Prg, E);
            if O<>nil then
              List.Items.Add(O.Inspect)
          end;
        finally
          FreeAndNil(Prg);
        end;
      finally
        List.Items.Add('Gc before sweep Object = ' + IntToStr(Ev.Gc.ElemCount) + ', in trash = ' + IntToStr(Ev.Gc.TrashCount));
        Ev.Sweep(E);
        List.Items.Add('Gc 1° after sweep Object = ' + IntToStr(Ev.Gc.ElemCount) + ', in trash = ' + IntToStr(Ev.Gc.TrashCount));
        Ev.Sweep(E);
        List.Items.Add('Gc 2° after sweep Object = ' + IntToStr(Ev.Gc.ElemCount) + ', in trash = ' + IntToStr(Ev.Gc.TrashCount));
        Ev.Free;
        E.Free;
      end;
    finally
      P.Free;
    end;
  finally
    L.Free;
  end;
end;

procedure TLPMain.Describe(node: TASTNode; SParent: TTreeNode);
var
  N,N1:TTreeNode;
  i:Integer;
  key: TASTExpression;
begin
  if (node<>nil) then
  begin
    if node is TASTProgram then
    begin
      for i := 0 to TASTProgram(node).FStatements.Count-1 do
        Describe(TASTProgram(node).FStatements[i], nil);
    end
    else
    if node is TASTExpressionStatement then
      Describe(TASTExpressionStatement(node).Expression, TVAST.Items.AddChild(SParent, node.toString))
    else
    if node is TASTLetStatement then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      N:= TVAST.Items.AddChild(N, TASTLetStatement(node).Name.toString);
      Describe(TASTLetStatement(node).Expression, N);
    end
    else
    if node is TASTImportStatement then
      N:= TVAST.Items.AddChild(SParent, node.toString)
    else
    if node is TASTReturnStatement then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTReturnStatement(node).ReturnValue, N);
    end
    else
    if node is TASTBlockStatement then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      for i := 0 to TASTBlockStatement(node).Statements.Count-1 do
        Describe(TASTBlockStatement(node).Statements[i], N);
    end
    else
    if node is TASTPrefixExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTPrefixExpression(node).Right, N);
    end
    else
    if node is TASTInfixExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTInfixExpression(node).Left, N);
      Describe(TASTInfixExpression(node).Right, N);
    end
    else
    if node is TASTPostfixExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTPostfixExpression(node).Left, N);
    end
    else
    if node is TASTAssignExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTAssignExpression(node).Name, N);
      Describe(TASTAssignExpression(node).Expression, N);
    end
    else
    if node is TASTIfExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTIfExpression(node).Condition, N);
      Describe(TASTIfExpression(node).Consequence, N);
      Describe(TASTIfExpression(node).Alternative, N);
    end
    else
    if node is TASTSwitchExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTSwitchExpression(node).Value, N);
      for i := 0 to TASTSwitchExpression(node).Choises.Count-1 do
        Describe(TASTSwitchExpression(node).Choises[i], N);
    end
    else
    if node is TASTCaseExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      for i := 0 to TASTCaseExpression(node).Values.Count-1 do
        Describe(TASTCaseExpression(node).Values[i], N);
      Describe(TASTCaseExpression(node).Body, N);
    end
    else
    if node is TASTTernaryExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTTernaryExpression(node).Condition, N);
      Describe(TASTTernaryExpression(node).IfTrue, N);
      Describe(TASTTernaryExpression(node).IfFalse, N);
    end
    else
    if node is TASTWhileExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTWhileExpression(node).Condition, N);
      Describe(TASTWhileExpression(node).Body, N);
    end
    else
    if node is TASTForExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTForExpression(node).Condition, N);
      Describe(TASTForExpression(node).Expression, N);
      Describe(TASTForExpression(node).Body, N);
    end
    else
    if node is TASTCallExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTCallExpression(node).Funct, N);
      N:= TVAST.Items.AddChild(N, 'args');
      for i := 0 to TASTCallExpression(node).Args.Count-1 do
        Describe(TASTCallExpression(node).Args[i], N);
    end
    else
    if node is TASTFunctionLiteral then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTFunctionLiteral(node).Body, N);
    end
    else
    if node is TASTArrayLiteral then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      if Assigned(TASTArrayLiteral(node).Elements) then
        for i := 0 to TASTArrayLiteral(node).Elements.Count-1 do
          Describe(TASTArrayLiteral(node).Elements[i], N);
    end
    else
    if node is TASTIndexExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTIndexExpression(node).Left, N);
      Describe(TASTIndexExpression(node).Index, N);
    end
    else
    if node is TASTHashLiteral then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      for key in TASTHashLiteral(node).Pairs.Keys do
      begin
        N1:= TVAST.Items.AddChild(N, key.toString);
        Describe(TASTHashLiteral(node).Pairs[key],N1);
      end;
    end
    else
    if (node is TASTNumberLiteral)
    or (node is TASTStringLiteral)
    or (node is TASTBoolean)
    or (node is TASTIdentifier)
    or (node is TASTNullLiteral)
    then
      TVAST.Items.AddChild(SParent, node.toString);
  end;
end;


procedure TLPMain.FormCreate(Sender: TObject);
begin
  pcMain.TabIndex := 0;
  MLog:= List;
end;

procedure init;
begin
  builtins['println'] := TBuiltinObject.Create(_PrintLn);
end;

initialization
  init;

end.
