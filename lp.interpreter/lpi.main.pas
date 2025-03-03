unit lpi.main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, xmldom, XMLIntf, msxmldom, XMLDoc, ComCtrls, StdCtrls

  ,Generics.Collections
  ,ExtCtrls
  ,lp.lexer, lp.token, lp.parser, lp.environment , lp.evaluator
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


implementation

{$R *.dfm}

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
begin
  L:=TLexer.Create(MSouce.Lines.Text);
  try
    P := TParser.Create(L);
    try
      E:=TEnvironment.Create;
      try
        O := Eval(P.ParseProgram, E);
        if O<>nil then
        try
          List.Items.Add(O.Inspect)
        finally
          O.Free;
        end;
      finally
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
    if node is TASTIfExpression then
    begin
      N:= TVAST.Items.AddChild(SParent, node.toString);
      Describe(TASTIfExpression(node).Condition, N);
      Describe(TASTIfExpression(node).Consequence, N);
      Describe(TASTIfExpression(node).Alternative, N);
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
    then
      TVAST.Items.AddChild(SParent, node.toString);
  end;
end;


procedure TLPMain.FormCreate(Sender: TObject);
begin
  pcMain.TabIndex := 0;
end;

end.
