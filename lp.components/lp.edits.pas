unit lp.edits;

interface

uses Classes, StdCtrls, ExtCtrls, Messages, Graphics, Controls, Windows, SysUtils, Forms;

type
  TWMScrollEvent = Procedure(Message: TMessage) of object;
  TLPIOnDrawBreakPoint = function(ACurrentLine:Integer): Boolean of object;
  TLPIOnBreakPointClick = procedure(ALine:Integer) of object;

  TLPIMemo = class(TMemo)
  private
    { Private declarations }
    FPaintBox: TPaintBox;
    FGutterSize: Word;
    FBreakPointSize: Word;
    FBorder: Word;
    FGutterColor: TColor;
    FGutterTextColor: TColor;
    FOnVerticalScroll: TWMScrollEvent;
    FOnHorizontalScroll: TWMScrollEvent;
    FOnDrawBreakPoint: TLPIOnDrawBreakPoint;
    FOnBreakPointClick: TLPIOnBreakPointClick;
    procedure SetEditRect;
    procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetGutterSize(const Value: Word);
    procedure SetGutterColor(const Value: TColor);
    procedure SetGutterTextColor(const Value: TColor);
    procedure SetBreakPointSize(const Value: Word);
    function  DoDrawBreakPoint(ACurrentLine:Integer): Boolean;
    procedure DoBreakPointClick(ALine:Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnVericalScroll: TWMScrollEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TWMScrollEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property GutterSize: Word read FGutterSize write SetGutterSize;
    property BreakPointSize: Word read FBreakPointSize write SetBreakPointSize;
    property GutterColor: TColor read FGutterColor write SetGutterColor default clInfoBk;
    property GutterTextColor: TColor read FGutterTextColor write SetGutterTextColor default clMenuText;
    property OnDrawBreakPoint: TLPIOnDrawBreakPoint read FOnDrawBreakPoint write FOnDrawBreakPoint;
    property OnBreakPointClick: TLPIOnBreakPointClick read FOnBreakPointClick write FOnBreakPointClick;
  end;

  TLPIListBox = class(TListBox)
  private
    { Private declarations }
    FPaintBox: TPaintBox;
    FGutterSize: Word;
    FArrowPointSize: Word;
    FBorder: Word;
    FGutterColor: TColor;
    FGutterTextColor: TColor;
    FOnVerticalScroll: TWMScrollEvent;
    FOnHorizontalScroll: TWMScrollEvent;
    FOnDrawArrow: TLPIOnDrawBreakPoint;
    procedure SetEditRect;
    procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetArrowPointSize(const Value: Word);
    procedure SetGutterColor(const Value: TColor);
    procedure SetGutterSize(const Value: Word);
    procedure SetGutterTextColor(const Value: TColor);
    function  DoDrawArrow(ACurrentLine:Integer): Boolean;
  protected
    { Protected declarations }
    procedure CreateWnd; override;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  published
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property OnVericalScroll: TWMScrollEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnHorizontalScroll: TWMScrollEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property GutterSize: Word read FGutterSize write SetGutterSize;
    property ArrowPointSize: Word read FArrowPointSize write SetArrowPointSize;
    property GutterColor: TColor read FGutterColor write SetGutterColor;
    property GutterTextColor: TColor read FGutterTextColor write SetGutterTextColor;
    property OnDrawArrow: TLPIOnDrawBreakPoint read FOnDrawArrow write FOnDrawArrow;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('LPI System', [TLPIMemo, TLPIListBox]);
end;


procedure TLPIMemo.WMHScroll(var Message: TMessage);
begin
  Inherited;
  If Assigned(FOnHorizontalScroll) Then
    FOnHorizontalScroll(Message);
end;

procedure TLPIMemo.WMVScroll(var Message: TMessage);
begin
  Inherited;
  If Assigned(FOnVerticalScroll) Then
    FOnVerticalScroll(Message);
end;

procedure TLPIMemo.setGutterSize(const Value: Word);
begin
  FGutterSize := Value;
  SetEditRect;
end;

constructor TLPIMemo.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:= ControlStyle + [csAcceptsControls];
  FBorder:=2;
  FGutterSize := 0;
  FBreakPointSize := 0;
  FPaintBox:= TPaintBox.Create(Self);
  FPaintBox.Width:= FGutterSize + FBreakPointSize + FBorder;
  FPaintBox.Height:= Height;
  FPaintBox.Visible:= True;
  FPaintBox.Parent:= Self;
  FPaintBox.Cursor:= crArrow;
  FPaintBox.OnMouseUp := DoMouseUp;
  FGutterColor:= Color;
  FGutterTextColor:= Font.Color;
end;

destructor TLPIMemo.Destroy;
begin
  inherited;
end;

procedure TLPIMemo.DoBreakPointClick(ALine: Integer);
begin
  if Assigned(FOnBreakPointClick) then
    FOnBreakPointClick(ALine);
end;

function TLPIMemo.DoDrawBreakPoint(ACurrentLine:Integer):Boolean;
begin
  Result := False;
  if Assigned(FOnDrawBreakPoint) then
    Result := FOnDrawBreakPoint(ACurrentLine);
end;

procedure TLPIMemo.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  J,S,L:Integer;
begin
  if Button = mbLeft then
  begin
    S:= FPaintBox.Canvas.TextHeight('W');
    J:= Perform(EM_GETFIRSTVISIBLELINE,0,0);
    L:= Y div S + J + 1;

    DoBreakPointClick(L);
    SendMessage(Handle,WM_PAINT,0,0);
  end;
end;

procedure TLPIMemo.WMPaint(var Msg: TWMPaint);
var
  TextSize: Integer;
  I,J:Integer;
begin
  inherited;
  FPaintBox.Canvas.Font:= Font;

  if (FBreakPointSize>0) then
  begin
    FPaintBox.Canvas.Brush.Color:= clBtnFace;
    FPaintBox.Canvas.FillRect(Rect(0,0,FBreakPointSize,FPaintBox.Height));
  end;

  { --  draw line border -- }
  FPaintBox.Canvas.FillRect(Rect(FBreakPointSize+FGutterSize, 0, FBreakPointSize+FGutterSize+FBorder, FPaintBox.Height));

  FPaintBox.Canvas.Brush.Color:= FGutterColor;
  FPaintBox.Canvas.Font.Color:= FGutterTextColor;
  FPaintBox.Canvas.FillRect(Rect(FBreakPointSize,0,FPaintBox.Width-FBorder,FPaintBox.Height));

  TextSize:= FPaintBox.Canvas.TextHeight('W');
  I := 0;
  J := Perform(EM_GETFIRSTVISIBLELINE,0,0);

  while ((I*TextSize) < fPaintBox.Height) do
    begin
      FPaintBox.Canvas.Brush.Color := FGutterColor;
      if (((J+I+1) mod 10 = 0) or ((J+I+1)=1))  then
        FPaintBox.Canvas.TextOut(FPaintBox.Canvas.ClipRect.Right - 2 - FPaintBox.Canvas.TextWidth(IntToStr(J+I+1)), (I * TextSize), IntToStr(J+I+1))
      else
      if ((J+I+1) mod 5 = 0)  then
        FPaintBox.Canvas.TextOut(FPaintBox.Canvas.ClipRect.Right - 2 - FPaintBox.Canvas.TextWidth('-'), (I * TextSize), '-')
      else
        FPaintBox.Canvas.TextOut(FPaintBox.Canvas.ClipRect.Right - 2 - FPaintBox.Canvas.TextWidth('.'), (I * TextSize), '.');

      if DoDrawBreakPoint(J+I+1) then
      begin
        FPaintBox.Canvas.Brush.Color := clRed;
        FPaintBox.Canvas.RoundRect(Rect(FPaintBox.Canvas.ClipRect.Left+2
          ,(I * TextSize)+2
          ,FPaintBox.Canvas.ClipRect.Left+2+10
          ,(I * TextSize)+TextSize-2)
          , 5 ,5
          );
      end;

      Inc(I);
    end;
end;

procedure TLPIMemo.CreateWnd;
begin
  inherited;
  SetEditRect;
end;

procedure TLPIMemo.SetBreakPointSize(const Value: Word);
begin
  FBreakPointSize := Value;
  SetEditRect;
  SendMessage(Handle,WM_PAINT,0,0);
end;

procedure TLPIMemo.SetEditRect;
var
  R: TRect;
  SBInfo: TScrollBarInfo;
  sysSize: Integer;
  sysScrollSize: Integer;
  {$IFNDEF WIN32}
  p: IntPtr;
  {$ENDIF}
begin
  if Self.BorderStyle <> bsNone then
    sysSize:= GetSystemMetrics(SM_CXEDGE)
  else
    sysSize:= 0;

  FillChar(SBInfo, Sizeof(SBInfo),0);
  SBInfo.cbSize := Sizeof(SBInfo);
  GetScrollBarInfo(Self.Handle, Integer(OBJID_VSCROLL), SBInfo);
  if (Self.ScrollBars in [ssVertical, ssBoth]) and
    not(
      (SBInfo.rgstate[0] and STATE_SYSTEM_INVISIBLE) = STATE_SYSTEM_INVISIBLE
    ) then
    sysScrollSize:= GetSystemMetrics(SM_CXVSCROLL)
  else
    sysScrollSize:= 0;

  R := RECT(FGutterSize+FBreakPointSize+sysSize+FBorder,0,Self.Width - sysScrollSize - sysSize,Self.Height - sysScrollSize);

  {$IFNDEF WIN32}
  p := Marshal.AllocHGlobal(Marshal.SizeOf(TypeOf(TRect)));
  Marshal.StructureToPtr(R,p,false);
  {$ENDIF}

  SendMessage(Handle,EM_SETRECT,0,{$IFDEF WIN32}Integer(@R){$ELSE}Integer(p){$ENDIF});
  FPaintBox.Width:= fGutterSize + FBreakPointSize + FBorder;
  FPaintBox.Height:= Height - sysScrollSize;
end;

procedure TLPIMemo.WMSize(var Message: TWMSize);
begin
  inherited;
  SetEditRect;
end;

procedure TLPIMemo.SetGutterColor(const Value: TColor);
begin
  FGutterColor:= Value;
  SendMessage(Handle,WM_PAINT,0,0);
end;

procedure TLPIMemo.SetGutterTextColor(const Value: TColor);
begin
  FGutterTextColor:= Value;
  SendMessage(Handle,WM_PAINT,0,0);
end;


{ TLPIListBox }

constructor TLPIListBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:= ControlStyle + [csAcceptsControls];
  Style := lbOwnerDrawFixed;
  FBorder:=2;
  FGutterSize := 0;
  FArrowPointSize := 0;
  FPaintBox:= TPaintBox.Create(Self);
  FPaintBox.Width:= FGutterSize + FArrowPointSize + FBorder;
  FPaintBox.Height:= Height;
  FPaintBox.Visible:= True;
  FPaintBox.Parent:= Self;
  FPaintBox.Cursor:= crArrow;
  FGutterColor:= Color;
  FGutterTextColor:= Font.Color;
end;

procedure TLPIListBox.CreateWnd;
begin
  inherited;
  SetEditRect;
end;

destructor TLPIListBox.Destroy;
begin
  inherited;
end;

function TLPIListBox.DoDrawArrow(ACurrentLine: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnDrawArrow) then
    Result := FOnDrawArrow(ACurrentLine);
end;

procedure TLPIListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Flags: Longint;
  Data: String;
begin
  Rect.Left := Rect.Left + FGutterSize + FArrowPointSize + FBorder;

  if odSelected in State then
  begin
    Canvas.Brush.Color:= clRed;
    Canvas.Font.Color := clWhite;
    Canvas.Font.Style := [fsBold, fsUnderline];
  end;

  Canvas.FillRect(Rect);
  if Index < Count then
  begin
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    if not UseRightToLeftAlignment then
      Inc(Rect.Left, 2)
    else
      Dec(Rect.Right, 2);
    Data := '';
    if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
      Data := DoGetData(Index)
    else
      Data := Items[Index];
    DrawText(Canvas.Handle, Data, Length(Data), Rect, Flags);
  end;

  if (odFocused in State) and not (odNoFocusRect in State) then
    Canvas.DrawFocusRect(Rect);
end;

procedure TLPIListBox.SetArrowPointSize(const Value: Word);
begin
  FArrowPointSize := Value;
  SetEditRect;
  SendMessage(Handle,WM_PAINT,0,0);
end;

procedure TLPIListBox.SetEditRect;
var
  R: TRect;
  sysSize: Integer;
  {$IFNDEF WIN32}
  p: IntPtr;
  {$ENDIF}
begin
  if Self.BorderStyle <> bsNone then
    sysSize:= GetSystemMetrics(SM_CXEDGE)
  else
    sysSize:= 0;

  R := RECT(FGutterSize+FArrowPointSize+sysSize+FBorder,0,Self.Width  - sysSize,Self.Height);

  {$IFNDEF WIN32}
  p := Marshal.AllocHGlobal(Marshal.SizeOf(TypeOf(TRect)));
  Marshal.StructureToPtr(R,p,false);
  {$ENDIF}

  SendMessage(Handle,EM_SETRECT,0,{$IFDEF WIN32}Integer(@R){$ELSE}Integer(p){$ENDIF});
  FPaintBox.Width:= fGutterSize + FArrowPointSize + FBorder;
  FPaintBox.Height:= Height;
end;

procedure TLPIListBox.SetGutterColor(const Value: TColor);
begin
  FGutterColor := Value;
  SendMessage(Handle,WM_PAINT,0,0);
end;

procedure TLPIListBox.SetGutterSize(const Value: Word);
begin
  FGutterSize := Value;
  SetEditRect;
  SendMessage(Handle,WM_PAINT,0,0);
end;

procedure TLPIListBox.SetGutterTextColor(const Value: TColor);
begin
  FGutterTextColor := Value;
  SendMessage(Handle,WM_PAINT,0,0);
end;

procedure TLPIListBox.WMHScroll(var Message: TMessage);
begin
  Inherited;
  If Assigned(FOnHorizontalScroll) Then
    FOnHorizontalScroll(Message);
end;

procedure TLPIListBox.WMPaint(var Msg: TWMPaint);
var
  TextSize: Integer;
  I,J:Integer;
begin
  inherited;
  FPaintBox.Canvas.Font:= Font;

  if (FArrowPointSize>0) then
  begin
    FPaintBox.Canvas.Brush.Color:= clBtnFace;
    FPaintBox.Canvas.FillRect(Rect(0,0,FArrowPointSize,FPaintBox.Height));
  end;

  { --  draw line border -- }
  FPaintBox.Canvas.FillRect(Rect(FArrowPointSize+FGutterSize, 0, FArrowPointSize+FGutterSize+FBorder, FPaintBox.Height));

  FPaintBox.Canvas.Brush.Color:= FGutterColor;
  FPaintBox.Canvas.Font.Color:= FGutterTextColor;
  FPaintBox.Canvas.FillRect(Rect(FArrowPointSize,0,FPaintBox.Width-FBorder,FPaintBox.Height));

  TextSize:= ItemHeight;// FPaintBox.Canvas.TextHeight('W');
  I := 0;
  J := Perform(EM_GETFIRSTVISIBLELINE,0,0);

  while ((I*TextSize) < fPaintBox.Height) do
    begin
      FPaintBox.Canvas.Brush.Color := FGutterColor;
      if (((J+I+1) mod 10 = 0) or ((J+I+1)=1))  then
        FPaintBox.Canvas.TextOut(FPaintBox.Canvas.ClipRect.Right - 2 - FPaintBox.Canvas.TextWidth(IntToStr(J+I+1)), (I * TextSize), IntToStr(J+I+1))
      else
      if ((J+I+1) mod 5 = 0)  then
        FPaintBox.Canvas.TextOut(FPaintBox.Canvas.ClipRect.Right - 2 - FPaintBox.Canvas.TextWidth('-'), (I * TextSize), '-')
      else
        FPaintBox.Canvas.TextOut(FPaintBox.Canvas.ClipRect.Right - 2 - FPaintBox.Canvas.TextWidth('.'), (I * TextSize), '.');

      if DoDrawArrow(J+I+1) then
      begin
        FPaintBox.Canvas.Brush.Color := clNone;
        FPaintBox.Canvas.Pen.Color := clGray;
        FPaintBox.Canvas.Polygon([
          Point(FPaintBox.Canvas.ClipRect.Left + 2,(I * TextSize) + 2),
          Point(FPaintBox.Canvas.ClipRect.Left + 2 + 10,(I * TextSize) + 2 + 5),
          Point(FPaintBox.Canvas.ClipRect.Left + 2,(I * TextSize)+10+2)
        ]);
        FPaintBox.Canvas.Brush.Color := clRed;
        FPaintBox.Canvas.Pen.Color := clBlack;
        FPaintBox.Canvas.Polygon([
          Point(FPaintBox.Canvas.ClipRect.Left + 2+1,(I * TextSize) + 2 + 1),
          Point(FPaintBox.Canvas.ClipRect.Left + 2 + 10+1,(I * TextSize) + 2 + 5+1),
          Point(FPaintBox.Canvas.ClipRect.Left + 2+1,(I * TextSize)+10+2+1)
        ]);
      end;

      Inc(I);
    end;
end;

procedure TLPIListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  SetEditRect;
end;

procedure TLPIListBox.WMVScroll(var Message: TMessage);
begin
  Inherited;
  If Assigned(FOnVerticalScroll) Then
    FOnVerticalScroll(Message);
end;

end.
