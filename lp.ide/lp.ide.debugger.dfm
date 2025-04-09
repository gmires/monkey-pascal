object LPIdeDebugger: TLPIdeDebugger
  Left = 319
  Top = 177
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'LP Debugger'
  ClientHeight = 671
  ClientWidth = 1216
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Consolas'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 426
    Top = 31
    Height = 640
    Color = clBtnFace
    ParentColor = False
    ExplicitLeft = 575
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1216
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = -178
    ExplicitWidth = 1314
    object TbMain: TToolBar
      Left = 0
      Top = 0
      Width = 1216
      Height = 31
      Align = alClient
      ButtonHeight = 30
      ButtonWidth = 31
      Caption = 'TbMain'
      Color = clSilver
      DrawingStyle = dsGradient
      Images = ILDebugger
      ParentColor = False
      TabOrder = 0
      ExplicitWidth = 1314
      object tbStepInto: TToolButton
        Left = 0
        Top = 0
        Hint = 'Prossimo Step'
        ImageIndex = 0
        ParentShowHint = False
        ShowHint = True
        OnClick = tbStepIntoClick
      end
      object tbContinue: TToolButton
        Left = 31
        Top = 0
        Hint = 'Continua'
        ImageIndex = 1
        ParentShowHint = False
        ShowHint = True
        OnClick = tbContinueClick
      end
      object tbExit: TToolButton
        Left = 62
        Top = 0
        Hint = 'Esci'
        Caption = 'tbExit'
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = tbExitClick
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 31
    Width = 426
    Height = 640
    Align = alLeft
    BevelOuter = bvNone
    Color = clBlack
    ParentBackground = False
    TabOrder = 1
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 426
      Height = 640
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel5'
      TabOrder = 0
      ExplicitWidth = 807
      ExplicitHeight = 218
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 420
        Height = 13
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Environment'
        Color = clNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        ExplicitWidth = 72
      end
      object Splitter2: TSplitter
        Left = 0
        Top = 407
        Width = 426
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 16
        ExplicitWidth = 394
      end
      object VLEnv: TValueListEditor
        AlignWithMargins = True
        Left = 3
        Top = 19
        Width = 420
        Height = 385
        Align = alClient
        DrawingStyle = gdsGradient
        FixedCols = 1
        KeyOptions = [keyUnique]
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        PopupMenu = pmEnv
        TabOrder = 0
        TitleCaptions.Strings = (
          'Name'
          'Value')
        ExplicitLeft = -137
        ColWidths = (
          159
          255)
        RowHeights = (
          18
          18)
      end
      object Panel4: TPanel
        Left = 0
        Top = 410
        Width = 426
        Height = 230
        Align = alBottom
        BevelOuter = bvNone
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 1
        object Label3: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 420
          Height = 13
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Evaluate/Modify'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 93
        end
        object edtEval: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 19
          Width = 420
          Height = 21
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnKeyDown = edtEvalKeyDown
          ExplicitWidth = 281
        end
        object MEvalResult: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 43
          Width = 420
          Height = 184
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Consolas'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 1
          ExplicitWidth = 281
          ExplicitHeight = 578
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 429
    Top = 31
    Width = 787
    Height = 640
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    Enabled = False
    ParentBackground = False
    TabOrder = 2
    ExplicitLeft = 5
    ExplicitWidth = 231
    ExplicitHeight = 588
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 781
      Height = 13
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Debugger Source'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 97
    end
    object LSource: TLPIListBox
      AlignWithMargins = True
      Left = 3
      Top = 19
      Width = 781
      Height = 618
      Style = lbOwnerDrawFixed
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
      GutterSize = 24
      ArrowPointSize = 16
      GutterColor = clWindow
      GutterTextColor = clWindowText
      OnDrawArrow = LSourceDrawArrow
      ExplicitLeft = 128
      ExplicitTop = 91
      ExplicitWidth = 1088
      ExplicitHeight = 449
    end
  end
  object ILDebugger: TImageList
    Height = 24
    Width = 24
    Left = 132
    Top = 113
    Bitmap = {
      494C010103004401940118001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000001800000001002000000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FBFEFF0029C9FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009BF300FDFFFF00000000000000
      00004C483D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FAFFFE0028C7FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009DF400FFFFFE000000
      0000514A3B00514A390000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFDFD000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FDFFFF00AF99
      5F00B29A6000AD9962002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF500534C3800524B
      3C00524A3900514A3B004F4B3900FFFFFE000000000000000000000000000000
      00000000000000000000FFFEFF00221EFF00201EFF001F1DFF00FFFFFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B2996100B2996100B29A
      6000B0986200AD9B60002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF500544A39005249
      3B00514A3B00514A3B004F493C00FFFFFE000000000000000000000000000000
      000000000000000000007978FE001F1FFF001F1FFF001F1FFF001D1FFF007E7C
      F300FFFFFE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EDE2C700B2986200FFFEFF000000
      000000000000FFFFFC0031C6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009EF20000000000FBFC
      FA00514A3B00524B3C0000000000000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF00FFFCFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B4996100AF996500000000000000
      00000000000000000000FFFEFF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009DF700FFFEFF0000000000FDFF
      FF004E483B000000000000000000000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1EFF002221FF00FDFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B298620000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF002224FB00FFFFFE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B6996000AC986100000000000000
      00000000000000000000000000002EC8FD002FC6FF002EC6FF002EC6FF002EC6
      FF00009CF600009CF600009CF600009CF600049CF40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001D1FFF00F3EDFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F1E3C600B6996000000000000000
      000000000000000000002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009DF600FFFEFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF00221F
      FF00FDFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B4986200B2996100B299
      6100B1986000B39860002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF50051483F00524B
      3C0051493C00FFFEFF0000000000000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF00FDFEFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFF00FBFFFE00B099
      6100AF986000B39860002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF50051483F00514A
      3B00514A3B00514A3B004E483B00000000000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF00FDFEFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFEFF002FC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF600FDFEFF000000
      0000FFFEFF0000000000514A3B00FFFFFB000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001F1E
      FF00FBFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002EC8FD002FC6FF002EC6FF002EC6FF002EC6
      FF00009CF600009CF600009CF600009CF600049CF40000000000000000000000
      0000000000000000000051493C004F4A3B000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF001F1FFF001D1FFF00F3EDFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFE00514A3B000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF001F1FFF002223FD00FFFFFE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A7976900FDFF
      FF000000000000000000000000002EC6FF002BC5FF002EC6FF002EC6FF002EC6
      FF00029DF400029DF400029DF400009CF500009EF60000000000000000000000
      00000000000000000000504A3D004F4A3B000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF001F1FFF001F1FFF00FBFFFE00FFFEFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFD00B2996100B39A
      620000000000FFFFFC0031C6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009EF200000000000000
      00000000000000000000514A3B00CECAC5000000000000000000000000000000
      000000000000000000007A78FF001F1FFF001F1FFF001F1FFF001F1FFF001F1F
      FF001F1FFF00FFFCFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2996100B2996100B09B5E00B299
      6100B29A6000AD9B60002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF50054493B005249
      3B0051493C00514A3B00544C3F00000000000000000000000000000000000000
      000000000000000000007978FE001F1FFF001F1FFF001F1FFF001D1FFF007E7C
      F300FFFFFE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B2996100B2996100B2986200B299
      6100B2996100AE9B5E002EC6FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009CF5004D4D3500514A
      3B00514A3B000000000000000000000000000000000000000000000000000000
      00000000000000000000F8FBFF00201FFF001F1FFF001F1EFF00FFFFFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F9F9F9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFFFE00B2996100B09A
      6000FDFFFF00FAFFFE0028C7FF002EC6FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009CF500009DF400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCFCFC000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFEFF00B39B65000000
      00000000000000000000FBFEFF0029C9FF002EC6FF002EC6FF002EC6FF002EC6
      FF00009CF500009CF500009CF500009CF500009BF300FDFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FDFFFE00FFFFFE00FFFFFE00FFFFFE00FFFF
      FE00000000000000000000000000FFFEFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFFFFFFFF000000FFFFFFFF
      FFFFFFFFFF000000FC0037FFFFFFFFFFFF000000F80013FFFFFFF0000F000000
      C00000FC1FFFE0000F000000800000FC07FFE00007000000180023FC03FFE000
      070000003C0027FC00FFE000070000007FFFFFFC003FE000070000003E007FFC
      001FE000070000003C001FFC0007E00007000000800003FC0003E00007000000
      800001FC0003E00007000000F80014FC0007E00007000000FE007CFC001FE000
      07000000FFFFFCFC003FE00007000000CE007CFC007FE0000700000088003CFC
      03FFE00007000000000001FC07FFE00007000000000007FC1FFFE00007000000
      80003FFFFFFFF0000F0000009C003FFFFFFFFFFFFF000000FE0EFFFFFFFFFFFF
      FF000000FFFFFFFFFFFFFFFFFF00000000000000000000000000000000000000
      000000000000}
  end
  object pmEnv: TPopupMenu
    OnPopup = pmEnvPopup
    Left = 265
    Top = 260
    object MnuCopyValue: TMenuItem
      Caption = 'Copy Value'
      OnClick = MnuCopyValueClick
    end
  end
end
