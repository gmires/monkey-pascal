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
      object tbStepInto: TToolButton
        Left = 0
        Top = 0
        Hint = 'Step Into'
        ImageIndex = 0
        ParentShowHint = False
        ShowHint = True
        OnClick = tbStepIntoClick
      end
      object tbContinue: TToolButton
        Left = 31
        Top = 0
        Hint = 'Continue'
        ImageIndex = 1
        ParentShowHint = False
        ShowHint = True
        OnClick = tbContinueClick
      end
      object tbExit: TToolButton
        Left = 62
        Top = 0
        Hint = 'Stop'
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
        Top = 43
        Width = 420
        Height = 361
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
        end
      end
      object ComboEnv: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 19
        Width = 420
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        Style = csDropDownList
        TabOrder = 2
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
      494C01010300C401CC0118001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
      00000000000000000000F7842000F5861A000000000000000000000000000000
      000000000000B96F20009D6C310000000000AD6B2100B5711C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000075B138006EB232006A96460000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004363F700425FFF00425EFF00425EFF00405DFF00415E
      FF00415EFF00425FFE00415EFF00415EFF003E5EFE004A65FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E7D8C000DC7D2400DF832500D8812B00C85F0A00000000000000
      0000E3C9A500D8771400D6680E0000000000C9580000DA701600C9743D000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006AAC360072AE340070B032006BB13000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000345CFF003F60FC00415EFF00415EFF00415EFF00415EFF00405D
      FE00405DFE00405DFE00415EFF00405DFE00405DFE00405DFE002E3DF8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E6DCC100E07D2600DD802400EA7F1F00F17F2100D9831E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006BAE360071AF330070AE35006BAD2B0071B03700619F
      2800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004460FF003B63FB00415EFF00415EFF00415EFF00415EFF00415E
      FF00415EFF00415EFF00405DFE00405DFE00405DFE00405DFE00395DFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E7DBB700E0812100DF802300E0802200E0802200E0802200D881
      2600E97F1C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EAF340070AF340070AF340070AF340070AF340071B0
      35006BAB2E0081AD550000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003D5DFF00415FFB00425EFF00415EFF00415EFF00415EFF00415E
      FF00415EFF00415EFF00415EFF00415EFF00415EFF00405DFE003F5BFD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E4D9B700DF7F2000DF802300E0802200E0802200E0802200E17F
      2300E17D2700DE812000A65F1200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EAE330070AF340070AF340070AF340070AF340070AF
      340071AF33006EAF300076B83700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5CFF004260FD00415EFF00415EFF00415EFF00415EFF00405D
      FE00405DFE00415EFF00405DFE00415EFF00415EFF00405DFE003F5BFD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E4D9B700DF7F2000DF802300E0802200E0802200E0802200E07E
      2500E0802200D6812500E6812100E38525000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006DAD340070AF340070AF340070AF340070AF340070AF
      340070AF340070AF340070AF330070AD3800508A220000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5CFF004260FD00415EFF00415EFF00415EFF00415EFF00405D
      FE00405DFE00415EFF00405DFE00415EFF00415EFF00405DFE003F5BFD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E1D4B000DF7F2000DF802300E0802200E0802200E0802200E080
      2200E0802200E0802200E0802200DF7F2100D27E1D00A5560B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006DAD350070AF340070AF340070AF340070AF340070AF
      340070AF340070AF340070AF350070B0310070AE36006DB42B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5CFF004260FD00415EFF00415EFF00415EFF00415EFF00405D
      FE00405DFE00415EFF00415EFF00405DFE00405DFE00405DFE00415DFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E1D3B000DF7F2000DF802300E0802200E0802200E0802200E080
      2200E0802200E0802200E0802200E0802200DA852400A6550300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EAE360070AF340070AF340070AF340070AF340070AF
      340070AF340070AF340070B0320070AD380070AF33006DB02D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5CFF004260FD00415EFF00415EFF00415EFF00415EFF00415E
      FF00415EFF00415EFF00415EFF00405DFE00405DFE00405DFE003F5BFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E2D0AD00E2811F00DF802300E0802200E0802200E0802200E181
      1F00DE7E2700DE822000DD812700E8892800FFFEFB0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EB0330070AF340070AF340070AF340070AF340070AF
      34006FAF340070AE34006FAF2F006DB236005A862C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5DFF004260FE00415EFF00415EFF00415EFF00405DFE00415E
      FF00415EFF00415EFF00405DFE00405DFE00405DFE00405DFE003F5BFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E2CFAD00E2811F00DF802300E0802200E0802200E0802200E180
      2400DC812100DE7D2500B8641F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EB0330070AF340070AF340070AF340070AF340070AF
      34006FAF340070B432006FAF3300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5DFF004260FE00415EFF00415EFF00415EFF00405DFE00415E
      FF00415EFF00415EFF00405DFE00405DFE00405DFE00405DFE003F5BFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E1CEAD00E2811F00DF802300E0802200E0802200E0802200DF81
      2100D7761C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EAF350070AF340070AF34006BB1320074B032006CAC
      32006EB12B00FDFEFC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003C5DFF004260FE00415EFF00415EFF00405DFE00405DFE00405D
      FE00405DFE00405DFE00405DFE00405DFE00405DFE00405DFE00415CFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E5CFAD00DD7F2600E17F2500E1821F00E17F2500E8842200FBEA
      D400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006EAE360070AF340070AF340072B12E006DB52E0083A8
      6000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003F5BFF003E64F500415EFF00415EFF00405DFE00405DFE00405D
      FE00405DFE00435CFE00435CFE00435CFE00435CFE00435CFE004061FE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DDD1BA00DB791C00DC832900E67C2100BE671D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000006AAB330070AF34006BAA2F0060982000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000003059FF00435BFF00405DFE00405DFE00405DFE00405DFE00405D
      FE00405DFE00435CFE00435CFE00435CFE00435CFE00465FFF002E4CFA000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C1651400CA6410000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000060A0270067A82200FFFFFE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000003656FF00415EFE00415EFE00405EFE00415EFE00405E
      FE00405EFE00445CFE00445DFE00445DFE00455DFE002F4AFA00000000000000
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
      FFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000
      FFFFFFFFFFFFFFFFFF000000FCF93FFC7FFFFC003F000000F8311FFC3FFFF800
      1F000000F81FFFFC0FFFF8001F000000F807FFFC03FFF8001F000000F801FFFC
      01FFF8001F000000F800FFFC007FF8001F000000F8003FFC003FF8001F000000
      F8003FFC003FF8001F000000F8007FFC007FF8001F000000F801FFFC01FFF800
      1F000000F807FFFC03FFF8001F000000F80FFFFC0FFFF8001F000000F83FFFFC
      3FFFF8001F000000FCFFFFFC7FFFFC003F000000FFFFFFFFFFFFFFFFFF000000
      FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFF
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
