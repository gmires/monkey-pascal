object LPMain: TLPMain
  Left = 331
  Top = 145
  Caption = 'LP Interpreter v.1.0'
  ClientHeight = 815
  ClientWidth = 1150
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1150
    Height = 36
    Align = alTop
    TabOrder = 0
    object BtnDescribe: TButton
      AlignWithMargins = True
      Left = 943
      Top = 4
      Width = 100
      Height = 28
      Margins.Right = 0
      Align = alRight
      Caption = 'Describe'
      TabOrder = 0
      OnClick = BtnDescribeClick
    end
    object BtnRun: TButton
      AlignWithMargins = True
      Left = 1046
      Top = 4
      Width = 100
      Height = 28
      Align = alRight
      Caption = 'Run'
      TabOrder = 1
      OnClick = BtnRunClick
    end
  end
  object pcMain: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 39
    Width = 1144
    Height = 773
    ActivePage = TabSource
    Align = alClient
    TabOrder = 1
    ExplicitTop = 44
    ExplicitHeight = 768
    object TabSource: TTabSheet
      Caption = 'Source'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 0
        Width = 1130
        Height = 13
        Margins.Top = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'LP Source'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 55
      end
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 582
        Width = 1130
        Height = 13
        Margins.Top = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = 'Console Log'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 67
      end
      object SplitLog: TSplitter
        AlignWithMargins = True
        Left = 3
        Top = 598
        Width = 1130
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 610
      end
      object MSouce: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 16
        Width = 1130
        Height = 563
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'import "stdlib";'
          ''
          'let main = fn() {'
          '  let n = 12;'
          ''
          '  let fibonacci = fn(x) {'
          '    if (x == 0) {'
          '      return 0               '
          '    } else {'
          '      if (x == 1) {'
          '        return 1;     '
          '      } else {'
          '        return fibonacci(x - 1) + fibonacci(x - 2); '
          '      }'
          '    }'
          '  };'
          ''
          '  println(fibonacci(n));'
          '};'
          ''
          'main();')
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 8
        ExplicitHeight = 572
      end
      object List: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 607
        Width = 1130
        Height = 135
        Align = alBottom
        ItemHeight = 13
        PopupMenu = pmLog
        TabOrder = 1
      end
    end
    object TabAST: TTabSheet
      Caption = 'AST View'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label3: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 0
        Width = 1130
        Height = 13
        Margins.Top = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'AST Tree View'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 80
      end
      object TVAST: TTreeView
        AlignWithMargins = True
        Left = 3
        Top = 16
        Width = 1130
        Height = 726
        Align = alClient
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        ExplicitLeft = 95
        ExplicitTop = 160
        ExplicitWidth = 121
        ExplicitHeight = 97
      end
    end
  end
  object pmLog: TPopupMenu
    Left = 872
    Top = 688
    object MnuClearLog: TMenuItem
      Caption = 'Clear Log'
      OnClick = MnuClearLogClick
    end
  end
end
