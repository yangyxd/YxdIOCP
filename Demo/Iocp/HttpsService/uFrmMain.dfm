object HttpService: THttpService
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Https '#36855#20320#26381#21153
  ClientHeight = 111
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  PrintScale = poPrintToFit
  ScreenSnap = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  GlassFrame.Bottom = 16
  GlassFrame.Enabled = True
  GlassFrame.Left = 10
  GlassFrame.Right = 150
  GlassFrame.SheetOfGlass = True
  GlassFrame.Top = 10
  object Label1: TLabel
    Left = 16
    Top = 18
    Width = 60
    Height = 14
    Caption = #26381#21153#31471#21475#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 0
    Top = 97
    Width = 335
    Height = 14
    Align = alBottom
    Alignment = taCenter
    Caption = 'YangYxd '#29256#26435#25152#26377
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ExplicitWidth = 101
  end
  object Label3: TLabel
    Left = 16
    Top = 53
    Width = 39
    Height = 13
    Caption = #29366#24577#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 61
    Top = 53
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 61
    Top = 72
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 82
    Top = 16
    Width = 55
    Height = 21
    MaxLength = 6
    TabOrder = 0
    Text = '443'
  end
  object BitBtn1: TBitBtn
    Left = 168
    Top = 13
    Width = 89
    Height = 25
    Caption = #21551#21160#26381#21153
    TabOrder = 1
    TabStop = False
    OnClick = BitBtn1Click
  end
  object XPManifest1: TXPManifest
    Left = 136
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = Timer1Timer
    Left = 246
    Top = 48
  end
end
