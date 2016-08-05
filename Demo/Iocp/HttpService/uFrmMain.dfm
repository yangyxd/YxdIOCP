object HttpService: THttpService
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Http '#36855#20320#26381#21153
  ClientHeight = 78
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  GlassFrame.Enabled = True
  GlassFrame.Left = 10
  GlassFrame.Top = 10
  GlassFrame.Right = 150
  GlassFrame.Bottom = 16
  GlassFrame.SheetOfGlass = True
  OldCreateOrder = True
  PrintScale = poPrintToFit
  ScreenSnap = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
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
    Top = 64
    Width = 274
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
  object Edit1: TEdit
    Left = 84
    Top = 15
    Width = 55
    Height = 21
    MaxLength = 6
    TabOrder = 0
    Text = '8080'
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
end
