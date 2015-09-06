object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'IocpUDP ECHO'
  ClientHeight = 284
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 192
    Top = 13
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object lbSvrInfo: TLabel
    Left = 8
    Top = 56
    Width = 44
    Height = 13
    Caption = 'lbSvrInfo'
  end
  object Label3: TLabel
    Left = 8
    Top = 81
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label5: TLabel
    Left = 8
    Top = 139
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label6: TLabel
    Left = 8
    Top = 173
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label7: TLabel
    Left = 89
    Top = 120
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label8: TLabel
    Left = 89
    Top = 155
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label9: TLabel
    Left = 8
    Top = 192
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label10: TLabel
    Left = 8
    Top = 211
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label11: TLabel
    Left = 8
    Top = 230
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label12: TLabel
    Left = 8
    Top = 254
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = #24320#21551
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = #20572#27490
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 222
    Top = 8
    Width = 59
    Height = 21
    TabOrder = 2
    Text = '9983'
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 336
    Top = 128
  end
  object XPManifest1: TXPManifest
    Left = 336
    Top = 208
  end
end
