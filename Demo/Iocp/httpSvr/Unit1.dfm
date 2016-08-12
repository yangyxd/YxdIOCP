object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'IOCP Http '#26381#21153
  ClientHeight = 328
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
  object Label2: TLabel
    Left = 8
    Top = 273
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
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 2
    Text = '8080'
  end
  object Button3: TButton
    Left = 449
    Top = 8
    Width = 75
    Height = 25
    Caption = #20840#37096#26029#24320
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 530
    Top = 8
    Width = 92
    Height = 25
    Caption = #20869#23384#27744#27979#35797
    TabOrder = 4
  end
  object Button5: TButton
    Left = 449
    Top = 249
    Width = 173
    Height = 25
    Caption = 'GMT'#36716#25442#20989#25968#24615#33021#27979#35797
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 547
    Top = 218
    Width = 75
    Height = 25
    Caption = 'Button6'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 547
    Top = 187
    Width = 75
    Height = 25
    Caption = 'Button7'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 336
    Top = 128
  end
end
