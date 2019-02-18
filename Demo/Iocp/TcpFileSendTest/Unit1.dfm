object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'YxdIcop TCP '#25968#25454#20256#36755#36895#24230#27979#35797
  ClientHeight = 569
  ClientWidth = 711
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
  DesignSize = (
    711
    569)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 64
    Width = 104
    Height = 13
    Caption = #25991#20214#20256#36755#23458#25143#31471#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 48
    Top = 126
    Width = 60
    Height = 13
    Caption = #26381#21153#31471#21475#65306
  end
  object Label3: TLabel
    Left = 48
    Top = 153
    Width = 84
    Height = 13
    Caption = #35201#21457#36865#30340#25991#20214#65306
  end
  object Label4: TLabel
    Left = 17
    Top = 16
    Width = 104
    Height = 13
    Caption = #25991#20214#20256#36755#26381#21153#31471#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 174
    Top = 320
    Width = 60
    Height = 13
    Caption = #20256#36755#29366#24577#12290
  end
  object Label6: TLabel
    Left = 17
    Top = 368
    Width = 65
    Height = 13
    Caption = #26085#24535#20449#24687#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 48
    Top = 99
    Width = 60
    Height = 13
    Caption = #26381#21153#22320#22336#65306
  end
  object edtPort: TEdit
    Left = 174
    Top = 13
    Width = 121
    Height = 21
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 0
    Text = '9983'
  end
  object btnOpen: TButton
    Left = 307
    Top = 11
    Width = 75
    Height = 25
    Caption = #24320#21551#26381#21153
    TabOrder = 1
    OnClick = btnOpenClick
  end
  object Edit1: TEdit
    Left = 174
    Top = 123
    Width = 121
    Height = 21
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 2
    Text = '9983'
  end
  object Edit2: TEdit
    Left = 174
    Top = 150
    Width = 467
    Height = 21
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 3
    Text = 'E:\back\OS\windows_10_enterprise_x64_2017.iso'
  end
  object Button1: TButton
    Left = 174
    Top = 192
    Width = 91
    Height = 25
    Caption = #24314#31435#36830#25509
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 174
    Top = 240
    Width = 91
    Height = 25
    Caption = #21457#36865#25991#20214
    TabOrder = 5
    OnClick = Button2Click
  end
  object pbSendFile: TProgressBar
    Left = 174
    Top = 288
    Width = 467
    Height = 17
    Max = 10000
    TabOrder = 6
  end
  object mmoLog: TMemo
    Left = 17
    Top = 400
    Width = 680
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object Edit3: TEdit
    Left = 174
    Top = 96
    Width = 121
    Height = 21
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 8
    Text = '127.0.0.1'
  end
  object CheckBox1: TCheckBox
    Left = 432
    Top = 15
    Width = 97
    Height = 17
    Caption = #23581#35797#35299#30721
    TabOrder = 9
    OnClick = CheckBox1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 384
    Top = 200
  end
end
