object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 443
  ClientWidth = 811
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
    Left = 8
    Top = 32
    Width = 6
    Height = 13
    Caption = '0'
  end
  object edtPort: TEdit
    Left = 9
    Top = 7
    Width = 121
    Height = 21
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 0
    Text = '9983'
  end
  object btnOpen: TButton
    Left = 142
    Top = 5
    Width = 75
    Height = 25
    Caption = #24320#21551
    TabOrder = 1
    OnClick = btnOpenClick
  end
  object btnDisconectAll: TButton
    Left = 223
    Top = 5
    Width = 113
    Height = 25
    Caption = #26029#24320#25152#26377
    TabOrder = 2
    OnClick = btnDisconectAllClick
  end
  object btnGetWorkerState: TButton
    Left = 351
    Top = 5
    Width = 121
    Height = 25
    Caption = #33719#21462#32447#31243#24037#20316#29366#24577
    TabOrder = 3
    OnClick = btnGetWorkerStateClick
  end
  object btnFindContext: TButton
    Left = 476
    Top = 5
    Width = 100
    Height = 25
    Caption = #27979#35797#26597#25214'Context'
    TabOrder = 4
    OnClick = btnFindContextClick
  end
  object btnPostWSAClose: TButton
    Left = 578
    Top = 5
    Width = 103
    Height = 25
    Caption = #24322#27493#26029#24320
    TabOrder = 5
    OnClick = btnPostWSACloseClick
  end
  object btnReOpenTest: TButton
    Left = 687
    Top = 5
    Width = 106
    Height = 25
    Caption = #24320#20851#27979#35797
    TabOrder = 6
    OnClick = btnReOpenTestClick
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 48
    Width = 811
    Height = 395
    ActivePage = TabSheet1
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 7
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlMonitor: TPanel
        Left = 0
        Top = 0
        Width = 803
        Height = 367
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = #21457#36865#25968#25454
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        803
        367)
      object Memo1: TMemo
        Left = 16
        Top = 48
        Width = 773
        Height = 303
        Anchors = [akLeft, akTop, akRight, akBottom]
        ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
        Lines.Strings = (
          '0123456789')
        TabOrder = 0
      end
      object Button6: TButton
        Left = 16
        Top = 17
        Width = 161
        Height = 25
        Caption = #26356#26032#35201#21457#36865#30340#25968#25454
        TabOrder = 1
        OnClick = Button6Click
      end
    end
  end
  object Button3: TButton
    Left = 587
    Top = 36
    Width = 106
    Height = 25
    Caption = #21024#38500#31532#19968#20010
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button2: TButton
    Left = 376
    Top = 36
    Width = 100
    Height = 25
    Caption = #23458#25143#36830#25509
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 478
    Top = 36
    Width = 103
    Height = 25
    Caption = '+1000'#36830#25509
    TabOrder = 10
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 245
    Top = 36
    Width = 121
    Height = 21
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 11
    Text = '127.0.0.1'
  end
  object Button5: TButton
    Left = 697
    Top = 36
    Width = 96
    Height = 25
    Caption = #21024#38500#20840#37096
    TabOrder = 12
    OnClick = Button5Click
  end
  object tmrReader: TTimer
    OnTimer = tmrReaderTimer
    Left = 672
    Top = 88
  end
  object tmrTest: TTimer
    Enabled = False
    OnTimer = tmrTestTimer
    Left = 672
    Top = 128
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 672
    Top = 168
  end
end
