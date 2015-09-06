object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 428
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
  object edtPort: TEdit
    Left = 135
    Top = 5
    Width = 50
    Height = 21
    TabOrder = 0
    Text = '9983'
  end
  object pgcMain: TPageControl
    Left = 1
    Top = 32
    Width = 811
    Height = 395
    ActivePage = TabSheet1
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = #30417#25511#38754#26495
      object tsMonitor: TPanel
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
      DesignSize = (
        803
        367)
      object Memo1: TMemo
        Left = 0
        Top = 30
        Width = 801
        Height = 337
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        Lines.Strings = (
          '0123456789')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Button6: TButton
        Left = 3
        Top = 3
        Width = 161
        Height = 25
        Caption = #26356#26032#35201#21457#36865#30340#25968#25454
        TabOrder = 1
        OnClick = Button6Click
      end
    end
    object tsLog: TTabSheet
      Caption = #26085#24535
      ImageIndex = 1
      object mmoLog: TMemo
        Left = 0
        Top = 0
        Width = 803
        Height = 367
        Align = alClient
        Lines.Strings = (
          'mmoLog')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object Button3: TButton
    Left = 411
    Top = 3
    Width = 106
    Height = 25
    Caption = #21024#38500#31532#19968#20010
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button2: TButton
    Left = 200
    Top = 3
    Width = 100
    Height = 25
    Caption = #23458#25143#36830#25509
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 302
    Top = 3
    Width = 103
    Height = 25
    Caption = '+1000'#36830#25509
    TabOrder = 4
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 5
    Text = '127.0.0.1'
  end
  object Button5: TButton
    Left = 521
    Top = 3
    Width = 96
    Height = 25
    Caption = #21024#38500#20840#37096
    TabOrder = 6
    OnClick = Button5Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 672
    Top = 168
  end
end
