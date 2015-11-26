object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'UDP Test Client'
  ClientHeight = 302
  ClientWidth = 686
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    686
    302)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 12
    Width = 72
    Height = 13
    Caption = #26381#21153#22120#22320#22336#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 202
    Top = 12
    Width = 36
    Height = 13
    Caption = #31471#21475#65306
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 412
    Top = 12
    Width = 60
    Height = 13
    Caption = #21457#36865#27425#25968#65306
  end
  object Label4: TLabel
    Left = 548
    Top = 12
    Width = 72
    Height = 13
    Caption = #24037#20316#32447#31243#25968#65306
  end
  object Label5: TLabel
    Left = 5
    Top = 38
    Width = 96
    Height = 13
    Caption = #33258#23450#20041#25253#25991#20869#23481#65306
  end
  object Edit1: TEdit
    Left = 83
    Top = 9
    Width = 107
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object Edit2: TEdit
    Left = 239
    Top = 9
    Width = 43
    Height = 21
    TabOrder = 1
    Text = '9983'
  end
  object CheckBox1: TCheckBox
    Left = 309
    Top = 11
    Width = 97
    Height = 17
    Caption = #22810#32447#31243#27979#35797
    TabOrder = 2
  end
  object Edit3: TEdit
    Left = 469
    Top = 9
    Width = 73
    Height = 21
    TabOrder = 3
    Text = '10000'
  end
  object Edit4: TEdit
    Left = 621
    Top = 9
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '100'
  end
  object Edit19: TEdit
    Left = 3
    Top = 58
    Width = 617
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    BiDiMode = bdLeftToRight
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ImeName = #20013#25991' ('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    ParentBiDiMode = False
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 5
    Text = '0123456789'
  end
  object Button32: TButton
    Left = 626
    Top = 46
    Width = 52
    Height = 30
    Anchors = [akTop, akRight]
    Caption = #21457#36865
    TabOrder = 6
    OnClick = Button32Click
  end
  object ServerInfo: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 82
    Width = 680
    Height = 217
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ImeName = #20013#25991' ('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    ItemHeight = 15
    ParentFont = False
    PopupMenu = PopupMenu2
    TabOrder = 7
  end
  object ProgressBar1: TProgressBar
    Left = 107
    Top = 39
    Width = 513
    Height = 12
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    Visible = False
  end
  object PopupMenu2: TPopupMenu
    Left = 312
    Top = 151
    object MenuItem1: TMenuItem
      Caption = #22797#21046'(&C)'
      OnClick = MenuItem1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MenuItem3: TMenuItem
      Caption = #28165#31354'(&C)'
      OnClick = MenuItem3Click
    end
    object muScroll: TMenuItem
      Caption = #33258#21160#28378#21160'(&S)'
      Checked = True
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 384
    Top = 168
  end
end
