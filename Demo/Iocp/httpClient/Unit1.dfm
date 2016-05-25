object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'HttpClient Demo'
  ClientHeight = 415
  ClientWidth = 703
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    703
    415)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 90
    Height = 25
    Caption = 'HttpHeader'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 120
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Dictionary'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 112
    Width = 90
    Height = 25
    Caption = 'HttpGet'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 208
    Top = 24
    Width = 107
    Height = 25
    Caption = 'Authentication'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 321
    Top = 24
    Width = 112
    Height = 25
    Caption = 'EntityRanges'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Edit1: TEdit
    Left = 24
    Top = 152
    Width = 663
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    TabOrder = 5
    Text = 'https://www.baidu.com/index.php?tn=monline_3_dg'
  end
  object Memo1: TMemo
    Left = 24
    Top = 192
    Width = 663
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    ImeName = #20013#25991'('#31616#20307') - '#25628#29399#20116#31508#36755#20837#27861
    Lines.Strings = (
      'Host: dc.csdn.net'
      
        'User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:46.0) Gecko/2' +
        '0100101 Firefox/46.0'
      'Accept: image/png,image/*;q=0.8,*/*;q=0.5'
      'Accept-Language: zh-CN,zh;q=0.8,en-US;q=0.5,en;q=0.3'
      'Accept-Encoding: gzip, deflate'
      'Referer: http://blog.csdn.net/lifj07/article/details/8638098'
      
        'Cookie: _ga=GA1.2.1898972551.1442887588; uuid_tt_dd=166785864162' +
        '296400_20150922; __gads=ID=df40523a53978dfd:T=1442887674:S=ALNI_' +
        'MZDWAFFlh29OZdjFkUvmAo8q-frjQ; __qca=P0-2042628187-1442887616450' +
        '; __utma=17226283.1898972551.1442887588.1443078039.1443078039.1;' +
        ' __message_sys_msg_id=0; __message_gu_msg_id=0; __message_cnel_m' +
        'sg_id=0; __message_in_school=0; lzstat_uv=39076768823670047054|3' +
        '491229; UN=yangyxd; UE="yangyxd@126.com"; BT=1459828833983; __me' +
        'ssage_district_code=350000; dc_tos=o7nq7q; dc_session_id=1464052' +
        '598466'
      'Connection: keep-alive'
      '')
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object Button6: TButton
    Left = 439
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Button6'
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 120
    Top = 112
    Width = 75
    Height = 25
    Caption = 'HttpPost'
    TabOrder = 8
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 520
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Button8'
    TabOrder = 9
  end
  object Button9: TButton
    Left = 601
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Button9'
    TabOrder = 10
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 201
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Button10'
    TabOrder = 11
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 24
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Button11'
    TabOrder = 12
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 105
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Button12'
    TabOrder = 13
    OnClick = Button12Click
  end
  object IocpHttpClient1: TIocpHttpClient
    AllowCookies = True
    HandleRedirects = True
    Accept = 'text/html'
    AcceptEncoding = 'gzip, deflate'
    AcceptLanguage = 'zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3'
    UserAgent = 
      'Mozilla/5.0 (Windows NT 5.1; rv:12.0) Gecko/20100101 Firefox/12.' +
      '0'
    RecvTimeOut = 0
    ConnectionTimeOut = 30000
    MaxRedirects = 5
    Left = 232
    Top = 64
  end
end
