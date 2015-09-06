unit uFrmSelRanage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TSelRanagedEvent = procedure (Sender: TObject; const ARect: TRect; 
    AWndHandle: THandle; ABitmap: TBitmap) of object;

type
  TFrmSelRanage = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    ScreenBmp, ScreenBmpL: TBitmap;
    ARect: TRect;
    Point: Tpoint;
    IsPoint, IsOK: Boolean;
    BeginX, BeginY, MX, MY: Integer;
    MsgW, MsgH: Integer;
    // 选取的窗口句柄
    SelectWndHandle: THandle;
    FOnSelRanged: TSelRanagedEvent;

    // 选取框 8 个控制点矩形区域
    FCTL, FCTC, FCTR, FCL, FCR, FCBL, FCBC, FCBR: TRect;
    FMoveMode: Integer;

    FSender: TComponent;
    FLastX: Integer;
    FMove: Boolean;
    
    procedure CalcFCRect();
    function CheckMouseState(Point: TPoint): Integer;
    function IsMove: Boolean;
    procedure Init(Sender: TComponent = nil; ALastX: Integer = 0;
      AMove: Boolean = False);
  public
    { Public declarations }
    // 选取的区域坐标
    property SelectRect: TRect read ARect;
    property OnSelRanged: TSelRanagedEvent read FOnSelRanged write FOnSelRanged;
  end;  

// 开始截屏, 如果AHideFrom为True，则会自动隐藏调用者窗口，请在OnSelRanged中调用Show显示  
procedure ShowSnapScreen(Sender: TComponent; OnSelRanged: TSelRanagedEvent; AHideFrom: Boolean = True); 
  
implementation

{$R *.dfm}

var
  FrmSelRanage: TFrmSelRanage;

procedure ShowSnapScreen(Sender: TComponent; OnSelRanged: TSelRanagedEvent; AHideFrom: Boolean); 
var
  LastX: Integer;
begin
  LastX := $FFFFFF;
  if AHideFrom and (Sender is TCustomForm) then begin
    LastX := TCustomForm(Sender).Left;
    TCustomForm(Sender).Left := -99999;
  end;
  Sleep(100);
  Application.ProcessMessages;
  if not Assigned(FrmSelRanage) then
    FrmSelRanage := TFrmSelRanage.Create(Sender);
  FrmSelRanage.OnSelRanged := OnSelRanged;
  FrmSelRanage.Init(Sender, LastX, LastX <> $FFFFFF);
  FrmSelRanage.Show;
end;

function GetDCBitmap(const AHdc: HDC; const w,h: Word): HBITMAP;
var
  hMemDc: HDC;
  hBmp, hOldBmp: HBITMAP;
begin
  hMemDc := CreateCompatibleDC(0);
  hBmp := CreateCompatibleBitmap(AHdc, w, h);
  hOldBmp := SelectObject(hMemDC, hBmp);
  BitBlt(hMemDc, 0, 0, w, h, AHdc, 0, 0, SRCCOPY);
  hBmp := SelectObject(hMemDc, hOldBmp);
  Result := hBmp;
  DeleteDC(hMemDC);
  ReleaseDC(HWND_DESKTOP, AHdc);
end;

// 截屏函数
function GetScreenBitmap: HBITMAP;
var
  dc: HDC;
  ARect: TRect;
begin
  ARect.Right := GetSystemMetrics(SM_CXSCREEN);
  ARect.Bottom := GetSystemMetrics(SM_CYSCREEN);
  dc := GetDC(GetDesktopWindow);
  Result := GetDCBitmap(dc, ARect.Right, ARect.Bottom);
end;

// 获取一个设备场景图像
function GetHdcBitmap(const AHdc: HDC): HBITMAP;
var
  ARect: TRect;
begin
  GetClipBox(AHdc, ARect);
  Result := GetDCBitmap(AHdc, ARect.Right, ARect.Bottom);
end;

// 获取一个窗口的图像
function GetWindowBitmap(const AhWnd: HWND): HBITMAP;
var
  ARect: TRect;
begin
  if GetWindowRect(AhWnd, ARect) then begin
    Result := GetDCBitmap(GetWindowDC(AhWnd),
      ARect.Right - ARect.Left,
      ARect.Bottom - ARect.Top);
  end else
    Result := 0;
end;

// 设置到 Bitmap 中
procedure SetBitmap(aBitmap: HBITMAP; destBmp: TBitmap);
var
  bm: BITMAP;
  hMemDc: HDC;
  hOldBmp: HBITMAP;
begin
  if aBitmap = 0 then Exit;
  if GetObject(aBitmap, SizeOf(bm), @bm) = 0 then Exit;
  destBmp.SetSize(bm.bmWidth, bm.bmHeight);
  hMemDc := CreateCompatibleDC(0);
  hOldBmp := SelectObject(hMemDC, aBitmap);
  BitBlt(destBmp.Canvas.Handle, 0, 0, bm.bmWidth, bm.bmHeight, hMemDc, 0, 0, SRCCOPY);
  SelectObject(hMemDc, hOldBmp);
  DeleteDC(hMemDC);
end;

function GetWndHandle(AComHandle: THandle): THandle;
begin
  Result := 0;
  while AComHandle <> 0 do
  begin
    Result := AComHandle;
    AComHandle := Windows.GetParent(AComHandle);
  end;
end;

function GetXYWndHandle(X, Y: Integer): THandle;
var
  tmphWnd: THandle;
  pt: TPoint;
begin
  pt.X := X;
  pt.Y := Y;
  tmphWnd := WindowFromPoint(pt);
  Result := GetWndHandle(tmphWnd); 
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end; 

// 改亮度
function ChangeBrightness(bmp: TBitmap; s: Integer): Boolean;
var
  p: PByteArray;
  x, y, tx: Integer;
begin
  try
    //24位真彩色
    Bmp.PixelFormat := pf24Bit; 
    bmp.Canvas.Lock;
    for y := 0 to Bmp.Height - 1 do begin
      p := Bmp.scanline[y];
      for x := 0 to Bmp.Width - 1 do begin
        tx := x * 3;
        //每个象素点的R、G、B分量进行调节
        if s > 0 then begin
          p[tx] := Min(255, p[tx] + s); //不能越界，限制在0～255
          p[tx + 1] := Min(255, p[tx + 1] + s);
          p[tx + 2] := Min(255, p[tx + 2] + s);
        end else begin
          p[tx] := max(0, p[tx] + s); //不能越界，限制在-255～0
          p[tx + 1] := max(0, p[tx + 1] + s);
          p[tx + 2] := max(0, p[tx + 2] + s);
        end;
      end;
    end;
    bmp.Canvas.Unlock;   
    Result := true;
  except
    Result := false;
  end; 
end;

{ TFrmSelRanage }

procedure TFrmSelRanage.CalcFCRect;
var
  W, H: Integer;
const
  RWH = 3;
begin
  // FCTL, FCTC, FCTR, FCL, FCR, FCBL, FCBC, FCBR
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;
  SetRect(FCTL, ARect.Left - RWH, ARect.Top - RWH, ARect.Left + RWH, ARect.Top + RWH);
  SetRect(FCTC, ARect.Left + W div 2 - RWH, ARect.Top - RWH, ARect.Left + W div 2 + RWH, ARect.Top + RWH);
  SetRect(FCTR, ARect.Right - RWH, ARect.Top - RWH, ARect.Right + RWH, ARect.Top + RWH);
  SetRect(FCL, ARect.Left - RWH, ARect.Top + H div 2 - RWH, ARect.Left + RWH, ARect.Top + H div 2 + RWH);
  SetRect(FCR, ARect.Right - RWH, FCL.Top, ARect.Right + RWH, FCL.Bottom);
  SetRect(FCBL, ARect.Left - RWH, ARect.Bottom - RWH, ARect.Left + RWH, ARect.Bottom + RWH);
  SetRect(FCBC, FCTC.Left, ARect.Bottom - RWH, FCTC.Right, ARect.Bottom + 1);
  SetRect(FCBR, ARect.Right - RWH, ARect.Bottom - RWH, ARect.Right + RWH, ARect.Bottom + RWH);   
end;

function TFrmSelRanage.CheckMouseState(Point: TPoint): Integer;
begin
  if not IsPoint then begin
    Result := -1;
    Cursor := crCross;
    Exit;
  end;  
  if PtInRect(FCTL, Point) then begin
    Cursor := crSizeNWSE;
    Result := 1;
  end else if PtInRect(FCTC, Point) then begin
    Cursor := crSizeNS;
    Result := 2;
  end else if PtInRect(FCTR, Point) then begin
    Cursor := crSizeNESW;
    Result := 3;
  end else if PtInRect(FCL, Point) then begin
    Cursor := crSizeWE;
    Result := 4;
  end else if PtInRect(FCR, Point) then begin 
    Cursor := crSizeWE; 
    Result := 5;
  end else if PtInRect(FCBL, Point) then begin
    Cursor := crSizeNESW;  
    Result := 6;
  end else if PtInRect(FCBC, Point) then begin
    Cursor := crSizeNS;
    Result := 7;
  end else if PtInRect(FCBR, Point) then begin
    Cursor := crSizeNWSE;
    Result := 8;
  end else if PtInRect(ARect, Point) then begin             
    Cursor := crSizeAll;
    Result := 0;
  end else begin
    Cursor := crCross;
    Result := -1;
  end;
end;

procedure TFrmSelRanage.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FOnSelRanged) then begin
    if not IsOK then begin
      FillChar(ARect, SizeOf(ARect), 0);
      ScreenBmpL.SetSize(0, 0);
    end;
    if FMove then begin
      TCustomForm(FSender).Left := FLastX;
    end;
    FOnSelRanged(Self, ARect, SelectWndHandle, ScreenBmpL);
  end;
  FreeAndNil(ScreenBmpL);
end;

procedure TFrmSelRanage.FormCreate(Sender: TObject);
begin
  ScreenBmp := TBitmap.Create;  
  DoubleBuffered := True;
end;

procedure TFrmSelRanage.FormDestroy(Sender: TObject);
begin
  ScreenBmp.Free;
  FreeAndNil(ScreenBmpL);
end;

procedure TFrmSelRanage.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27 then begin
    if IsPoint then begin
      IsPoint := False; // 清除选框
      CheckMouseState(Point);
      Invalidate;
    end else
      Close; // ESC键关闭
  end;  
end;

procedure TFrmSelRanage.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssRight in shift then Self.Close;
  if (IsPoint = True) and (ssLeft in shift) then begin
    GetCursorPos(point);
    FMoveMode := CheckMouseState(Point);
    if FMoveMode = 0 then begin
      if (ssDouble in Shift) then begin      
        IsOK := True;
        Hide;
        try
          if ARect.Left < 0 then ARect.Left := 0;
          if ARect.Top < 0 then ARect.Top := 0;
          if ARect.Right > Width then ARect.Right := Width;
          if ARect.Bottom > Height then ARect.Bottom := Height;          
          // 以矩形选择框的左上角坐标来计算当前选择区域的窗口句柄
          SelectWndHandle := GetXYWndHandle(ARect.Left, ARect.Top);
          ScreenBmpL.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
          BitBlt(ScreenBmpL.Canvas.Handle, 0, 0, ScreenBmpL.Width, 
            ScreenBmpL.Height, ScreenBmp.Canvas.Handle, ARect.Left, ARect.Top, SRCCOPY);            
        finally
          Self.Close;
        end;
      end;
    end;
  end;
  if (ssLeft in Shift) then begin
    BeginX := X;
    BeginY := Y; //记录鼠标按下时的X,Y坐标
    MX := X;
    MY := Y;
  end;
end;

procedure TFrmSelRanage.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  L, T, R, B: Integer;
begin
  GetCursorPos(point);
  CheckMouseState(Point);
  if (ssLeft in shift) and (not IsOK) then begin
    if IsMove then begin
      L := ARect.Left;
      T := ARect.Top;
      R := ARect.Right;
      B := ARect.Bottom;
      case FMoveMode of
        0: OffsetRect(ARect, X - MX, Y - MY);
        1: 
          begin
            L := L + X - MX; 
            T := T + Y - MY;
          end;
        2: 
          begin
            T := T + Y - MY;
          end;
        3:
          begin
            R := R + X - MX; 
            T := T + Y - MY;
          end;
        4:
          begin
            L := L + X - MX;
          end;
        5:
          begin
            R := R + X - MX;
          end;
        6: 
          begin
            L := L + X - MX;
            B := B + Y - MY;
          end;
        7:
          begin
            B := B + Y - MY;              
          end;
        8:
          begin
            R := R + X - MX; 
            B := B + Y - MY;
          end;
      end;  
      if FMoveMode <> 0 then begin
        Arect.Left := Min(L, R);
        Arect.Top := Min(T, B);
        Arect.Right := Max(L, R);
        Arect.Bottom := Max(T, B);          
      end;  
      CalcFCRect(); 
      MX := X;
      MY := Y;
    end else begin
      Arect.Left := Min(BeginX, X);
      Arect.Top := Min(BeginY, Y);
      Arect.Right := Max(BeginX, X);
      Arect.Bottom := Max(BeginY, Y);
      CalcFCRect();
      IsPoint := True;
    end;
    Invalidate;
  end;
end;

procedure TFrmSelRanage.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMoveMode := -1;
end;

procedure TFrmSelRanage.FormPaint(Sender: TObject);

  procedure DrawRectangle(const R: TRect);
  begin
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;
  
  procedure DrawRect(const R: TRect);
  begin
    Canvas.FillRect(R);
  end;
var
  X, Y: Integer;
  Msg: string;
begin
  if not Assigned(ScreenBmpL) then begin
    Canvas.FillRect(Canvas.ClipRect);
    Exit;
  end;
  Canvas.Draw(0, 0, ScreenBmpL);
  if IsOk then Exit;  
  if IsPoint then begin
    BitBlt(Canvas.Handle, ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, 
      ScreenBmp.Canvas.Handle, ARect.Left, ARect.Top, SRCCOPY);
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := $00FFCC00;
    DrawRectangle(ARect);

    //FCTL, FCTC, FCTR, FCL, FCR, FCBL, FCBC, FCBR
    Canvas.Brush.Color := $00CC6633;
    Canvas.Brush.Style := bsSolid;
    DrawRect(FCTL);
    DrawRect(FCTC);
    DrawRect(FCTR);
    DrawRect(FCL);
    DrawRect(FCR);
    DrawRect(FCBL);
    DrawRect(FCBC);
    DrawRect(FCBR);

    Canvas.Font.Size := 9;
    Canvas.Font.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := $000000;
    
    Msg := Format(' X: %d, Y: %d, W: %d, H: %d ', [ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top]);
    if MsgH = 0 then     
      MsgH := Canvas.TextHeight(Msg) + 4;
    MsgW := Canvas.TextWidth(Msg) + 4;
    
    if ARect.Top > MsgH then
      Y := ARect.Top - MsgH + 2
    else if ARect.Bottom - ARect.Top > MsgH then
      Y := ARect.Top + 2
    else begin
      Y := ARect.Bottom + 2;
      if Y + MsgH > Height - 2 then
        Y := Height - MsgH - 2;
    end;

    if ARect.Left + MsgW > Width - 2 then
      X := Width - MsgW - 2
    else
      X := ARect.Left + 2;
    if X < 2 then
      X := 2;
    
    Canvas.TextOut(X, Y, Msg);
  end;
end;

procedure TFrmSelRanage.Init(Sender: TComponent; ALastX: Integer; AMove: Boolean);
var
  hBmp: HBITMAP;
begin
  FSender := Sender;
  FLastX := ALastX;
  FMove := AMove;
  
  IsPoint := False;
  FMoveMode := -1;
  IsOK := False;
  FillChar(ARect, SizeOf(ARect), 0);
  hBmp := GetScreenBitmap;
  SetBitmap(hbmp, ScreenBmp);
  ScreenBmpL := TBitmap.Create;
  ScreenBmpL.Assign(ScreenBmp);
  ChangeBrightness(ScreenBmpL, -60);
  SelectWndHandle := 0;
  Repaint;
end;

function TFrmSelRanage.IsMove: Boolean;
begin
  Result := (FMoveMode <> -1) and (IsPoint);
end;

end.
