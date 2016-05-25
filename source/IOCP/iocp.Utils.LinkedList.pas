unit iocp.Utils.LinkedList;

interface

uses
  SysUtils;

type
  PLinkItem = ^TLinkItem;
  TLinkItem = packed record
    Next: PLinkItem;
    Prev: PLinkItem;
    Value: TObject;
  end;

  TLinkedListAction = (lla_clear {清除}, lla_unlink{解除链接}, lla_replace{替换Value});
  TOnCompareItem = function (const A, B: TObject): Integer of object;
  TOnDeleteItem = procedure (const Item: TObject; Action: TLinkedListAction) of object;

  /// <summary>
  /// 双向链表
  /// </summary>
  TLinkedList = class(TObject)
  private
    FFirst: PLinkItem;
    FLast: PLinkItem;
    FCount: Integer;
    FOnDelete: TOnDeleteItem;
    FOnCompareItem: TOnCompareItem;
    function GetCount: Integer;
    function GetIsEmpty: Boolean;
    function GetItem(const Index: Integer): TObject;
    procedure SetItem(const Index: Integer; const Value: TObject);
    procedure DoOutOfBoundsError(const Index: Integer);
  protected
    function isElementIndex(const AIndex: Integer): Boolean; inline;
    function isPositionIndex(const AIndex: Integer): Boolean; inline;
    function Node(const AIndex: Integer): PLinkItem;
    function unlinkNode(P: PLinkItem): Boolean;
    procedure linkBefore(const V: TObject; const P: PLinkItem);
    procedure DoDelete(var P: PLinkItem; Aciton: TLinkedListAction); inline;
    function DoDefaultCompareItem(const A, B: TObject): Integer; virtual;
  public
    constructor Create(); 
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Add(V: TObject); overload; inline;
    procedure Add(const APosition: Integer; V: TObject); overload;
    procedure AddFirst(V: TObject);
    procedure AddLast(V: TObject);
    procedure AddAll(V: TLinkedList); overload;
    procedure AddAll(V: array of TObject); overload;
    function Contains(V: TObject): Boolean;
    function Exist(V: TObject): Boolean;
    function IndexOf(V: TObject): Integer;
    function LastIndexOf(V: TObject): Integer;
    function Offer(V: TObject): Boolean;
    function OfferFirst(V: TObject): Boolean;
    function OfferLast(V: TObject): Boolean;
    function Pop(): TObject; 
    function Peek(): TObject;
    function PeekLast(): TObject;
    function Poll(): TObject; inline;
    function PollFirst(): TObject;
    function PollLast(): TObject;
    function Element(): TObject;
    procedure Push(V: TObject); inline;
    function Remove(): TObject; overload; inline;
    function Remove(V: TObject): Boolean; overload;
    function Remove(Index: Integer): Boolean; overload;
    function RemoveFirst(): TObject; inline;
    function RemoveLast(): TObject;
    property First: PLinkItem read FFirst;
    property Last: PLinkItem read FLast; 
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
    property Items[const Index: Integer]: TObject read GetItem write SetItem; default;
    property OnDelete: TOnDeleteItem read FOnDelete write FOnDelete;
    property OnCompareItem: TOnCompareItem read FOnCompareItem write FOnCompareItem;
  end;

implementation

{ TLinkedList }

procedure TLinkedList.Add(V: TObject);
begin
  AddLast(V);
end;

procedure TLinkedList.Add(const APosition: Integer; V: TObject);
begin
  if isPositionIndex(APosition) then begin
    if APosition = FCount then
      AddLast(V)
    else
      linkBefore(V, Node(APosition));
  end else
    DoOutOfBoundsError(APosition);
end;

procedure TLinkedList.AddAll(V: TLinkedList);
var
  P: PLinkItem;
begin
  if Assigned(V) then begin
    P := V.FFirst;
    while P <> nil do begin
      AddLast(P.Value);
      P := P.Next;
    end;
  end;
end;

procedure TLinkedList.AddAll(V: array of TObject);
var
  I: Integer;
begin
  for I := 0 to High(V) do
    AddLast(V[i]);   
end;

procedure TLinkedList.AddFirst(V: TObject);
var
  P: PLinkItem;
begin
  New(P);
  P.Value := V;
  P.Prev := nil;
  P.Next := FFirst;
  if FFirst = nil then
    FLast := P
  else
    FFirst.Prev := P;
  FFirst := P;
  Inc(FCount);
end;

procedure TLinkedList.AddLast(V: TObject);
var
  P: PLinkItem;
begin
  New(P);
  P.Value := V;
  if FFirst = nil then begin
    P.Prev := nil;
    FFirst := P;
    FLast := FFirst;
  end else begin
    P.Prev := FLast;
    FLast.Next := P;
    FLast := FLast.Next;
  end;
  FLast.Next := nil;
  Inc(FCount);  
end;

procedure TLinkedList.Clear;
var
  P, N: PLinkItem;
begin
  P := FFirst;
  while P <> nil do begin
    N := P.Next;
    DoDelete(P, lla_clear);
    P := N;
  end;
  FFirst := nil;
  FLast := nil;
  FCount := 0;
end;

function TLinkedList.Contains(V: TObject): Boolean;
begin
  Result := IndexOf(V) > -1;
end;

constructor TLinkedList.Create;
begin
  FCount := 0;
  FFirst := nil;
  FLast := nil;
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TLinkedList.DoDefaultCompareItem(const A, B: TObject): Integer;
begin
  if Assigned(FOnCompareItem) then
    Result := FOnCompareItem(A, B)
  else begin
    if Integer(A) - Integer(B) = 0 then
      Result := 1
    else
      Result := 0;
  end;
end;

procedure TLinkedList.DoDelete(var P: PLinkItem; Aciton: TLinkedListAction);
begin
  if Assigned(FOnDelete) then
    FOnDelete(P.Value, Aciton);
  Dispose(P);
end;

procedure TLinkedList.DoOutOfBoundsError(const Index: Integer);
begin
  raise Exception.Create(Format('OutOfBounds. Index: %d, Size: %d.', [Index, FCount]))
end;

function TLinkedList.Element: TObject;
begin
  if FFirst <> nil then
    Result := FFirst.Value
  else
    Result := nil;
end;

function TLinkedList.Exist(V: TObject): Boolean;
begin
  Result := IndexOf(V) > -1;
end;

function TLinkedList.GetCount: Integer;
begin
  Result := FCount;
end;

function TLinkedList.GetIsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TLinkedList.GetItem(const Index: Integer): TObject;
begin
  if isElementIndex(Index) then
    Result := Node(Index).Value
  else begin
    Result := nil;
    DoOutOfBoundsError(Index);
  end;
end;

function TLinkedList.IndexOf(V: TObject): Integer;
var
  P: PLinkItem;
begin
  P := FFirst;
  Result := -1;
  while P <> nil do begin
    Inc(Result);
    if DoDefaultCompareItem(P.Value, V) > 0 then
      Exit
    else
      P := P.Next;
  end;
  Result := -1;
end;

function TLinkedList.isElementIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FCount);
end;

function TLinkedList.isPositionIndex(const AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex <= FCount);
end;

function TLinkedList.LastIndexOf(V: TObject): Integer;
var
  P: PLinkItem;
begin
  P := FLast;
  Result := FCount;
  while P <> nil do begin
    Dec(Result);
    if DoDefaultCompareItem(P.Value, V) > 0 then
      Exit
    else
      P := P.Prev;
  end;
  Result := -1;
end;

procedure TLinkedList.linkBefore(const V: TObject; const P: PLinkItem);
var
  NewP: PLinkItem;
begin
  New(NewP);
  NewP.Value := V;
  NewP.Next := P;
  NewP.Prev := P.Prev;
  if P.Prev = nil then
    FFirst := NewP
  else begin
    P.Prev.Next := NewP;
    P.Prev := NewP;
  end;
  Inc(FCount);
end;

function TLinkedList.Node(const AIndex: Integer): PLinkItem;
var
  I: Integer;
begin
  if (AIndex < (FCount shr 1)) then begin
    Result := FFirst;
    for I := 1 to AIndex do
      Result := Result.Next;
  end else begin
    Result := FLast;
    for I := FCount - 2 downto AIndex do
      Result := Result.Prev;  
  end; 
end;

function TLinkedList.Offer(V: TObject): Boolean;
begin
  Add(V);
  Result := True;
end;

function TLinkedList.OfferFirst(V: TObject): Boolean;
begin
  AddFirst(V);
  Result := True;
end;

function TLinkedList.OfferLast(V: TObject): Boolean;
begin
  AddLast(V);
  Result := True;
end;

function TLinkedList.Peek: TObject;
begin
  if FFirst <> nil then
    Result := FFirst.Value
  else
    Result := nil;
end;

function TLinkedList.PeekLast: TObject;
begin
  if FLast <> nil then
    Result := FLast.Value
  else
    Result := nil;
end;

function TLinkedList.Poll: TObject;
begin
  Result := PollFirst;
end;

function TLinkedList.PollFirst: TObject;
begin
  if FFirst <> nil then begin
    Result := FFirst.Value;
    unlinkNode(FFirst);
  end else
    Result := nil;
end;

function TLinkedList.PollLast: TObject;
begin
  if FLast <> nil then begin
    Result := FLast.Value;
    unlinkNode(FLast);
  end else
    Result := nil;
end;

function TLinkedList.Pop: TObject;
begin
  if FFirst <> nil then begin
    Result := FFirst.Value;
    unlinkNode(FFirst);
  end else
    Result := nil;
end;

procedure TLinkedList.Push(V: TObject);
begin
  AddFirst(V);
end;

function TLinkedList.Remove(Index: Integer): Boolean;
begin
  if not isElementIndex(Index) then
    Result := False
  else
    Result := unlinkNode(Node(Index));
end;

function TLinkedList.Remove: TObject;
begin
  Result := Pop;
end;

function TLinkedList.Remove(V: TObject): Boolean;
var
  P: PLinkItem;
begin
  P := FFirst;
  while P <> nil do begin
    if DoDefaultCompareItem(P.Value, V) > 0 then begin
      Result := unlinkNode(P);
      Exit;
    end else
      P := P.Next;
  end;
  Result := False;
end;

function TLinkedList.RemoveFirst: TObject;
begin
  Result := Pop;
end;

function TLinkedList.RemoveLast: TObject;
begin
  Result := PollLast();
end;

procedure TLinkedList.SetItem(const Index: Integer; const Value: TObject);
var
  P: PLinkItem;
begin
  if isElementIndex(Index) then begin
    P := Node(Index);
    if P <> nil then begin
      if Assigned(FOnDelete) then
        FOnDelete(P.Value, lla_replace);
      P.Value := Value
    end;
  end else
    DoOutOfBoundsError(Index); 
end;

function TLinkedList.unlinkNode(P: PLinkItem): Boolean;
begin
  if P = nil then
    Result := False
  else begin
    if P = FFirst then begin
      FFirst := FFirst.Next;
      if FFirst = nil then
        FLast := nil
      else
        FFirst.Prev := nil;
    end else if P = FLast then begin
      FLast := P.Prev;
      if FLast = nil then
        FFirst := nil
      else
        FLast.Next := nil;
    end else begin
      P.Prev.Next := P.Next;
      P.Next.Prev := P.Prev;
    end;
    DoDelete(P, lla_unlink);
    Dec(FCount);
    Result := True;
  end;
end;

end.
