{*******************************************************}
{                                                       }
{       RTTI 基础库                                     }
{                                                       }
{       版权所有 (C) 2013      YangYxd                  }
{                                                       }
{*******************************************************}
{
 --------------------------------------------------------------------
  说明
 --------------------------------------------------------------------
  YXDRtti中绝大多数代码来自swish的QJSON，感谢swish和QJson
  QJson来自QDAC项目，版权归swish(QQ:109867294)所有
  QDAC官方群：250530692 

 --------------------------------------------------------------------
  更新记录
 --------------------------------------------------------------------

 2016.10.20 ver 1.0.2
 --------------------------------------------------------------------
  - 修复 writeValue 时 key 为空时的错误
 
 2014.08.05 ver 1.0.1
 --------------------------------------------------------------------
  - 支持YxdJSON的序列化与反序列化DataSet.

 2014.08.01 ver 1.0.0
 --------------------------------------------------------------------
  - 支持YxdJSON的序列化与反序列化，反序列化暂时不支持反射实例化对象，
    需要先创建好对象再通过RTTI来完成初始化,在D2007下，仅支持TObject型
    的published段属性、变量读写。
  - 兼容XE6、Delphi2007(已测试)，支持Win32, Android平台。
  - 暂不支持XML与INI的格式的序列化。将来可能会做大改。
 --------------------------------------------------------------------
}
unit YxdRtti;

interface

{$DEFINE USEYxdStr}               // 是否使用YxdStr单元
{$DEFINE USEIniSerialize}         // 使用INI序列化模块
{$DEFINE USEXmlSerialize}         // 使用XML序列化模块
{$DEFINE USEJsonSerialize}        // 使用Json序列化模块

{$DEFINE USEDataSet}              // 是否使用DataSet序列化功能

{$IF RTLVersion>=26}
{$DEFINE USE_UNICODE}
{$IFEND}

uses
  {$IFDEF USEYxdStr}YxdStr, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF USE_UNICODE}Generics.Collections, Rtti, {$ENDIF}
  {$IFDEF USE_UNICODE}Soap.EncdDecd, System.NetEncoding, {$ELSE}Base64, {$ENDIF}
  {$IFDEF USEDataSet}DB, DBClient, {$ENDIF}
  {$IFDEF USEJsonSerialize}YxdJson, {$ENDIF}   
  SysUtils, Classes, Variants, TypInfo, Math;

type
  /// <summary>
  /// 序列化类型
  /// </summary>
  TSerializeType = (afXML,{XML格式} afIni,{ini文件} afJson {json格式});

  {$IFDEF USE_UNICODE}
  TValueArray = array of TValue;
  {$ENDIF}

  {$IFDEF USE_UNICODE}
  /// <summary>
  /// 注解类定义：字段名称
  /// </summary>
  FieldNameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    property Name: string read FName;
  end;
  {$ENDIF}

  {$IFDEF USEDataSet}
  // DataSet Helper
  TDateSetHelper = class helper for TDataSet
    function Exist(const FieldName: string): Boolean;
    function GetBoolean(const FieldName: string; DefaultValue: Boolean = False): Boolean;
    function GetInt(const FieldName: string; DefaultValue: Integer = 0): Integer;
    function GetDWORD(const FieldName: string; DefaultValue: Cardinal = 0): Cardinal;
    function GetFloat(const FieldName: string; DefaultValue: Double = 0): Double;
    function GetDateTime(const FieldName: string): TDateTime;
    function GetVariant(const FieldName: string): Variant;
    function GetString(const FieldName: string): string;
    function GetWideString(const FieldName: string): WideString;
  end;
  {$ENDIF}

type
  /// <summary>
  /// 序列化写入器基类
  /// </summary>
  TSerializeWriter = class
  protected
    procedure BeginRoot; virtual; abstract;
    procedure EndRoot; virtual; abstract;

    procedure BeginData(const Name: string; const IsArray: Boolean); virtual; abstract;
    procedure EndData(); virtual; abstract;

    procedure Add(const Value: string); overload; virtual; abstract;
    procedure Add(const Value: Integer); overload; virtual; abstract;
    procedure Add(const Value: Cardinal); overload; virtual; abstract;
    procedure Add(const Value: Double); overload; virtual; abstract;
    procedure Add(const Value: Variant); overload; virtual; abstract;
    procedure AddTime(const Value: TDateTime); overload; virtual; abstract;
    procedure AddInt64(const Value: Int64); overload; virtual; abstract;

    procedure WriteString(const Name, Value: string); virtual; abstract;
    procedure WriteInt(const Name: string; const Value: Integer); virtual; abstract;
    procedure WriteInt64(const Name: string; const Value: Int64); virtual; abstract;
    procedure WriteUInt(const Name: string; const Value: Cardinal); virtual; abstract;
    procedure WriteDateTime(const Name: string; const Value: TDateTime); virtual; abstract;
    procedure WriteBoolean(const Name: string; const Value: Boolean); virtual; abstract;
    procedure WriteFloat(const Name: string; const Value: Double); overload; virtual; abstract;
    procedure WriteVariant(const Name: string; const Value: Variant); overload; virtual; abstract;
  public
    {$IFNDEF USE_UNICODE}
    function ToString(): string; virtual;
    {$ENDIF}
    function IsArray: Boolean; virtual;
  end;

type
  TYxdSerialize = class
  protected
    class procedure LoadCollection(AIn: JSONBase; ACollection: TCollection);
    class function ArrayItemTypeName(ATypeName: JSONString): JSONString;
    class function ArrayItemType(ArrType: PTypeInfo): PTypeInfo;
  public
    class function GetObjectTypeInfo(AObj: TObject): PTypeInfo;

    class procedure Serialize(Writer: TSerializeWriter; const Key: string; ASource: TObject); overload;
    class procedure Serialize(Writer: TSerializeWriter; const Key: string; ASource: Pointer; AType: PTypeInfo); overload;
    {$IFDEF USEDataSet}
    class procedure Serialize(Writer: TSerializeWriter; const Key: string; ADataSet: TDataSet;
      const PageIndex, PageSize: Integer; Base64Blob: Boolean = True); overload;
    {$ENDIF}
    {$IFDEF USE_UNICODE}
    class procedure Serialize(Writer: TSerializeWriter; const Key: string; AInstance: TValue); overload;
    {$ENDIF}

    class procedure ReadValue(AIn: JSONBase; ADest: Pointer; aType: {$IFDEF USE_UNICODE}PTypeInfo{$ELSE}PTypeInfo{$ENDIF}); overload;
    {$IFDEF USEDataSet}
    class procedure ReadValue(AIn: TDataSet; ADest: Pointer; aType: {$IFDEF USE_UNICODE}PTypeInfo{$ELSE}PTypeInfo{$ENDIF}); overload;
    {$ENDIF}
    class procedure ReadObject(AIn: JSONBase; ADest: TObject);
    class procedure WriteValue(AOut: JSONBase; const Key: JSONString; ASource: Pointer; AType: PTypeInfo); overload;
    {$IFDEF USEDataSet}
    /// <summary>
    /// 将指定JSON数据转换到DataSet中
    /// <param name="AIn">输入JSON对象</param>
    /// <param name="ADest">目标DataSet数据集对象</param>
    /// <returns>返回成功加载的数据行数。返回-1表示参数错误</returns>
    /// </summary>
    class function ReadDataSet(AIn: JSONBase; ADest: TDataSet): Integer;
    /// <summary>
    /// 序列化DataSet为Json对象
    /// <param name="AOut">输出到指定的Json对象</param>
    /// <param name="Key">如果Key不为空，则在Aout建立一个以Key命名的子对象输出</param>
    /// <param name="ADataSet">待序列化的DataSet数据集</param>
    /// <param name="PageIndex">从第几页开始序列化，PageSize > 0 时有效。</param>
    /// <param name="PageSize">分页时每页行数。</param>
    /// </summary>
    class procedure WriteDataSet(AOut: JSONBase; const Key: JSONString; ADataSet: TDataSet;
      const PageIndex, PageSize: Integer; Base64Blob: Boolean = True);
    {$ENDIF}
    {$IFDEF USE_UNICODE}
    class procedure ReadValue(AIn: JSONBase; AInstance: TValue); overload;
    class procedure ReadRecord<T>(AIn: JSONBase; out AInstance: T); overload;
    {$IFDEF USEDataSet}
    class procedure ReadValue(AIn: TDataSet; AInstance: TValue); overload;
    class procedure ReadRecord<T>(AIn: TDataSet; out AInstance: T); overload;
    {$ENDIF}
    class function WriteToValue(AIn: PJSONValue): TValue; overload;
    class function WriteToValue(AIn: JSONBase): TValue; overload;
    class procedure WriteValue(AOut: JSONBase; const Key: JSONString; AInstance: TValue); overload;
    {$ENDIF}
  end;

type
  TQueueValue = Boolean;
  PQueueItem = ^TQueueItem;
  TQueueItem = record
    Data: TQueueValue;
    Next: PQueueItem;
  end;

  TSimpleQueue = class(TObject)
  private
    FCount: Integer;
    FHead: PQueueItem;
    FTail: PQueueItem;
    FDefaultValue: TQueueValue;
    function InnerPop: PQueueItem;
    procedure InnerAddToTail(AData: PQueueItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IsEmpty: Boolean;
    function Size: Integer;
    function DeQueue(): TQueueValue; overload;
    function DeQueue(var Dest: TQueueValue): Boolean; overload;
    procedure EnQueue(AData: TQueueValue);
    property DefaultValue: TQueueValue read FDefaultValue write FDefaultValue;
  end;

type
  TJsonSerializeWriter = class(TSerializeWriter)
  private
    FData: TStringCatHelper;
    FState: TSimpleQueue;
    FIsArray: Boolean;
    FDoEscape: Boolean;
    procedure WriteName(const Name: string); inline;
  protected
    procedure BeginRoot; override;
    procedure EndRoot; override;

    procedure BeginData(const Name: string; const IsArray: Boolean); override;
    procedure EndData(); override;

    procedure Add(const Value: string); overload; override;
    procedure Add(const Value: Integer); overload; override;
    procedure Add(const Value: Cardinal); overload; override;
    procedure Add(const Value: Double); overload; override;
    procedure Add(const Value: Variant); overload; override;
    procedure AddTime(const Value: TDateTime); overload; override;
    procedure AddInt64(const Value: Int64); overload; override;

    procedure WriteString(const Name, Value: string); override;
    procedure WriteInt(const Name: string; const Value: Integer); override;
    procedure WriteInt64(const Name: string; const Value: Int64); override;
    procedure WriteUInt(const Name: string; const Value: Cardinal); override;
    procedure WriteDateTime(const Name: string; const Value: TDateTime); override;
    procedure WriteBoolean(const Name: string; const Value: Boolean); override;
    procedure WriteFloat(const Name: string; const Value: Double); override;
    procedure WriteVariant(const Name: string; const Value: Variant); override;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString(): string; override;
    function IsArray: Boolean; override;
    property DoEscape: Boolean read FDoEscape write FDoEscape;
  end;

implementation

{$IFDEF USEDataSet}
const
  CSBlobs: JSONString = '[blobs]<';  //不要修改这个, 长度为8，方便快速比对
  CSBlobBase64: JSONString = '[BS]'; //使用Base64编码Blob时的识别前缀
{$ENDIF}

resourcestring
  SUnsupportPropertyType = '不支持的属性类型.';
  SMissRttiTypeDefine = '无法找到 %s 的RTTI类型信息，尝试将对应的类型单独定义(如array[0..1] of Byte改为TByteArr=array[0..1]，然后用TByteArr声明)。';
  SArrayTypeMissed = '未知的数组元素类型.';
  SErrorJsonType = '错误的Json类型.';

{ FiledNameAttribute }

{$IFDEF USE_UNICODE}
constructor FieldNameAttribute.Create(const AName: string);
begin
  FName := AName;
end; 
{$ENDIF}

{$IFDEF USE_UNICODE}
//修正XE6中System.rtti中TValue对tkSet类型处理的Bug
function SetAsOrd(AValue: TValue): Int64;
var
  ATemp: Integer;
begin
  AValue.ExtractRawData(@ATemp);
  case GetTypeData(AValue.TypeInfo).OrdType of
    otSByte:
      Result := PShortint(@ATemp)^;
    otUByte:
      Result := PByte(@ATemp)^;
    otSWord:
      Result := PSmallint(@ATemp)^;
    otUWord:
      Result := PWord(@ATemp)^;
    otSLong:
      Result := PInteger(@ATemp)^;
    otULong:
      Result := PCardinal(@ATemp)^;
  else
    Result := 0
  end;
end;
{$ENDIF}

{ TDateSetHelper }

{$IFDEF USEDataSet}
function TDateSetHelper.Exist(const FieldName: string): Boolean;
begin
  Result := FindField(FieldName) <> nil;
end;

function TDateSetHelper.GetBoolean(const FieldName: string;
  DefaultValue: Boolean): Boolean;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) then
    Result := F.AsBoolean
  else
    Result := DefaultValue;
end;

function TDateSetHelper.GetDateTime(const FieldName: string): TDateTime;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) then
    Result := F.AsDateTime
  else
    Result := 0;
end;

function TDateSetHelper.GetDWORD(const FieldName: string;
  DefaultValue: Cardinal): Cardinal;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) then
    Result := F.AsInteger
  else
    Result := DefaultValue;
end;

function TDateSetHelper.GetFloat(const FieldName: string;
  DefaultValue: Double): Double;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) then
    Result := F.AsFloat
  else
    Result := DefaultValue;
end;

function TDateSetHelper.GetInt(const FieldName: string;
  DefaultValue: Integer): Integer;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) and (not F.IsNull) then
    Result := F.AsInteger
  else
    Result := DefaultValue;
end;

function TDateSetHelper.GetString(const FieldName: string): string;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) and (not F.IsNull) then
    Result := F.AsString
  else
    Result := '';
end;

function TDateSetHelper.GetVariant(const FieldName: string): Variant;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) then
    Result := F.AsVariant
  else
    Result := vaNull;
end;

function TDateSetHelper.GetWideString(const FieldName: string): WideString;
var
  F: TField;
begin
  F := FindField(FieldName);
  if Assigned(F) then
    Result := F.AsWideString
  else
    Result := '';
end;
{$ENDIF}

{ TSerializeWriter }

function TSerializeWriter.IsArray: Boolean;
begin
  Result := False;
end;

{$IFNDEF USE_UNICODE}
function TSerializeWriter.ToString: string;
begin
  Result := Self.ClassName;
end;
{$ENDIF}

{ TYxdSerialize }

class function TYxdSerialize.ArrayItemType(ArrType: PTypeInfo): PTypeInfo;
var
  ATypeData: PTypeData;
begin
  Result := nil;
  if (ArrType <> nil) and (ArrType.Kind in [tkArray,tkDynArray]) then begin
    ATypeData := GetTypeData(ArrType);
    if (ATypeData <> nil) then
      Result := ATypeData.elType2^;
    if Result = nil then begin
      if ATypeData.BaseType^ = TypeInfo(Byte) then
        Result := TypeInfo(Byte);
    end;
  end;
end;

class function TYxdSerialize.ArrayItemTypeName(ATypeName: JSONString): JSONString;
var
  p, ps: PJSONChar;
  ACount: Integer;
begin
  p := PJSONChar(ATypeName);
  if StartWith(p, 'TArray<', true) then begin
    Inc(p, 7);
    ps := p;
    ACount := 1;
    while ACount >0 do begin
      if p^ = '>' then
        Dec(ACount)
      else if p^ = '<' then
        Inc(ACount);
      Inc(p);
    end;
    Result := StrDupX(ps, p-ps-1);
  end else
    Result:='';
end;

class function TYxdSerialize.GetObjectTypeInfo(AObj: TObject): PTypeInfo;
begin
  if Assigned(AObj) then
    Result := AObj.ClassInfo
  else
    Result := nil;
end;

class procedure TYxdSerialize.Serialize(Writer: TSerializeWriter; const Key: string;
  ASource: Pointer; AType: PTypeInfo);

  procedure AddCollection(Writer: TSerializeWriter; ACollection:TCollection);
  var
    J: Integer;
  begin
    for J := 0 to ACollection.Count-1 do
      Serialize(Writer, '', ACollection.Items[J]{$IFNDEF USE_UNICODE}, GetObjectTypeInfo(ACollection.Items[J]){$ENDIF});
  end;

  {$IFDEF USE_UNICODE}
  procedure AddRecord(Writer: TSerializeWriter);
  var
    AObj: TObject;
    AValue: TValue;
    AFieldItem: TRttiField;
    AContext: TRttiContext;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    AFieldName: string;
    AFieldAttrItem: TCustomAttribute;
    II, J: Integer;
  begin
    AContext := TRttiContext.Create;
    ARttiType := AContext.GetType(AType);
    AFields := ARttiType.GetFields;
    //如果是从结构体，则记录其成员，如果是对象，则只记录其公开的属性，特殊处理TStrings和TCollection
    for J := Low(AFields) to High(AFields) do begin
      AFieldItem := AFields[J];
      if AFieldItem.FieldType <> nil then begin

        AFieldName := AFieldItem.Name;
        if AFieldItem.GetAttributes <> nil then begin
          for AFieldAttrItem in AFieldItem.GetAttributes do
            if AFieldAttrItem is FieldNameAttribute then begin
              AFieldName := FieldNameAttribute(AFieldAttrItem).Name;
              Break;
            end;
        end;

        case AFieldItem.FieldType.TypeKind of
          tkInteger:
            Writer.WriteInt(AFieldName, AFieldItem.GetValue(ASource).AsInteger);
          {$IFNDEF NEXTGEN}tkString,tkLString,tkWString,{$ENDIF !NEXTGEN}tkUString:
            Writer.WriteString(AFieldName, AFieldItem.GetValue(ASource).AsString);
          tkEnumeration:
            begin
              if GetTypeData(AFieldItem.FieldType.Handle).BaseType^ = TypeInfo(Boolean) then
                Writer.WriteBoolean(AFieldName, AFieldItem.GetValue(ASource).AsBoolean)
              else if JsonRttiEnumAsInt then
                Writer.WriteInt64(AFieldName, AFieldItem.GetValue(ASource).AsOrdinal)
              else
                Writer.WriteString(AFieldName, AFieldItem.GetValue(ASource).ToString);
            end;
          tkSet:
            begin
              if JsonRttiEnumAsInt then
                Writer.WriteInt(AFieldName, SetAsOrd(AFieldItem.GetValue(ASource)))
              else
                Writer.WriteString(AFieldName, AFieldItem.GetValue(ASource).ToString);
            end;
          tkChar,tkWChar:
            Writer.WriteString(AFieldName, AFieldItem.GetValue(ASource).ToString);
          tkFloat:
            begin
              if (AFieldItem.FieldType.Handle = TypeInfo(TDateTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TDate))
                 then
                Writer.WriteDateTime(AFieldName, AFieldItem.GetValue(ASource).AsExtended)
              else
                Writer.WriteFloat(AFieldName, AFieldItem.GetValue(ASource).AsExtended);
            end;
          tkInt64:
            Writer.WriteInt64(AFieldName, AFieldItem.GetValue(ASource).AsInt64);
          tkVariant:
            Writer.WriteVariant(AFieldName, AFieldItem.GetValue(ASource).AsVariant);
          tkArray, tkDynArray:
            begin
              Writer.BeginData(AFieldName, True);
              AValue := AFieldItem.GetValue(ASource);
              for II := 0 to AValue.GetArrayLength - 1 do
                Serialize(Writer, '', AValue.GetArrayElement(II));
              Writer.EndData;
            end;
          tkClass:
            begin
              AObj := AFieldItem.GetValue(ASource).AsObject;
              if (AObj is TStrings) then
                Writer.WriteString(AFieldName, TStrings(AObj).Text)
              else if AObj is TCollection then begin
                Writer.BeginData(AFieldName, True);
                AddCollection(Writer, AObj as TCollection);
                Writer.EndData;
              end else
                Serialize(Writer, AFieldName, AObj, AFieldItem.FieldType.Handle);
            end;
          tkRecord:
            Serialize(Writer, AFieldName, Pointer(IntPtr(ASource) + AFieldItem.Offset),
              AFieldItem.FieldType.Handle);
        end;
      end else
        raise Exception.CreateFmt(SMissRttiTypeDefine, [AFieldName]);
    end;
  end;
  {$ENDIF}

  procedure AddStrings(Writer: TSerializeWriter; Data: TStrings);
  var
    I: Integer;
  begin
    for I := 0 to Data.Count - 1 do
      Writer.Add(Data[I]);
  end;

  procedure AddObject(Writer: TSerializeWriter);
  var
    AName: JSONString;
    APropList: PPropList;
    ACount: Integer;
    AObj, AChildObj: TObject;
    J: Integer;
  begin
    AObj := ASource;
    ACount := GetPropList(AType, APropList);
    try
      for J := 0 to ACount - 1 do begin
        if APropList[J].PropType^.Kind in [tkMethod{$IFDEF USE_UNICODE}, tkProcedure{$ENDIF}] then
          Continue;
        if APropList[J].PropType^.Kind in
          [tkInterface{$IFDEF USE_UNICODE}, tkClassRef, tkPointer{$ENDIF}]
        then begin
          if IsDefaultPropertyValue(AObj, APropList[J], nil) then
            Continue;
        end;
        {$IF RTLVersion>25}
        AName := APropList[J].NameFld.ToString;
        {$ELSE}
        AName := String(APropList[J].Name);
        {$IFEND}
        case APropList[J].PropType^.Kind of
          tkClass:
            begin
              AChildObj := Pointer(GetOrdProp(AObj, APropList[J]));
              if AChildObj is TStrings then
                Writer.WriteString(AName, (AChildObj as TStrings).Text)
              else if AChildObj is TCollection then begin
                Writer.BeginData(AName, True);
                AddCollection(Writer, AChildObj as TCollection);
                Writer.EndData;
              end else if Assigned(AChildObj) then
                Serialize(Writer, AName, AChildObj);
            end;
          tkInteger:
            begin
              Writer.WriteInt(AName, GetOrdProp(AObj,APropList[J]));
            end;
          tkChar,tkString,tkWChar, tkLString, tkWString{$IFDEF USE_UNICODE}, tkUString{$ENDIF}:
            Writer.WriteString(AName, GetStrProp(AObj,APropList[J]));
          tkEnumeration:
            begin
              if GetTypeData(APropList[J]^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
                Writer.WriteBoolean(AName, GetOrdProp(AObj,APropList[J])<>0)
              else if JsonRttiEnumAsInt then
                Writer.WriteInt(AName, GetOrdProp(AObj,APropList[J]))
              else
                Writer.WriteString(AName, GetEnumProp(AObj,APropList[J]));
            end;
          tkSet:
            begin
              if JsonRttiEnumAsInt then
                Writer.WriteInt(AName, GetOrdProp(AObj, APropList[J]))
              else
                Writer.WriteString(AName, GetSetProp(AObj,APropList[J], True));
            end;
          tkFloat:
            begin
              Writer.WriteFloat(AName, GetFloatProp(AObj, APropList[J]));
            end;
          tkVariant:
            begin                
              Writer.WriteVariant(AName, GetPropValue(AObj,APropList[J]));
            end;
          tkInt64:
            Writer.WriteInt64(AName, GetInt64Prop(AObj,APropList[J]));
          tkRecord, tkArray, tkDynArray: //记录、数组、动态数组属性系统也不保存，也没提供所有太好的接口
            raise Exception.Create(SUnsupportPropertyType);
        end;
      end;
    finally
      FreeMem(APropList);
    end;
  end;

  {$IFDEF USE_UNICODE}
  procedure AddArray(Writer: TSerializeWriter);
  var
    I: Integer;
    AValue: TValue;
  begin
    TValue.Make(ASource, AType, AValue);
    Writer.BeginData(Key, True);
    for I := 0 to AValue.GetArrayLength - 1 do
      Serialize(Writer, '', AValue.GetArrayElement(I));
    Writer.EndData;
  end;
  {$ENDIF}

begin
  if not Assigned(ASource) then Exit;
  case AType.Kind of
    {$IFDEF USE_UNICODE}
    tkRecord:
      begin
        Writer.BeginData(Key, False);
        AddRecord(Writer);
        Writer.EndData;
      end;
    {$ENDIF}
    tkClass:
      begin
        if TObject(ASource) is TStrings then begin
          if (Key = '')  then begin
            if Writer.IsArray then
              AddStrings(Writer, TStrings(ASource))
            else
              Writer.WriteString('text', TStrings(ASource).Text);
          end else
            Writer.WriteString(Key, TStrings(ASource).Text);
        end else if TObject(ASource) is TCollection then begin
          AddCollection(Writer, TCollection(ASource))
        {$IFDEF USEDataSet}
        end else if TObject(ASource) is TDataSet then begin
          Serialize(Writer, Key, TDataSet(ASource), 0, -1)
        {$ENDIF}
        end else begin
          if Writer.IsArray then
            Writer.BeginData(Key, True)
          else 
            Writer.BeginData(Key, False);
          AddObject(Writer);
          Writer.EndData;
        end;
      end;
    {$IFDEF USE_UNICODE}
    tkDynArray, tkArray:
      begin
        Writer.BeginData(Key, True);
        AddArray(Writer);
        Writer.EndData;
      end;
    {$ENDIF}
  end;
end;

class procedure TYxdSerialize.Serialize(Writer: TSerializeWriter; const Key: string; ASource: TObject);
begin
  {$IFNDEF USE_UNICODE}
  Serialize(Writer, Key, ASource, GetObjectTypeInfo(ASource));
  {$ELSE}
  Serialize(Writer, Key, TValue(ASource));
  {$ENDIF}
end;

{$IFDEF USE_UNICODE}
class procedure TYxdSerialize.Serialize(Writer: TSerializeWriter; const Key: string; AInstance: TValue);
var
  I,C:Integer;
begin
  case AInstance.Kind of
    tkClass:
      Serialize(Writer, Key, aInstance.AsObject, AInstance.TypeInfo);
    tkRecord:
      Serialize(Writer, Key, aInstance.GetReferenceToRawData, aInstance.TypeInfo);
    tkArray, tkDynArray:
      begin
        Writer.BeginData(Key, True);
        C := aInstance.GetArrayLength;
        for I := 0 to C-1 do
          Serialize(Writer, '', AInstance.GetArrayElement(I));
        Writer.EndData;
      end;
    tkInteger:
      Writer.WriteInt(Key, AInstance.AsInteger);
    tkInt64:
      Writer.WriteInt64(Key, AInstance.AsInt64);
    tkChar, tkString,tkWChar, tkLString, tkWString, tkUString:
      Writer.WriteString(Key, AInstance.ToString);
    tkEnumeration:
      begin
        if GetTypeData(AInstance.TypeInfo)^.BaseType^ = TypeInfo(Boolean) then
          Writer.WriteBoolean(Key, aInstance.AsBoolean)
        else if JsonRttiEnumAsInt then
          Writer.WriteInt(Key, aInstance.AsOrdinal)
        else
          Writer.WriteString(Key, aInstance.ToString)
      end;
    tkSet:
      if JsonRttiEnumAsInt then
        Writer.WriteInt(Key, aInstance.AsInteger)
      else
        Writer.WriteString(Key, aInstance.ToString);
    tkVariant:
      Writer.WriteInt(Key, aInstance.AsVariant);
  end;
end;
{$ENDIF}

{$IFDEF USEDataSet}
class procedure TYxdSerialize.Serialize(Writer: TSerializeWriter; const Key: string;
  ADataSet: TDataSet; const PageIndex, PageSize: Integer; Base64Blob: Boolean);
begin

end;
{$ENDIF}

class procedure TYxdSerialize.LoadCollection(aIn: JSONBase; ACollection: TCollection);
var
  I: Integer;
  {$IFNDEF USE_UNICODE}
  Item: TCollectionItem;
  {$ENDIF}
begin
  if not Assigned(aIn) then Exit;  
  for I := 0 to aIn.Count - 1 do begin
    {$IFDEF USE_UNICODE}
    readValue(aIn, ACollection.Add);
    {$ELSE}
    Item := ACollection.Add;
    readValue(aIn, Item, GetObjectTypeInfo(Item));
    {$ENDIF}
  end;
end;


{$IFDEF USEDataSet}
{$IFDEF USE_UNICODE}
type TPointerStream = class(TCustomMemoryStream);
{$ENDIF}
class function TYxdSerialize.ReadDataSet(AIn: JSONBase; ADest: TDataSet): Integer;
var
  BlobStream: TStream;
  {$IFDEF USE_UNICODE} BSStream: TPointerStream;{$ENDIF}

  function IsBlob(p: Pointer; HighL: Integer): Boolean;
  begin
    {$IFDEF USE_UNICODE}
    Result := (HighL >= 18)
        and (PInt64(p)^ = $6F006C0062005B)
        and (PInt64(IntPtr(p)+8)^ = $3C005D00730062)
        and (PWord(IntPtr(p)+HighL-1)^ = $3E);
    {$ELSE}
    Result := (HighL >= 9)
        and (PInt64(IntPtr(p))^ = $3C5D73626F6C625B)
        and (PByte(IntPtr(p)+HighL)^ = $3E);
    {$ENDIF}
  end;

  function GetBlodValue(Field: TField; Item: PJSONValue; var Buf: TBytes): Integer;
  var
    I: Integer;
    p: {$IFDEF USE_UNICODE}PByte{$ELSE}PAnsiChar{$ENDIF};
    {$IFNDEF USE_UNICODE}BStmp: JSONString;{$ELSE}BStmp: TMemoryStream;{$ENDIF}
  begin
    Result := 0;
    I := High(Item.FValue);
    if I > -1 then begin
      p := @Item.FValue[0];
      {$IFDEF USE_UNICODE}
      if IsBlob(p, I) then begin
        Inc(p, 16);
        if (I >= 22) and (PInt64(p)^ = $5D00530042005B) then begin
          Inc(p, 8);
          if not Assigned(BSStream) then
            BSStream := TPointerStream.Create;
          BSStream.SetPointer(p, I-17-8);
          BSStream.Position := 0;
          BStmp := TMemoryStream.Create;
          try
            DecodeStream(BSStream, BStmp);
            if Assigned(BlobStream) then
              BlobStream.Free;
            BlobStream := ADest.CreateBlobStream(Field, bmWrite);
            BlobStream.Write(BSTmp.Memory^, BSTmp.Size);
            BlobStream.Free;
            BlobStream := nil;
          finally
            BStmp.Free;
          end;
          Result := 2;
        end else begin
          {$IFDEF USEYxdStr}YxdStr{$ELSE}YxdJson{$ENDIF}.HexToBin(Pointer(p), (I-17) shr 1, Buf);
          Result := 1;
        end;
      {$ELSE}
      if IsBlob(p, I) then begin
        Inc(p, 8);
        if (I >= 13) and (PDWORD(p)^ = $5D53425B) then begin
          Inc(p, 4);
          BStmp := Base64Decode(p^, I-8-4);
          if Assigned(BlobStream) then
            BlobStream.Free;
          BlobStream := ADest.CreateBlobStream(Field, bmWrite);
          BlobStream.Size := 0;
          if Length(BSTmp) > 0 then
            BlobStream.WriteBuffer(BSTmp[1], Length(BStmp));
          BlobStream.Free;
          BlobStream := nil;
          Result := 2;
        end else begin
          {$IFDEF USEYxdStr}YxdStr{$ELSE}YxdJson{$ENDIF}.HexToBin(p, High(Item.FValue)-8, Buf);
          Result := 1;
        end;
      {$ENDIF}
      end;
    end;
  end;

  procedure AddObjectMeta(Item: PJSONValue);
  begin
    case Item.FType of
      jdtString:
        begin
          if (Item.Size > 0) and IsBlob(@Item.FValue[0], High(Item.FValue)) then
            ADest.FieldDefs.Add(Item.FName, ftBlob, 20)
          else
            ADest.FieldDefs.Add(Item.FName, ftString, 30);
        end;
      jdtInteger:
        ADest.FieldDefs.Add(Item.FName, ftInteger);
      jdtFloat:
        ADest.FieldDefs.Add(Item.FName, ftFloat);
      jdtBoolean:
        ADest.FieldDefs.Add(Item.FName, ftBoolean);
      jdtDateTime:
        ADest.FieldDefs.Add(Item.FName, ftDateTime);
      jdtNull: ;
    else
      ADest.FieldDefs.Add(Item.FName, ftVariant);
    end;
  end;

  procedure AddItem(Field: TField; DataType: TFieldType; Item: PJSONValue);
  var
    Buf: TBytes;
  begin
    if Item.FType = jdtNull then begin
      Field.Value := NULL;
      Exit;
    end;
    case DataType of
      ftDate, ftTime, ftDateTime, ftTimeStamp{$IFDEF USE_UNICODE}, ftTimeStampOffset{$ENDIF}:
        Field.Value := Item.TryAsDatetime;
      ftBlob, ftGraphic, ftMemo, ftTypedBinary:
        begin
          case GetBlodValue(Field, Item, Buf) of
            0: Field.Value := Item.GetString;
            1:
              begin
                if Assigned(BlobStream) then
                  BlobStream.Free;
                BlobStream := ADest.CreateBlobStream(Field, bmWrite);
                BlobStream.Position := 0;
                BlobStream.WriteBuffer(Buf[0], Length(Buf));
                BlobStream.Free;
                BlobStream := nil;
              end;
          end;
        end
      else case Item.FType of
        jdtBoolean:
          Field.Value := Item.AsBoolean;
        jdtInteger:
          Field.Value := Item.AsInteger;
        jdtFloat:
          Field.Value := Item.AsFloat;
        jdtDateTime:
          Field.Value := Item.TryAsDatetime;
        jdtString:
          begin
            case GetBlodValue(Field, Item, Buf) of
              0: Field.Value := Item.GetString;
              1: Field.Value := Buf;
            end;
          end;
      end;
    end;
  end;

var
  FldName: string;
  Meta, MetaItem, Data: JSONArray;
  Item, ItemChild: PJSONValue;
  ItemObject: JSONBase;
  Field: TField;
  I: Integer;
begin
  Result := -1;
  if (not Assigned(aDest)) or (not Assigned(AIn)) then Exit;
  ADest.DisableControls;
  ADest.FieldDefs.Clear;
  ADest.Close;

  if (AIn.IsJSONArray) then begin
    Meta := nil;
    Data := JSONArray(aIn);
  end else begin
    Meta := JSONObject(AIn).GetJsonArray('meta');
    Data := JSONObject(AIn).GetJsonArray('data');
    if not Assigned(Data) then
      Data := JSONObject(AIn).GetJsonArray('rows');
  end;
  
  Result := 0;
  BlobStream := nil;
  {$IFDEF USE_UNICODE}BSStream := nil;{$ENDIF}
  if (not Assigned(Meta)) and (not Assigned(Data)) then Exit;
  try
    if (not Assigned(Meta)) then begin // 没用Meta数据，从数据字段中获取
      ItemObject := Data.GetJsonObject(0);
      if not Assigned(ItemObject) then
        Exit;
      for Item in ItemObject do begin
        if Item.FType = jdtNull then begin
          if Length(Item.FName) > 0 then  // 从下一条记录中查找
            for i := 1 to Data.Count - 1 do begin
              ItemObject := Data.GetJsonObject(I);
              if (ItemObject = nil) then Continue;
              ItemChild := JSONObject(ItemObject).GetItem(Item.FName);
              if (ItemChild = nil) or (ItemChild.FType = jdtNull) then Continue;
              AddObjectMeta(ItemChild);
              Break;
            end;
        end else
          AddObjectMeta(Item);
      end;
    end else begin
      for I := 0 to Meta.Count - 1 do begin
        MetaItem := Meta[I].AsJsonArray;
        if MetaItem = nil then Continue;
        ADest.FieldDefs.Add(
          MetaItem.Items[0].GetString,
          TFieldType(MetaItem.Items[1].AsInteger),
          MetaItem.Items[2].AsInteger,
          MetaItem.Items[3].AsBoolean);
      end;
    end;

    if not ADest.Active then begin
      if ADest is TClientDataSet then
        TClientDataSet(ADest).CreateDataSet
      else
        ADest.Open;
    end; 
    
    try
      for Item in Data do begin
        ItemObject := Item.GetObject;
        if ItemObject = nil then Continue;
        // 数组模式
        if ItemObject.IsJSONArray then begin
          ADest.Append;
          for I := 0 to ItemObject.Count - 1 do begin
            FldName := ADest.Fields[i].FieldName;
            AddItem(ADest.Fields[i], ADest.FieldDefs.Items[i].DataType, ItemObject.Items[i]);
          end;
          ADest.Post;
        end else begin  // 对象模式
          ADest.Append;
          for ItemChild in ItemObject do begin
            Field := ADest.FindField(ItemChild.FName);
            if not Assigned(Field) then
              Continue;
            FldName := ItemChild.FName;
            AddItem(Field, ADest.FieldDefs.Items[Field.Index].DataType, ItemChild); 
          end;
          ADest.Post;
        end;
      end;
    except
      raise Exception.CreateFmt('json将字段(%s)赋值给数据集发生异常！', [FldName]);
    end;
  finally
    if ADest.Active then
      ADest.First;
    ADest.EnableControls;
    if Assigned(BlobStream) then
      BlobStream.Free;
    {$IFDEF USE_UNICODE}
    if Assigned(BSStream) then
      BSStream.Free;
    {$ENDIF}
  end;
  Result := ADest.RecordCount;
end;
{$ENDIF}

class procedure TYxdSerialize.readObject(aIn: JSONBase; aDest: TObject);
begin
  if not Assigned(aDest) then Exit;
  {$IFDEF USE_UNICODE}
  readValue(aIn, aDest);
  {$ELSE}
  readValue(aIn, aDest, GetObjectTypeInfo(aDest));
  {$ENDIF}
end;

{$IFDEF USE_UNICODE}
class procedure TYxdSerialize.readRecord<T>(aIn: JSONBase; out aInstance: T);
begin
  readValue(aIn, @aInstance, TypeInfo(T));
end;
{$ENDIF}

{$IFDEF USE_UNICODE} {$IFDEF USEDataSet}
class procedure TYxdSerialize.readRecord<T>(aIn: TDataSet; out aInstance: T);
begin
  readValue(aIn, @aInstance, TypeInfo(T));
end;
{$ENDIF}{$ENDIF}

{$IFDEF USE_UNICODE}
class procedure TYxdSerialize.readValue(aIn: JSONBase; aInstance: TValue);
begin
  if aInstance.IsEmpty then
    Exit;
  if aInstance.Kind = tkRecord then
    readValue(aIn, aInstance.GetReferenceToRawData, aInstance.TypeInfo)
  else if aInstance.Kind = tkClass then
    readValue(aIn, aInstance.AsObject, aInstance.TypeInfo);
end;
{$ENDIF}

{$IFDEF USE_UNICODE} {$IFDEF USEDataSet}
class procedure TYxdSerialize.readValue(aIn: TDataSet; aInstance: TValue);
begin
  if aInstance.IsEmpty then
    Exit;
  if aInstance.Kind = tkRecord then
    readValue(aIn, aInstance.GetReferenceToRawData, aInstance.TypeInfo)
  else if aInstance.Kind = tkClass then
    readValue(aIn, aInstance.AsObject, aInstance.TypeInfo);
end;
{$ENDIF}{$ENDIF}

{$IFDEF USEDataSet}
class procedure TYxdSerialize.ReadValue(AIn: TDataSet; ADest: Pointer;
  aType: PTypeInfo);

  function StrToDateTimeDef(const V: string; const DefaultValue: TDateTime): TDateTime;
  begin
    if not(ParseDateTime(PJSONChar(V), Result) or
      ParseJsonTime(PJSONChar(V), Result) or ParseWebTime(PJSONChar(V), Result)) then
      Result := DefaultValue;
  end;

  {$IFDEF USE_UNICODE}
  procedure ToRecord;
  var
    AContext: TRttiContext;
    AFieldItem: TRttiField;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    ABaseAddr: Pointer;
    AFieldName: string;
    AFieldAttrItem: TCustomAttribute;
    AChild: TField;
    J: Integer;
  begin
    AContext := TRttiContext.Create;
    ARttiType := AContext.GetType(AType);
    ABaseAddr := ADest;
    AFields := ARttiType.GetFields;
    for J := Low(AFields) to High(AFields) do begin
      AFieldItem := AFields[J];
      if AFieldItem.FieldType <> nil then begin
        AFieldName := AFieldItem.Name;
        if AFieldItem.GetAttributes <> nil then begin
          for AFieldAttrItem in AFieldItem.GetAttributes do             
            if AFieldAttrItem is FieldNameAttribute then begin
              AFieldName := FieldNameAttribute(AFieldAttrItem).Name;
              Break;
            end;                 
        end;

        AChild := AIn.FindField(AFieldName);
        if AChild <> nil then begin
          case AFieldItem.FieldType.TypeKind of
            tkInteger:
              begin
                if AChild.DataType in [ftString, ftWideString] then
                  AFieldItem.SetValue(ABaseAddr, StrToIntDef(AChild.AsString, 0))
                else
                  AFieldItem.SetValue(ABaseAddr, AChild.AsInteger);
              end;
            {$IFNDEF NEXTGEN}
            tkString:
              PShortString(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := ShortString(AChild.AsString);
            {$ENDIF !NEXTGEN}
            tkUString{$IFNDEF NEXTGEN},tkLString,tkWString{$ENDIF !NEXTGEN}:
              AFieldItem.SetValue(ABaseAddr, AChild.AsString);
            tkEnumeration:
              begin
                if GetTypeData(AFieldItem.FieldType.Handle)^.BaseType^ = TypeInfo(Boolean) then
                  AFieldItem.SetValue(ABaseAddr, AChild.AsBoolean)
                else begin
                  case GetTypeData(AFieldItem.FieldType.Handle).OrdType of
                    otSByte:
                      begin
                        if AChild.DataType = ftInteger then
                          PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otUByte:
                      begin
                        if AChild.DataType = ftInteger then
                          PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otSWord:
                      begin
                        if AChild.DataType = ftInteger then
                          PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otUWord:
                      begin
                        if AChild.DataType = ftInteger then
                          PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otSLong:
                      begin
                        if AChild.DataType = ftInteger then
                          PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otULong:
                      begin
                        if AChild.DataType = ftInteger then
                          PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                  end;
                end;
              end;
            tkSet:
              begin
                case GetTypeData(AFieldItem.FieldType.Handle).OrdType of
                  otSByte:
                    begin
                      if AChild.DataType = ftInteger then
                        PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otUByte:
                    begin
                      if AChild.DataType = ftInteger then
                        PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otSWord:
                    begin
                      if AChild.DataType = ftInteger then
                        PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otUWord:
                    begin
                      if AChild.DataType = ftInteger then
                        PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otSLong:
                    begin
                      if AChild.DataType = ftInteger then
                        PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otULong:
                    begin
                      if AChild.DataType = ftInteger then
                        PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                end;
              end;
            tkChar, tkWChar:
              AFieldItem.SetValue(ABaseAddr, AChild.AsString);
            tkFloat:
              if (AFieldItem.FieldType.Handle = TypeInfo(TDateTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TDate))
              then begin
                if AChild.DataType in [ftString, ftWideString] then begin                  
                  AFieldItem.SetValue(ABaseAddr, StrToDateTimeDef(AChild.AsString, 0))
                end else
                  AFieldItem.SetValue(ABaseAddr, AChild.AsDateTime)
              end else
                AFieldItem.SetValue(ABaseAddr, AChild.AsFloat);
            tkInt64:
              begin
                if AChild.DataType in [ftString, ftWideString] then
                  AFieldItem.SetValue(ABaseAddr, StrToIntDef(AChild.AsString, 0))
                else
                  AFieldItem.SetValue(ABaseAddr, AChild.AsLargeInt);
              end;
            tkVariant:
              PVariant(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsVariant;
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}

  procedure ToObject;
  var
    AProp: PPropInfo;
    AObj: TObject;
    AChild: TField;
    J: Integer;
  begin
    AObj := aDest;
    for J := 0 to aIn.FieldCount - 1 do begin
      AChild := aIn.Fields.Fields[J];
      AProp := GetPropInfo(AObj, AChild.Name);
      if AProp <> nil then begin
        case AProp.PropType^.Kind of
          tkInteger:
            SetOrdProp(AObj, AProp, AChild.AsInteger);
          tkChar,tkString,tkWChar, tkLString, tkWString{$IFDEF USE_UNICODE}, tkUString{$ENDIF}:
            SetStrProp(AObj, AProp, AChild.AsString);
          tkEnumeration:
            begin
              if GetTypeData(AProp.PropType^)^.BaseType^ = TypeInfo(Boolean) then
                SetOrdProp(AObj, AProp, Integer(AChild.AsBoolean))
              else if AChild.DataType = ftInteger then
                SetOrdProp(AObj, AProp, AChild.AsInteger)
              else
                SetEnumProp(AObj, AProp, AChild.AsString);
            end;
          tkSet:
            begin
              if AChild.DataType = ftInteger then
                SetOrdProp(AObj, AProp, AChild.AsInteger)
              else
                SetSetProp(AObj, AProp, AChild.AsString);
            end;
          tkFloat:
            SetFloatProp(AObj, AProp, AChild.AsFloat);
          {$IFDEF USE_UNICODE}
          tkInt64:
            SetInt64Prop(AObj, AProp, AChild.AsLargeInt);
          {$ENDIF}
          tkVariant:
            SetVariantProp(AObj, AProp, AChild.AsVariant);
        end;
      end;
    end;
  end;

begin
  if (aDest <> nil) and (Assigned(aIn)) then begin
    {$IFDEF USE_UNICODE}
    if aType.Kind = tkRecord then
      ToRecord
    else if aType.Kind = tkClass then
      ToObject
    {$ELSE}
    if aType.Kind = tkClass then
      ToObject
    {$ENDIF}
    else
      raise Exception.Create(SUnsupportPropertyType);
  end;
end; 
{$ENDIF}

class procedure TYxdSerialize.readValue(aIn: JSONBase; aDest: Pointer;
  aType: PTypeInfo);

  procedure LoadClass(AObj: TObject; AChild: PJSONValue);
  begin
    if AObj is TStrings then
      (AObj as TStrings).Text := AChild.AsString
    else if AObj is TCollection then
      LoadCollection(AChild.AsJsonArray, AObj as TCollection)
    else if AObj <> nil then
      readValue(AChild.AsJsonObject, AObj{$IFNDEF USE_UNICODE}, GetObjectTypeInfo(AObj){$ENDIF});
  end;

  {$IFDEF USE_UNICODE}
  procedure ToRecord;
  var
    AContext: TRttiContext;
    AFieldItem: TRttiField;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    ABaseAddr: Pointer;
    AChild: PJSONValue;
    J: Integer;
  begin
    AContext := TRttiContext.Create;
    ARttiType := AContext.GetType(AType);
    ABaseAddr := ADest;
    AFields := ARttiType.GetFields;
    for J := Low(AFields) to High(AFields) do begin
      AFieldItem := AFields[J];
      if AFieldItem.FieldType <> nil then begin
        if aIn.IsJSONArray then
          AChild := JSONArray(aIn).Items[J]
        else
          AChild := JSONObject(aIn).getItem(AFieldItem.Name);
        if AChild <> nil then begin
          case AFieldItem.FieldType.TypeKind of
            tkInteger:
              AFieldItem.SetValue(ABaseAddr, AChild.AsInteger);
            {$IFNDEF NEXTGEN}
            tkString:
              PShortString(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := ShortString(AChild.AsString);
            {$ENDIF !NEXTGEN}
            tkUString{$IFNDEF NEXTGEN},tkLString,tkWString{$ENDIF !NEXTGEN}:
              AFieldItem.SetValue(ABaseAddr, AChild.AsString);
            tkEnumeration:
              begin
                if GetTypeData(AFieldItem.FieldType.Handle)^.BaseType^ = TypeInfo(Boolean) then
                  AFieldItem.SetValue(ABaseAddr, AChild.AsBoolean)
                else begin
                  case GetTypeData(AFieldItem.FieldType.Handle).OrdType of
                    otSByte:
                      begin
                        if AChild.FType = jdtInteger then
                          PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otUByte:
                      begin
                        if AChild.FType = jdtInteger then
                          PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otSWord:
                      begin
                        if AChild.FType = jdtInteger then
                          PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otUWord:
                      begin
                        if AChild.FType = jdtInteger then
                          PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otSLong:
                      begin
                        if AChild.FType = jdtInteger then
                          PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                    otULong:
                      begin
                        if AChild.FType = jdtInteger then
                          PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                        else
                          PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := GetEnumValue(AFieldItem.FieldType.Handle, AChild.AsString);
                      end;
                  end;
                end;
              end;
            tkSet:
              begin
                case GetTypeData(AFieldItem.FieldType.Handle).OrdType of
                  otSByte:
                    begin
                      if AChild.FType = jdtInteger then
                        PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PShortint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otUByte:
                    begin
                      if AChild.FType = jdtInteger then
                        PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PByte(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otSWord:
                    begin
                      if AChild.FType = jdtInteger then
                        PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PSmallint(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otUWord:
                    begin
                      if AChild.FType = jdtInteger then
                        PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PWord(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otSLong:
                    begin
                      if AChild.FType = jdtInteger then
                        PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PInteger(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                  otULong:
                    begin
                      if AChild.FType = jdtInteger then
                        PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsInteger
                      else
                        PCardinal(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := StringToSet(AFieldItem.FieldType.Handle, AChild.AsString);
                    end;
                end;
              end;
            tkChar, tkWChar:
              AFieldItem.SetValue(ABaseAddr, AChild.AsString);
            tkFloat:
              if (AFieldItem.FieldType.Handle = TypeInfo(TDateTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TDate))
                 then
                 AFieldItem.SetValue(ABaseAddr, AChild.TryAsDatetime)
              else
                AFieldItem.SetValue(ABaseAddr, AChild.AsFloat);
            tkInt64:
              AFieldItem.SetValue(ABaseAddr, AChild.AsInt64);
           tkVariant:
              PVariant(IntPtr(ABaseAddr)+AFieldItem.Offset)^ := AChild.AsVariant;
           tkArray, tkDynArray:
              readValue(AChild.AsJsonArray, Pointer(IntPtr(ABaseAddr)+AFieldItem.Offset),AFieldItem.FieldType.Handle);
           tkClass:
              LoadClass(AFieldItem.GetValue(ABaseAddr).AsObject, AChild);
           tkRecord:
              readValue(AChild.AsJsonObject, Pointer(IntPtr(ABaseAddr)+AFieldItem.Offset),AFieldItem.FieldType.Handle);
          end;
        end;
      end;
    end;
  end;
  {$ENDIF}
  
  procedure ToObject;
  var
    AProp: PPropInfo;
    AObj, AChildObj: TObject;
    AChild: PJSONValue;
    J: Integer;
  begin
    AObj := aDest;
    for J := 0 to aIn.Count - 1 do begin
      AChild := aIn.Items[J];
      AProp := GetPropInfo(AObj, AChild.FName);
      if AProp <> nil then begin
        case AProp.PropType^.Kind of
          tkClass:
            begin
              AChildObj:=Pointer(GetOrdProp(AObj,AProp));
              if AChildObj is TStrings then
                (AChildObj as TStrings).Text:=AChild.AsString
              else if AChildObj is TCollection then
                LoadCollection(AChild.AsJsonObject, AChildObj as TCollection)
              else
                readValue(AChild.AsJsonObject, AChildObj{$IFNDEF USE_UNICODE}, GetObjectTypeInfo(AChildObj){$ENDIF});
            end;
          tkRecord, tkArray, tkDynArray://tkArray,tkDynArray类型的属性没见过,tkRecord存疑
            readValue(AChild.AsJsonObject, Pointer(GetOrdProp(AObj, AProp)), AProp.PropType^);
          tkInteger:
            SetOrdProp(AObj, AProp, AChild.AsInteger);
          tkChar,tkString,tkWChar, tkLString, tkWString{$IFDEF USE_UNICODE}, tkUString{$ENDIF}:
            SetStrProp(AObj, AProp, AChild.AsString);
          tkEnumeration:
            begin
              if GetTypeData(AProp.PropType^)^.BaseType^ = TypeInfo(Boolean) then
                SetOrdProp(AObj, AProp, Integer(AChild.AsBoolean))
              else if AChild.FType = jdtInteger then
                SetOrdProp(AObj, AProp, AChild.AsInteger)
              else
                SetEnumProp(AObj, AProp, AChild.AsString);
            end;
          tkSet:
            begin
              if AChild.FType = jdtInteger then
                SetOrdProp(AObj, AProp, AChild.AsInteger)
              else
                SetSetProp(AObj, AProp, AChild.AsString);
            end;
          tkVariant:
            SetVariantProp(AObj, AProp, AChild.AsVariant);
          tkInt64:
            SetInt64Prop(AObj, AProp, AChild.AsInt64);
        end;
      end;
    end;
  end;

  procedure SetDynArrayLen(arr:Pointer; AType:PTypeInfo; ALen:NativeInt);
  var
    pmem: Pointer;
  begin
    pmem := PPointer(arr)^;
    DynArraySetLength(pmem, AType, 1, @ALen);
    PPointer(arr)^ := pmem;
  end;

  {$IFDEF USE_UNICODE}
  procedure ToArray;
  var
    AContext: TRttiContext;
    ASubType: TRttiType;
    S: JSONString;
    pd, pi: PByte;
    ASubTypeInfo: PTypeInfo;
    AChild: PJSONValue;
    I, AOffset: Integer;
  begin
    AContext := TRttiContext.Create;
    {$IF RTLVersion>25}
    S := ArrayItemTypeName(AType.NameFld.ToString);
    {$ELSE}
    S := ArrayItemTypeName(string(AType.Name));
    {$IFEND}
    ASubType := AContext.FindType(S);
    ASubTypeInfo := ASubType.Handle;
    if ASubType <> nil then begin
      SetDynArrayLen(ADest, AType, aIn.Count);
      pd := PPointer(ADest)^;
      for I := 0 to aIn.Count - 1 do begin
        AOffset := I * GetTypeData(AType).elSize;
        pi := Pointer(IntPtr(pd)+AOffset);
        AChild := aIn.Items[I];
        case ASubType.TypeKind of
          tkInteger:
            begin
              case GetTypeData(ASubTypeInfo).OrdType of
                otSByte:
                  PShortint(pi)^ := AChild.AsInteger;
                otUByte:
                  pi^ := AChild.AsInteger;
                otSWord:
                  PSmallint(pi)^ := AChild.AsInteger;
                otUWord:
                  PWord(pi)^ := AChild.AsInteger;
                otSLong:
                  PInteger(pi)^ := AChild.AsInteger;
                otULong:
                  PCardinal(pi)^ := AChild.AsInteger;
              end;
            end;
          {$IFNDEF NEXTGEN}
          tkChar:
            pi^ := Ord(PAnsiChar(AnsiString(AChild.AsString))[0]);
          {$ENDIF !NEXTGEN}
          tkEnumeration:
            begin
              if GetTypeData(ASubTypeInfo)^.BaseType^ = TypeInfo(Boolean) then
                PBoolean(pi)^ := AChild.AsBoolean
              else
                begin
                case GetTypeData(ASubTypeInfo)^.OrdType of
                  otSByte:
                    begin
                      if AChild.FType = jdtInteger then
                        PShortint(pi)^ := AChild.AsInteger
                      else
                        PShortint(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                    end;
                  otUByte:
                    begin
                      if AChild.FType = jdtInteger then
                        pi^ := AChild.AsInteger
                      else
                        pi^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                    end;
                  otSWord:
                    begin
                      if AChild.FType = jdtInteger then
                        PSmallint(pi)^ := AChild.AsInteger
                      else
                        PSmallint(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                    end;
                  otUWord:
                    begin
                      if AChild.FType = jdtInteger then
                        PWord(pi)^ := AChild.AsInteger
                      else
                        PWord(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                    end;
                  otSLong:
                    begin
                      if AChild.FType = jdtInteger then
                        PInteger(pi)^ := AChild.AsInteger
                      else
                        PInteger(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                    end;
                  otULong:
                    begin
                      if AChild.FType = jdtInteger then
                        PCardinal(pi)^ := AChild.AsInteger
                      else
                        PCardinal(pi)^ := GetEnumValue(ASubTypeInfo, AChild.AsString);
                    end;
                end;
              end;
            end;
          tkFloat:
            case GetTypeData(ASubTypeInfo)^.FloatType of
              ftSingle:
                PSingle(pi)^ := AChild.AsFloat;
              ftDouble:
                PDouble(pi)^ := AChild.AsFloat;
              ftExtended:
                PExtended(pi)^ := AChild.AsFloat;
              ftComp:
                PComp(pi)^ := AChild.AsFloat;
              ftCurr:
                PCurrency(pi)^ := AChild.AsFloat;
            end;
          {$IFNDEF NEXTGEN}
          tkString:
            PShortString(pi)^:=ShortString(AChild.AsString);
          {$ENDIF !NEXTGEN}
          tkSet:
            begin
              case GetTypeData(ASubTypeInfo)^.OrdType of
                otSByte:
                  begin
                    if AChild.FType = jdtInteger then
                      PShortint(pi)^ := AChild.AsInteger
                    else
                      PShortint(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
                  end;
                otUByte:
                  begin
                    if AChild.FType = jdtInteger then
                      pi^ := AChild.AsInteger
                    else
                      pi^ := StringToSet(ASubTypeInfo, AChild.AsString);
                  end;
                otSWord:
                  begin
                    if AChild.FType = jdtInteger then
                      PSmallint(pi)^ := AChild.AsInteger
                    else
                      PSmallint(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
                  end;
                otUWord:
                  begin
                    if AChild.FType = jdtInteger then
                      PWord(pi)^ := AChild.AsInteger
                    else
                      PWord(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
                  end;
                otSLong:
                  begin
                    if AChild.FType = jdtInteger then
                      PInteger(pi)^ := AChild.AsInteger
                    else
                      PInteger(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
                  end;
                otULong:
                  begin
                    if AChild.FType = jdtInteger then
                      PCardinal(pi)^ := AChild.AsInteger
                    else
                      PCardinal(pi)^ := StringToSet(ASubTypeInfo, AChild.AsString);
                  end;
              end;
            end;
          tkClass:
            LoadClass(PPointer(pi)^, AChild);
          tkWChar:
            PWideChar(pi)^ := PWideChar(AChild.AsString)[0];
          {$IFNDEF NEXTGEN}
          tkLString:
            PAnsiString(pi)^ := AnsiString(AChild.AsString);
          tkWString:
            PWideString(pi)^ := AChild.AsString;
          {$ENDIF}
          tkVariant:
            PVariant(pi)^ := AChild.AsVariant;
          tkArray,tkDynArray:
            readValue(AChild.AsJsonObject, pi, ASubTypeInfo);
          tkRecord:
            readValue(AChild.AsJsonObject, pi, ASubTypeInfo);
          tkInt64:
            PInt64(pi)^ := AChild.AsInt64;
          tkUString:
            PUnicodeString(pi)^ := AChild.AsString;
        end;
      end;
    end else
      raise Exception.Create(SArrayTypeMissed);
  end;
  {$ENDIF}

  {$IFDEF USE_UNICODE}
  function GetFixedArrayItemType:PTypeInfo;
  var
    pType: PPTypeInfo;
  begin
    pType := GetTypeData(AType)^.ArrayData.ElType;
    if pType = nil then
      Result := nil
    else
      Result := pType^;
  end;

  procedure ToFixedArray;
  var
    pi: Pointer;
    ASubType: PTypeInfo;
    AChild: PJSONValue;
    I, C, ASize: Integer;
  begin
    C := GetTypeData(AType).ArrayData.ElCount;
    ASubType := GetFixedArrayItemType;
    if ASubType = nil then Exit;
    ASize:=GetTypeData(ASubType).elSize;
    for I := 0 to C-1 do begin
      pi := Pointer(IntPtr(ADest)+ASize*I);
      AChild := aIn.Items[I];
      case ASubType.Kind of
        tkInteger:
          begin
            case GetTypeData(ASubType).OrdType of
              otSByte:
                PShortint(pi)^ := AChild.AsInteger;
              otUByte:
                PByte(pi)^ := AChild.AsInteger;
              otSWord:
                PSmallint(pi)^ := AChild.AsInteger;
              otUWord:
                PWord(pi)^ := AChild.AsInteger;
              otSLong:
                PInteger(pi)^ := AChild.AsInteger;
              otULong:
                PCardinal(pi)^ := AChild.AsInteger;
            end;
          end;
        {$IFNDEF NEXTGEN}
        tkChar:
          PByte(pi)^ := Ord(PAnsiChar(AnsiString(AChild.AsString))[0]);
        {$ENDIF !NEXTGEN}
        tkEnumeration:
          begin
            if GetTypeData(ASubType)^.BaseType^ = TypeInfo(Boolean) then
              PBoolean(pi)^ := AChild.AsBoolean
            else begin
              case GetTypeData(ASubType)^.OrdType of
                otSByte:
                  begin
                    if AChild.FType = jdtInteger then
                      PShortint(pi)^ := AChild.AsInteger
                    else
                      PShortint(pi)^ := GetEnumValue(ASubType, AChild.AsString);
                  end;
                otUByte:
                  begin
                    if AChild.FType = jdtInteger then
                      PByte(pi)^ := AChild.AsInteger
                    else
                      PByte(pi)^ := GetEnumValue(ASubType, AChild.AsString);
                  end;
                otSWord:
                  begin
                    if AChild.FType = jdtInteger then
                      PSmallint(pi)^ := AChild.AsInteger
                    else
                      PSmallint(pi)^ := GetEnumValue(ASubType, AChild.AsString);
                  end;
                otUWord:
                  begin
                    if AChild.FType = jdtInteger then
                      PWord(pi)^ := AChild.AsInteger
                    else
                      PWord(pi)^ := GetEnumValue(ASubType, AChild.AsString);
                  end;
                otSLong:
                  begin
                    if AChild.FType = jdtInteger then
                      PInteger(pi)^ := AChild.AsInteger
                    else
                      PInteger(pi)^ := GetEnumValue(ASubType, AChild.AsString);
                  end;
                otULong:
                  begin
                    if AChild.FType = jdtInteger then
                      PCardinal(pi)^ := AChild.AsInteger
                    else
                      PCardinal(pi)^ := GetEnumValue(ASubType, AChild.AsString);
                  end;
              end;
            end;
          end;
        tkFloat:
          case GetTypeData(ASubType)^.FloatType of
            ftSingle:
              PSingle(pi)^ := AChild.AsFloat;
            ftDouble:
              PDouble(pi)^ := AChild.AsFloat;
            ftExtended:
              PExtended(pi)^ := AChild.AsFloat;
            ftComp:
              PComp(pi)^ := AChild.AsFloat;
            ftCurr:
              PCurrency(pi)^ := AChild.AsFloat;
          end;
        {$IFNDEF NEXTGEN}
        tkString:
          PShortString(pi)^ := ShortString(AChild.AsString);
        {$ENDIF !NEXTGEN}
        tkSet:
          begin
            case GetTypeData(ASubType)^.OrdType of
              otSByte:
                begin
                if AChild.FType = jdtInteger then
                  PShortint(pi)^ := AChild.AsInteger
                else
                  PShortint(pi)^ := StringToSet(ASubType, AChild.AsString);
                end;
              otUByte:
                begin
                  if AChild.FType = jdtInteger then
                    PByte(pi)^ := AChild.AsInteger
                  else
                    PByte(pi)^ := StringToSet(ASubType, AChild.AsString);
                end;
              otSWord:
                begin
                  if AChild.FType = jdtInteger then
                    PSmallint(pi)^ := AChild.AsInteger
                  else
                    PSmallint(pi)^ := StringToSet(ASubType, AChild.AsString);
                end;
              otUWord:
                begin
                  if AChild.FType = jdtInteger then
                    PWord(pi)^ := AChild.AsInteger
                  else
                    PWord(pi)^ := StringToSet(ASubType, AChild.AsString);
                end;
              otSLong:
                begin
                  if AChild.FType = jdtInteger then
                    PInteger(pi)^ := AChild.AsInteger
                  else
                    PInteger(pi)^ := StringToSet(ASubType, AChild.AsString);
                end;
              otULong:
                begin
                  if AChild.FType = jdtInteger then
                    PCardinal(pi)^ := AChild.AsInteger
                  else
                    PCardinal(pi)^ := StringToSet(ASubType, AChild.AsString);
                end;
            end;
          end;
        tkClass:
          LoadClass(PPointer(pi)^, AChild);
        tkWChar:
          PWideChar(pi)^ := PWideChar(AChild.AsString)[0];
        {$IFNDEF NEXTGEN}
        tkLString:
          PAnsiString(pi)^ := AnsiString(AChild.AsString);
        tkWString:
          PWideString(pi)^ := AChild.AsString;
        {$ENDIF}
        tkVariant:
          PVariant(pi)^ := AChild.AsVariant;
        tkArray, tkDynArray:
          readValue(AChild.AsJsonObject, pi, ASubType);
        tkRecord:
          readValue(AChild.AsJsonObject, pi, ASubType);
        tkInt64:
          PInt64(pi)^ := AChild.AsInt64;
        tkUString:
          PUnicodeString(pi)^ := AChild.AsString;
      end;
    end;
  end;
  {$ENDIF}
begin
  if (aDest <> nil) and (Assigned(aIn)) then begin
    {$IFDEF USE_UNICODE}
    if aType.Kind = tkRecord then
      ToRecord
    else if aType.Kind = tkClass then
      ToObject
    else if aType.Kind = tkDynArray then
      ToArray
    else if aType.Kind = tkArray then
      ToFixedArray
    {$ELSE}
    if aType.Kind = tkClass then
      ToObject
    {$ENDIF}
    else
      raise Exception.Create(SUnsupportPropertyType);
  end;
end;

{$IFDEF USE_UNICODE}
class function TYxdSerialize.writeToValue(aIn: PJSONValue): TValue;
begin
  case aIn.FType of
    jdtString:
      Result := aIn.AsString;
    jdtInteger:
      Result := aIn.AsInt64;
    jdtFloat:
      Result := aIn.AsFloat;
    jdtDateTime:
      Result := aIn.TryAsDatetime;
    jdtBoolean:
      Result := aIn.AsBoolean;
    jdtObject:
      Result := writeToValue(aIn.AsJsonObject);
  else
    Result := TValue.Empty;
  end;
end;
{$ENDIF}

{$IFDEF USE_UNICODE}
class function TYxdSerialize.writeToValue(aIn: JSONBase): TValue;
var
  AValues: array of TValue;
  I: Integer;
begin
  if not Assigned(aIn) then Exit;
  SetLength(AValues, aIn.Count);
  for I := 0 to aIn.Count - 1 do
    AValues[I] := writeToValue(aIn.Items[I]);
  Result := TValue.FromArray(TypeInfo(TValueArray), AValues);
end;
{$ENDIF}

{$IFDEF USEDataSet}
class procedure TYxdSerialize.WriteDataSet(AOut: JSONBase; const Key: JSONString;
  ADataSet: TDataSet; const PageIndex, PageSize: Integer; 
  Base64Blob: Boolean);
var
  BlobStream: TMemoryStream;

  procedure AddDataSetMeta(MetaItem: JSONArray; Field: TField);
  begin
    MetaItem.Add(Field.FieldName);
    if Field.DataType = ftAutoInc then
      MetaItem.Add(Ord(ftLargeint))
    else
      MetaItem.Add(Ord(Field.DataType));
    MetaItem.Add(Field.Size);
    MetaItem.Add(Field.Required);
    MetaItem.Add(Field.DisplayLabel);
  end;

  procedure AddDataSetRow(DS: TDataSet; Item: JSONArray);
  var
    Field: TField;
  begin
    for Field in DS.Fields do begin
      // 判断字段是否在要求内
      if Field.IsNull then
        Item.Add(null)
      else begin
        case Field.DataType of
          ftBoolean:
            Item.Add(Field.AsBoolean);
          ftDate, ftTime, ftDateTime, ftTimeStamp{$IFDEF USE_UNICODE}, ftTimeStampOffset{$ENDIF}:
            Item.AddDateTime(Field.AsDateTime);
          ftInteger, ftWord, ftSmallint{$IFDEF USE_UNICODE}, ftShortint{$ENDIF}:
            Item.Add(Field.AsInteger);
          ftLargeint, ftAutoInc:
            Item.Add({$IFDEF USE_UNICODE}Field.AsLargeInt{$ELSE}Field.AsInteger{$ENDIF});
          ftFloat, ftBCD: // ftSingle
            Item.Add(Field.AsFloat);
          ftCurrency:
            Item.Add(Field.AsCurrency);
          ftString, ftWideString, ftGuid:
            Item.Add(Field.AsString);
          ftBlob, ftGraphic, ftMemo, ftTypedBinary:
            begin
              if not Assigned(BlobStream) then
                BlobStream := TMemoryStream.Create
              else
                BlobStream.Position := 0;
              TBlobField(Field).SaveToStream(BlobStream);
              {$IFDEF USE_UNICODE}
              if Base64Blob then begin
                Item.Add(CSBlobs + CSBlobBase64 + JSONString(EncodeBase64(BlobStream.Memory, BlobStream.Position)) + '>');
              end else
                Item.Add(CSBlobs + {$IFDEF USEYxdStr}YxdStr{$ELSE}YxdJson{$ENDIF}.BinToHex(BlobStream.Memory, BlobStream.Position) + '>');
              {$ELSE}
              if Base64Blob then
                Item.Add(CSBlobs + CSBlobBase64 + Base64Encode(BlobStream.Memory^, BlobStream.Position) + '>')
              else begin
                Item.Add(CSBlobs + {$IFDEF USEYxdStr}YxdStr{$ELSE}YxdJson{$ENDIF}.BinToHex(BlobStream.Memory, BlobStream.Position) + '>');
              end;
              {$ENDIF}
            end;
        else
          Item.Add(Field.AsString);
        end;
      end;
    end;
  end;

  procedure AddDataSet(DS: TDataSet);
  var
    Data: JSONArray;
    Field: TField;
    MoveIndex, StepIndex: Integer;
  begin
    Data := JSONObject(aOut).AddChildArray('meta');
    for Field in DS.Fields do
      AddDataSetMeta(Data.AddChildArray(), Field);

    BlobStream := nil;
    DS.DisableControls;
    try
      Data := JSONObject(aOut).AddChildArray('data');
      DS.First;
      // 分页移动记录
      if (PageIndex > 0) and (PageSize > 0) then begin
        MoveIndex := (PageIndex - 1) * PageSize;
        DS.MoveBy(MoveIndex);
      end;
      StepIndex := 0;
      while not DS.Eof do begin
        AddDataSetRow(DS, Data.AddChildArray);
        if (PageSize > 0) then begin
          Inc(StepIndex);
          if StepIndex >= PageSize then
            Break;
        end;
        DS.Next;
      end;
    finally
      DS.EnableControls;
      if Assigned(BlobStream) then
        BlobStream.Free;
    end;
  end;

begin
  if aOut.IsJSONArray then
    aOut := JSONArray(aOut).AddChildObject()
  else if key <> '' then
    aOut := JSONObject(aOut).addChildObject(key);
  AddDataSet(ADataSet);
end;
{$ENDIF}

class procedure TYxdSerialize.writeValue(aOut: JSONBase; const key: JSONString; aSource: Pointer;
  aType: PTypeInfo);
{$IFDEF USE_UNICODE}var AValue: TValue;{$ENDIF}

  procedure AddStringsToArray(AParent:JSONArray; AData:TStrings);
  var
    J: Integer;
  begin
    for J := 0 to AData.Count-1 do
      AParent.Add(AData[J]);
  end;

  procedure AddCollection(AParent:JSONBase; ACollection:TCollection);
  var
    J: Integer;
  begin
    for J := 0 to ACollection.Count-1 do
      writeValue(AParent, '', ACollection.Items[J]{$IFNDEF USE_UNICODE}, GetObjectTypeInfo(ACollection.Items[J]){$ENDIF});
  end;

  {$IFDEF USE_UNICODE}
  procedure AddRecord;
  var
    AObj: TObject;
    AFieldItem: TRttiField;
    AContext: TRttiContext;
    AFields: TArray<TRttiField>;
    ARttiType: TRttiType;
    AFieldName: string;
    AFieldAttrItem: TCustomAttribute;
    II, J: Integer;
  begin
    AContext := TRttiContext.Create;
    ARttiType := AContext.GetType(AType);
    AFields := ARttiType.GetFields;
    //如果是从结构体，则记录其成员，如果是对象，则只记录其公开的属性，特殊处理TStrings和TCollection
    for J := Low(AFields) to High(AFields) do begin
      AFieldItem := AFields[J];
      if AFieldItem.FieldType <> nil then begin
        AFieldName := AFieldItem.Name;
        if AFieldItem.GetAttributes <> nil then begin
          for AFieldAttrItem in AFieldItem.GetAttributes do
            if AFieldAttrItem is FieldNameAttribute then begin
              AFieldName := FieldNameAttribute(AFieldAttrItem).Name;
              Break;
            end;
        end;

        case AFieldItem.FieldType.TypeKind of
          tkInteger:
            JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsInteger);
          {$IFNDEF NEXTGEN}tkString,tkLString,tkWString,{$ENDIF !NEXTGEN}tkUString:
            JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsString);
          tkEnumeration:
            begin
              if GetTypeData(AFieldItem.FieldType.Handle).BaseType^ = TypeInfo(Boolean) then
                JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsBoolean)
              else if JsonRttiEnumAsInt then
                JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsOrdinal)
              else
                JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).ToString);
            end;
          tkSet:
            begin
              if JsonRttiEnumAsInt then
                JSONObject(aOut).put(AFieldName, SetAsOrd(AFieldItem.GetValue(ASource)))
              else
                JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).ToString);
            end;
          tkChar,tkWChar:
            JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).ToString);
          tkFloat:
            begin
              if (AFieldItem.FieldType.Handle = TypeInfo(TDateTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TTime)) or
                (AFieldItem.FieldType.Handle = TypeInfo(TDate))
                 then
                JSONObject(aOut).putDateTime(AFieldName, AFieldItem.GetValue(ASource).AsExtended)
              else
                JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsExtended);
            end;
          tkInt64:
            JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsInt64);
          tkVariant:
            JSONObject(aOut).put(AFieldName, AFieldItem.GetValue(ASource).AsVariant);
          tkArray, tkDynArray:
            with JSONObject(aOut).addChildArray(AFieldName) do begin
              AValue := AFieldItem.GetValue(ASource);
              for II := 0 to AValue.GetArrayLength - 1 do
                putObjectValue('', AValue.GetArrayElement(II));
            end;
          tkClass:
            begin
              AObj := AFieldItem.GetValue(ASource).AsObject;
              if (AObj is TStrings) then
                JSONObject(aOut).put(AFieldName, TStrings(AObj).Text)
              else if AObj is TCollection then
                AddCollection(JSONObject(aOut).addChildArray(AFieldName), AObj as TCollection)
              else
                writeValue(aOut, AFieldName, AObj, AFieldItem.FieldType.Handle);
            end;
          tkRecord:
            writeValue(aOut, AFieldName, Pointer(IntPtr(ASource) + AFieldItem.Offset),
              AFieldItem.FieldType.Handle);
        end;
      end else
        raise Exception.CreateFmt(SMissRttiTypeDefine,[AFieldItem.Name]);
    end;
  end;
  {$ENDIF}

  procedure AddObject;
  var
    AName: JSONString;
    APropList: PPropList;
    ACount: Integer;
    AObj, AChildObj: TObject;
    J: Integer;
  begin
    AObj := ASource;
    ACount := GetPropList(AType,APropList);
    try
      for J := 0 to ACount - 1 do begin
        if APropList[J].PropType^.Kind in [tkMethod{$IFDEF USE_UNICODE}, tkProcedure{$ENDIF}] then
          Continue;
        if APropList[J].PropType^.Kind in
          [tkInterface{$IFDEF USE_UNICODE}, tkClassRef, tkPointer{$ENDIF}]
        then begin
          if IsDefaultPropertyValue(AObj, APropList[J], nil) then
            Continue;
        end;
        {$IF RTLVersion>25}
        AName := APropList[J].NameFld.ToString;
        {$ELSE}
        AName := String(APropList[J].Name);
        {$IFEND}
        case APropList[J].PropType^.Kind of
          tkClass:
            begin
              AChildObj := Pointer(GetOrdProp(AObj, APropList[J]));
              if AChildObj is TStrings then
                JSONObject(aOut).put(AName, (AChildObj as TStrings).Text)
              else if AChildObj is TCollection then
                AddCollection(JSONObject(aOut).addChildArray(AName), AChildObj as TCollection)
              else
                writeValue(aOut, AName, AChildObj{$IFNDEF USE_UNICODE}, GetObjectTypeInfo(AChildObj){$ENDIF});
            end;
          tkInteger:
            JSONObject(aOut).put(AName, GetOrdProp(AObj,APropList[J]));
          tkChar,tkString,tkWChar, tkLString, tkWString{$IFDEF USE_UNICODE}, tkUString{$ENDIF}:
            JSONObject(aOut).put(AName, GetStrProp(AObj,APropList[J]));
          tkEnumeration:
            begin
              if GetTypeData(APropList[J]^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
                JSONObject(aOut).put(AName, GetOrdProp(AObj,APropList[J])<>0)
              else if JsonRttiEnumAsInt then
                JSONObject(aOut).put(AName, GetOrdProp(AObj,APropList[J]))
              else
                JSONObject(aOut).put(AName, GetEnumProp(AObj,APropList[J]));
            end;
          tkSet:
            begin
              if JsonRttiEnumAsInt then
                JSONObject(aOut).put(AName, GetOrdProp(AObj, APropList[J]))
              else
                JSONObject(aOut).put(AName, GetSetProp(AObj,APropList[J],True));
            end;
          tkVariant:
            JSONObject(aOut).put(AName, GetPropValue(AObj,APropList[J]));
          tkInt64:
            JSONObject(aOut).put(AName, GetInt64Prop(AObj,APropList[J]));
          tkRecord, tkArray, tkDynArray://记录、数组、动态数组属性系统也不保存，也没提供所有太好的接口
            raise Exception.Create(SUnsupportPropertyType);
        end;
      end;
    finally
      FreeMem(APropList);
    end;
  end;

  {$IFDEF USE_UNICODE}
  procedure AddArray;
  var
    I: Integer;
  begin
    TValue.Make(ASource, AType, AValue);
    for I := 0 to AValue.GetArrayLength - 1 do
      writeValue(aOut, '', AValue.GetArrayElement(I));
  end;
  {$ENDIF}
begin
  if not Assigned(ASource) then Exit;
  case AType.Kind of
    {$IFDEF USE_UNICODE}
    tkRecord:
      begin
        if aOut.IsJSONArray then
          aOut := JSONArray(aOut).AddChildObject()
        else begin
          if Key <> '' then
            aOut := JSONObject(aOut).addChildObject(key);
        end;
        AddRecord;
      end;
    {$ENDIF}
    tkClass:
      begin
        if TObject(ASource) is TStrings then begin
          if key = '' then begin
            if aOut.IsJSONArray then begin
              AddStringsToArray(JSONArray(AOut), TStrings(ASource))
            end else
              JSONObject(aOut).put('text', TStrings(ASource).Text)
          end else
            JSONObject(aOut).put(key, TStrings(ASource).Text)
        end else if TObject(ASource) is TCollection then
          AddCollection(aOut, TCollection(ASource))
        {$IFDEF USEDataSet}
        else if TObject(ASource) is TDataSet then
          WriteDataSet(aOut, Key, TDataSet(ASource), 0, -1)
        {$ENDIF}
        else begin
          if aOut.IsJSONArray then
            aOut := JSONArray(aOut).AddChildObject()
          else if key <> '' then
            aOut := JSONObject(aOut).addChildObject(key);
          AddObject;
        end;
      end;
    {$IFDEF USE_UNICODE}
    tkDynArray:
      begin
        if aOut.IsJSONArray then
          aOut := JSONArray(aOut).addChildArray()
        else
          aOut := JSONObject(aOut).addChildArray(key);
        AddArray;
      end;
    {$ENDIF}
  end;
end;

{$IFDEF USE_UNICODE}
class procedure TYxdSerialize.writeValue(aOut: JSONBase; const key: JSONString; aInstance: TValue);
var
  I,C:Integer;
begin
   if not Assigned(aOut) then Exit;
  case aInstance.Kind of
    tkClass:
      writeValue(aOut, key, aInstance.AsObject, aInstance.TypeInfo);
    tkRecord:
      writeValue(aOut, key, aInstance.GetReferenceToRawData, aInstance.TypeInfo);
    tkArray, tkDynArray:
      begin
        if not aOut.IsJSONArray then
          aOut := JSONObject(aOut).addChildArray(key)
        else
          aOut.Clear;
        C := aInstance.GetArrayLength;
        for I := 0 to C-1 do
          writeValue(aOut, '', AInstance.GetArrayElement(I));
      end;
    tkInteger, tkInt64:
      JSONObject(aOut).put(key, AInstance.AsInt64);
    tkChar, tkString,tkWChar, tkLString, tkWString, tkUString:
      JSONObject(aOut).put(key, aInstance.ToString);
    tkEnumeration:
      begin
        if GetTypeData(AInstance.TypeInfo)^.BaseType^ = TypeInfo(Boolean) then
          JSONObject(aOut).put(key, aInstance.AsBoolean)
        else if JsonRttiEnumAsInt then
          JSONObject(aOut).put(key, aInstance.AsOrdinal)
        else
          JSONObject(aOut).put(key, aInstance.ToString)
      end;
    tkSet:
      JSONObject(aOut).put(key, aInstance.ToString);
    tkVariant:
      JSONObject(aOut).put(key, aInstance.AsVariant)
  end;
end;
{$ENDIF} 

{ TSimpleQueue }

procedure TSimpleQueue.Clear;
var
  ANext: PQueueItem;
begin
  if FHead = nil then Exit;
  while FHead.Next <> nil do begin
    ANext := FHead.Next;
    Dispose(FHead);
    FHead := ANext;
  end;
  FCount := 0;
end;

constructor TSimpleQueue.Create;
begin
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

function TSimpleQueue.DeQueue(): TQueueValue;
var
  lvTemp: PQueueItem;
begin
  lvTemp := InnerPop;
  if lvTemp <> nil then begin
    Result := lvTemp.Data;
    Dispose(lvTemp);
  end else
    Result := FDefaultValue;
end;

function TSimpleQueue.DeQueue(var Dest: TQueueValue): Boolean;
var
  lvTemp: PQueueItem;
begin
  lvTemp := InnerPop;
  if lvTemp <> nil then begin
    Dest := lvTemp.Data;
    Dispose(lvTemp);
    Result := True;
  end else
    Result := False;
end;

destructor TSimpleQueue.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSimpleQueue.EnQueue(AData: TQueueValue);
var
  lvTemp: PQueueItem;
begin
  New(lvTemp);
  lvTemp.Data := AData;
  InnerAddToTail(lvTemp);
end;

procedure TSimpleQueue.InnerAddToTail(AData: PQueueItem);
begin
  if FTail = nil then begin
    FTail := AData;
    AData.Next := nil;
  end else begin
    AData.Next := FHead;
  end;
  FHead := AData;
  Inc(FCount);
end;

function TSimpleQueue.InnerPop: PQueueItem;
begin
  Result := FHead;
  if Result <> nil then begin
    FHead := Result.Next;
    if FHead = nil then
      FTail := nil;
    Dec(FCount);
  end;
end;

function TSimpleQueue.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TSimpleQueue.Size: Integer;
begin
  Result := FCount;
end;

{ TJsonSerializeWriter }

procedure TJsonSerializeWriter.AddInt64(const Value: Int64);
begin
  FData.Cat(IntToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.Add(const Value: Integer);
begin
  FData.Cat(IntToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.Add(const Value: string);

  procedure CatValue(const AValue: JSONString);
  var
    ps: PJSONChar;
    {$IFNDEF USE_UNICODE}w: Word;{$ENDIF}
  begin
    ps := PJSONChar(AValue);
    while ps^ <> #0 do begin
      case ps^ of
        #7:   FData.Cat(Char7, 2);
        #9:   FData.Cat(Char9, 2);
        #10:  FData.Cat(Char10, 2);
        #12:  FData.Cat(Char12, 2);
        #13:  FData.Cat(Char13, 2);
        '\':  FData.Cat(CharBackslash, 2);
        '"':  FData.Cat(CharQuoter, 2);
        else begin
          if ps^ < #$1F then begin
            FData.Cat(CharCode, 4);
            if ps^ > #$F then
              FData.Cat(CharNum1, 1)
            else
              FData.Cat(CharNum0, 1);
            FData.Cat(HexChar(Ord(ps^) and $0F));
          end else if (ps^ <= #$7E) or (not FDoEscape) then//英文字符区
            FData.Cat(ps, 1)
          else
            {$IFDEF USE_UNICODE}
            FData.Cat(CharEscape, 2).Cat(
              HexChar((PWord(ps)^ shr 12) and $0F)).Cat(
              HexChar((PWord(ps)^ shr 8) and $0F)).Cat(
              HexChar((PWord(ps)^ shr 4) and $0F)).Cat(
              HexChar(PWord(ps)^ and $0F));
            {$ELSE}
            begin
            w := PWord(AnsiDecode(ps, 2))^;
            FData.Cat(CharEscape, 2).Cat(
              HexChar((w shr 12) and $0F)).Cat(
              HexChar((w shr 8) and $0F)).Cat(
              HexChar((w shr 4) and $0F)).Cat(
              HexChar(w and $0F));
            Inc(ps);
            end;
            {$ENDIF}
        end;
      end;
      Inc(ps);
    end;
  end;

begin
  FData.Cat('"');
  CatValue(Value);
  FData.Cat('",');
end;

procedure TJsonSerializeWriter.Add(const Value: Cardinal);
begin
  FData.Cat(IntToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.Add(const Value: Variant);

  procedure SetVariantArray();
  var
    I: Integer;
  begin
    BeginData('', True);
    for I := VarArrayLowBound(Value, VarArrayDimCount(Value))
      to VarArrayHighBound(Value, VarArrayDimCount(Value)) do
      Add(Value[I]);
    EndData;
  end;

begin
  case FindVarData(Value)^.VType of
    varBoolean: Add(Boolean(Value));
    varByte, varWord, varSmallint, varInteger, varShortInt:
      Add(Integer(Value));
    varLongWord:
      Add(Cardinal(Value));
    varInt64:
      AddInt64(Integer(Value));
    varSingle, varDouble, varCurrency:
      Add(Double(Value));
    varDate:
      Add(VarToDateTime(Value));
    varOleStr, varString:
      Add(string(Value));
    else begin
      if VarIsArray(Value) then begin
        SetVariantArray();
      end else begin
        Add('');
      end;
    end;
  end;
end;

procedure TJsonSerializeWriter.Add(const Value: Double);
begin
  FData.Cat(YxdJson.FloatToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.AddTime(const Value: TDateTime);

  procedure StrictJsonTime(ATime:TDateTime);
  const
    JsonTimeStart: PJSONChar = '"/DATE(';
    JsonTimeEnd:   PJSONChar = ')/"';
  var
    MS: Int64; //时区信息不保存
  begin
    MS := Trunc(ATime * 86400000);
    FData.Cat(JsonTimeStart, 7);
    FData.Cat(IntToStr(MS));
    FData.Cat(JsonTimeEnd, 3);
  end;

  function ValueAsDateTime(const DateFormat, TimeFormat, DateTimeFormat: JSONString; const AValue: TDateTime): JSONString;
  var
    ADate: Integer;
  begin
    ADate := Trunc(AValue);
    if SameValue(ADate, 0) then begin //Date为0，是时间
      if SameValue(AValue, 0) then
        Result := FormatDateTime(DateFormat, AValue)
      else
        Result := FormatDateTime(TimeFormat, AValue);
    end else begin
      if SameValue(AValue-ADate, 0) then
        Result := FormatDateTime(DateFormat, AValue)
      else
        Result := FormatDateTime(DateTimeFormat, AValue);
    end;
  end;

begin
  if StrictJson then
    StrictJsonTime(Value)
  else
    FData.Cat('"').Cat(ValueAsDateTime(JsonDateFormat, JsonTimeFormat, JsonDateTimeFormat, Value)).Cat('"');
  FData.Cat(',');
end;

procedure TJsonSerializeWriter.BeginData(const Name: string; const IsArray: Boolean);
begin
  FState.EnQueue(FIsArray);
  FIsArray := IsArray;
  if Name <> '' then
    FData.Cat('"').Cat(Name).Cat('":');
  if IsArray then
    FData.Cat('[')
  else
    FData.Cat('{');
end;

procedure TJsonSerializeWriter.BeginRoot;
begin
end;

constructor TJsonSerializeWriter.Create;
begin
  FData := TStringCatHelper.Create;
  FState := TSimpleQueue.Create;
  FDoEscape := True;
end;

destructor TJsonSerializeWriter.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FState);
  inherited;
end;

procedure TJsonSerializeWriter.EndData;
begin
  if (FData.Last = ',') then
    FData.Back(1);
  if FIsArray then
    FData.Cat('],')
  else
    FData.Cat('},');
  FIsArray := FState.DeQueue;
end;

procedure TJsonSerializeWriter.EndRoot;
begin
end;

function TJsonSerializeWriter.IsArray: Boolean;
begin
  Result := FIsArray;
end;

function TJsonSerializeWriter.ToString: string;
begin
  if (FData.Last = ',') then
    FData.Back(1);
  Result := FData.Value;
end;

procedure TJsonSerializeWriter.WriteBoolean(const Name: string;
  const Value: Boolean);
begin
  WriteName(Name);
  if Value then
    FData.Cat('true')
  else
    FData.Cat('false');
  FData.Cat(',');
end;

procedure TJsonSerializeWriter.WriteDateTime(const Name: string; const Value: TDateTime);
begin
  WriteName(Name);
  AddTime(Value);
end;

procedure TJsonSerializeWriter.WriteFloat(const Name: string;
  const Value: Double);
begin
  WriteName(Name);
  FData.Cat(YxdJson.FloatToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.WriteInt(const Name: string;
  const Value: Integer);
begin
  WriteName(Name);
  FData.Cat(IntToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.WriteInt64(const Name: string;
  const Value: Int64);
begin
  WriteName(Name);
  FData.Cat(IntToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.WriteName(const Name: string);
begin
  if FIsArray then Exit;
  FData.Cat('"').Cat(Name).Cat('":');
end;

procedure TJsonSerializeWriter.WriteString(const Name, Value: string);
begin
  WriteName(Name);
  Add(Value);
end;

procedure TJsonSerializeWriter.WriteUInt(const Name: string;
  const Value: Cardinal);
begin
  WriteName(Name);
  FData.Cat(IntToStr(Value)).Cat(',');
end;

procedure TJsonSerializeWriter.WriteVariant(const Name: string;
  const Value: Variant);

  procedure SetVariantArray();
  var
    I: Integer;
  begin
    BeginData(Name, True);
    for I := VarArrayLowBound(Value, VarArrayDimCount(Value))
      to VarArrayHighBound(Value, VarArrayDimCount(Value)) do
      Add(Value[I]);
    EndData;
  end;

begin
  case FindVarData(Value)^.VType of
    varBoolean: WriteBoolean(Name, Value);
    varByte, varWord, varSmallint, varInteger, varShortInt:
      WriteInt(Name, Value);
    varLongWord:
      WriteUInt(Name, Value);
    varInt64:
      WriteInt64(Name, Value);
    varSingle, varDouble, varCurrency:
      WriteFloat(Name, Value);
    varDate:
      WriteDateTime(Name, Value);
    varOleStr, varString:
      WriteString(Name, Value);
    else begin
      if VarIsArray(Value) then begin
        SetVariantArray();
      end else begin
        WriteString(Name, '');
      end;
    end;
  end;
end;


end.
