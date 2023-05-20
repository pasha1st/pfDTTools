unit pf1Cdtlib;
interface
uses System.Classes,System.SysUtils,System.ZLib;

{$IF NOT Defined(TBufferedFileStream)}
  {$DEFINE DefineBufferedFileStream}
type
  TBufferedFileStream = class(TFileStream)
  private
    FFilePos, FBufStartPos, FBufEndPos: Int64;
    FBuffer: PByte;
    FBufferSize: Integer;
    FModified: Boolean;
    FBuffered: Boolean;
  protected
    procedure SetSize(const NewSize: Int64); override;
    /// <summary>
    ///    SyncBuffer writes buffered and not yet written data to the file.
    ///    When ReRead is True, then buffer will be repopulated from the file.
    ///    When ReRead is False, then buffer will be emptied, so next read or
    ///    write operation will repopulate buffer.
    /// </summary>
    procedure SyncBuffer(ReRead: Boolean);
  public
    constructor Create(const AFileName: string; Mode: Word; BufferSize: Integer = 32768); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal; BufferSize: Integer = 32768); overload;
    destructor Destroy; override;
    /// <summary>
    ///    FlushBuffer writes buffered and not yet written data to the file.
    /// </summary>
    procedure FlushBuffer; inline;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{$IFEND}

type
  DT_OPS=(DT_OP_NOP,DT_OP_ENDGROUP,DT_OP_NEXTITEM,DT_OP_STARTGROUP
  	,DT_OP_DIGIT_0,DT_OP_DIGIT_1,DT_OP_DIGIT_2,DT_OP_DIGIT_3
  	,DT_OP_DIGIT_4,DT_OP_DIGIT_5,DT_OP_DIGIT_6,DT_OP_DIGIT_7
  	,DT_OP_DIGIT_8,DT_OP_DIGIT_9
  	,DT_OP_DECIMAL_BYTE,DT_OP_DECIMAL_BYTE_NEG
  	,DT_OP_DECIMAL_WORD,DT_OP_DECIMAL_WORD_NEG
  	,DT_OP_DECIMAL_U32,DT_OP_DECIMAL_U32_NEG
  	,DT_OP_DECIMAL_INT64
  	,DT_OP_GUID,DT_OP_UNQUOTE
	,DT_OP_WSTRING1B,DT_OP_WSTRING2B,DT_OP_WSTRING8B
	,DT_OP_ASTRING1B,DT_OP_ASTRING2B,DT_OP_ASTRING8B
	,DT_OP_EOF
	,DT_OP_UNKNOWN
  	);

  DT_OPS_GENERIC=(DT_OPG_NOP,DT_OPG_ENDGROUP,DT_OPG_NEXTITEM,DT_OPG_STARTGROUP
  	,DT_OPG_DIGIT
  	,DT_OPG_DECIMAL
  	,DT_OPG_GUID,DT_OPG_UNQUOTE
	,DT_OPG_WSTRING
	,DT_OPG_ASTRING
	,DT_OPG_EOF
	,DT_OPG_UNKNOWN
  	);

  TDT_OP_Record=record
    op:DT_OPS;
    opg:DT_OPS_GENERIC;
    s:AnsiString;
    ws:WideString;
    n:int64;
    StrAsHex:boolean;
  end;

  T1CdtObject=class
  protected
    fin:TStream;
    own_streams:array of TStream;
    op_stack:array[0..4] of DT_OPS;
    op_stack_idx:integer;
    FEof:boolean;
    FLastOpByte:byte;
    FDTVer:AnsiChar;
    FCurrentScanState:record
      Level:integer;
      Quote:boolean;
    end;
    procedure InitScanState;
  public
    constructor Create(st:TStream;_own:boolean=false);overload;
    constructor Create(fn:WideString;unpacked:boolean=false);overload;
    destructor Destroy;override;
    function GetOp():DT_OPS;
    function GetOpEx(astr_as_raw:boolean=false;decode_data:boolean=true):TDT_OP_Record;
    property Eof:boolean read FEof;
    procedure ReadGuid(out guid:TGuid);overload;
    function ReadGuid:TGuid;overload;
    function Read(var buf;count:int64):int64;
    function GetPosition:int64;
    property Position:int64 read GetPosition;
    property LastOpByte:byte read FLastOpByte;
    property DTVer:AnsiChar read FDTVer;

    function _GetFirstOwnStream:TStream; // for internal use only!
    property _GetInStream:TStream read fin; // for internal use only!
  end;

  Tdt1CField=record
    name:AnsiString;
    _type:AnsiString;
    length,precision:integer;
    can_null:boolean;
  end;
  Tdt1CTable=record
    name:AnsiString;
    fields:array of Tdt1CField;
  end;
//function GetTableDef(name:AnsiString;ver:integer=80313):Tdt1CTable;

const
  dtOpNames:array[DT_OPS] of String=('DT_OP_NOP','DT_OP_ENDGROUP'
	,'DT_OP_NEXTITEM','DT_OP_STARTGROUP'
  	,'DT_OP_DIGIT_0','DT_OP_DIGIT_1','DT_OP_DIGIT_2','DT_OP_DIGIT_3'
  	,'DT_OP_DIGIT_4','DT_OP_DIGIT_5','DT_OP_DIGIT_6','DT_OP_DIGIT_7'
  	,'DT_OP_DIGIT_8','DT_OP_DIGIT_9'
  	,'DT_OP_DECIMAL_BYTE','DT_OP_DECIMAL_BYTE_NEG'
  	,'DT_OP_DECIMAL_WORD','DT_OP_DECIMAL_WORD_NEG'
  	,'DT_OP_DECIMAL_U32','DT_OP_DECIMAL_U32_NEG'
  	,'DT_OP_DECIMAL_INT64'
  	,'DT_OP_GUID','DT_OP_UNQUOTE'
	,'DT_OP_WSTRING1B','DT_OP_WSTRING2B','DT_OP_WSTRING8B'
	,'DT_OP_ASTRING1B','DT_OP_ASTRING2B','DT_OP_ASTRING8B'
	,'DT_OP_EOF'
	,'DT_OP_UNKNOWN'
  	);

const
  dtHdrMagic='1CIBDmpF';

function Guid1CtoStringMS(const guid:TGuid): AnsiString;
function Guid1CtoString1C(const guid:TGuid): AnsiString;

implementation

var
  dtOps:array[0..$1F] of DT_OPS=(DT_OP_NOP
  	,DT_OP_DIGIT_0,DT_OP_DIGIT_1,DT_OP_DIGIT_2,DT_OP_DIGIT_3
  	,DT_OP_DIGIT_4,DT_OP_DIGIT_5,DT_OP_DIGIT_6,DT_OP_DIGIT_7
  	,DT_OP_DIGIT_8,DT_OP_DIGIT_9
  	,DT_OP_DECIMAL_BYTE,DT_OP_DECIMAL_BYTE_NEG
  	,DT_OP_DECIMAL_WORD,DT_OP_DECIMAL_WORD_NEG
  	,DT_OP_DECIMAL_U32,DT_OP_DECIMAL_U32_NEG
  	,DT_OP_DECIMAL_INT64 //11
  	,DT_OP_UNKNOWN,DT_OP_UNKNOWN,DT_OP_UNKNOWN
  	,DT_OP_GUID,DT_OP_UNQUOTE
	,DT_OP_WSTRING1B,DT_OP_WSTRING2B,DT_OP_WSTRING8B
	,DT_OP_ASTRING1B,DT_OP_ASTRING2B,DT_OP_ASTRING8B //1c
	,DT_OP_UNKNOWN,DT_OP_UNKNOWN,DT_OP_UNKNOWN
  );

constructor T1CdtObject.Create(st:TStream;_own:boolean=false);
var
  i:integer;
begin
//  own_stream:=false;
  op_stack_idx:=0;
  fin:=st;
  if _own then begin
    i:=length(own_streams);
    SetLength(own_streams,i+1);
    own_streams[i]:=st;
  end;
  InitScanState;
end;

constructor T1CdtObject.Create(fn:WideString;unpacked:boolean=false);
var
  hdr:array[0..7] of AnsiChar;
begin
  if unpacked then begin
    Create(TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite),true);
  end else begin
    SetLength(own_streams,1);
    own_streams[0]:=TFileStream.Create(fn,fmOpenRead or fmShareDenyWrite);
    own_streams[0].Read(hdr[0],SizeOf(hdr));
    if hdr<>dtHdrMagic then begin
      FreeAndNil(own_streams[0]);
      SetLength(own_streams,0);
      Raise Exception.Create('File header mismatch');
    end;
    own_streams[0].Read(FDTVer,1);
    Create(TZDecompressionStream.Create(own_streams[0],-15),true);
  end;
end;

destructor T1CdtObject.Destroy;
var
  i:integer;
begin
  for i:=Length(own_streams)-1 downto 0 do
    FreeAndNil(own_streams[i]);
end;

function T1CdtObject.GetOp():DT_OPS;
var
  b:byte;
begin
  result:=DT_OP_EOF;
  if op_stack_idx>0 then begin
    dec(op_stack_idx);
    result:=op_stack[op_stack_idx];
    exit;
  end;
  if Eof then begin
//    result:=DT_OP_EOF;
    exit;
  end;
  try
    if fin.Read(b,1)<=0 then begin
      FEof:=true;
//      result:=DT_OP_EOF;
      exit;
    end;
    FLastOpByte:=b;
    op_stack[op_stack_idx]:=dtOps[b and $1F];
    if (b and $40)<>0 then begin
      inc(op_stack_idx);
      op_stack[op_stack_idx]:=DT_OP_STARTGROUP;
    end;
    if (b and $80)<>0 then begin
      inc(op_stack_idx);
      op_stack[op_stack_idx]:=DT_OP_NEXTITEM;
    end;
    if (b and $20)<>0 then begin
      inc(op_stack_idx);
      op_stack[op_stack_idx]:=DT_OP_ENDGROUP;
    end;
    result:=op_stack[op_stack_idx];
  except
    FEof:=true;
//    result:=DT_OP_EOF;
    exit;
  end;
end;

function Guid1CtoStringMS(const guid:TGuid): AnsiString;
var
  i:integer;
  p:PByteArray;
begin
  p:=@guid;

  result:=AnsiString(IntToHex(PLongWord(@p^[0])^,8))+'-'+
    AnsiString(IntToHex(PWord(@p^[4])^,4))+'-'+
    AnsiString(IntToHex(PWord(@p^[6])^,4))+'-'+
    AnsiString(IntToHex(p^[8],2))+AnsiString(IntToHex(p^[9],2))+'-';
  for i := 0 to 5 do
    result:=result+AnsiString(IntToHex(p^[10+i],2));

{  for i := 0 to 3 do
    result:=result+IntToHex(p^[12+i],2);
  result:=result+'-';
  for i := 0 to 1 do
    result:=result+IntToHex(p^[10+i],2);
  result:=result+'-';
  for i := 0 to 1 do
    result:=result+IntToHex(p^[8+i],2);
  result:=result+'-';
  for i := 0 to 1 do
    result:=result+IntToHex(p^[0+i],2);
  result:=result+'-';
  for i := 0 to 5 do
    result:=result+IntToHex(p^[2+i],2);}
end;

function Guid1CtoString1C(const guid:TGuid): AnsiString;
var
  i:integer;
  p:PByteArray;
begin
  p:=@guid;

  for i := 0 to 3 do
    result:=result+IntToHex(p^[12+i],2);
  result:=result+'-';
  for i := 0 to 1 do
    result:=result+IntToHex(p^[10+i],2);
  result:=result+'-';
  for i := 0 to 1 do
    result:=result+IntToHex(p^[8+i],2);
  result:=result+'-';
  for i := 0 to 1 do
    result:=result+IntToHex(p^[0+i],2);
  result:=result+'-';
  for i := 0 to 5 do
    result:=result+IntToHex(p^[2+i],2);
end;

function T1CdtObject.GetOpEx(astr_as_raw:boolean=false;decode_data:boolean=true): TDT_OP_Record;
var
  b:byte;
  fl:boolean;
  g:TGuid;
  l4:integer;
  l8:int64;
  _as:AnsiString;

  procedure ReadWS(size:int64;out ws:WideString);
  begin
    SetLength(ws,size);
    if size>0 then begin
      self.Read(PWideChar(ws)^,Length(ws)*2);
    end;
  end;

  //returns true if string of Ansi chars
  function ReadS(size:int64;out s:AnsiString):boolean;
  var
    i:integer;
  begin
    SetLength(s,size);
    if size>0 then begin
      self.Read(PAnsiChar(s)^,Length(s));
    end;
    result:=true;
    for i:=1 to Length(s) do
      if (s[i]<#$20)or(s[i]>#$7F) then begin
        result:=false;
        break;
      end;
  end;
const
  HexChars:array[0..$F] of AnsiChar='0123456789abcdef';
begin
  result.op:=DT_OP_EOF;
  result.opg:=DT_OPG_EOF;
  result.StrAsHex:=false;
  if op_stack_idx>0 then begin
    dec(op_stack_idx);
    result.op:=op_stack[op_stack_idx];
  end else begin
    if Eof then begin
      exit;
    end;
    try
      if fin.Read(b,1)<=0 then begin
        FEof:=true;
        exit;
      end;
      FLastOpByte:=b;
      op_stack[op_stack_idx]:=dtOps[b and $1F];
      if (b and $40)<>0 then begin
        inc(op_stack_idx);
        op_stack[op_stack_idx]:=DT_OP_STARTGROUP;
      end;
      if (b and $80)<>0 then begin
        inc(op_stack_idx);
        op_stack[op_stack_idx]:=DT_OP_NEXTITEM;
      end;
      if (b and $20)<>0 then begin
        inc(op_stack_idx);
        op_stack[op_stack_idx]:=DT_OP_ENDGROUP;
      end;
      result.op:=op_stack[op_stack_idx];
    except
      FEof:=true;
      exit;
    end;
  end;

  case result.op of
    DT_OP_NEXTITEM:
      result.opg:=DT_OPG_NEXTITEM;
    DT_OP_STARTGROUP:
      begin
        result.opg:=DT_OPG_STARTGROUP;
        inc(FCurrentScanState.Level);
        result.n:=FCurrentScanState.Level;
      end;
    DT_OP_ENDGROUP:
      begin
        result.opg:=DT_OPG_ENDGROUP;
        dec(FCurrentScanState.Level);
        result.n:=FCurrentScanState.Level;
      end;
    DT_OP_DIGIT_0..DT_OP_DIGIT_9:
      begin
        result.opg:=DT_OPG_DIGIT;
        result.n:=byte(result.op)-byte(DT_OP_DIGIT_0);
        if decode_data then
          result.s:=AnsiChar(result.n+byte('0'));
      end;
    DT_OP_DECIMAL_BYTE:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,1);
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_DECIMAL_BYTE_NEG:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,1);result.n:=-result.n;
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_DECIMAL_WORD:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,2);
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_DECIMAL_WORD_NEG:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,2);result.n:=-result.n;
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_DECIMAL_U32:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,4);
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_DECIMAL_U32_NEG:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,4);result.n:=-result.n;
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_DECIMAL_INT64:
      begin
        result.opg:=DT_OPG_DECIMAL;
        result.n:=0;
        Read(result.n,8);
        if decode_data then
          result.s:=AnsiString(IntToStr(result.n));
      end;
    DT_OP_GUID:
      begin
        result.opg:=DT_OPG_GUID;
        result.n:=0;
        ReadGuid(g);
        if decode_data then
          result.s:=Guid1CtoStringMS(g);
      end;
    DT_OP_UNQUOTE:
      begin
        result.opg:=DT_OPG_UNQUOTE;
        FCurrentScanState.Quote:=false;
        result.n:=byte(FCurrentScanState.Quote);
      end;
    DT_OP_WSTRING1B:
      begin
        result.opg:=DT_OPG_WSTRING;
        l8:=0;
        Read(l8,1);
        ReadWS(l8,result.ws);
        result.n:=byte(FCurrentScanState.Quote);
        FCurrentScanState.Quote:=true;
      end;
    DT_OP_WSTRING2B:
      begin
        result.opg:=DT_OPG_WSTRING;
        l8:=0;
        Read(l8,2);
        ReadWS(l8,result.ws);
        result.n:=byte(FCurrentScanState.Quote);
        FCurrentScanState.Quote:=true;
      end;
    DT_OP_WSTRING8B:
      begin
        result.opg:=DT_OPG_WSTRING;
        l8:=0;
        Read(l8,8);
        ReadWS(l8,result.ws);
        result.n:=byte(FCurrentScanState.Quote);
        FCurrentScanState.Quote:=true;
      end;
    DT_OP_ASTRING1B:
      begin
        result.opg:=DT_OPG_ASTRING;
        l8:=0;
        Read(l8,1);
        fl:=ReadS(l8,_as);
        if decode_data then begin
          if fl or astr_as_raw then begin
            result.n:=byte(FCurrentScanState.Quote);
            result.s:=_as;
          end else begin
            result.StrAsHex:=true;
            SetLength(result.s,length(_as)*2);
            for l4 := 1 to length(_as) do begin
              result.s[l4*2-1]:=HexChars[byte(_as[l4])shr 4 ];
              result.s[l4*2  ]:=HexChars[byte(_as[l4])and $F];
            end;
          end;
        end;
        FCurrentScanState.Quote:=true;
      end;
    DT_OP_ASTRING2B:
      begin
        result.opg:=DT_OPG_ASTRING;
        l8:=0;
        Read(l8,2);
        fl:=ReadS(l8,_as);
        if decode_data then begin
          if fl or astr_as_raw then begin
            result.n:=byte(FCurrentScanState.Quote);
            result.s:=_as;
          end else begin
            result.StrAsHex:=true;
            SetLength(result.s,length(_as)*2);
            for l4 := 1 to length(_as) do begin
              result.s[l4*2-1]:=HexChars[byte(_as[l4])shr 4 ];
              result.s[l4*2  ]:=HexChars[byte(_as[l4])and $F];
            end;
          end;
        end;
        FCurrentScanState.Quote:=true;
      end;
    DT_OP_ASTRING8B:
      begin
        result.opg:=DT_OPG_ASTRING;
        l8:=0;
        Read(l8,8);
        fl:=ReadS(l8,_as);
        if decode_data then begin
          if fl or astr_as_raw then begin
            result.n:=byte(FCurrentScanState.Quote);
            result.s:=_as;
          end else begin
            result.StrAsHex:=true;
            SetLength(result.s,length(_as)*2);
            for l4 := 1 to length(_as) do begin
              result.s[l4*2-1]:=HexChars[byte(_as[l4])shr 4 ];
              result.s[l4*2  ]:=HexChars[byte(_as[l4])and $F];
            end;
          end;
        end;
        FCurrentScanState.Quote:=true;
      end;
    DT_OP_NOP:
      result.opg:=DT_OPG_NOP;
    DT_OP_EOF:
      begin
        result.opg:=DT_OPG_EOF;
        result.n:=FCurrentScanState.Level;
      end;
  else
    raise Exception.Create('Unknown OP: '+IntToStr(byte(result.op)));
  end;
end;

procedure T1CdtObject.ReadGuid(out guid:TGuid);
begin
  if fin.Read(guid,SizeOf(guid))<SizeOf(guid) then
    FEof:=true;
end;

function T1CdtObject.ReadGuid:TGuid;
begin
  ReadGuid(result);
end;

function T1CdtObject.Read(var buf;count:int64):int64;
begin
  result:=fin.Read(buf,count);
  if result=0 then FEof:=true;
end;

function T1CdtObject.GetPosition():int64;
begin
  result:=fin.Position;
end;

procedure T1CdtObject.InitScanState;
begin
  FCurrentScanState.Level:=0;
  FCurrentScanState.Quote:=true;
end;

function T1CdtObject._GetFirstOwnStream:TStream;
begin
  result:=own_streams[0];
end;

{$IFDEF DefineBufferedFileStream}
{ TBufferedFileStream }

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  BufferSize: Integer);
begin
{$IF Defined(MSWINDOWS)}
  Create(AFilename, Mode, 0, BufferSize);
{$ELSEIF Defined(POSIX)}
  Create(AFilename, Mode, FileAccessRights, BufferSize);
{$IFEND POSIX}
end;

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
  inherited Create(AFileName, Mode, Rights);
  FBufferSize := BufferSize;
  GetMem(FBuffer, FBufferSize);
  FBuffered := True;
  SyncBuffer(True);
end;

destructor TBufferedFileStream.Destroy;
begin
  SyncBuffer(False);
  FreeMem(FBuffer, FBufferSize);
  inherited Destroy;
end;

procedure TBufferedFileStream.SyncBuffer(ReRead: boolean);
var
  LLen: Longint;
begin
  if FModified then
  begin
    if inherited Seek(FBufStartPos, soBeginning) <> FBufStartPos then
      raise EWriteError.Create('Write error');
    LLen := Longint(FBufEndPos - FBufStartPos);
    if inherited Write(FBuffer^, LLen) <> LLen then
      raise EWriteError.Create('Write error');
    FModified := False;
  end;
  if ReRead then
  begin
    FBufStartPos := inherited Seek(FFilePos, soBeginning);
    FBufEndPos := FBufStartPos + inherited Read(FBuffer^, FBufferSize);
  end
  else
  begin
    inherited Seek(FFilePos, soBeginning);
    FBufEndPos := FBufStartPos;
  end;
end;

procedure TBufferedFileStream.FlushBuffer;
begin
  SyncBuffer(False);
end;

function TBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
var
  PSrc: PByte;
begin
  if Count >= FBufferSize then
  begin
    SyncBuffer(False);
    Result := inherited Read(Buffer, Count)
  end
  else
  begin
    if (FBufStartPos > FFilePos) or (FFilePos + Count > FBufEndPos) then
      SyncBuffer(True);
    if Count < FBufEndPos - FFilePos then
      Result := Count
    else
      Result := FBufEndPos - FFilePos;
    PSrc := FBuffer + (FFilePos - FBufStartPos);
{$IF DEFINED(CPUARM32)}
    Move(PSrc^, Buffer, Result);
{$ELSE}
    case Result of
      SizeOf(Byte):
        PByte(@Buffer)^ := PByte(PSrc)^;
      SizeOf(Word):
        PWord(@Buffer)^ := PWord(PSrc)^;
      SizeOf(Cardinal):
        PCardinal(@Buffer)^ := PCardinal(PSrc)^;
      SizeOf(UInt64):
        PUInt64(@Buffer)^ := PUInt64(PSrc)^;
    else
      Move(PSrc^, Buffer, Result);
    end;
{$IFEND}
  end;
  FFilePos := FFilePos + Result;
end;

function TBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
var
  PDest: PByte;
begin
  if Count >= FBufferSize then
  begin
    SyncBuffer(False);
    Result := inherited Write(Buffer, Count);
    FFilePos := FFilePos + Result;
  end
  else
  begin
    if (FBufStartPos > FFilePos) or (FFilePos + Count > FBufStartPos + FBufferSize) then
      SyncBuffer(True);
    Result := Count;
    PDest := FBuffer + (FFilePos - FBufStartPos);
{$IF DEFINED(CPUARM32)}
    Move(Buffer, PDest^, Result);
{$ELSE}
    case Result of
      SizeOf(Byte):
        PByte(PDest)^ := PByte(@Buffer)^;
      SizeOf(Word):
        PWord(PDest)^ := PWord(@Buffer)^;
      SizeOf(Cardinal):
        PCardinal(PDest)^ := PCardinal(@Buffer)^;
      SizeOf(UInt64):
        PUInt64(PDest)^ := PUInt64(@Buffer)^;
    else
      Move(Buffer, PDest^, Result);
    end;
{$IFEND}
    FModified := True;
    FFilePos := FFilePos + Result;
    if FFilePos > FBufEndPos then
      FBufEndPos := FFilePos;
  end;
end;

function TBufferedFileStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Read(Buffer[Offset], Count);
end;

function TBufferedFileStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Write(Buffer[Offset], Count);
end;

function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if not FBuffered then
    FFilePos := inherited Seek(Offset, Origin)
  else
    case Origin of
      soBeginning:
        begin
          if (Offset < FBufStartPos) or (Offset > FBufEndPos) then
            SyncBuffer(False);
          FFilePos := Offset;
        end;
      soCurrent:
        begin
          if (FFilePos + Offset < FBufStartPos) or (FFilePos + Offset > FBufEndPos) then
            SyncBuffer(False);
          FFilePos := FFilePos + Offset;
        end;
      soEnd:
        begin
          SyncBuffer(False);
          FFilePos := inherited Seek(Offset, soEnd);
        end;
    end;
  Result := FFilePos;
end;

procedure TBufferedFileStream.SetSize(const NewSize: Int64);
begin
  if NewSize < FBufEndPos then
    SyncBuffer(False);
  FBuffered := False;
  try
    inherited SetSize(NewSize);
  finally
    FBuffered := True;
  end;
end;
{$ENDIF}

(*
function GetTableDef(name:AnsiString;ver:integer=80313):Tdt1CTable;
begin
  name:=UpperCase(name);
  if name='CONFIG' then begin
    result.name:='CONFIG';
    SetLength(Result.fields,6);
    with Result.fields[0] do begin
      name:='FILENAME';
      _type:='string';
      length:=128;
      precision:=0;
      can_null:=false;
    end;
    with Result.fields[1] do begin
      name:='CREATION';
      _type:='datetime';
      length:=0;
      precision:=0;
      can_null:=false;
    end;
    with Result.fields[2] do begin
      name:='MODIFIED';
      _type:='datetime';
      length:=0;
      precision:=0;
      can_null:=false;
    end;
    with Result.fields[3] do begin
      name:='ATTRIBUTES';
      _type:='number';
      length:=5;
      precision:=0;
      can_null:=false;
    end;
    with Result.fields[4] do begin
      name:='DATASIZE';
      _type:='number';
      length:=20;
      precision:=0;
      can_null:=false;
    end;
    with Result.fields[5] do begin
      name:='BINARYDATA';
      _type:='image';
      length:=0;
      precision:=0;
      can_null:=false;
    end;
{    if ver>=80313 then begin //!!!
      SetLength(Result.fields,7);
      with Result.fields[6] do begin
        name:='PARTNO';
        _type:='number';
        length:=10;
        precision:=0;
        can_null:=false;
      end;
    end;}
  end;

end;
*)

end.