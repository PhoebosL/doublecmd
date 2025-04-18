unit uSynDiffControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, LCLVersion,
  SynEditMiscClasses, SynGutterBase,
  SynGutter, LazSynEditText, uDiffOND
{$IF DEFINED(LCL_VER_499)}
  , LazEditTextGridPainter, LazEditTextAttributes
{$ELSE}
  , SynTextDrawer
{$ENDIF}
  ;

const
  { Default differ colors }
  clPaleGreen: TColor = $AAFFAA;
  clPaleRed  : TColor = $AAAAFF;
  clPaleBlue : TColor = $FFAAAA;

type
  TPaintStyle = (psForeground, psBackground);

{$IF NOT DEFINED(LCL_VER_499)}
type
  TLazEditTextGridPainter = TheTextDrawer;
{$ENDIF}

type

  { TDiffColors }

  TDiffColors = class(TPersistent)
  private
    fColors: array [TChangeKind] of TColor;
    fOnChange: TNotifyEvent;
    function GetColor(const AIndex: TChangeKind): TColor;
    procedure SetColor(const AIndex: TChangeKind; const AValue: TColor);
  public
    constructor Create;
    procedure Assign(aSource: TPersistent); override;
    property Colors[const aIndex: TChangeKind]: TColor read GetColor write SetColor; default;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Added: TColor index ckAdd read GetColor write SetColor;
    property Modified: TColor index ckModify read GetColor write SetColor;
    property Deleted: TColor index ckDelete read GetColor write SetColor;
  end;

  { TSynDiffGutter }

  TSynDiffGutter = class(TSynGutter)
  protected
    procedure CreateDefaultGutterParts; override;
  end;

  { TSynDiffGutterLineNumber }

  TSynDiffGutterLineNumber = class(TSynGutterPartBase)
  private
    FTextDrawer: TLazEditTextGridPainter;

    FDigitCount: integer;
    FAutoSizeDigitCount: integer;
    FLeadingZeros: boolean;

    procedure SetDigitCount(AValue : integer);
    procedure SetLeadingZeros(const AValue : boolean);
    function FormatLineNumber(Line: PtrInt; Kind: TChangeKind): string;
  protected
    procedure Init; override;
    function  PreferedWidth: Integer; override;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure BufferChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject{$IF DEFINED(LCL_VER_499)}; Changes: TSynStatusChanges{$ENDIF});
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
  published
    property MarkupInfo;
    property DigitCount: integer read FDigitCount write SetDigitCount;
    property LeadingZeros: boolean read FLeadingZeros write SetLeadingZeros;
  end;

  { TSynDiffGutterChanges }

  TSynDiffGutterChanges = class(TSynGutterPartBase)
  protected
    function  PreferedWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
  end;

  { TSynDiffEdit }

  TSynDiffEdit = class(TSynEdit)
  private
    FPaintStyle: TPaintStyle;
    FEncoding: String;
    FColors: TDiffColors;
    FOriginalFile,
    FModifiedFile: TSynDiffEdit;
  private
    procedure SetModifiedFile(const AValue: TSynDiffEdit);
    procedure SetOriginalFile(const AValue: TSynDiffEdit);
    procedure SetPaintStyle(const AValue: TPaintStyle);
  protected
    function CreateGutter(AOwner: TSynEditBase; ASide: TSynGutterSide;
                          ATextDrawer: TLazEditTextGridPainter): TSynGutter; override;
    procedure SpecialLineMarkupEvent(Sender: TObject; Line: Integer;
                                     var Special: boolean; AMarkup: TSynSelectedColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Renumber;
    procedure StartCompare;
    procedure FinishCompare;
    function DiffBegin(ALine: Integer): Integer;
    function DiffEnd(ALine: Integer): Integer;
    property PaintStyle: TPaintStyle read FPaintStyle write SetPaintStyle;
    property Encoding: String read FEncoding write FEncoding;
    property Colors: TDiffColors read FColors write FColors;
    property OriginalFile: TSynDiffEdit read FOriginalFile write SetOriginalFile;
    property ModifiedFile: TSynDiffEdit read FModifiedFile write SetModifiedFile;
  published
    property OnStatusChange;
  end;

  { TStringsHelper }

  TStringsHelper = class helper for TStrings
  private
    function GetKind(AIndex: Integer): TChangeKind;
    function GetNumber(AIndex: Integer): PtrInt;
    procedure SetKind(AIndex: Integer; AValue: TChangeKind);
    procedure SetNumber(AIndex: Integer; AValue: PtrInt);
  public
    procedure Renumber;
    procedure RemoveFake;
    procedure Append(const S: String; AKind: TChangeKind);
    procedure InsertFake(AIndex: Integer; AKind: TChangeKind);
    procedure SetKindAndNumber(AIndex: Integer; AKind: TChangeKind; ANumber: PtrInt);
  public
    property Kind[AIndex: Integer]: TChangeKind read GetKind write SetKind;
    property Number[AIndex: Integer]: PtrInt read GetNumber write SetNumber;
  end;

implementation

uses
  LCLIntf, LCLType, SynEditMiscProcs, SynEditTypes;

const
  KindShift = 8;   // Line kind shift
  KindMask  = $FF; // Line kind mask
  FakeLine  = PtrInt(High(PtrUInt) shr KindShift);

{ TDiffColors }

function TDiffColors.GetColor(const AIndex: TChangeKind): TColor;
begin
  Result:= fColors[AIndex];
end;

procedure TDiffColors.SetColor(const AIndex: TChangeKind; const AValue: TColor);
begin
  if fColors[AIndex] <> AValue then
  begin
    fColors[AIndex] := AValue;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

constructor TDiffColors.Create;
begin
  fColors[ckAdd] := clPaleGreen;
  fColors[ckModify] := clPaleBlue;
  fColors[ckDelete] := clPaleRed;
end;

procedure TDiffColors.Assign(aSource: TPersistent);
begin
  if (aSource is TDiffColors) then
  with (aSource as TDiffColors) do
  begin
    fColors[ckAdd]:= Added;
    fColors[ckModify]:= Modified;
    fColors[ckDelete]:= Deleted;
  end;
end;

{ TSynDiffGutter }

procedure TSynDiffGutter.CreateDefaultGutterParts;
begin
  if Side <> gsLeft then Exit;

  with TSynDiffGutterLineNumber.Create(Parts) do
  Name:= 'SynDiffGutterLineNumber';
  with TSynDiffGutterChanges.Create(Parts) do
  Name:= 'SynDiffGutterChanges';
end;

{ TSynDiffEdit }

procedure TSynDiffEdit.SetModifiedFile(const AValue: TSynDiffEdit);
begin
  if FModifiedFile <> AValue then
  begin
    if (AValue <> nil) and (FOriginalFile <> nil) then
      raise Exception.Create('Having both ModifiedFile and OriginalFile is not supported');
    FModifiedFile := AValue;
  end;
end;

procedure TSynDiffEdit.SetOriginalFile(const AValue: TSynDiffEdit);
begin
  if FOriginalFile <> AValue then
  begin
    if (AValue <> nil) and (FModifiedFile <> nil) then
      raise Exception.Create('Having both OriginalFile and ModifiedFile is not supported');
    FOriginalFile := AValue;
  end;
end;

procedure TSynDiffEdit.SetPaintStyle(const AValue: TPaintStyle);
begin
  if FPaintStyle <> AValue then
  begin
    FPaintStyle := AValue;
    Invalidate;
  end;
end;

function TSynDiffEdit.CreateGutter(AOwner: TSynEditBase; ASide: TSynGutterSide;
  ATextDrawer: TLazEditTextGridPainter): TSynGutter;
begin
  Result := TSynDiffGutter.Create(AOwner, ASide, ATextDrawer);
end;

procedure TSynDiffEdit.SpecialLineMarkupEvent(Sender: TObject; Line: Integer;
  var Special: boolean; AMarkup: TSynSelectedColor);
var
  Kind: TChangeKind;
  LineColor: TColor;
begin
  if Line > Lines.Count then Exit;

  Kind:= Lines.Kind[Line - 1];

  if (Kind <> ckNone) then
  with AMarkup do
  begin
    case Kind of
      ckDelete: LineColor := FColors.Deleted;
      ckAdd:    LineColor := FColors.Added;
      ckModify:
        if Assigned(Highlighter) then
          Exit
        else
          LineColor := FColors.Modified;
    end;
    Special:= True;
    if FPaintStyle = psForeground then
      begin
        Foreground := LineColor;
        Background := clWindow;
      end
    else
      begin
        Foreground:= clWindowText;
        Background := LineColor;
      end;
  end;
end;

procedure TSynDiffEdit.StartCompare;
begin
  BeginUpdate;
  // Remove fake lines
  Lines.RemoveFake;
end;

procedure TSynDiffEdit.FinishCompare;
begin
  EndUpdate;
  Invalidate;
end;

function TSynDiffEdit.DiffBegin(ALine: Integer): Integer;
var
  Kind: TChangeKind;
begin
  Result:= ALine;
  if ALine = 0 then Exit;
  // Skip lines with current difference type
  Kind := Lines.Kind[ALine];
  while (ALine > 0) and (Lines.Kind[ALine] = Kind) do Dec(ALine);
  Result:= ALine + 1;
end;

function TSynDiffEdit.DiffEnd(ALine: Integer): Integer;
var
  Kind: TChangeKind;
begin
  Result:= ALine;
  if ALine = Lines.Count - 1 then Exit;
  // Skip lines with current difference type
  Kind := Lines.Kind[ALine];
  while (ALine < Lines.Count - 1) and (Lines.Kind[ALine] = Kind) do Inc(ALine);
  Result:= ALine - 1;
end;

constructor TSynDiffEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color:= clWindow;
  Font.Color:= clWindowText;
  FPaintStyle:= psBackground;
  FColors:= TDiffColors.Create;
  OnSpecialLineMarkup:= @SpecialLineMarkupEvent;
end;

destructor TSynDiffEdit.Destroy;
begin
  FreeAndNil(FColors);
  inherited Destroy;
end;

procedure TSynDiffEdit.Renumber;
begin
  Lines.Renumber;
  Repaint;
end;

{ TSynDiffGutterChanges }

function TSynDiffGutterChanges.PreferedWidth: Integer;
begin
  Result := 4;
end;

constructor TSynDiffGutterChanges.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MarkupInfo.Background := clNone;
end;

procedure TSynDiffGutterChanges.Paint(Canvas: TCanvas; AClip: TRect; FirstLine,
  LastLine: Integer);
var
  rcLine: TRect;
  LineCount: Integer;
  LineHeight: Integer;
  I, LineNumber: Integer;
  SynDiffEdit: TSynDiffEdit;
  LineTop: Integer;
  AliasMode: TAntialiasingMode;
begin
  if not Visible then Exit;

  SynDiffEdit:= TSynDiffEdit(SynEdit);
  LineHeight:= SynDiffEdit.LineHeight;
  LineCount:= SynDiffEdit.Lines.Count;
  LineTop:= ToIdx(GutterArea.TextArea.TopLine);

  if MarkupInfo.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfo.Background;
    Canvas.FillRect(AClip);
  end;

  Canvas.Pen.Width := Width;
  Canvas.Pen.EndCap:= pecFlat;
  AliasMode:= Canvas.AntialiasingMode;
  Canvas.AntialiasingMode:= amOff;

  rcLine := AClip;
  rcLine.Left := rcLine.Left + Width div 2;
  rcLine.Bottom := AClip.Top;
  for I := LineTop + FirstLine to LineTop + LastLine do
  begin
    LineNumber := ViewedTextBuffer.DisplayView.ViewToTextIndex(I);
    // next line rect
    rcLine.Top := rcLine.Bottom;
    Inc(rcLine.Bottom, LineHeight);
    if (LineNumber >= 0) and (LineNumber < LineCount) then
    begin
      case SynDiffEdit.Lines.Kind[LineNumber] of
        ckNone:
            Continue;
        ckAdd:
            Canvas.Pen.Color := SynDiffEdit.FColors.Added;
        ckDelete:
            Canvas.Pen.Color := SynDiffEdit.FColors.Deleted;
        ckModify:
            Canvas.Pen.Color := SynDiffEdit.FColors.Modified;
      end;
      Canvas.Line(rcLine.Left, rcLine.Top + 1, rcLine.Left, rcLine.Bottom - 1);
    end;
  end;
  Canvas.AntialiasingMode := AliasMode;
end;

{ TSynDiffGutterLineNumber }

procedure TSynDiffGutterLineNumber.SetDigitCount(AValue: integer);
begin
  AValue := MinMax(AValue, 2, 12);
  if FDigitCount <> AValue then
  begin
    FDigitCount := AValue;
    if AutoSize then begin
      FAutoSizeDigitCount := Max(FDigitCount, FAutoSizeDigitCount);
      DoAutoSize;
    end else
      FAutoSizeDigitCount := FDigitCount;
    DoChange(Self);
  end;
end;

procedure TSynDiffGutterLineNumber.SetLeadingZeros(const AValue: boolean);
begin
  if FLeadingZeros <> AValue then
  begin
    FLeadingZeros := AValue;
    DoChange(Self);
  end;
end;

function TSynDiffGutterLineNumber.FormatLineNumber(Line: PtrInt;
  Kind: TChangeKind): string;
var
  I: Integer;
begin
  Result := EmptyStr;
  // if a symbol must be showed
  if (Line = 0) or (Line = FakeLine) then
    begin
      case Kind of
      ckAdd:
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '+';
      ckDelete:
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '-';
      else
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '.';
      end;
    end
  // else format the line number
  else
  begin
    Str(Line : FAutoSizeDigitCount, Result);
    if FLeadingZeros then
      for I := 1 to FAutoSizeDigitCount - 1 do
      begin
        if (Result[I] <> ' ') then Break;
        Result[I] := '0';
      end;
  end;
end;

function TSynDiffGutterLineNumber.PreferedWidth: Integer;
begin
  Result := FAutoSizeDigitCount * FTextDrawer.CharWidth + 1;
end;

procedure TSynDiffGutterLineNumber.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
var
  nDigits: Integer;
begin
  if not (Visible and AutoSize) then Exit;

  nDigits := Max(Length(IntToStr(TextBuffer.Count)), FDigitCount);
  if FAutoSizeDigitCount <> nDigits then begin
    FAutoSizeDigitCount := nDigits;
    DoAutoSize;
  end;
end;

procedure TSynDiffGutterLineNumber.BufferChanged(Sender: TObject);
begin
  LineCountChanged(nil, 0, 0);
end;

procedure TSynDiffGutterLineNumber.FontChanged(Sender: TObject{$IF DEFINED(LCL_VER_499)}; Changes: TSynStatusChanges{$ENDIF});
begin
  DoAutoSize;
end;

procedure TSynDiffGutterLineNumber.Init;
begin
  inherited Init;
  FTextDrawer := Gutter.TextDrawer;
  ViewedTextBuffer.AddChangeHandler(senrLineCount, @LineCountChanged);
  ViewedTextBuffer.AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
{$IF DEFINED(LCL_VER_499)}
  FriendEdit.RegisterStatusChangedHandler(@FontChanged, [scFontOrStyleChanged]);
{$ELSE}
  FTextDrawer.RegisterOnFontChangeHandler(@FontChanged);
{$ENDIF}
  LineCountchanged(nil, 0, 0);
end;

constructor TSynDiffGutterLineNumber.Create(AOwner: TComponent);
begin
  FDigitCount := 4;
  FAutoSizeDigitCount := FDigitCount;
  FLeadingZeros := False;
  inherited Create(AOwner);
end;

destructor TSynDiffGutterLineNumber.Destroy;
begin
{$IF DEFINED(LCL_VER_499)}
  ViewedTextBuffer.RemoveHandlers(Self);
  FriendEdit.UnRegisterStatusChangedHandler(@FontChanged);
{$ELSE}
  ViewedTextBuffer.RemoveHanlders(Self);
  FTextDrawer.UnRegisterOnFontChangeHandler(@FontChanged);
{$ENDIF}
  inherited Destroy;
end;

procedure TSynDiffGutterLineNumber.Assign(Source: TPersistent);
var
  Src: TSynDiffGutterLineNumber;
begin
  if Assigned(Source) and (Source is TSynDiffGutterLineNumber) then
  begin
    Src := TSynDiffGutterLineNumber(Source);
    FLeadingZeros := Src.FLeadingZeros;
    FDigitCount := Src.FDigitCount;
    FAutoSizeDigitCount := Src.FAutoSizeDigitCount;
  end;
  inherited Assign(Source);
end;

procedure TSynDiffGutterLineNumber.Paint(Canvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: Integer);
var
  S: String;
  rcLine: TRect;
  LineNumber: PtrInt;
  LineKind: TChangeKind;
  I, LineHeight: Integer;
  SynDiffEdit: TSynDiffEdit;
  LineCount: Integer;
  IRange: TLineRange;
  LineTop: TLinePos;
begin
  if not Visible then Exit;

  SynDiffEdit:= TSynDiffEdit(SynEdit);
  LineHeight:= SynDiffEdit.LineHeight;
  LineCount:= SynDiffEdit.Lines.Count;
  LineTop:= ToIdx(GutterArea.TextArea.TopLine);
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
{$IF DEFINED(LCL_VER_499)}
  FTextDrawer.BeginCustomCanvas(Canvas);
  try
    fTextDrawer.SetFrame(MarkupInfoInternal.FrameColor, slsSolid);
{$ELSE}
  FTextDrawer.BeginDrawing(Canvas.Handle);
  try
    fTextDrawer.SetFrameColor(MarkupInfo.FrameColor);
{$ENDIF}
    if MarkupInfo.Background <> clNone then
      FTextDrawer.BackColor := MarkupInfo.Background
    else begin
      FTextDrawer.BackColor := Gutter.Color;
    end;
    if MarkupInfo.Foreground <> clNone then
      fTextDrawer.ForeColor := MarkupInfo.Foreground
    else begin
      fTextDrawer.ForeColor := SynDiffEdit.Font.Color;
    end;
    fTextDrawer.Style := MarkupInfo.Style;
    // prepare the rect initially
    rcLine := AClip;
    rcLine.Bottom := AClip.Top;
    for I := LineTop + FirstLine to LineTop + LastLine do
    begin
      LineNumber := ToPos(ViewedTextBuffer.DisplayView.ViewToTextIndexEx(I, IRange));
      if (LineNumber < 1) or (LineNumber > LineCount) then Break;
      LineKind := SynDiffEdit.Lines.Kind[LineNumber - 1];
      LineNumber:= SynDiffEdit.Lines.Number[LineNumber - 1];
      // next line rect
      rcLine.Top := rcLine.Bottom;
      // Get the formatted line number or dot
      S := FormatLineNumber(LineNumber, LineKind);
      Inc(rcLine.Bottom, LineHeight);
      if I <> IRange.Top then S := '';
      // erase the background and draw the line number string in one go
      fTextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

    // now erase the remaining area if any
    if AClip.Bottom > rcLine.Bottom then
    begin
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := AClip.Bottom;
      with rcLine do
        fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
    end;
  finally
{$IF DEFINED(LCL_VER_499)}
    fTextDrawer.EndCustomCanvas;
{$ELSE}
    fTextDrawer.EndDrawing;
{$ENDIF}
  end;
end;

{ TStringsHelper }

function TStringsHelper.GetKind(AIndex: Integer): TChangeKind;
var
  AKind: PtrInt;
begin
  AKind:= PtrInt(Objects[AIndex]);
  Result:= TChangeKind(AKind and KindMask);
end;

function TStringsHelper.GetNumber(AIndex: Integer): PtrInt;
begin
  Result:= PtrInt(Objects[AIndex]) shr KindShift;
end;

procedure TStringsHelper.SetKind(AIndex: Integer; AValue: TChangeKind);
var
  ANumber: PtrInt;
begin
  ANumber:= GetNumber(AIndex);
  Objects[AIndex]:= TObject(PtrInt(AValue) or (ANumber shl KindShift));
end;

procedure TStringsHelper.SetNumber(AIndex: Integer; AValue: PtrInt);
var
  AKind: TChangeKind;
begin
  AKind:= GetKind(AIndex);
  Objects[AIndex]:= TObject(PtrInt(AKind) or (AValue shl KindShift));
end;

procedure TStringsHelper.RemoveFake;
var
  I: Integer;
begin
  for I:= Count - 1 downto 0 do
  begin
    if ((PtrInt(Objects[I]) shr KindShift) = FakeLine) and (Self[I] = EmptyStr) then
      Delete(I);
  end;
end;

procedure TStringsHelper.Renumber;
var
  I, N: Integer;
begin
  N:= 1;
  for I:= 0 to Count - 1 do
  begin
    if ((PtrInt(Objects[I]) shr KindShift) <> FakeLine) then
    begin
      Number[I] := N;
      Inc(N);
    end;
  end;
end;

procedure TStringsHelper.Append(const S: String; AKind: TChangeKind);
begin
  InsertObject(Count, S, TObject(PtrInt(AKind) or (Count shl KindShift)));
end;

procedure TStringsHelper.InsertFake(AIndex: Integer; AKind: TChangeKind);
begin
  InsertObject(AIndex, EmptyStr, TObject(PtrInt(AKind) or PtrInt(FakeLine shl KindShift)));
end;

procedure TStringsHelper.SetKindAndNumber(AIndex: Integer; AKind: TChangeKind;
  ANumber: PtrInt);
begin
  Objects[AIndex]:= TObject(PtrInt(AKind) or (ANumber shl KindShift));
end;

end.

