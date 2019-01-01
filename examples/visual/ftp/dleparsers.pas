unit dleparsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type
  TDirEntryParser=class;
  TSeparatorsKind = (skRest, skSpace);


  { TDirParser }

  TDirParser=class
  private
    FList: TList;
    FPrefParser: TDirEntryParser;
    procedure AddParser(Parser:TDirEntryParser);
  public
    constructor create;
    destructor Destroy; override;
    function Parse(Entry: PChar): TDirEntryParser;
    property PrefParser: TDirEntryParser read FPrefParser write FPrefParser;
  end;
  
  { TDirEntryParser }

  TDirEntryParser=class
  protected
    Start: PChar;
    FAttributes: string;
    FDate: TDateTime;
    FIsDir: boolean;
    FIsLink: boolean;
    FLinkName: string;
    FName: string;
    FSize: Int64;
    function GetString(Separators: TSeparatorsKind): string;
    procedure SkipBlanks;
  public
    constructor Create(owner: TDirParser);
    function NextEntry: boolean; virtual;
    function Parse(Entry: PChar):boolean; virtual; abstract;
    function Description: string; virtual; abstract;
    property IsDir: boolean read FIsDir;
    property IsLink: boolean read FIsLink;
    property EntryName: string read FName;
    property LinkName: string read FLinkName;
    property Attributes: string read FAttributes;
    property EntrySize: Int64 read FSize;
    property Date: TDateTime read FDate;
  end;

  { TUnixDirEntryParser }

  TUnixDirEntryParser=class(TDirEntryParser)
  public
    function Parse(Entry: PChar): boolean; override;
    function Description: string; override;
  end;

  { TDosStyleEntryParser }
  TDosStyleEntryParser=class(TDirEntryParser)
  public
    function Parse(Entry: PChar): boolean; override;
    function Description: string; override;
  end;
  
var
  DirParser: TDirParser;

implementation

{ TDirParser }

procedure TDirParser.AddParser(Parser: TDirEntryParser);
begin
  FList.Add(Parser);
end;

constructor TDirParser.create;
begin
  FList := TList.Create;
end;

destructor TDirParser.Destroy;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    if assigned(FList[i]) then
      TDirEntryParser(FList[i]).Free;
  FList.Free;
  inherited Destroy;
end;

function TDirParser.Parse(Entry: Pchar): TDirEntryParser;
var
  Parser: TDirEntryParser;
  i: Integer;
begin
  // try first our prefered parser
  if Assigned(FPrefParser) then
    if FPrefParser.Parse(Entry) then begin
      Result := FPrefParser;
      exit;
    end;
    
  Result := nil;
  for i:=0 to FList.Count-1 do begin
    Parser := TDirEntryParser(FList[i]);
    if Parser<>nil then
      if Parser.Parse(Entry) then begin
        Result := Parser;
        break;
      end;
  end;

end;

{ TDirEntryParser }

function TDirEntryParser.GetString(Separators: TSeparatorsKind): string;
var
  Lead: Pchar;
begin
  SkipBlanks;

  // end of string?
  if Start^=#0 then begin
    result := '';
    exit;
  end;

  Lead := Start;
  // find next blank
  while not (Lead^ in [#0, #13, #10])
      and ((Separators = skRest) or (Lead^ <> ' ')) do
    Inc(Lead);

  // copy string
  SetLength(Result, Lead-Start);
  Move(Start^,Result[1], Lead-Start);

  Start := Lead;
end;

function TDirEntryParser.NextEntry: boolean;
var
  P: PChar;
begin
  P := Start;
  while not (P^ in [#0, #13]) do
    Inc(P);
  if P^ = #0 then
    exit(false);
  Inc(P);
  if P^ = #10 then
    Inc(P);
  Result := Parse(P);
end;

procedure TDirEntryParser.SkipBlanks;
var
  P: PChar;
begin
  P := Start;
  while P^=' ' do
    Inc(P);
  Start := P;
end;

constructor TDirEntryParser.Create(owner: TDirParser);
begin
  Owner.AddParser(Self);
end;

{ TUnixDirEntryParser }

function TUnixDirEntryParser.Parse(Entry: PChar): boolean;
  function MonthToWord(aMonth: string): word;
  const
   MonthNames: array[1..12] of string[4] =
     ('Jan','Feb','Mar','Apr','May','Jun',
      'Jul','Aug','Sep','Oct','Nov','Dec');
  var
    i: Word;
  begin
    result:=1;
    for i:=1 to 12 do begin
      if CompareText(aMonth, MonthNames[i])=0 then begin
        result := i;
        break;
      end;
    end;
  end;
var
  aDay,aMonth,aYear: word;
  TimeOrYear, S: string;
begin
  Start := Entry;
  if Start^ in ['d','-','l'] then begin
    FIsDir := (Start^='d');
    FIsLink:= (Start^='l');
    FAttributes := GetString(skSpace); // attributes
    GetString(skSpace); // #
    GetString(skSpace); // user
    GetString(skSpace); // group
    FSize := StrToInt64Def(GetString(skSpace), -1); //size

    //
    aMonth := MonthToWord(GetString(skSpace));
    aDay   := StrToIntDef(GetString(skSpace), 1);
    TimeOrYear:=GetString(skSpace); // time or date
    if pos(':',TimeOrYear)=0 then begin
      aYear := StrToIntDef(TimeOrYear, 1970);
      TimeOrYear:='00:00';
    end else
      aYear := CurrentYear;
    FDate := ComposeDateTime(EncodeDate(aYear,aMonth,aDay),
                             StrToTime(TimeOrYear));

    if FIsLink then begin
      FName := '';
      repeat
        S := GetString(skSpace);
        if S = '->' then break;
        if Length(FName) = 0 then
          FName := S
        else
          FName := FName + ' ' + S;
      until false;
      FLinkName := GetString(skRest);
    end else
      FName := GetString(skRest);
    result :=(FAttributes<>'')and(FSize>=0)and(FName<>'');
  end else
    result := False;
end;

function TUnixDirEntryParser.Description: string;
begin
  Result:='Unix Dir listing entry parser';
end;


{ TDosStyleEntryParser }

function TDosStyleEntryParser.Parse(Entry: PChar): boolean;
var
  OldDateFormat: string;
  TempStr:string;
  tmpDate,tmpTime:TDateTime;
begin
  Result:= False;
  Start := Entry;
  if Start^ in ['0'..'9'] then begin
    try
      OldDateFormat:=ShortDateFormat;
      ShortDateFormat:='mm-dd-yy';
      if TryStrToDate(GetString(skSpace), tmpDate) then begin
        TempStr:=GetString(skSpace);
        Insert(' ', TempStr, 6);
        if TryStrToTime(TempStr, tmpTime) then begin
          FDate:=ComposeDateTime(tmpDate,tmpTime);
          TempStr:=GetString(skSpace);
          FIsDir:=(TempStr<>'') and (TempStr[1]='<');
          FIsLink:=False;
          if FIsDir then
            FSize:=0
          else
            FSize:=StrToInt64Def(TempStr, -1);
          if FSize>=0 then
            FName:=GetString(skRest)
          else
            FName:='';
          Result :=(FSize>=0)and(FName<>'');
        end else begin
          WriteLn('Failed to parse time: ',TempStr);
        end;
      end;
      if not result then
        WriteLn('  Failed to parse: ',Entry);
    finally
      ShortDateFormat:=OldDateFormat;
    end;
  end;
end;

function TDosStyleEntryParser.Description: string;
begin
  Result:='DOS style listing entry parser';
end;

initialization
  DirParser := TDirParser.Create;
  TUnixDirEntryParser.Create(DirParser);
  TDosStyleEntryParser.Create(DirParser);
end.

