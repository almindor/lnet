program httptest;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  lNet, lHTTP;
  
const
  MAX_COUNT = 10000;

type

  { TTest }

  TTest = class
   protected
    FHTTP: TLHTTPClient;
    FBuffer: string;
    FExpected: string;
    FQuit: Boolean;
    FCount: Integer;
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    function OnInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: integer): integer;
    procedure OnDoneInput(aSocket: TLHTTPClientSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Get;
  end;

{ TTest }

procedure TTest.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
  FQuit := True;
end;

procedure TTest.OnDs(aSocket: TLSocket);
begin
  Writeln('Lost connection');
  FQuit := True;
end;

function TTest.OnInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
  ASize: integer): integer;
begin
  FBuffer := FBuffer + aBuffer;
  Result := aSize;
end;

procedure TTest.OnDoneInput(aSocket: TLHTTPClientSocket);
begin
  if FBuffer <> FExpected then
    Writeln('ERROR, doesn''t match!')
  else
    Write('.');
    
  Inc(FCount);

  if FCount > MAX_COUNT then begin
    FQuit := True;
    Writeln('DONE');
  end else begin
    FBuffer := '';
    FHTTP.SendRequest;
  end;
end;

constructor TTest.Create;
var
  f: TFileStream;
begin
  FHTTP := TLHTTPClient.Create(nil);
  FHTTP.OnError := @OnEr;
  FHTTP.OnInput := @OnInput;
  FHTTP.OnDisconnect := @OnDs;
  FHTTP.OnDoneInput := @OnDoneInput;
  
  FHTTP.Timeout := 1000;
  
  FHTTP.Host := 'members.chello.sk';
  FHTTP.URI := '/ales/index.html';
  
  f := TFileStream.Create('expected.txt', fmOpenRead);
  SetLength(FExpected, f.Size);
  f.Read(FExpected[1], f.Size);
  f.Free;
end;

destructor TTest.Destroy;
begin
  FHTTP.Free;

  inherited Destroy;
end;

procedure TTest.Get;
begin
  FHTTP.SendRequest;

  repeat
    FHTTP.CallAction;
  until FQuit or not FHTTP.Connected;
end;

var
  t: TTest;
begin
  t := TTest.Create;
  
  t.Get;
  
  t.Free;
end.

