program fpget;

{$mode objfpc}
{$h+}

uses
  sysutils, strutils, lnet, lhttp, lHTTPUtil, lnetSSL, URIParser;

var
  HttpClient: TLHTTPClient;
  OutputFile: file;
  Done: boolean;

type
  THTTPHandler = class
  public
    procedure ClientDisconnect(ASocket: TLSocket);
    procedure ClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure ClientError(const Msg: string; aSocket: TLSocket);
    function ClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; 
      ASize: Integer): Integer;
    procedure ClientProcessHeaders(ASocket: TLHTTPClientSocket);
  end;

procedure THTTPHandler.ClientError(const Msg: string; aSocket: TLSocket);
begin
  writeln('Error: ', Msg);
end;

procedure THTTPHandler.ClientDisconnect(ASocket: TLSocket);
begin
  writeln('Disconnected.');
  done := true;
end;
  
procedure THTTPHandler.ClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  writeln('done.');
  close(OutputFile);
  ASocket.Disconnect;
end;

function THTTPHandler.ClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: Integer): Integer;
begin
  blockwrite(outputfile, ABuffer^, ASize, Result);
  write(IntToStr(ASize) + '...');
  Result := ASize;
end;

procedure THTTPHandler.ClientProcessHeaders(ASocket: TLHTTPClientSocket);
begin
  write('Response: ', HTTPStatusCodes[ASocket.ResponseStatus], ' ', 
    ASocket.ResponseReason, ', data...');
end;

var
  URL, Host, URI, FileName, AltFileName: string;
  Port: Word;
  dummy: THTTPHandler;
  index: Integer;
  UseSSL: Boolean;
  SSLSession: TLSSLSession;
begin
  if ParamCount = 0 then
  begin
    writeln('Specify URL (and optionally, filename).');
    exit;
  end;

  { parse URL }
  URL := ParamStr(1);
  
  UseSSL := DecomposeURL(URL, Host, URI, Port);
  Writeln('Host: ', Host, ' URI: ', URI, ' Port: ', Port);

  if ParamCount >= 2 then
    FileName := ParamStr(2)
  else begin
    index := RPos('/', URI);
    if index > 0 then
      FileName := Copy(URI, index+1, Length(URI)-index);
    if Length(FileName) = 0 then
      FileName := 'index.html';
  end;

  if FileExists(FileName) then
  begin
    index := 1;
    repeat
      AltFileName := FileName + '.' + IntToStr(index);
      inc(index);
    until not FileExists(AltFileName);
    writeln('"', FileName, '" exists, writing to "', AltFileName, '"');
    FileName := AltFileName;
  end;

  assign(OutputFile, FileName);
  rewrite(OutputFile, 1);

  HttpClient := TLHTTPClient.Create(nil);

  SSLSession := TLSSLSession.Create(HttpClient);
  SSLSession.SSLActive := UseSSL;

  HttpClient.Session := SSLSession;
  HttpClient.Host := Host;
  HttpClient.Method := hmGet;
  HttpClient.Port := Port;
  HttpClient.URI := URI;
  HttpClient.Timeout := -1;
  HttpClient.OnDisconnect := @dummy.ClientDisconnect;
  HttpClient.OnDoneInput := @dummy.ClientDoneInput;
  HttpClient.OnError := @dummy.ClientError;
  HttpClient.OnInput := @dummy.ClientInput;
  HttpClient.OnProcessHeaders := @dummy.ClientProcessHeaders;
  HttpClient.SendRequest;
  Done := false;

  while not Done do
    HttpClient.CallAction;
  HttpClient.Free;
end.
