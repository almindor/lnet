unit uLinkTCP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt,
  lNet, lEvents;

type

  { TLink }

  TLink = class
  public
    ServerSocket: TLSocket;
    ServerBuffer: string;
    ClientTCP: TLTcp;
    ClientBuffer: string;
    Number: Integer;

    constructor Create(aCount: Integer);
  end;

  { TLinkServer }

  TLinkServer = class(TComponent)
  private
    FHost: string;
    FPort: Word;
    FServer: TLTcp;
    FCount: Integer;
    FEventer: TLEventer;
    FQuit: Boolean;
    // server callbacks
    procedure OnEr(const aMsg: string; aSocket: TLSocket);
    procedure OnAc(aSocket: TLSocket);
    procedure OnCs(aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
    // client callbacks
    procedure OnCliEr(const aMsg: string; aSocket: TLSocket);
    procedure OnCliCo(aSocket: TLSocket);
    procedure OnCliCs(aSocket: TLSocket);
    procedure OnCliRe(aSocket: TLSocket);
    procedure OnCliDs(aSocket: TLSocket);
    // helpers
    procedure ConnectToTarget(aLink: TLink);
    procedure SendToTarget(aLink: TLink);
    procedure SendToClient(aLink: TLink);
  public
    constructor Create(const aHost: string; const aServPort, aCliPort: Word);
    destructor Destroy; override;
    procedure Run;
  end;

implementation

constructor TLink.Create(aCount: Integer);
begin
  Number := aCount;
end;

{ TLinkServer }

procedure TLinkServer.OnEr(const aMsg: string; aSocket: TLSocket);
var
  l: TLink;
  c: Integer = -1;
begin
  l := TLink(aSocket.UserData);
  if Assigned(l) then begin
    c := l.Number;
    Writeln('Listener error: ', aMsg, ' on connection: ', c);
    l.ClientTCP.Free;
    l.Free;
    aSocket.UserData := nil;
    Exit;
  end;
  Writeln('Listener error: ', aMsg);
  FQuit := True;
end;

procedure TLinkServer.OnAc(aSocket: TLSocket);
var
  l: TLink;
begin
  Inc(FCount);
  l := TLink.Create(FCount);
  l.ServerSocket := aSocket;
  aSocket.UserData := l;

  ConnectToTarget(l);
  Writeln('Listener accepted connection: ', l.Number, ' from: ', aSocket.PeerAddress);
end;

procedure TLinkServer.OnCs(aSocket: TLSocket);
var
  l: TLink;
begin
  l := TLink(aSocket.UserData);
  SendToTarget(l);
end;

procedure TLinkServer.OnRe(aSocket: TLSocket);
var
  l: TLink;
  s: string;
begin
  l := TLink(aSocket.UserData);
  if FServer.GetMessage(s) > 0 then begin
    l.ServerBuffer := l.ServerBuffer + s;
    SendToTarget(l);
  end;
end;

procedure TLinkServer.OnDs(aSocket: TLSocket);
var
  l: TLink;
begin
  l := TLink(aSocket.UserData);
  Writeln('Listener disconnect on connection: ', l.Number);
  l.ClientTCP.Free; // TODO: this causes a hard discon, handle softly later
  l.Free;
  aSocket.UserData := nil;
end;

procedure TLinkServer.OnCliEr(const aMsg: string; aSocket: TLSocket);
var
  l: TLink;
  c: Integer = -1;
begin
  l := TLink(aSocket.UserData);
  if Assigned(l) then begin
    c := l.Number;
    Writeln('Target error: ', aMsg, ' on connection: ', c);
    l.ServerSocket.Disconnect;
    Exit;
  end;
  Writeln('Target error: ', aMsg);
end;

procedure TLinkServer.OnCliCo(aSocket: TLSocket);
var
  l: TLink;
begin
  l := TLink(aSocket.UserData);
  Writeln('Target connect on connection: ', l.Number);
  SendToTarget(l); // just in case they sent something before we connected to target
end;

procedure TLinkServer.OnCliCs(aSocket: TLSocket);
var
  l: TLink;
begin
  l := TLink(aSocket.UserData);
  SendToClient(l);
end;

procedure TLinkServer.OnCliRe(aSocket: TLSocket);
var
  l: TLink;
  s: string;
begin
  l := TLink(aSocket.UserData);
  if l.ClientTCP.GetMessage(s, aSocket) > 0 then begin
    l.ClientBuffer := l.ClientBuffer + s;
    SendToClient(l);
  end;
end;

procedure TLinkServer.OnCliDs(aSocket: TLSocket);
var
  l: TLink;
begin
  l := TLink(aSocket.UserData);
  Writeln('Target disconnect on connection: ', l.Number);
  l.ServerSocket.Disconnect; // will trigget OnDs eventually
end;

procedure TLinkServer.ConnectToTarget(aLink: TLink);
begin
  aLink.ClientTCP := TLTcp.Create(nil);
  aLink.ClientTCP.Eventer := FEventer;
  aLink.ClientTCP.OnError := @OnCliEr;
  aLink.ClientTCP.OnConnect := @OnCliCo;
  aLink.ClientTCP.OnCanSend := @OnCliCs;
  aLink.ClientTCP.OnReceive := @OnCliRe;
  aLink.ClientTCP.OnDisconnect := @OnCliDs;

  if aLink.ClientTCP.Connect(FHost, FPort) then
    aLink.ClientTCP.Iterator.UserData := aLink;
end;

procedure TLinkServer.SendToTarget(aLink: TLink);
var
  n: Integer;
begin
  if (Length(aLink.ServerBuffer) > 0) and (aLink.ClientTCP.Connected) then begin
    n := aLink.ClientTCP.SendMessage(aLink.ServerBuffer);
    if n > 0 then
      Delete(aLink.ServerBuffer, 1, n); // not efficient, rework later
  end;
end;

procedure TLinkServer.SendToClient(aLink: TLink);
var
  n: Integer;
begin
  if Length(aLink.ClientBuffer) > 0 then begin
    n := FServer.SendMessage(aLink.ClientBuffer, aLink.ServerSocket);
    Delete(aLink.ClientBuffer, 1, n); // not efficient, rework later
  end;
end;

constructor TLinkServer.Create(const aHost: string; const aServPort, aCliPort: Word);
begin
  inherited Create(nil);

  FHost := aHost; // main server host
  FPort := aCliPort; // main server port
  FEventer := BestEventerClass.Create; // common eventer

  FServer := TLTcp.Create(Self); // free server on self.free
  FServer.Eventer := FEventer;
  FServer.Timeout := 100;
  FServer.OnError := @OnEr;
  FServer.OnAccept := @OnAc;
  FServer.OnCanSend := @OnCs;
  FServer.OnReceive := @OnRe;
  FServer.OnDisconnect := @OnDs;

  FServer.Listen(aServPort);
end;

destructor TLinkServer.Destroy;
begin
  inherited Destroy;

  // AFTER inherited (which destroys the TCPs)
  FEventer.Free;
end;

procedure TLinkServer.Run;
begin
  Writeln('Linkserver running... press escape to stop');
  while not FQuit do begin
    FEventer.CallAction;
    if KeyPressed
    and (ReadKey = #27) then
      FQuit := True;
  end;
end;

end.

