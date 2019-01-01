program lSMTPClient;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Crt, lNet, lSMTP, lNetSSL;
  
type

  { TDoer }

  { TLSMTPClientTest }

  TLSMTPClientTest = class(TComponent)
   private
    FSMTP: TLSMTPClient; // this is THE smtp connection
    FSSL: TLSSLSession;
    FQuit: Boolean;  // helper for main loop
    function GetAnswer(const s: string; const MaskInput: Boolean = False): string;
    procedure PrintUsage(const Msg: string);
    { these events are used to see what happens on the SMTP connection. They are used via "CallAction".
      OnReceive will get fired whenever new data is received from the SMTP server.
      OnConnect will get fired when connecting to server ended with success.
      OnDisconnect will get fired when the other side closed connection gracefully.
      OnError will get called when any kind of net error occurs on the connection. }
    procedure OnReceive(aSocket: TLSocket);
    procedure OnConnect(aSocket: TLSocket);
    procedure OnDisconnect(aSocket: TLSocket);
    procedure OnError(const msg: string; aSocket: TLSocket);
    { This event is used to monitor TLS handshake. If SSL or TLS is used
      we will know if the handshake went ok if this event is fired on the seesion }
    procedure OnSSLConnect(aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;
    procedure Run; // main loop
  end;

{ TLSMTPClientTest }

function TLSMTPClientTest.GetAnswer(const s: string; const MaskInput: Boolean = False): string;
var
  c: Char;
begin
  Result := '';
  Write(s, ': ');
  while True do begin
    FSMTP.CallAction;
    if KeyPressed then begin
      c := ReadKey;
      case c of
        #27      : begin
                     FSMTP.Quit;
                     Exit('');
                   end;
        #13      : begin
                     Writeln;
                     Exit;
                   end;
        #8       : if Length(Result) > 0 then begin
                     SetLength(Result, Length(Result)-1);
                     GotoXY(WhereX-1, WhereY);
                     Write(' ');
                     GotoXY(WhereX-1, WhereY);
                   end;
        else begin
          Result := Result + c;
          if MaskInput then
            Write('*')
          else
            Write(c);
        end;
      end;
    end;
  end;
end;

procedure TLSMTPClientTest.PrintUsage(const Msg: string);
begin
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' ', Msg);
  Writeln('       -s is used to specify that an implicit SSL connection is required');
end;

procedure TLSMTPClientTest.OnReceive(aSocket: TLSocket);
var
  s: string;
begin
  if FSMTP.GetMessage(s) > 0 then // if we actually received something from SMTP server, write it for the user
    Write(s);
end;

procedure TLSMTPClientTest.OnConnect(aSocket: TLSocket);
begin
  Writeln('Connected'); // inform user of successful connect
end;

procedure TLSMTPClientTest.OnDisconnect(aSocket: TLSocket);
begin
  Writeln('Lost connection'); // inform user about lost connection
  FQuit := True; // since SMTP shouldn't do this unless we issued a QUIT, consider it to be end of session and quit program
end;

procedure TLSMTPClientTest.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg); // inform about error
end;

procedure TLSMTPClientTest.OnSSLConnect(aSocket: TLSocket);
begin
  Writeln('SSL handshake was successful');
end;

constructor TLSMTPClientTest.Create(aOwner: TComponent);
begin
  inherited;

  FQuit := False;
  
  FSSL := TLSSLSession.Create(Self);
  FSSL.SSLActive := False; // make it "off" by default
  FSSL.OnSSLConnect := @OnSSLConnect; // let's watch if TLS/SSL handshake is ok

  FSMTP := TLSMTPClient.Create(Self);
  FSMTP.Session := FSSL; // set the SSL session, so if it's a SSL/TLS SMTP we can use it
  FSMTP.Timeout := 100; // responsive enough, but won't hog CPU
  FSMTP.OnReceive := @OnReceive; // assign all events
  FSMTP.OnConnect := @OnConnect;
  FSMTP.OnDisconnect := @OnDisconnect;
  FSMTP.OnError := @OnError;
end;

procedure TLSMTPClientTest.Run;
var
  s, Addr, Subject, Sender, Recipients, Message: string;
  Port: Word = 25;
  c: Char;
begin
  if ParamCount > 0 then begin
    Addr := ParamStr(1); // get address and port from commandline args
    if ParamCount > 1 then try
      Port := Word(StrToInt(ParamStr(2)));
    except
      if (ParamCount > 2) and (ParamStr(3) = '-s') then begin
        Port := 25;
        FSSL.SSLActive := True;
      end else begin
        Writeln('Wrong argument #2');
        Halt(1);
      end;
    end;
    if (ParamCount > 2) and (ParamStr(3) = '-s') then
      FSSL.SSLActive := True;

    Write('Connecting to ', Addr, ':', Port, '... ');
    if FSMTP.Connect(Addr, Port) then repeat  // try to connect 
      FSMTP.CallAction;  // if inital connect went ok, wait for "acknowlidgment" or otherwise
      if KeyPressed then
        if ReadKey = #27 then
          FQuit := True;  // if user doesn't wish to wait, quit
    until FQuit or FSMTP.Connected; // if user quit, or we connected, then continue

    if not FQuit then begin // if we connected send HELO
      FSMTP.Ehlo;
      Writeln('Press escape to quit');
      Writeln('Press "a" to authenticate (AUTH LOGIN)');
      Writeln('Press "e" to issue additional EHLO');
      Writeln('Press "t" to STARTTLS');
      Writeln('Press "return" to compose an email');
    end;

    while not FQuit do begin // if we connected, do main loop
      FSMTP.CallAction; // main event mechanism, make sure to call periodicly and ASAP, or specify high timeout
      if KeyPressed then begin
        c := ReadKey;
        case c of // let's see what the user wants
          #27: if FSMTP.Connected then // and we're connected, then do a graceful QUIT, waiting for server to disconnect
                 FSMTP.Quit
               else
                 FQuit := True; // otherwise just quit from this end
          'a': begin // user wants to authenticate
                 s := GetAnswer('Username'); // first get username
                 FSMTP.AuthLogin(s, GetAnswer('Password', True)); // then get password (mask it) and pass to auth
               end;
          't': FSMTP.StartTLS; // technically we should now "wait" for TLS handshake to finish, but aaaaw skip it, we'll see if it succeeded via OnSSLConnect event
          'e': FSMTP.Ehlo;
        else begin // otherwise, user wants to compose email
          Sender := GetAnswer('From'); // get info about email from console input
          Recipients := GetAnswer('Recipients');
          Subject := GetAnswer('Subject');
          Message := GetAnswer('Data');
          FSMTP.SendMail(Sender, Recipients, Subject, Message); // send the mail given user data
        end;
        end; // case
      end;
    end;
  end else PrintUsage('<SMTP server hostname/IP> [port] [-s]');
end;

var
  SMTP: TLSMTPClientTest;
begin
  SMTP := TLSMTPClientTest.Create(nil);
  SMTP.Run;
  SMTP.Free;
end.
