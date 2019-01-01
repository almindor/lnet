program ltclient;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, lTelnet, lNet;
  
{ This is a rather simple Telnet client,
  it accepts IP/url and port as arguments
  See file ltelnet.pas if you want to know
  how it works. }
  
type

  { TLTelnetTest }

  TLTelnetTest = class
   private
    FCon: TLTelnetClient; // THE telnet connection
    FQuit: Boolean;
    // the old known event, only Error is checked in this basic telnet client
    procedure OnError(const msg: string; aSocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TLTelnetTest }

constructor TLTelnetTest.Create;
begin
  FCon := TLTelnetClient.Create(nil);
  FCon.OnError := @OnError; // assign callbacks
  FCon.Timeout := 100; // responsive enough but won't hog cpu
end;

destructor TLTelnetTest.Destroy;
begin
  FCon.Free;
  inherited Destroy;
end;

procedure TLTelnetTest.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
  FQuit := True;
end;
  
procedure TLTelnetTest.Run;
var
  s, SendStr: string;
  c: Char;
  AD: string;
  PORT: Word;
begin
  if ParamCount > 1 then // get some info regarding host from commandline
    try
      AD := Paramstr(1);
      PORT := Word(StrToInt(Paramstr(2)));
    except
      Writeln('Invalid command line parameters');
      Exit;
    end else begin
      Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' IP PORT');
      Exit;
    end;

  FQuit := False;

  if FCon.Connect(AD, PORT) then begin // try connecting to other side
    Writeln('Connecting... press any key to cancel'); // if initial connect worked, inform user and wait
    repeat
      FCon.CallAction; // repeat this to get info
      if KeyPressed then
        Halt;
    until FCon.Connected; // wait until timeout or we actualy connected
    
    Writeln('Connected');
    SendStr := '';
    
    while not FQuit do begin // if we connected, do main loop
      if KeyPressed then begin
        c := ReadKey;
        case c of
          #27: FQuit := True; // user wants to quit
           #8: if Length(SendStr) > 0 then begin // delete last char
                 GotoXY(WhereX-1, WhereY);
                 Write(' ');
                 GotoXY(WhereX-1, WhereY);
                 SetLength(SendStr, Length(SendStr) - 1);
               end;
        else   if c = #13 then begin // if it's enter, send the message
                 Writeln;
                 FCon.SendMessage(SendStr + #13#10); // don't forget terminator
                 SendStr := '';
               end else begin
                 SendStr := SendStr + c;
                 if not FCon.OptionIsSet(TS_ECHO) then // if echo is not on, we need to do echo locally
                   Write(c);
               end;
        end;
      end;
      { Always see if we got new messages, this is somewhat different from others (EG: TLTcp). 
        Instead of callbacks/events, we check this, because in telnet, the library needs to check for 
        special sequences containing telnet info and process them before giving the data to user. 
        So telnet has it's own internal buffer, unlike most other lNet protocols/connections }
      if FCon.GetMessage(s) > 0 then 
        Write(s);
      FCon.CallAction; // don't forget to make the clock tick :)
    end;
  end;
  
  if FCon.GetMessage(s) > 0 then
    Write(s);
end;

var
  Telnet: TLTelnetTest;
begin
  Telnet := TLTelnetTest.Create;
  Telnet.Run;
  Telnet.Free;
end.

