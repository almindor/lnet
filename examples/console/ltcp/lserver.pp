program lserver;

{$mode objfpc}{$H+}

uses
  Classes, Crt, SysUtils, lNet;
  
type

{ TLTCPTest }

  TLTCPTest = class
   private
    FCon: TLTCP; // THE server connection
   {  these are all events which happen on our server connection. They are called inside CallAction
      OnEr gets fired when a network error occurs.
      OnAc gets fired when a new connection is accepted on the server socket.
      OnRe gets fired when any of the server sockets receives new data.
      OnDs gets fired when any of the server sockets disconnects gracefully.
   }
    procedure OnEr(const msg: string; aSocket: TLSocket);
    procedure OnAc(aSocket: TLSocket);
    procedure OnRe(aSocket: TLSocket);
    procedure OnDs(aSocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run; // main loop with CallAction
  end;

procedure TLTCPTest.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);  // if error occured, write it explicitly
end;

procedure TLTCPTest.OnAc(aSocket: TLSocket);
begin
  Writeln('Connection accepted from ', aSocket.PeerAddress); // on accept, write whom we accepted
end;

procedure TLTCPTest.OnRe(aSocket: TLSocket);
var
  s: string;
  n: Integer;
begin
  if aSocket.GetMessage(s) > 0 then begin // if we received anything (result is in s)
    Writeln('Got: "', s, '" with length: ', Length(s)); // write message and it's length
    FCon.IterReset; // now it points to server socket
    while FCon.IterNext do begin // while we have clients to echo to
      n := FCon.SendMessage(s, FCon.Iterator);
      if n < Length(s) then // try to send to each of them
        Writeln('Unsuccessful send, wanted: ', Length(s), ' got: ', n); // if send fails write error
    end;
  end;
end;

procedure TLTCPTest.OnDs(aSocket: TLSocket);
begin
  Writeln('Lost connection'); // write info if connection was lost
end;

constructor TLTCPTest.Create;
begin
  FCon := TLTCP.Create(nil); // create new TCP connection
  FCon.OnError := @OnEr;     // assign all callbacks
  FCon.OnReceive := @OnRe;
  FCon.OnDisconnect := @OnDs;
  FCon.OnAccept := @OnAc;
  FCon.Timeout := 100; // responsive enough, but won't hog cpu
  FCon.ReuseAddress := True;
end;

destructor TLTCPTest.Destroy;
begin
  FCon.Free; // free the TCP connection
  inherited Destroy;
end;

procedure TLTCPTest.Run;
var
  Quit: Boolean; // main loop control
  Port: Word;    // the port to connect to
begin
  if ParamCount > 0 then begin // we need one argument
    try
      Port := Word(StrToInt(ParamStr(1))); // try to parse port from argument
    except
      on e: Exception do begin
        Writeln(e.message);
        Halt;
      end;
    end;
    Quit := false;

    if FCon.Listen(Port) then begin // if listen went ok
      Writeln('Server running!');
      Writeln('Press ''escape'' to quit, ''r'' to restart');
      repeat
        FCon.Callaction; // eventize the lNet
        if Keypressed then // if user provided input
          case readkey of
           #27: quit := true; // if he pressed "escape" then quit
           'r': begin       // if he pressed 'r' then restart the server
                  Writeln('Restarting...');
                  FCon.Disconnect;
                  FCon.Listen(Port);
                  Quit := false;
                end;
          end;
      until Quit; // until user quit
    end; // listen
  end else Writeln('Usage: ', ParamStr(0), ' <port>');
end;

var
  TCP: TLTCPTest;
begin
  TCP := TLTCPTest.Create;
  TCP.Run;
  TCP.Free;
end.

