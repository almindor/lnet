program lclient;

{$mode objfpc}{$H+}

uses
  Classes, Crt, SysUtils, lNet;
  
type

  { TLTCPTest }

  TLTCPTest = class
   private
    FQuit: boolean;
    FCon: TLTcp; // the connection
   {  these are all events which happen on our server connection. They are called inside CallAction
      OnEr gets fired when a network error occurs.
      OnRe gets fired when any of the server sockets receives new data.
      OnDs gets fired when any of the server sockets disconnects gracefully.
   }
    procedure OnDs(aSocket: TLSocket); 
    procedure OnRe(aSocket: TLSocket);
    procedure OnEr(const msg: string; aSocket: TLSocket);
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

// implementation

procedure TLTCPTest.OnDs(aSocket: TLSocket);
begin
  Writeln('Lost connection');
end;

procedure TLTCPTest.OnRe(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then
    Writeln(s);
end;

procedure TLTCPTest.OnEr(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg); // if error occured, write it
  FQuit := true;             // and quit ASAP
end;

constructor TLTCPTest.Create;
begin
  FCon := TLTCP.Create(nil); // create new TCP connection with no parent component
  FCon.OnError := @OnEr; // assign callbacks
  FCon.OnReceive := @OnRe;
  FCOn.OnDisconnect := @OnDs;
  FCon.Timeout := 100; // responsive enough, but won't hog cpu
end;

destructor TLTCPTest.Destroy;
begin
  FCon.Free; // free the connection
  inherited Destroy;
end;

procedure TLTCPTest.Run;
var
  s, Address: string; // message-to-send and address
  c: char;
  Port: Word;

begin
  if ParamCount > 1 then begin // we need atleast one parameter
    try
      Address := ParamStr(1); // get address from argument
      Port := Word(StrToInt(ParamStr(2))); // try to parse port from argument
    except
      on e: Exception do begin
        Writeln(e.message); // write error on failure
        Halt;
      end;
    end;

    s := '';

    if FCon.Connect(Address, Port) then begin // if connect went ok
      Writeln('Connecting... [press any key to cancel] ');
      FQuit := False;
      repeat
        FCon.CallAction; // wait for "OnConnect"
        if KeyPressed then // if user pressed anything, quit waiting
          FQuit := True;
      until FCon.Connected or FQuit;
      
      if not FQuit then begin // if we connected succesfuly
        Writeln('Connected, write your messages, send with ''return'' or press ''escape'' to quit');
        repeat
          if Keypressed then begin // if user provided inpur
            c := Readkey; // get key pressed
            case c of
              #8:  begin      // backspace deletes from message-to-send
                     if Length(s) > 1 then
                       Delete(s, Length(s)-1, 1)
                     else
                       s := '';
                     GotoXY(WhereX-1, WhereY);
                     Write(' ');
                     GotoXY(WhereX-1, WhereY);
                   end;
              #10,
              #13: begin // both "return" and "enter" send the message
                     FCon.SendMessage(s);
                     s := '';
                     Writeln;
                   end;
              #27: FQuit := true; // "escape" quits
              else begin
                s := s + c; // other chars get added to "message-to-send"
                Write(c); // and written so we know what we want to send
              end;
            end;
          end;
          FCon.Callaction; // eventize lNet loop
        until FQuit; // repeat until user quit or error happened
      end; // if not FQuit
    end; // if Connect
  end else Writeln('Usage: ', ParamStr(0), ' <address> <port>');
end;

var
  TCP: TLTCPTest;
begin
  TCP := TLTCPTest.Create;
  TCP.Run;
  TCP.Free;
end.
