program lFTPClient;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Crt, lFTP, lNet;
  
type

  { TClient }

  TClient = class
   private
    { These are the events which will get called when something happens on a socket.
      OnConnect will get called when the client connection finished connecting successfuly.
      OnReceive will get called when any data is received on the data stream (the one for files).
      OnControl will get called when any data/info is received on the command stream (the one with command/responses for server.
      OnSent will get called after sending big pieces of data to the other side, it'll report the progress indicated by "Bytes".
      OnError will get called when any network error occurs, like ECONNRESET }
    procedure OnConnect(aSocket: TLSocket);
    procedure OnReceive(aSocket: TLSocket);
    procedure OnControl(aSocket: TLSocket);
    procedure OnSent(aSocket: TLSocket; const Bytes: Integer);
    procedure OnError(const msg: string; aSocket: TLSocket);
   protected
    FCon: TLFTPClient;  // the FTP connection itself
    FConnected: Boolean;
    FQuit: Boolean;     // used as controller of the main loop
    FFile: TFileStream; // file stream to save "GET" files into
    function UserString: string;
    function GetAnswer(const s: string; const NoEcho: Boolean = False): string;
    procedure PrintHelp;
   public
    constructor Create;
    destructor Destroy; override;
    procedure Run(const Host: string; const Port: Word); // this is where the main loop is
  end;

procedure TClient.OnConnect(aSocket: TLSocket);
begin
  FConnected := True;
  Writeln('Connected succesfuly');
end;

procedure TClient.OnReceive(aSocket: TLSocket);
const
  BUFFER_SIZE = 65536; // usual maximal recv. size defined by OS, no problem if it's more or less really
var
  n: Integer;
  Buf: array[0..BUFFER_SIZE-1] of Byte;
begin
  if FCon.CurrentStatus = fsRetr then begin // if we're in getting mode
    Write('.'); // inform of progress
    n := FCon.GetData(Buf, BUFFER_SIZE); // get data, n is set to the amount
    if (n = 0)
    and (not FCon.DataConnection.Connected) then // if we got disconnected then
      FreeAndNil(FFile)  // close the file
    else
      FFile.Write(Buf, n); // otherwise, write the data to file
  end else
    Write(FCon.GetDataMessage); // if we got data and we weren't in getting mode, write it on the screen as FTP info
end;

procedure TClient.OnControl(aSocket: TLSocket);
var
  s: string;
begin
  if FCon.GetMessage(s) > 0 then // if we got some new message about FTP status, write it
    Writeln(s);
end;

procedure TClient.OnSent(aSocket: TLSocket; const Bytes: Integer);
begin
  Write('.'); // inform on progress, very basic
end;

procedure TClient.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg); // just write the error out
end;

constructor TClient.Create;
begin
  FConnected := False; // we're not connected yet
  FCon := TLFTPClient.Create(nil);
  FCon.Timeout := 50; // 50 milliseconds is nice to save CPU but fast enough to be responsive to humans
  FCon.OnConnect := @OnConnect; // assign all events
  FCon.OnReceive := @OnReceive;
  FCon.OnControl := @OnControl;
  FCon.OnSent := @OnSent;
  FCon.OnError := @OnError;
end;

destructor TClient.Destroy;
begin
  FCon.Free;
end;

procedure TClient.PrintHelp;
begin
  Writeln('lNet example FTP client copyright (c) 2005 by Ales Katona');
  Writeln('Commands:');
  Writeln('?   - Print this help');
  Writeln('ESC - Quit');
  Writeln('l - List remote directory');
  Writeln('L - Nlst remote directory (lists only files sometimes)');
  Writeln('g/G - Get remote file');
  Writeln('p/P - Put local file');
  Writeln('b/B - Change mode (binary on/off)');
  Writeln('s/S - Get server system info');
  Writeln('h/H - Print server help');
  Writeln('x/X - Print current working directory');
  Writeln('c/C - Change remote directory');
  Writeln('m/M - Create new remote directory');
  Writeln('r/R - Remove remote directory');
  Writeln('n/N - Rename remote file/directory');
  Writeln('d/D - Delete remote file');
  Writeln('e/E - Echo on/off');
  Writeln('f/F - Feature list');
end;

procedure TClient.Run(const Host: string; const Port: Word);
var
  s, Name, Pass, Dir: string;
begin
  Dir := ExtractFilePath(ParamStr(0)); // get current working directory
  FFile := nil; // set "GET" file to nothing for now
  Name := GetAnswer('USER [' + GetEnvironmentVariable(UserString) + ']', False); // get info about username and pass from console
  if Length(Name) = 0 then // if username wasn't set, presume it's the same as environment var for USER
    Name := GetEnvironmentVariable('USER');
  Pass := GetAnswer('PASS', True); // get password from user console

  if FCon.Connect(Host, PORT) then begin // if initial connect call worked
    Writeln('Connecting... press escape to cancel'); // write info about status
    repeat 
      FCon.CallAction; // repeat this until we either get connected, fail or user decides to quit manually
      if KeyPressed then
        if ReadKey = #27 then Exit;
    until FConnected;
  end else Halt;

  if FCon.Authenticate(Name, Pass) then begin // if authentication with server passed
    FCon.Binary := True; // set binary mode, others are useless anyhow
    s := '';
    Writeln('Press "?" for help'); // just info
    while not FQuit do begin // main loop is here, for events and user interaction
      if KeyPressed then case ReadKey of // this is all user interaction stuff
             #27: FQuit := True; // escape quits the client
             '?': PrintHelp;
        'g', 'G': begin // "GET" file, this means:
                    s := GetAnswer('Filename'); // we need to find out which file from user
                    if Length(s) > 0 then begin // then if it was valid info
                      s := ExtractFileName(s); // see if the file exists already on local disk/dir 
                      if FileExists(Dir + s) then
                        DeleteFile(Dir + s); // if so, delete it (I know it's not the best idea, but it's a simple client)
                      FreeAndNil(FFile); // ensure any old file/data is not used
                      FFile := TFileStream.Create(Dir + s, fmOpenWrite or fmCreate); // create new file for the incomming one
                      FCon.Retrieve(s); // send request for the file over FTP control connnection
                    end;
                  end;
             'l': FCon.List; // and send request for file listing
             'L': FCon.Nlst; // send request for new type of file listing
        'p', 'P': begin
                     s := GetAnswer('Filename'); // see which file the user wants to PUT on the server
                    if FileExists(Dir + s) then // if it exits locally
                      FCon.Put(Dir + s) // then send it over
                    else
                      Writeln('No such file "', s, '"'); // otherwise inform user of their error
                  end;
        'b', 'B': FCon.Binary := not FCon.Binary; // set or unset binary
        's', 'S': FCon.SystemInfo; // request systeminfo from server
        'h', 'H': FCon.Help(GetAnswer('Help verb')); // request help from server, argument input from console
        'x', 'X': FCon.PresentWorkingDirectory; // get current working directory info from server
        'c', 'C': FCon.ChangeDirectory(GetAnswer('New dir')); // change directory, new dir is read from user console
        'm', 'M': FCon.MakeDirectory(GetAnswer('New dir')); // make a new directory on server, dirname is read from user console
        'n', 'N': FCon.Rename(GetAnswer('From'), GetAnswer('To')); // rename a file, old and new names read from user console
        'r', 'R': FCon.RemoveDirectory(GetAnswer('Dirname')); // delete a directory on server, name read from user console
        'd', 'D': FCon.DeleteFile(GetAnswer('Filename')); // delete a file on server, name read from user console
        'e', 'E': FCon.Echo := not FCon.Echo; // set echo mode on/off
        'f', 'F': FCon.ListFeatures; // get all FTP features from server
      end;
      FCon.CallAction; // this needs to be called ASAP, in a loop. It's the magic function which makes all the events work :)
    end;
  end else FCon.GetMessage(s); // if the authentication failed, get reason from server
  if Length(s) > 0 then // if reason was given, write it
    Write(s);
  FreeAndNil(FFile); // make sure not to leak memory
end;

function TClient.UserString: string;
begin
  {$ifdef WINDOWS}
    Result := 'USERNAME';
  {$else}
    Result := 'USER';
  {$endif}
end;

function TClient.GetAnswer(const s: string; const NoEcho: Boolean = False): string;
var
  c: Char;
begin
  Result := '';
  Write(s, ': ');
  while True do begin
    FCon.CallAction;
    if KeyPressed then begin
      c := ReadKey;
      case c of
        #13, #27 : begin
                     Writeln;
                     Exit;
                   end;
        #8       : if Length(Result) > 0 then begin
                     SetLength(Result, Length(Result)-1);
                     if not NoEcho then begin
                       GotoXY(WhereX-1, WhereY);
                       Write(' ');
                       GotoXY(WhereX-1, WhereY);
                     end;
                   end;
        else begin
          Result := Result + c;
          if not NoEcho then
            Write(c);
        end;
      end;
    end;
  end;
end;

var
  aClient: TClient;
  IP: string;
  Port: Word = 21;
begin
  if Paramcount > 0 then begin
    IP := ParamStr(1);
    PORT := 21;
    if ParamCount > 1 then try
      Port := Word(StrToInt(ParamStr(2)));
    except
      on e: Exception do begin
        Writeln(e.message);
        Halt;
      end;
    end;

    aClient := TClient.Create;
    aClient.Run(IP, Port);
    aClient.Free;
  end else Writeln('Usage: ', ParamStr(0), ' IP [PORT]');
end.

