program stress1;

{ Stress test creation/free cycle, as well as server
  disconnect/connect on the other end. Use with TCP examples }

{$mode objfpc}{$H+}

uses
  Classes, Crt, SysUtils, lNet, lEvents;

var
  TCP: TLTCP;
  i: Integer;
begin
  i := 0;
  while true do
  begin
    TCP := TLTCP.Create(nil);
    TCP.Connect('127.0.0.1', 4665);
    TCP.Free;
    Inc(i);
    Writeln(IntToStr(i));
  end;
end.

