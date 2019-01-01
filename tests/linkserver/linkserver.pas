program linkserver;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  uLinkTCP;

{$R *.res}

var
  ls: TLinkServer;
begin
  if Paramcount <> 3 then begin
    Writeln('Usage: ', ParamStr(0), ' <Server Host> <Port> <Server Port>');
    Exit;
  end;

  ls := TLinkServer.Create(ParamStr(1), StrToInt(ParamStr(2)), StrToInt(ParamStr(3)));
  ls.Run;
  ls.Free;
end.

