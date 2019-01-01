unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lNetComponents, lNet;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    procedure OnCo(aSocket: TLSocket);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  tcp: TLTCPComponent;
begin
  tcp := TLTCPComponent.Create(nil);
  tcp.OnConnect := @OnCo;
  tcp.Connect('127.0.0.1', 4665);
end;

procedure TForm1.OnCo(aSocket: TLSocket);
begin
  if Assigned(aSocket) then
    aSocket.Creator.Free;

  Edit1.Text := IntToStr(StrToInt(Edit1.Text) + 1);

  Button1Click(nil);
end;

end.

