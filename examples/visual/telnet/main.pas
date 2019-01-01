unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  lNetComponents, StdCtrls, lNet, Menus;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDisconnect: TButton;
    ButtonConnect: TButton;
    EditSend: TEdit;
    EditHost: TEdit;
    EditPort: TEdit;
    GroupBoxServer: TGroupBox;
    LabelPort: TLabel;
    LabelHost: TLabel;
    MainMenu1: TMainMenu;
    MenuItemAbout: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    Telnet: TLTelnetClientComponent;
    MemoText: TMemo;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure EditSendKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure TelnetConnect(aSocket: TLSocket);
    procedure TelnetDisconnect(aSocket: TLSocket);
    procedure TelnetError(const msg: string; aSocket: TLSocket);
    procedure TelnetReceive(aSocket: TLSocket);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

{ TFormMain }

procedure TFormMain.ButtonConnectClick(Sender: TObject);
begin
  Telnet.Connect(EditHost.Text, StrToInt(EditPort.Text));
end;

procedure TFormMain.ButtonDisconnectClick(Sender: TObject);
begin
  if Telnet.Connected then begin
    Telnet.Disconnect;
    MemoText.Append('Disconnected from server');
  end;
end;

procedure TFormMain.EditSendKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then begin
    Telnet.SendMessage(EditSend.Text + #13#10); // we must send the terminator so the message is considered complete!
    EditSend.Text := '';
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  EditHost.SetFocus; // for gtk2 so it's not confusing for the memo which is readonly
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('Copyright (c) 2008 by Aleš Katona. All rights deserved! :)', mtInformation, [mbOK], 0);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.TelnetConnect(aSocket: TLSocket);
begin
  MemoText.Append('Connected!');
end;

procedure TFormMain.TelnetDisconnect(aSocket: TLSocket);
begin
  MemoText.Append('Disconnected!');
end;

procedure TFormMain.TelnetError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
end;

procedure TFormMain.TelnetReceive(aSocket: TLSocket);
var
  s: string;
begin
  if Telnet.GetMessage(s) > 0 then
    MemoText.Append(s);
end;

initialization
  {$I main.lrs}

end.

