unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, lNetComponents, lNet, ExtCtrls, Menus;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDiconnect: TButton;
    ButtonConnect: TButton;
    ButtonListen: TButton;
    CheckBoxSSL: TCheckBox;
    SSL: TLSSLSessionComponent;
    LTCP: TLTCPComponent;
    LUDP: TLUDPComponent;
    EditPort: TEdit;
    EditIP: TEdit;
    LabelPort: TLabel;
    LabelHostName: TLabel;
    GBConnection: TRadioGroup;
    MainMenu1: TMainMenu;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemFile: TMenuItem;
    RBTCP6: TRadioButton;
    RBTCP: TRadioButton;
    RBUDP: TRadioButton;
    ButtonSend: TButton;
    EditSend: TEdit;
    MemoText: TMemo;
    TimerQuit: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LTCPComponentConnect(aSocket: TLSocket);
    procedure ListenButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DiconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LTCPComponentError(const msg: string; aSocket: TLSocket);
    procedure LTCPComponentAccept(aSocket: TLSocket);
    procedure LTCPComponentReceive(aSocket: TLSocket);
    procedure LTcpComponentDisconnect(aSocket: TLSocket);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure RBTCP6Change(Sender: TObject);
    procedure RBTCPChange(Sender: TObject);
    procedure RBUDPChange(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: char);
    procedure TimerQuitTimer(Sender: TObject);
  private
    FNet: TLConnection;
    FIsServer: Boolean;
    procedure SendToAll(const aMsg: string);
  public
    { public declarations }
  end; 

var
  FormMain: TFormMain;

implementation

uses
  lCommon;

{ TFormMain }

procedure TFormMain.ConnectButtonClick(Sender: TObject);
begin
  SSL.SSLActive := CheckBoxSSL.Checked;
  if FNet.Connect(EditIP.Text, StrToInt(EditPort.Text)) then
    FIsServer := False;
end;

procedure TFormMain.ListenButtonClick(Sender: TObject);
begin
  SSL.SSLActive := CheckBoxSSL.Checked;

  if FNet.Listen(StrToInt(EditPort.Text)) then begin
    MemoText.Append('Accepting connections');
    FIsServer := True;
  end;
end;

procedure TFormMain.LTCPComponentConnect(aSocket: TLSocket);
begin
  MemoText.Append('Connected to remote host');
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if FNet.Connected then begin
    CloseAction := caNone; // make sure we quit gracefuly
    FNet.Disconnect; // call disconnect (soft)
    TimerQuit.Enabled := True; // if time runs out, quit ungracefully
  end;
end;

procedure TFormMain.LTCPComponentError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.LTCPComponentAccept(aSocket: TLSocket);
begin
  MemoText.Append('Connection accepted');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.LTCPComponentReceive(aSocket: TLSocket);
var
  s: string;
begin
  if aSocket.GetMessage(s) > 0 then begin
    MemoText.Append(s);
    MemoText.SelStart := Length(MemoText.Lines.Text);

    if FNet is TLUdp then begin // echo to sender if UDP
      if FIsServer then
        FNet.SendMessage(s);
    end else if FIsServer then // echo to all if TCP
      SendToAll(s);
  end;
end;

procedure TFormMain.LTcpComponentDisconnect(aSocket: TLSocket);
begin
  MemoText.Append('Connection lost');
  MemoText.SelStart := Length(MemoText.Lines.Text);
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('TCP/UDP example copyright(c) 2005-2009 by Ales Katona. All rights deserved ;)',
             mtInformation, [mbOK], 0);
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.SendButtonClick(Sender: TObject);
begin
  if Length(EditSend.Text) > 0 then begin

    if FIsServer then begin
      SendToAll(EditSend.Text);
      MemoText.Append(EditSend.Text);
    end else
      FNet.SendMessage(EditSend.Text);
    
    EditSend.Text := '';
  end;
end;

procedure TFormMain.DiconnectButtonClick(Sender: TObject);
begin
  FNet.Disconnect;
  MemoText.Append('Disconnected');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FNet := LTCP;
  FIsServer := False;
end;

procedure TFormMain.RBTCP6Change(Sender: TObject);
begin
  FNet.Disconnect;
  FNet := LTCP;
  LTCP.SocketNet := LAF_INET6;
  if EditIP.Text = 'localhost' then
    EditIP.Text := '::1';
end;

procedure TFormMain.RBTCPChange(Sender: TObject);
begin
  FNet.Disconnect;
  FNet := LTCP;
  LTCP.SocketNet := LAF_INET;
  if EditIP.Text = '::1' then
    EditIP.Text := 'localhost';
end;

procedure TFormMain.RBUDPChange(Sender: TObject);
begin
  FNet.Disconnect;
  FNet := LUDP;
  if EditIP.Text = '::1' then
    EditIP.Text := 'localhost';
end;

procedure TFormMain.SendEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    SendButtonClick(Sender);
end;

procedure TFormMain.TimerQuitTimer(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.SendToAll(const aMsg: string);
var
  n: Integer;
begin
  if FNet is TLUdp then begin // UDP, use broadcast
    n := TLUdp(FNet).SendMessage(aMsg, LADDR_BR);
    if n < Length(aMsg) then
      MemoText.Append('Error on send [' + IntToStr(n) + ']');
  end else begin // TCP
    FNet.IterReset; // start at server socket
    while FNet.IterNext do begin // skip server socket, go to clients only
      n := FNet.SendMessage(aMsg, FNet.Iterator);
      if n < Length(aMsg) then
        MemoText.Append('Error on send [' + IntToStr(n) + ']');
    end;
  end;
end;

initialization
  {$I main.lrs}

end.

