unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Menus, ExtCtrls, LCLType,
  lMimeWrapper, lNetComponents, lSMTP, lNet;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonAuth: TButton;
    ButtonTLS: TButton;
    ButtonSend: TButton;
    ButtonConnect: TButton;
    CheckBoxSSL: TCheckBox;
    GBConnection: TGroupBox;
    EditFrom: TEdit;
    EditSubject: TEdit;
    EditTo: TEdit;
    GBEmail: TGroupBox;
    LabelAtt: TLabel;
    ListBoxAttachments: TListBox;
    SSLSession: TLSSLSessionComponent;
    MainMenu: TMainMenu;
    LabelSubject: TLabel;
    LabelTo: TLabel;
    LabelFrom: TLabel;
    MemoText: TMemo;
    MenuItemLogs: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemAdd: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    OD: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenuAttachments: TPopupMenu;
    ProgressBar: TProgressBar;
    SMTP: TLSMTPClientComponent;
    EditServer: TEdit;
    EditPort: TEdit;
    LabelPort: TLabel;
    LabelServer: TLabel;
    SB: TStatusBar;
    TimerQuit: TTimer;
    procedure ButtonAuthClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonTLSClick(Sender: TObject);
    procedure CheckBoxSSLChange(Sender: TObject);
    procedure EditFromKeyPress(Sender: TObject; var Key: Char);
    procedure EditServerKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemAddClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemLogsClick(Sender: TObject);
    procedure SMTPConnect(aSocket: TLSocket);
    procedure SMTPDisconnect(aSocket: TLSocket);
    procedure SMTPError(const msg: string; aSocket: TLSocket);
    procedure SMTPReceive(aSocket: TLSocket);
    procedure SMTPFailure(aSocket: TLSocket; const aStatus: TLSMTPStatus);
    procedure SMTPSent(aSocket: TLSocket; const Bytes: Integer);
    procedure SMTPSuccess(aSocket: TLSocket; const aStatus: TLSMTPStatus);
    procedure SSLSessionSSLConnect(aSocket: TLSocket);
    procedure TimerQuitTimer(Sender: TObject);
  private
    FDataSent: Int64;
    FDataSize: Int64;
    FMimeStream: TMimeStream;
    FQuit: Boolean; // to see if we force quitting
    procedure RefreshFeatureList;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  Logs;

{ TMainForm }

procedure TMainForm.SMTPConnect(aSocket: TLSocket);
begin
  SB.SimpleText := 'Connected to server...';
  FormLogs.MemoLogs.Append(SB.SimpleText);
  ButtonSend.Enabled := SMTP.Connected;
  ButtonConnect.Caption := 'Disconnect';
end;

procedure TMainForm.SMTPDisconnect(aSocket: TLSocket);
begin
  ButtonAuth.Visible := False;
  ButtonTLS.Visible := False;
  SB.SimpleText := 'Disconnected from server';
  FormLogs.MemoLogs.Append(SB.SimpleText);
  ButtonSend.Enabled := SMTP.Connected;
  ButtonConnect.Caption := 'Connect';
end;

procedure TMainForm.SMTPError(const msg: string; aSocket: TLSocket);
begin
  SMTPDisconnect(nil);
  SB.SimpleText := msg;
  FormLogs.MemoLogs.Append(msg);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SMTP.OnError := @SMTPError;
  FMimeStream := TMimeStream.Create;
  FMimeStream.AddTextSection(''); // for the memo
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FMimeStream.Free;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('SMTP example copyright (c) 2006-2008 by Ales Katona. All rights deserved ;)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.MenuItemAddClick(Sender: TObject);
begin
  if OD.Execute then
    if FileExists(OD.FileName) then begin
      FMimeStream.AddFileSection(OD.FileName);
      ListBoxAttachments.Items.Add(ExtractFileName(OD.FileName));
    end;
end;

procedure TMainForm.MenuItemDeleteClick(Sender: TObject);
begin
  if  (ListBoxAttachments.ItemIndex >= 0)
  and (ListBoxAttachments.ItemIndex < FMimeStream.Count - 1) then begin
    FMimeStream.Delete(ListBoxAttachments.ItemIndex + 1); // 0th is the text of memo
    ListBoxAttachments.Items.Delete(ListBoxAttachments.ItemIndex);
  end;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MenuItemLogsClick(Sender: TObject);
begin
  FormLogs.Show;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
begin
  if (not SMTP.Connected) and (ButtonConnect.Caption = 'Connect') then begin
    SMTP.Connect(EditServer.Text, Word(StrToInt(EditPort.Text)));
    ButtonConnect.Caption := 'Connecting';
    SB.SimpleText := 'Connecting...';
  end else if ButtonConnect.Caption = 'Connecting' then begin
    SMTP.Disconnect;
    ButtonConnect.Caption := 'Connect';
    SB.SimpleText := 'Aborted connect!';
  end else
    SMTP.Quit; // server will respond and we'll make a clean disconnect (see SMTP rfc)
end;

procedure TMainForm.ButtonAuthClick(Sender: TObject);
var
  aName: string = '';
  aPass: string = '';
begin
  if InputQuery('Name', 'Please specify login name', False, aName) then
    if InputQuery('Password', 'Please specify login password', True, aPass) then begin
      if SMTP.HasFeature('AUTH LOGIN') then // use login if possible
        SMTP.AuthLogin(aName, aPass)
      else if SMTP.HasFeature('AUTH PLAIN') then // fall back to plain if possible
        SMTP.AuthPlain(aName, aPass);
    end;
end;

procedure TMainForm.ButtonSendClick(Sender: TObject);
begin
  if Length(EditFrom.Text) < 6 then
    SB.SimpleText := '"Mail from" info is missing or irrelevant'
  else if Length(EditTo.Text) < 6 then
    SB.SimpleText := '"Mail to" info is missing or irrelevant'
  else begin
    FMimeStream.Reset; // make sure we can read it again
    TMimeTextSection(FMimeStream[0]).Text := MemoText.Text; // change to text

    ProgressBar.Position := 0;
    FDataSent := 0;
    FDataSize := FMimeStream.Size; // get size to send
    
    SMTP.SendMail(EditFrom.Text, EditTo.Text, EditSubject.Text, FMimeStream); // send the stream
  end;
end;

procedure TMainForm.ButtonTLSClick(Sender: TObject);
begin
  SMTP.StartTLS;
end;

procedure TMainForm.CheckBoxSSLChange(Sender: TObject);
begin
  SSLSession.SSLActive := CheckBoxSSL.Checked;
  if CheckBoxSSL.Checked then begin
    if EditPort.Text = '25'then
      EditPort.Text := '465';
  end else begin
    if EditPort.Text = '465' then
      EditPort.Text := '25';
  end;
end;

procedure TMainForm.EditFromKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    ButtonSendClick(nil);
end;

procedure TMainForm.EditServerKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    ButtonConnectClick(nil);
end;

procedure TMainForm.SMTPReceive(aSocket: TLSocket);
var
  s, st: string;
begin
  if SMTP.GetMessage(s) > 0 then begin
    st := StringReplace(s, #13, '', [rfReplaceAll]);
    st := StringReplace(st, #10, '', [rfReplaceAll]);
    SB.SimpleText := st;
    FormLogs.MemoLogs.Append(s);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;

  if not FQuit and SMTP.Connected then begin
    CloseAction := caNone; // make sure we quit gracefuly
    SMTP.Quit; // the quit success/failure CBs will close our form
    TimerQuit.Enabled := True; // if this runs out, quit ungracefully
  end;
end;

procedure TMainForm.SMTPFailure(aSocket: TLSocket;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssCon,
    ssEhlo: RefreshFeatureList;
    ssData: begin
              MessageDlg('Error sending message', mtError, [mbOK], 0);
              SMTP.Rset;
            end;
    ssQuit: begin
              SMTP.Disconnect;
              Close;
            end;
  end;
end;

procedure TMainForm.SMTPSent(aSocket: TLSocket; const Bytes: Integer);
begin
  FDataSent := FDataSent + Bytes;
  
  ProgressBar.Position := Round((FDataSent / FDataSize) * 100);
end;

procedure TMainForm.SMTPSuccess(aSocket: TLSocket;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssCon : begin
              if SMTP.HasFeature('EHLO') then // check for EHLO support
                SMTP.Ehlo(EditServer.Text)
              else
                SMTP.Helo(EditServer.Text);
            end;
    ssEhlo: RefreshFeatureList;
    
    ssAuthLogin,
    ssAuthPlain : ButtonAuth.Visible := False;

    ssData: MessageDlg('Message sent successfuly', mtInformation, [mbOK], 0);
    ssQuit: begin
              SMTP.Disconnect;
              if TimerQuit.Enabled then
                Close;
            end;
  end;
end;

procedure TMainForm.SSLSessionSSLConnect(aSocket: TLSocket);
begin
  SB.SimpleText := 'TLS handshake complete';
  FormLogs.MemoLogs.Append(SB.SimpleText);
  { re-ehlo to get new feature list, do this here, not on SMTPSuccess because
    handshake can take time and while it's in progress, sending of any data
    including SMTP commands is going to fail and disconnect us! }
  SMTP.Ehlo;
end;

procedure TMainForm.TimerQuitTimer(Sender: TObject);
begin
  FQuit := True;
  Close;
end;

procedure TMainForm.RefreshFeatureList;
begin
  with FormLogs do begin
    ListBoxFeatures.Clear;
    ListBoxFeatures.Items.AddStrings(MainForm.SMTP.FeatureList);
  end;
  ButtonTLS.Visible := SMTP.HasFeature('STARTTLS');
  ButtonAuth.Visible := SMTP.HasFeature('AUTH PLAIN') or SMTP.HasFeature('AUTH LOGIN');
end;

initialization
  {$I main.lrs}

end.

